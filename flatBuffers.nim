import std / macros

import macros

import std / tables

template debug(body: untyped): untyped =
  when defined(DEBUG_FLATBUFFERS):
    body

## The variant logic is inspired by Flatty's approach to variants.
## It's unfinished though. Once I have some time I'll probably fix
## it to work correctly.
proc getRecList(n: NimNode): NimNode =
  var typ = n.getTypeImpl()
  while typ.typeKind == ntyTypeDesc:
    typ = typ[1].getTypeImpl()
  case typ.typeKind
  of ntyObject, ntyGenericInst:
    if typ.kind == nnkObjectTy:
      result = typ[2]
    else:
      result = typ
  of ntyTuple: result = typ
  else:
    doAssert false, "Invalid type kind: " & $typ.typeKind

iterator variantFields(typ: NimNode): NimNode =
  for ch in typ:
    case ch.kind
    of nnkIdentDefs: discard
    of nnkRecCase:
      yield ch[0][0]
    else:
      discard #echo ch.treerepr

macro isVariant(typ: typed, field: static string): untyped =
  ## Checks if `field` refers to a variant object field
  let typImpl = getRecList(typ)
  result = newLit false
  for f in variantFields(typImpl):
    if f.strVal == field:
      result = newLit true

macro isVariantObj(typ: typed): untyped =
  ## Checks if `field` refers to a variant object field
  let typImpl = getRecList(typ)
  if typImpl.kind != nnkRecList: return newLit(false)
  result = newLit false
  for f in variantFields(typImpl):
    return newLit(true)

macro getVariantFields(typ: typed): untyped =
  let typImpl = getRecList(typ)
  result = nnkBracket.newTree()
  for f in variantFields(typImpl):
    result.add newLit(f.strVal)

macro accessField(arg: typed, field: static string): untyped =
  result = nnkDotExpr.newTree(arg, ident(field))

macro getVariantField(n: typed): untyped =
  ## Only for single fields / first field
  let typImpl = getRecList(n)
  result = nnkBracket.newTree()
  for f in variantFields(typImpl):
    return nnkDotExpr.newTree(n, f)

proc getVariantFieldName(n: NimNode): NimNode =
  ## Only for single fields / first field
  let typImpl = getRecList(n)
  result = nnkBracket.newTree()
  for f in variantFields(typImpl):
    return ident(f.strVal)

macro new*(v: typed, d: typed): untyped =
  ## Creates a new object variant with the discriminator field.
  ## -> This is taken from `flatty`!
  let
    typ = v.getTypeInst()
    fieldName = getVariantFieldName(v)
  return quote do:
    `v` = `typ`(`fieldName`: `d`)

template address(x: typed): untyped =
  when (NimMajor, NimMinor, NimPatch) < (1, 9, 0):
    unsafeAddr(x)
  else:
    addr(x)

type
  BufferObj* {.acyclic.} = object
    size*: int
    owned*: bool
    data*: pointer
    offsetOf*: int
    children*: seq[Buffer]
  Buffer* = ref BufferObj

  SimpleTypes* = SomeNumber | char | bool | enum

proc `=copy`(dest: var BufferObj, source: BufferObj) {.error: "Copying a buffer is not allowed at the moment.".}

proc `=destroy`(x: var BufferObj) =
  `=destroy`(x.children)
  if x.owned and x.data != nil:
    deallocShared(x.data)
    x.data = nil

proc `$`*(b: Buffer): string =
  result = "Buffer(size: " & $b.size & ", owned: " & $b.owned & ", data: " & $b.data.repr & ", offsetOf: " & $b.offsetOf & ", children: " & $b.children.len & ")"

proc newBuf*(size: int, owned = true): Buffer =
  if size > 0:
    result = Buffer(owned: owned, size: size, data: allocShared0(size), offsetOf: 0)
  else:
    result = Buffer(owned: false, size: size, data: nil, offsetOf: 0)

proc newBuf*(buf: pointer, size: int): Buffer =
  result = Buffer(owned: false, size: size, data: buf, offsetOf: 0)

proc newBuf*[T](s: seq[T]): Buffer =
  result = Buffer(owned: false,
                  data: (if s.len > 0: cast[pointer](addr(s[0]))
                         else: nil),
                  offsetOf: 0)

proc toString*(b: Buffer): string =
  ## Copies the content of the `Buffer` to a normal Nim string to easily pass the data around.
  ## This is us using `string` as a `seq[byte]`! We'd use `seq[byte]` directly, but the stdlib
  ## makes that unwieldy.
  result = newString(b.size)
  if b.size > 0:
    copyMem(result[0].addr, b.data, b.size)

proc fromString*(s: string): Buffer =
  ## Copies over the string buffer to a `Buffer`.
  ## Use `newBuf` with a `pointer` argument (`s[0].addr`) if you want to
  ## avoid a copy.
  result = newBuf(s.len)
  if s.len > 0:
    copyMem(result.data, s[0].addr, s.len)

proc getSize*[T: object | tuple](x: T): int
#proc getSize*[T: ref object](x: T): int
proc getSize*[T: SimpleTypes](x: T): int =
  result = sizeof(T)

import typetraits
proc getSize*[T: pointer|ptr](x: T): int =
  result = sizeof(T)

proc getSize*[T: proc](x: T): int =
  ## We do not attempt to serialize a proc!
  result = 0

proc getSize*[T: array](x: T): int =
  result = sizeof(T)

proc getSize*[T: string | cstring](x: T): int =
  result = sizeof(int) + sizeof(byte) * x.len

proc getSize*[T](x: set[T]): int =
  result = sizeof(int) + x.card * sizeof(T)

proc getSize*[T](x: openArray[T]): int =
  when T is SimpleTypes:
    result = sizeof(int) + sizeof(T) * x.len ## data size + length field
  else:
    inc result, sizeof(int)
    for el in x:
      inc result, getSize(el)

proc getSize*[T: object | tuple](x: T): int =
  for field, val in fieldPairs(x):
    inc result, getSize(val)

proc getSize*[T: ref object](x: T): int =
  # sizeof(x)
  if x.isNil:
    result = sizeof(int) # int for size indicator for the ref data
  else:
    result = sizeof(int) + getSize(x[])

proc getSize*[T: distinct](x: T): int = result = getSize(distinctBase(x))

proc getSize*[T](x: typedesc[T]): int =
  var tmp: T
  result = getSize(tmp)

proc `+%`(x: pointer, offset: int): pointer =
  result = cast[pointer](cast[uint](x) + offset.uint)

proc copyData*(buf: Buffer, target, source: pointer, size: int) =
  copyMem(target, source, size)
  inc buf.offsetOf, size

proc asFlat*[T: object | tuple](buf: var Buffer, x: T)
proc asFlat*[T: ref object](buf: var Buffer, x: T)
proc asFlat*[T: SimpleTypes](buf: var Buffer, x: T) =
  let size = getSize(x)
  var target = buf.data +% buf.offsetOf
  buf.copyData(target, address(x), size)

proc asFlat*[T; N: static int](buf: var Buffer, x: array[N, T]) =
  let size = getSize(x)
  var target = buf.data +% buf.offsetOf
  buf.copyData(target, address(x[0]), size)

proc asFlat*[T](buf: var Buffer, x: set[T]) =
  # 1. copy the length
  buf.asFlat(x.card)
  # 2. copy the content (element wise; could convert to `seq` and copy flat)
  for el in x:
    buf.asFlat(el)

proc getAddr(x: string): uint =
  if x.len > 0:
    result = cast[uint](address(x[0]))
  else:
    result = 0

proc asFlat*[T: string | cstring](buf: var Buffer, x: T) =
  debug:
    echo "Writing string: ", x, " of len ", x.len
  # 1. copy the length
  buf.asFlat(x.len)
  # 2. now copy the content
  if x.len > 0:
    var target = buf.data +% buf.offsetOf
    buf.copyData(target, x[0].addr, x.len * sizeof(byte))

proc asFlat*[T](buf: var Buffer, x: openArray[T]) =
  # 1. copy the length
  buf.asFlat(x.len)
  # 2. now copy the content
  if x.len > 0:
    when T is SimpleTypes:
      var target = buf.data +% buf.offsetOf
      buf.copyData(target, x[0].addr, x.len * sizeof(T))
    else:
      for el in x:
        buf.asFlat(el)

proc asFlat*[T; U](buf: var Buffer, x: Table[T, U]) =
  buf.asFlat(x.len)
  for k, v in pairs(x):
    buf.asFlat((k, v))

macro writeFields(buf, x, fields: typed): untyped =
  ## Given variant fields `fields` as a `nnkBracket` constructs
  ## `asFlat` calls for each field from object `x` to write it to
  ## the `buf`
  doAssert fields.kind == nnkBracket, "Call `getVariantFields(T)` for argument"
  result = newStmtList()
  for f in fields:
    result.add nnkCall.newTree(ident"asFlat",
                               buf,
                               nnkDotExpr.newTree(x, ident(f.strVal)))

proc asFlat*[T: object | tuple](buf: var Buffer, x: T) =
  ## Variant objects are stored as
  ## `[variant_field_0, variant_field_1, ..., variant_field_N, remaining_fields]`
  ## so that we can construct the correct variant object before hand.
  when isVariantObj(T):
    const fields = getVariantFields(T)
    # 1. write all fields *first*
    writeFields(buf, x, getVariantFields(T))
    # 2. write data of remaining fields
    for field, val in fieldPairs(x):
      when field notin fields: # skip variant fields!
        buf.asFlat(val)
  else:
    for field, val in fieldPairs(x):
      debug:
        when typeof(val) is SimpleTypes:
          echo "Writing (non ref, non variant): ", field, " = ", val
        else:
          echo "Writing (non ref, non variant): ", field, " = ", typeof(val)
      buf.asFlat(val)

proc asFlat*[T: ref object](buf: var Buffer, x: T) =
  debug:
    echo "Flatten ref ", T, " is nil? ", x.isNil
  if x.isNil:
    # Store length as 0
    buf.asFlat(0) # nothing else to do
  else:
    # 1. store length of data
    buf.asFlat(getSize(x[]))
    # 2. store data itself
    buf.asFlat(x[])

proc asFlat*[T: proc](buf: var Buffer, x: T) =
  ## XXX: need to differentiate between pointers to things we can or cannot
  ## deference!
  discard # nothing to do, cannot serialize a proc
  #if not x.isNil:
  #  buf.asFlat(x[])

proc asFlat*[T: distinct](buf: var Buffer, x: T) = buf.asFlat(distinctBase(x))

proc asFlat*[T](x: T): Buffer =
  ## Converts a given Nim object into a `Buffer`
  let size = getSize(x)
  result = newBuf(size)
  result.asFlat(x)

proc flatTo*[T](buf: Buffer): T
proc flatTo*[T: SimpleTypes | pointer | enum](x: var T, buf: Buffer) =
  let size = getSize(x)
  var source = buf.data +% buf.offsetOf
  buf.copyData(addr(x), source, size)

## XXX: `flatTo` for fixed length arrays!
proc flatTo*[T: array](x: var T, buf: Buffer) =
  let size = getSize(x)
  var source = buf.data +% buf.offsetOf
  buf.copyData(addr(x), source, size)

proc readInt*(buf: Buffer): int =
  flatTo(result, buf)

proc flatTo*[T: string | cstring](x: var T, buf: Buffer) =
  # 1. read length
  let length = readInt(buf)
  # 2. read data
  let source = buf.data +% buf.offsetOf
  if source != nil and length > 0:
    x.setLen(length)
    let size = length * sizeof(byte)
    buf.copyData(x[0].addr, source, size)

proc flatTo*[T](x: var set[T], buf: Buffer) =
  # 1. read the length
  let length = readInt(buf)
  # 2. read the data
  for i in 0 ..< length:
    var el: T ## `set[T]` is only for simple types!
    el.flatTo(buf)
    x.incl el

proc flatTo*[T](x: var seq[T], buf: Buffer) =
  # construct a child buffer.
  # 1. extract the number of elements
  let len = readInt(buf)
  # 2. set size of resulting seq
  x.setLen(len)
  # 3. copy elements
  when T is SimpleTypes:
    # flat, all in one go
    let source = buf.data +% buf.offsetOf
    if source != nil and len > 0:
      buf.copyData(x[0].addr, source, sizeof(T) * x.len)
  else:
    # copy element by element
    for i in 0 ..< x.len:
      x[i].flatTo(buf)

proc flatTo*[T; U](x: var Table[T, U], buf: Buffer) =
  let len = readInt(buf)
  for i in 0 ..< len:
    var kv: (T, U)
    kv.flatTo(buf)
    x[kv[0]] = kv[1]

macro newVariantObj(typ, fields, args: typed): untyped =
  ## Constructs a variant object given variant field names `fields`
  ## and the values to be written as stored in the variable `args`.
  ## If more than one field, `args` is an anonymous `tuple`.
  doAssert fields.kind == nnkBracket, "Call `getVariantFields(T)` for argument"
  doAssert args.kind == nnkSym # Must be an identifier for a tuple!
  result = nnkObjConstr.newTree(typ)
  for i in 0 ..< fields.len:
    let f = fields[i]
    doAssert f.kind == nnkStrLit
    if fields.len > 1:
      result.add nnkExprColonExpr.newTree(
        ident(f.strVal),
        nnkBracketExpr.newTree(args, newLit i)
      )
    else: # only a single field, just use `args` directly! Not a tuple
      result.add nnkExprColonExpr.newTree(
        ident(f.strVal),
        args
      )
  debug:
    echo "NEW VARIANT OBJ= ", result.repr

macro variantFieldTuple(typ, fields: typed): untyped =
  doAssert fields.kind == nnkBracket, "Call `getVariantFields(T)` for argument"
  result = nnkPar.newTree()
  for f in fields:
    result.add quote do:
      typeof(accessField(`typ`, `f`))
  debug:
    echo "VARIANT FIELD TUP: ", result.repr

proc flatTo*[T: object | tuple](x: var T, buf: Buffer) =
  debug:
    echo "flatTo obj : ", T
  when isVariantObj(T):
    const fields = getVariantFields(T)
    # 1. construct tuple to store variant object fields
    var varKindDat: variantFieldTuple(x, getVariantFields(T))
    # 2. Read the variant object fields
    varKindDat.flatTo(buf)
    # 3. construct valid variant object
    x = newVariantObj(T, getVariantFields(T), varKindDat)
    # 4. fill it
    for field, val in fieldPairs(x):
      when field notin fields: # skip variantfields
        val.flatTo(buf)
  else:
    for field, val in fieldPairs(x):
      debug:
        echo "FLAT TO (non ref, non variant), ", field, " = ", typeof(val)
      val.flatTo(buf)
  debug:
    echo "======== ", T, " ========= ", x

proc flatTo*[T: ref object](x: var T, buf: Buffer) =
  # 1. read length
  debug:
    echo "flat To ref obj ", T
  let length = readInt(buf)
  if length > 0:
    x = T() # initialize it
    # and fill
    x[].flatTo(buf)

proc flatTo*[T: proc](x: var T, buf: Buffer) = discard
  ## We do not attempt to (de)serialize procs!

proc flatTo*[T: distinct](x: var T, buf: Buffer) = flatTo(distinctBase(x), buf)

proc flatTo*[T](buf: Buffer): T =
  ## Returns a sequence of `T` from the given buffer, taking into account conversion from
  ## `ptr char` to `string` and nested buffer children to `seq[U]`.
  # 1. reset offset of in case this buffer was already used to write to
  buf.offsetOf = 0
  result.flatTo(buf)

proc saveBuffer*(b: Buffer, fname: string) =
  ## Convenience helper to write a buffer to a file
  writeFile(fname, toOpenArray(cast[ptr UncheckedArray[byte]](b.data), 0, b.size-1))

proc writeBuffer*(b: Buffer, fname: string) {.deprecated: "Please use `saveBuffer` instead.".} =
  saveBuffer(b, fname)

proc saveBuffer*[T](x: T, fname: string) =
  ## Convenience helper to write a type T as a flat binary data file `fname`
  let b = asFlat(x)
  b.saveBuffer(fname)

import std / memfiles
from std / os import removeFile
proc loadBuffer*[T](fname: string, deleteFile = false): T =
  ## Convenience helper to directly load a type `T` from a given file `fname`
  var mfile = memfiles.open(fname)
  if deleteFile: # can already delete the file. Useful if we want to clean up fast
    removeFile(fname)
  let b = newBuf(mfile.mem, mfile.size)
  result = flatTo[T](b)
  memfiles.close(mfile)

when isMainModule:
  block A:
    var data = newSeq[(int, (float, string), seq[string])]()
    data.add (0xAFFEAFFE.int, (2342.2, "hello"), @["A", "HALO"])
    #buf.add (0x13371337.int, ("", 52.2), @["B", "FOO"])
    let buf = asFlat(data)

    let xx = flatTo[(int, (float, string), seq[string])](buf)
    echo xx

  block B:
    var data = newSeq[(int, (float, string), seq[int])]()
    data.add (0xAFFEAFFE.int, (2342.2, "hello"), @[1, 2, 3, 4, 5])
    #data.add (0x13371337.int, ("", 52.2), @["B", "FOO"])
    let buf = asFlat(data)

    let xx = flatTo[(int, (float, string), seq[int])](buf)
    echo xx

  block C:
    type
      Command = object
        cmd: string
        outputFile: string

    let x = "hello"
    let y = "fuck \n foobar"

    var cmd = Command(cmd: x, outputFile: y)
    let buf = asFlat(@[cmd, cmd])
    echo buf

    writeBuffer(buf)

    echo "\n\nREADING"

    let cmd2 = flatTo[Command](buf)
    echo cmd2
