import std / macros

import macros

import std / tables


## The variant logic is inspired by Flatty's approach to variants.
## It's unfinished though. Once I have some time I'll probably fix
## it to work correctly.
proc getRecList(n: NimNode): NimNode =
  var typ = n.getTypeImpl()
  while typ.typeKind == ntyTypeDesc:
    #echo typ.treerepr
    typ = typ[1].getTypeImpl()
  #echo n.treerepr
  #echo n.getTypeImpl.treerepr
  case typ.typeKind
  of ntyObject, ntyGenericInst: result = typ[2] #n.getTypeImpl()[2]
  of ntyTuple: result = typ
  else:
    doAssert false, "Invalid type kind: " & $typ.typeKind

iterator variantFields(typ: NimNode): NimNode =
  for ch in typ:
    #echo ch.treerepr
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
  #echo result.treerepr

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
  #echo result.treerepr

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
proc getSize*[T: SimpleTypes](x: T): int =
  result = sizeof(T)

import typetraits
proc getSize*[T: distinct](x: T): int =
  result = sizeof(distinctBase(T))

proc getSize*[T: pointer|ptr](x: T): int =
  result = sizeof(T)

proc getSize*[T: array](x: T): int =
  result = sizeof(T)

proc getSize*[T: string | cstring](x: T): int =
  result = sizeof(int) + sizeof(byte) * x.len

proc getSize*[T](x: seq[T]): int =
  when T is SimpleTypes:
    result = sizeof(int) + sizeof(T) * x.len
  else:
    #echo "get size of"
    inc result, sizeof(int)
    for el in x:
      #echo "cannot iterate over nothing"
      inc result, getSize(el)
    #echo "result 0"

proc getSize*[T: object | tuple](x: T): int =
  for field, val in fieldPairs(x):
    inc result, getSize(val)

proc getSize*[T](x: typedesc[T]): int =
  var tmp: T
  result = getSize(tmp)

proc `+%`(x: pointer, offset: int): pointer =
  result = cast[pointer](cast[uint](x) + offset.uint)

#proc asFlat*[T](x: openArray[T]): Buffer

proc copyData(buf: Buffer, target, source: pointer, size: int) =
  copyMem(target, source, size)
  #var tbuf = cast[ptr UncheckedArray[char]](target)
  #var sbuf = cast[ptr UncheckedArray[char]](source)
  #for i in 0 ..< size:
  #  echo "Copying: ", cast[uint](sbuf[i])
  #  tbuf[i] = sbuf[i]
  inc buf.offsetOf, size

proc asFlat[T: object | tuple](buf: var Buffer, x: T)
proc asFlat[T: SimpleTypes](buf: var Buffer, x: T) =
  let size = getSize(x)
  var target = buf.data +% buf.offsetOf
  buf.copyData(target, address(x), size)

proc asFlat[T: distinct](buf: var Buffer, x: T) =
  let size = getSize(x)
  var target = buf.data +% buf.offsetOf
  buf.copyData(target, address(x), size)

proc asFlat[T; N: static int](buf: var Buffer, x: array[N, T]) =
  let size = getSize(x)
  var target = buf.data +% buf.offsetOf
  buf.copyData(target, address(x[0]), size)

proc getAddr(x: string): uint =
  if x.len > 0:
    result = cast[uint](address(x[0]))
  else:
    result = 0

proc asFlat[T: string | cstring](buf: var Buffer, x: T) =
  # 1. copy the length
  buf.asFlat(x.len)
  # 2. now copy the content
  if x.len > 0:
    var target = buf.data +% buf.offsetOf
    buf.copyData(target, x[0].addr, x.len * sizeof(byte))

proc asFlat[T](buf: var Buffer, x: seq[T]) =
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

proc asFlat[T; U](buf: var Buffer, x: Table[T, U]) =
  #echo "Writing table len : ", x.len
  buf.asFlat(x.len)
  for k, v in pairs(x):
    #echo "writing kv ", (k, v)
    buf.asFlat((k, v))

proc asFlat[T: object | tuple](buf: var Buffer, x: T) =
  when isVariantObj(T):
    #echo "writing: variant"
    #buf.asFlat(x.getVariantField())
    #echo "wrote variant"
    const fields = getVariantFields(T)
    for field, val in fieldPairs(x):
      #echo "field: ", field
      #when field notin fields:
      when field in fields:
        # copy zero instead
        var tmp = default(typeof(val))
        buf.asFlat(tmp)
      else:
        buf.asFlat(val)
  else:
    for field, val in fieldPairs(x):
      buf.asFlat(val)

  #for field, val in fieldPairs(x):
  #  buf.asFlat(val)

proc writeBuffer*(b: Buffer, fname = "/tmp/hexdat.dat") =
  writeFile(fname, toOpenArray(cast[ptr UncheckedArray[byte]](b.data), 0, b.size-1))

#proc asFlat*[T](x: openArray[T]): Buffer =
#  if x.len > 0:
#    # get real size of data. Walsk all elements if non trivial and gets their size.
#    # Includes (len, pointer) pairs for string / seq
#    let size = getSize(@x)
#    result = newBuf(size + sizeof(int))
#    echo "Writing length: ", x.len, " of type: ", T
#    result.asFlat(x.len)
#    for el in x:
#      result.asFlat(el)
#      when typeof(T) isnot tuple|object: # in the other case incrementation is done in the `asFlat` proc above
#        inc result.offsetOf, sizeof(T)
#  else:
#    result = newBuf(0)
#
#proc asFlat*[T: not openArray](x: T): Buffer =
#  result = asFlat([x])

proc asFlat*[T](x: T): Buffer =
  let size = getSize(x)
  #echo "Object ", T, " of size: ", size
  result = newBuf(size)
  result.asFlat(x)

proc flatTo*[T](buf: Buffer): T
proc flatTo*[T: SimpleTypes | pointer | enum](x: var T, buf: Buffer) =
  let size = getSize(x)
  var source = buf.data +% buf.offsetOf
  buf.copyData(addr(x), source, size)

#proc flatTo*[T: enum](x: var T, buf: Buffer) =
#  let size = getSize(x)
#  var source = buf.data +% buf.offsetOf
#  buf.copyData(addr(x), source, size)

## XXX: `flatTo` for fixed length arrays!
proc flatTo*[T: array](x: var T, buf: Buffer) =
  let size = getSize(x)
  var source = buf.data +% buf.offsetOf
  buf.copyData(addr(x), source, size)

proc readInt(buf: Buffer): int =
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

proc flatTo*[T: object | tuple](x: var T, buf: Buffer) =
  when isVariantObj(T):
    #var tmp: typeof(x.getVariantField())
    #tmp.flatTo(buf)
    #new(x, tmp)
    const fields = getVariantFields(T)
    for field, val in fieldPairs(x):
      #echo "field: ", field, " (and vf ", fields, " )"
      when field in fields:
        # skip ahead
        inc buf.offsetOf, getSize(val)
      else:
        val.flatTo(buf)
      #when field notin fields:
      #  val.flatTo(buf)
  else:
    for field, val in fieldPairs(x):
      val.flatTo(buf)

proc flatTo*[T](buf: Buffer): T =
  ## Returns a sequence of `T` from the given buffer, taking into account conversion from
  ## `ptr char` to `string` and nested buffer children to `seq[U]`.
  #echo "type T ", T
  # 1. reset offset of in case this buffer was already used to write to
  buf.offsetOf = 0
  result.flatTo(buf)
  #echo "Getting size is broken:"
  ##let siz = getSize(T)
  ##echo "hah, siz is 0 ", siz
  ##let len = buf.size div siz ## This is not accurate!
  ## set `offsetOf` to 0 to copy from beginning
  #buf.offsetOf = 0
  ##echo "Length is?? ", len
  #when T is SimpleTypes:
  #  result = newSeqOfCap[T](buf.size div sizeof(T))
  #else:
  #  result = newSeqOfCap[T](256) # just go with something
  #while buf.offsetOf < buf.size:
  #  # copy element by element
  #  var tmp: T
  #  tmp.flatTo(buf)
  #  result.add tmp

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
