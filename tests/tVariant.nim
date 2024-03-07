import ../flatBuffers

type
  Typ = enum A, B, C
  Yar = enum X, Y
  Foo = object
    bar: int
    baz: string
    case kind: Typ
    of A: a: float
    of B: b: int
    of C: c: string
    case kYar: Yar
    of X: x: seq[int]
    of Y: y: seq[float]

block:
  let foo = Foo(bar: 12, baz: "hoo", kind: A, a: 1.5145, kYar: X, x: @[12, 6, 8])

  const fname = "./variant_foo.dat"
  asFlat(foo).writeBuffer(fname)

  let buf = readFile(fname).fromString
  echo flatTo[Foo](buf)
block:
  let foo = Foo(bar: 12, baz: "hoo", kind: C, c: "hahaha", kYar: Y, y: @[12.0, 6.8, 8.2])
  for field, val in fieldPairs(foo):
    echo field, " = ", val

  const fname = "./variant_foo.dat"
  asFlat(foo).writeBuffer(fname)

  let buf = readFile(fname).fromString
  echo flatTo[Foo](buf)
