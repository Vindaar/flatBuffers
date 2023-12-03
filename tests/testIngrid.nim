import ingrid / [ingrid_types, tos_helpers]
import ingrid / fadc_helpers

import flatBuffers

type
  Foo = object
    valid: bool
    val: int
    x: char
    y: uint8

let f = Foo(valid: true, val: 123, x: 'A', y: 53)

let b = asFlat f
echo b.size
b.writeBuffer("/tmp/test_foo.dat")

echo "BUFFER: ", b.offsetOf

block:
  let t = true
  var b: bool
  copyMem(b.addr, t.addr, 1)
  echo b

echo flatTo[Foo](b)

block Ingrid:
  let files = @["/home/basti/CastData/data/2017/Run_96_171123-10-42/data000001.txt",
                "/home/basti/CastData/data/2017/Run_96_171123-10-42/data000005.txt"]
  let mf = readMemFilesIntoBuffer(files)

  echo sizeof(bool)

  for i, f in mf:
    let b = processEventWithScanf(f)

    echo b

    let buf = asFlat(b)
    buf.writeBuffer("/tmp/buffer_" & $i & ".dat")


    echo "back"
    let ev = flatTo[Event](buf)
    echo ev


block Fadc:
  let files = @["/home/basti/CastData/data/2017/Run_96_171123-10-42/data000001.txt-fadc",
                "/home/basti/CastData/data/2017/Run_96_171123-10-42/data000005.txt-fadc"]
  let mf = readMemFilesIntoBuffer(files)

  echo sizeof(bool)

  for i, f in mf:
    let b = readFadcFile(f)

    #echo b

    echo "to flat"
    let buf = asFlat(b)
    echo "did2 ", buf.size
    echo buf.offsetOf
    #buf.writeBuffer("/tmp/buffer_" & $i & ".dat")
    echo "wrote buffer, or crashed"

    echo "back"
    let ev = flatTo[FadcFile](buf)
    echo ev
