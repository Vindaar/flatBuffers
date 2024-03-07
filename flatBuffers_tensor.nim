import arraymancer
import ./flatBuffers
proc getSize*[T](t: Tensor[T]): int =
  when T is KnownSupportsCopyMem:
    result = t.size * sizeof(T) + sizeof(int)
  else:
    result = sizeof(int)
    for i in 0 ..< t.size:
      result += getSize(t[i])

proc `+%`(x: pointer, offset: int): pointer =
  result = cast[pointer](cast[uint](x) + offset.uint)

proc asFlat*[T](buf: var Buffer, t: Tensor[T]) =
  # 1. store size
  buf.asFlat(t.size)
  when T is KnownSupportsCopyMem:
    var target = buf.data +% buf.offsetOf
    buf.copyData(target, t.toUnsafeView(), t.size * sizeof(T))
  else:
    for i in 0 ..< t.size:
      buf.asFlat(t[i])

proc flatTo*[T](x: var Tensor[T], buf: Buffer) =
  echo "flat to tensor:"
  # 1. read size
  let size = readInt(buf)
  # 2. copy data
  echo "Got size: ", size
  x = newTensorUninit[T](size)
  when T is KnownSupportsCopyMem:
    let source = buf.data +% buf.offsetOf
    buf.copyData(x.toUnsafeView(), source, size * sizeof(T))
  else:
    for i in 0 ..< size:
      flatTo(x[i], buf)
