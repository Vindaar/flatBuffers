import arraymancer
import ../flatBuffers
proc getSize*[T](t: Tensor[T]): int =
  # 1. size of the shape data
  result = getSize(@(t.shape))
  # 2. size of the content
  when T is KnownSupportsCopyMem:
    result += t.size * sizeof(T)
  else:
    for i in 0 ..< t.size:
      result += getSize(t[i])

proc `+%`(x: pointer, offset: int): pointer =
  result = cast[pointer](cast[uint](x) + offset.uint)

proc asFlat*[T](buf: var Buffer, t: Tensor[T]) =
  if t.size > 0:
    # 1. store the shape data
    buf.asFlat(@(t.shape))
    # 2. store the data
    when T is KnownSupportsCopyMem:
      var target = buf.data +% buf.offsetOf
      buf.copyData(target, t.toUnsafeView(), t.size * sizeof(T))
    else:
      for i in 0 ..< t.size:
        buf.asFlat(t[i])
  else:
    # nil tensor: store empty shape
    buf.asFlat(newSeq[int]())

proc asFlat*[T](t: Tensor[T]): Buffer =
  result = newBuf(t.size)
  result.asFlat(t)

proc flatTo*[T](x: var Tensor[T], buf: Buffer) =
  # 1. read the shape
  var shape: seq[int]
  shape.flatTo(buf)
  if shape.len > 0:
    # 2. copy data
    x = newTensorUninit[T](shape)
    when T is KnownSupportsCopyMem:
      let source = buf.data +% buf.offsetOf
      buf.copyData(x.toUnsafeView(), source, size * sizeof(T))
    else:
      for i in 0 ..< size:
        flatTo(x.unsafe_raw_offset[i], buf)
