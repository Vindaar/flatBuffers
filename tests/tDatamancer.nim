import datamancer
import ../flatBuffers
import ../flatBuffers_tensor

let df = toDf({"x" : @[1, 2, 3], "y" : @[5.5, 2.1, 1.484],
               "z" : @["helow", "world", "foo"], "c" : "Hehehe"})

echo df
echo df.data.repr
df.asFlat.writeBuffer()

echo "\n\n\n\n\n\n"

let buf = readFile("/tmp/hexdat.dat").fromString
let dfBack = flatTo[DataFrame](buf)
echo dfBack
