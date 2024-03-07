import tables
import ../flatBuffers
type
  DataTable*[T] = ref object
    len*: int
    data*: OrderedTable[string, T] ## `T` is the column kind

  DataFrame = DataTable[float]


let foo = DataFrame(len: 5, data: toOrderedTable({"a" : 1.0, "b" : 6.2, "c" : 21.1, "d" : 12.1, "e": 885.2}))
#echo foo
const fname = "./tTable.dat"
foo.asFlat().writeBuffer(fname)

let buf = readFile(fname).fromString
echo flatTo[DataFrame](buf).repr
