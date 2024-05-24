import std/[strformat, os]
import ../flatBuffers

# From issue #1
# https://github.com/Vindaar/flatBuffers/issues/1
# extended with a `set[uint16` field to test `-d:FullSets=true/false`

type
  FileWithPerms* = object
    path*: string
    permissions*: set[FilePermission]
    uid*: set[uint16]

const testFile = FileWithPerms(
  path: "/a/file",
  permissions: {fpUserRead, fpUserWrite},
  uid: {5'u16, 8512'u16}
)

## XXX: turn into a test where we either produce the file or
## read it. Then compile with and without `FullSets` and verify
## that we can read both correctly independent of `FullSets`.

const TestFilePath = "tests/flatbuf-file.bin"

proc save() =
  testFile.saveBuffer(TestFilePath)
  echo "File size in bytes: ", getFileSize(TestFilePath)
  when FullSets:
    doAssert getFileSize(TestFilePath) == 8211
  else:
    doAssert getFileSize(TestFilePath) == 39

proc load() =
  let flatbufFile = loadBuffer[FileWithPerms](TestFilePath)
  echo fmt"File is {flatbufFile} after write/read"

  doAssert flatbufFile.path == testFile.path
  doAssert flatbufFile.permissions == testFile.permissions
  doAssert flatbufFile.uid == testFile.uid

when isMainModule:
  import cligen
  dispatchMulti([save], [load])
