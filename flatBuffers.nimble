# Package

version       = "0.1.4"
author        = "Vindaar"
description   = "Package to turn (nested) Nim objects to flat buffers and back."
license       = "MIT"


# Dependencies

requires "nim >= 2.0.0"


proc fullSetsTest() =
  ## Checks if the `-d:FullSets` behavior is as expected
  exec "nim c -d:FullSets=false tests/tSets.nim"
  exec "tests/tSets save" # save in packed form
  exec "tests/tSets load" # verify can load
  exec "nim c -d:FullSets=true tests/tSets.nim" # recompile full set
  exec "tests/tSets load" # verify we can _still_ load
  exec "tests/tSets save" # save again in full
  exec "tests/tSets load" # check again
  exec "rm tests/flatbuf-file.bin"

task test, "Run all tests":
  fullSetsTest()
