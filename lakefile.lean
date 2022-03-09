import Lake

open System Lake DSL

def leanSoureDir := "lib"
def cppCompiler := "c++"
def cppDir : FilePath := "c"
def ffiSrc := cppDir / "ffi.c"
def ffiO := "ffi.o"
def ffiLib := "libffi.a"
def includesDir := "/usr/include/"
def libsDir := "/usr/lib/x86_64-linux-gnu/"
def mySQLLinkArg := "-lmysqlclient"

def ffiOTarget (pkgDir : FilePath) : FileTarget :=
  let oFile := pkgDir / defaultBuildDir / cppDir / ffiO
  let srcTarget := inputFileTarget <| pkgDir / ffiSrc
  fileTargetWithDep oFile srcTarget fun srcFile => do
    compileO oFile srcFile
      #["-I", (← getLeanIncludeDir).toString, "-I", includesDir] cppCompiler

def cLibTarget (pkgDir : FilePath) : FileTarget :=
  let libFile := pkgDir / defaultBuildDir / cppDir / ffiLib
  staticLibTarget libFile #[ffiOTarget pkgDir]

package LeanMySQL (pkgDir) {
  srcDir := leanSoureDir
  libRoots := #[`LeanMySQL, `DataFrame, `DataEntries, `SQLDSL, `SQLSyntax, `Utils]
  moreLibTargets := #[cLibTarget pkgDir]
  moreLinkArgs := #["-L", libsDir, mySQLLinkArg]
}
