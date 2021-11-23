import Lake
open System Lake DSL

def leanSoureDir := "lib"
def cCompiler := "g++"
def cppDir : FilePath := "cpp"
def ffiSrc := cppDir / "ffi.cpp"
def ffiO := "ffi.o"
def ffiLib := "libffi.a"
def mySQLIncludeDir := "/usr/include/mysql"
def mySQLLinkArg := "-lmysqlclient"

def ffiOTarget (pkgDir : FilePath) : FileTarget :=
  let oFile := pkgDir / defaultBuildDir / cppDir / ffiO
  let srcTarget := inputFileTarget <| pkgDir / ffiSrc
  fileTargetWithDep oFile srcTarget fun srcFile => do
    compileO oFile srcFile
      #["-I", (← getLeanIncludeDir).toString, "-I", mySQLIncludeDir] cCompiler

def cLibTarget (pkgDir : FilePath) : FileTarget :=
  let libFile := pkgDir / defaultBuildDir / cppDir / ffiLib
  staticLibTarget libFile #[ffiOTarget pkgDir]

package LeanMySQL (pkgDir) {
  srcDir := leanSoureDir
  libRoots := #[`Ffi]
  moreLibTargets := #[cLibTarget pkgDir]
  moreLinkArgs := #[mySQLLinkArg]
}
