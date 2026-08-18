import Std.Data.HashMap
-- import Batteries

open Std (HashMap)
open IO.FS (withTempDir writeFile readFile)

namespace Latex2Lean

def runAnalysisProcess (input : String) : IO String := do
  withTempDir fun dir => do
    let dir := dir.normalize
    let inputPath := dir / "input"
    writeFile inputPath input
    let output ← IO.Process.output {
      cwd := "./analysis",
      cmd := "make",
      args := #[inputPath.toString]
    }
    if output.exitCode != 0 then
      throw $ .userError s!"
        Exit code: {output.exitCode}
        Stdout: {output.stdout}
        Stderr: {output.stderr}
      ".trim
    return output.stdout
