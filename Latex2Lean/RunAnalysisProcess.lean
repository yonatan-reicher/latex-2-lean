module

public import Latex2Lean.Csv
public import Latex2Lean.Analysis.Basic
public import Std.Data.HashMap

public section

-- import Batteries

open IO.FS (withTempDir writeFile readFile)
open Std (HashMap)
open System (FilePath)

namespace Latex2Lean

def runAnalysisProcess (input : String) : IO (String × Array (String × Csv)) := do
  withTempDir fun dir => do
    let dir := dir.normalize
    -- Initialize the input file.
    let inputPath := dir / "input"
    writeFile inputPath input
    -- Make me some output paths.
    let outputPaths := Analysis.names.map λname => (name, dir / s!"{name}.csv")
    let runResult ← IO.Process.output {
      cwd := "./analysis",
      cmd := "make",
      args := #[
        "run",
        s!"INPUT={inputPath.toString}",
        (s!"OUTPUTS={·}")
        <| " ".intercalate
        <| outputPaths.toList.map fun (name, path) => s!"{name}={path.toString}"
      ]
    }
    let success :=
      runResult.exitCode == 0
      && runResult.stderr.isEmpty
    if not success then
      throw $ .userError s!"
        Exit code: {runResult.exitCode}
        Stdout: {runResult.stdout}
        Stderr: {runResult.stderr}
      ".trimAscii.copy
    let outputs ← outputPaths.mapM fun (name, path) => do
      let text ← readFile path
      let csv ← .ofExcept <| Csv.read path text
      return (name, csv)
    return (runResult.stdout, outputs)