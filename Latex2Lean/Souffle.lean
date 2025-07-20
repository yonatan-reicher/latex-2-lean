import Std
import Batteries
import Latex2Lean.Csv


/-!
This file is all about calling our souffle code, reading from it, and whatever
you want!
-/


open System (mkFilePath FilePath)
open IO (println)
open IO.FS (
  withTempDir
  readFile
  writeFile
)




namespace Souffle


/--
This function calls the `wslpath` command to convert windows paths to WSL paths.
-/
private def wslpath (path : FilePath) : IO String := do
  let output <- IO.Process.run {
    cmd := "wsl",
    args := #["wslpath", s!"{path.toString}"],
  }
  return output.trim



def call (inputs : Array Csv) (wsl := true) : IO (Array Csv) := do
  withTempDir λ dir => do
    let dir := dir.normalize
    -- Prepare the inputs
    for csv in inputs do
      let path := dir / (csv.fileName.replace ".csv" "" ++ ".facts")
      let toWrite := csv.write (delim := "\t") -- Inputs is Tab seperated
      writeFile path ("\n".intercalate toWrite.toList)
    -- Run the analysis
    let output <- if wsl then
      let dirString <- wslpath dir.toString
      IO.Process.output {
        cmd := "wsl",
        args := #[
          -- "--exec",   -- Execute a linux command directly
          "bash",     -- That command should be bash
          "--login",  -- In a login shell, more likely to have `souffle` as a command
          "-c", s!"./makefile.py run \"--directory-output={dirString}\"", -- Run the makefile
          -- TODO: Can the temporary directory have spaces in it? If so, we maybe
          -- should handle that shit.
        ],
        cwd := some $ mkFilePath ["souffle-analysis"],
      }
    else
      let dirString := dir.toString
      IO.Process.output {
        cmd := "bash",
        args := #[
          "-c", s!"./makefile.py run \"--directory-output={dirString}\"",
        ],
        cwd := some $ mkFilePath ["souffle-analysis"],
      }
    if output.exitCode != 0 then
      throw $ IO.userError s!"
        Exit code: {output.exitCode}
        Stdout: {output.stdout}
        Stderr: {output.stderr}
      ".trim
    let entries <- dir.readDir
    entries.mapM fun file => do
      let name := file.fileName
      let text <- readFile file.path
      let text := text.trim
      let lines := text.splitOn "\n"
      let csv <- Csv.read name lines |> IO.ofExcept
      return csv


-- #eval call #[] (wsl := false)
