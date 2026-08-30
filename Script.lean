import Init.Notation

open System (FilePath)

def getLeanFiles : IO (Array FilePath) := do
  let cwd ← IO.currentDir
  return (← cwd.walkDir λ path => return notHidden path)
    |>.filter (·.extension = some "lean")
where
  notHidden path := path.fileName.filter (¬·.startsWith '.') |>.isSome

def partitionByImports (text : String) : Array String.Slice × Array String.Slice :=
  let lines := text.lines.toArray
  let lastIdxAfterImport := 
    Prod.snd <|
    lines.foldl (init := (0, 0)) λ (idx, lastIdxAfterImport) line =>
      (idx + 1, if line.startsWith "import" then idx + 1 else lastIdxAfterImport)
  (lines.take lastIdxAfterImport, lines.drop lastIdxAfterImport)

def reconstruct (before after : Array String.Slice) :=
  #["module", ""].map (·.toSlice)
  ++ before.map (·.replace "import" "public import" |>.toSlice)
  ++ #["", "public section", ""].map (·.toSlice)
  ++ after
  |>.map (·.copy)
  |>.toList
  |> "\n".intercalate

def ArrayT m [Monad m] α := m (Array α)

instance {m} [instM : Monad m] : Monad (ArrayT m) where
  pure := instM.pure ∘ Array.singleton
  bind x f := instM.bind x fun y => y.flatMapM f

instance {m} [Monad m] : MonadLift m (ArrayT m) where
  monadLift := Functor.map Array.singleton

#eval show IO _ from show ArrayT IO _ from do
  let path ← getLeanFiles
  let text ← IO.FS.readFile path
  let (before, after) := partitionByImports text
  let reconstructed := reconstruct before after
  IO.println reconstructed
  -- IO.FS.writeFile path reconstructed
  return (path, reconstructed)

