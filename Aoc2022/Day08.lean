import Aoc2022.Utils

open System

namespace Day8

def input : FilePath := "/home/fred/lean/aoc2022/input_08"

/-
PART 1:
-/

def keepMax (c : Nat) : StateM Nat Nat := do
  let curMax ← get
  let x := max curMax c
  modify (fun _ => x)
  return curMax

def maxFromLeft (row : Array Nat) : Array Nat := (row.mapM keepMax).run' 0

def visibleFromLeft (row : Array Nat) : Array Bool :=
  row.zipWith (maxFromLeft row) (fun cur mx => cur > mx)

def _root_.Bool.toNat (b : Bool) := if b then 1 else 0

def first_part : IO Nat := do
  let grid := (← IO.FS.lines input).map (·.data.toArray)
                                  |>.map (fun row => row.map (·.toNat - '0'.toNat + 1))
  let gridFlip := grid.map (·.reverse)
  let gridTranspose := grid.transpose
  let gridTransposeFlip := gridTranspose.map (·.reverse)
  let visFromLeft := grid.map visibleFromLeft
  let visFromRight := gridFlip.map visibleFromLeft
                            |>.map (·.reverse)
  let visFromTop := gridTranspose.map visibleFromLeft
                               |>.transpose
  let visFromBot := gridTransposeFlip.map visibleFromLeft
                                    |>.map (·.reverse)
                                    |>.transpose 
  let outputGrid := visFromLeft.zipWith2D visFromRight (· || ·)
                              |>.zipWith2D visFromTop (· || ·)
                              |>.zipWith2D visFromBot (· || ·)
  return outputGrid.foldl (fun cur row => cur + row.foldl (fun x y => x + y.toNat) 0) 0

/-
PART 2:
-/

def second_part : IO Nat := do
  let grid := (← IO.FS.lines input).map (·.data.toArray)
                                  |>.map (fun row => row.map (·.toNat - '0'.toNat + 1))
  let dim := grid.size
  let mut output := Array.mkArray₂ dim dim 0
  for i in [0:dim] do
    for j in [0:dim] do
      let mut fromLeft := 0
      let mut fromRight := 0
      let mut fromTop := 0
      let mut fromBot := 0
      for k in [1:i+1] do
        fromLeft := fromLeft + 1
        if grid[i-k]![j]! ≥ grid[i]![j]! then break
      for k in [i+1:dim] do
        fromRight := fromRight + 1
        if grid[k]![j]! ≥ grid[i]![j]! then break
      for k in [1:j+1] do
        fromTop := fromTop + 1
        if grid[i]![j-k]! ≥ grid[i]![j]! then break
      for k in [j+1:dim] do
        fromBot := fromBot + 1
        if grid[i]![k]! ≥ grid[i]![j]! then break
      output := output.modify₂ i j (fun _ => fromLeft * fromRight * fromTop * fromBot)
  return output.foldtl (fun x y => max x y) 0

end Day8
