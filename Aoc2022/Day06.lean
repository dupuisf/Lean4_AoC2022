import Aoc2022.Utils

open System

namespace Day6

def input : FilePath := "/home/fred/lean/aoc2022/input_06"

/-
PART 1:
-/

def allDifferentAux : List Char → Bool
| []        => true 
| c :: tail => if tail.contains c then false else allDifferentAux tail

def _root_.String.allDifferent (s : String) : Bool := allDifferentAux s.data

def findDifferent (n : Nat) (buf : String) (c : Char) : Option String :=
  if buf.length < n then some (buf.push c)
    else if buf.allDifferent then none else some ((buf.drop 1).push c)

def first_part : IO Nat := do
  let rawdata ← IO.FS.readFile input
  let pos := rawdata.iter.foldUntil "" (findDifferent 4)
  return pos.2.pos.byteIdx

/-
PART 2:
-/

def second_part : IO Nat := do
  let rawdata ← IO.FS.readFile input
  let pos := rawdata.iter.foldUntil "" (findDifferent 14)
  return pos.2.pos.byteIdx

end Day6
