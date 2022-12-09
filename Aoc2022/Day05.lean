import Std.Data.Array.Basic
import Aoc2022.Utils
import Lean.Data.Parsec

open System Lean Parsec

namespace Day5

def input : FilePath := "/home/fred/lean/aoc2022/input_05"

/-
PART 1:
-/

def move : Parsec (Nat × Nat × Nat) := do 
  skipString "move "
  let x ← natNum
  skipString " from "
  let y ← natNum
  skipString " to "
  let z ← natNum
  return (x, y-1, z-1)

def move! (s : String) : Nat × Nat × Nat :=
  match move s.iter with
  | Parsec.ParseResult.success _ x => x 
  | Parsec.ParseResult.error _ _ => panic! "bla"

def parse_stack_line (l : String) (acc : Array (List Char)) : Array (List Char) :=
    let l₁ := l.toCharArray.filterWithIdx (fun idx _ => idx % 4 == 1)
    acc.zipWith l₁
      (fun (a : List Char) (c : Char) => if c == ' ' then a else c :: a)

def first_part : IO String := do
  let rawdata ← IO.FS.lines input
  let sep := rawdata.findIdx! (· == "")
  let numstacks := (rawdata[sep-1]!.length + 1)/4
  let init_stacks := Array.foldr 
    (fun acc line => parse_stack_line acc line)
    (Array.mkArray numstacks [])
    rawdata
    (start := sep-1)
    (stop := 0)
  let final_stacks := Array.foldl
    (fun stacks line => 
      let (num, fr, to) := move! line
      let moved := stacks[fr]!.take num
      (stacks.set! fr (stacks[fr]!.drop num)).set! to (moved.reverse ++ stacks[to]!))
    init_stacks
    rawdata
    (start := sep+1)
  let top_of_the_stacks := final_stacks.map List.head!
  return String.ofCharArray top_of_the_stacks

/-
PART 2:
-/

def second_part : IO String := do
  let rawdata ← IO.FS.lines input
  let sep := rawdata.findIdx! (· == "")
  let numstacks := (rawdata[sep-1]!.length + 1)/4
  let init_stacks := Array.foldr 
    (fun acc line => parse_stack_line acc line)
    (Array.mkArray numstacks [])
    rawdata
    (start := sep-1)
    (stop := 0)
  let final_stacks := Array.foldl
    (fun stacks line => 
      let (num, fr, to) := move! line
      let moved := stacks[fr]!.take num
      (stacks.set! fr (stacks[fr]!.drop num)).set! to (moved ++ stacks[to]!))
    init_stacks
    rawdata
    (start := sep+1)
  let top_of_the_stacks := final_stacks.map List.head!
  return String.ofCharArray top_of_the_stacks

end Day5
