import Aoc2022.Utils

open System

namespace Day10

def input : FilePath := "/home/fred/lean/aoc2022/input_10"

/-
PART 1:
-/

def first_part : IO Int := do
  let commands := (← IO.FS.lines input).map String.splitOn
                                      |>.map (fun s => match s with
                                                       | ["noop"] => none
                                                       | ["addx", n] => some n.toInt!
                                                       | _ => unreachable!)
                                      |>.push none
  let mut curCycle := 0
  let mut X : Int := 1
  let mut totalStrength : Int := 0
  for cmd in commands do 
    match cmd with 
    | none => curCycle := curCycle + 1
              if (curCycle + 20) % 40 = 0 then 
                totalStrength := totalStrength + X * curCycle
    | some n => curCycle := curCycle + 1
                if (curCycle + 20) % 40 = 0 then totalStrength := totalStrength + X * curCycle
                curCycle := curCycle + 1
                if (curCycle + 20) % 40 = 0 then totalStrength := totalStrength + X * curCycle
                X := X + n
  return totalStrength

/-
PART 2:
-/

def printPixel (curCycle : Nat) (X : Int) : IO Unit := do
  if curCycle % 40 = 0 then IO.print "\n"
  if (X - curCycle % 40).natAbs ≤ 1 then IO.print "#"
  else IO.print "."

def second_part : IO Unit := do
  let commands := (← IO.FS.lines input).map String.splitOn
                                      |>.map (fun s => match s with
                                                       | ["noop"] => none
                                                       | ["addx", n] => some n.toInt!
                                                       | _ => unreachable!)
                                      |>.push none
  let mut curCycle := 0
  let mut X : Int := 1
  for cmd in commands do 
    match cmd with 
    | none => printPixel curCycle X
              curCycle := curCycle + 1
    | some n => printPixel curCycle X
                curCycle := curCycle + 1
                printPixel curCycle X
                curCycle := curCycle + 1
                X := X + n
  
end Day10
