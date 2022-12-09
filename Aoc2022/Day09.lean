import Aoc2022.Utils
import Std.Data.RBMap

open System Std

abbrev Pos := Int × Int
instance : LT Pos := ⟨Prod.lexLt⟩

instance : DecidableRel (α:=Pos) (· < ·) := fun a b => Prod.lexLtDec a b

instance : Ord Pos := ⟨fun a b => compareOfLessAndEq a b⟩

namespace Day9

def input : FilePath := "/home/fred/lean/aoc2022/input_09"




/-
PART 1:
-/

def moveTail (headPos : Pos) (tailPos : Pos) : Pos := Id.run do
  let dx := headPos.1 - tailPos.1
  let dy := headPos.2 - tailPos.2
  if dy.natAbs ≤ 1 ∧ dx.natAbs ≤ 1 then return tailPos
  match (dx, dy) with
  | (2, 0)    => return (tailPos.1 + 1, tailPos.2)
  | (2, 1)    => return (tailPos.1 + 1, tailPos.2 + 1)
  | (2, 2)    => return (tailPos.1 + 1, tailPos.2 + 1)
  | (1, 2)    => return (tailPos.1 + 1, tailPos.2 + 1)
  | (0, 2)    => return (tailPos.1,     tailPos.2 + 1)
  | (-1, 2)   => return (tailPos.1 - 1, tailPos.2 + 1)
  | (-2, 2)   => return (tailPos.1 - 1, tailPos.2 + 1)
  | (-2, 1)   => return (tailPos.1 - 1, tailPos.2 + 1)
  | (-2, 0)   => return (tailPos.1 - 1, tailPos.2)
  | (-2, -1)  => return (tailPos.1 - 1, tailPos.2 - 1)
  | (-2, -2)  => return (tailPos.1 - 1, tailPos.2 - 1)
  | (-1, -2)  => return (tailPos.1 - 1, tailPos.2 - 1)
  | (0, -2)   => return (tailPos.1,     tailPos.2 - 1)
  | (1, -2)   => return (tailPos.1 + 1, tailPos.2 - 1)
  | (2, -2)   => return (tailPos.1 + 1, tailPos.2 - 1)
  | (2, -1)   => return (tailPos.1 + 1, tailPos.2 - 1)
  | _ => unreachable!

def moveHead (headPos : Pos) (dir : String) ( dist : Nat := 1 ) : Pos :=
  match dir with 
  | "R" => (headPos.1 + dist, headPos.2)
  | "L" => (headPos.1 - dist, headPos.2)
  | "U" => (headPos.1, headPos.2 + dist)
  | "D" => (headPos.1, headPos.2 - dist)
  | _ => unreachable!

def first_part : IO Nat := do
  let commands := (← IO.FS.lines input).map String.splitOn
                                     |>.map (fun s => match s with
                                                      | [c, num] => (c, num.toNat!)
                                                      | _ => unreachable!)
                                     |>.push ("R", 1)
  let mut poucet : RBSet Pos compare := RBSet.empty
  let mut headPos : Pos := ⟨0, 0⟩
  let mut tailPos : Pos := ⟨0, 0⟩
  for cmd in commands do
    let (dir, dist) := cmd
    for _ in [0:dist] do
      match poucet.find? tailPos with
      | some _ => headPos := moveHead headPos dir
                  tailPos := moveTail headPos tailPos
      | none => poucet := poucet.insert tailPos
                headPos := moveHead headPos dir
                tailPos := moveTail headPos tailPos
  return poucet.size

#eval first_part

def maxPos : IO String := do
  let commands := (← IO.FS.lines input).map String.splitOn
                                     |>.map (fun s => match s with
                                                      | [c, num] => (c, num.toNat!)
                                                      | _ => unreachable!)
                                     |>.push ("R", 1)
  let mut headPos : Pos := ⟨0, 0⟩
  let mut tailPos : Pos := ⟨0, 0⟩
  let mut maxR : Int := 0
  let mut minR : Int := 0
  let mut maxU : Int := 0
  let mut minU : Int := 0
  for cmd in commands do
    let (dir, dist) := cmd
    for _ in [0:dist] do
      if headPos.1 ≥ maxR then maxR := headPos.1
      if headPos.1 ≤ minR then minR := headPos.1
      if headPos.2 ≥ maxR then maxU := headPos.2
      if headPos.2 ≤ minR then minU := headPos.2
      headPos := moveHead headPos dir
      tailPos := moveTail headPos tailPos
  return s!"({minR}, {minU}) ({maxR}, {maxU})"


/-
PART 2:
-/

def moveLink (old : Pos) : StateM Pos Pos := do
  let fakeTail := moveTail (← get) old
  set fakeTail
  return fakeTail

def moveRope (rope : Array Pos) (dir : String) : Array Pos := 
  (rope.mapM moveLink).run' (moveHead rope[0]! dir (dist := 2))

def second_part : IO Nat := do
  let commands := (← IO.FS.lines input).map String.splitOn
                                     |>.map (fun s => match s with
                                                      | [c, num] => (c, num.toNat!)
                                                      | _ => unreachable!)
                                     |>.push ("R", 1)
  let mut poucet : RBSet Pos compare := RBSet.empty
  let mut rope : Array Pos := Array.mkArray 10 ⟨0, 0⟩
  for cmd in commands do
    let (dir, dist) := cmd
    for _ in [0:dist] do
      match poucet.find? rope[9]! with
      | some _ => rope := moveRope rope dir
      | none => poucet := poucet.insert rope[9]!
                rope := moveRope rope dir
  return poucet.size

end Day9