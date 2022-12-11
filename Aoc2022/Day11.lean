import Aoc2022.Utils
import Std.Data.Array.Basic

open System Lean.Parsec

namespace Day11

def input : FilePath := "/home/fred/lean/aoc2022/input_11_test"

structure Monkey (α : Type _) where
  items : Array α
  op : α → α
  test : α
  ifTrue : Nat
  ifFalse : Nat
deriving Inhabited

def Monkey.catchItem (m : Monkey α) (i : α) : Monkey α :=
  { m with items := m.items.push i }

def Monkey.throwItem [BEq α] (m : Monkey α) (i : α) : Monkey α :=
  { m with items := m.items.erase i }

/-
PART 1:
-/

def first_part : IO Nat := do
  let rawdata ← IO.FS.lines input
  let mut curLine := 0
  let mut monkeys : Array (Monkey Nat) := #[]
  while curLine < rawdata.size do
    let items := rawdata[curLine+1]!.drop 18 |>.splitOn ", " |>.map String.toNat! |>.toArray
    let opRaw := rawdata[curLine+2]!.drop 23 |>.splitOn
    let op : Nat → Nat := match opRaw with
                           | ["*", "old"] => fun x => x*x
                           | ["*", n] => fun x => x * n.toNat!
                           | ["+", n] => fun x => x + n.toNat!
                           | _ => unreachable!
    let test := rawdata[curLine+3]!.drop 21 |>.toNat!
    let ifTrue := rawdata[curLine+4]!.drop 29 |>.toNat!
    let ifFalse := rawdata[curLine+5]!.drop 30 |>.toNat!
    monkeys := monkeys.push ⟨items, op, test, ifTrue, ifFalse⟩ 
    curLine := curLine + 7

  let n := monkeys.size
  let mut monkeyActivity : Array Nat := Array.mkArray n 0
  for _rnd in [1:21] do
    for m in [0:n] do 
      for i in monkeys[m]!.items do 
        monkeyActivity := monkeyActivity.modify m (· + 1)
        let i' := (monkeys[m]!.op i) / 3
        if i' % monkeys[m]!.test == 0 then
          monkeys := monkeys.modify monkeys[m]!.ifTrue (fun _ => monkeys[monkeys[m]!.ifTrue]!.catchItem i')
          monkeys := monkeys.modify m (fun _ => monkeys[m]!.throwItem i)
        else
          monkeys := monkeys.modify monkeys[m]!.ifFalse (fun _ => monkeys[monkeys[m]!.ifFalse]!.catchItem i')
          monkeys := monkeys.modify m (fun _ => monkeys[m]!.throwItem i)
  let sorted := monkeyActivity.qsort (· > ·)
  return sorted[0]! * sorted[1]!


/-
PART 2:
Doesn't work due to stack overflow (!) at around 7500 rounds
-/

def second_part (numRounds : Nat) : IO Nat := do
  let rawdata ← IO.FS.lines input
  let mut curLine := 0
  let mut monkeys : Array (Monkey UInt64) := #[]
  while curLine < rawdata.size do
    let items := rawdata[curLine+1]!.drop 18 |>.splitOn ", " |>.map String.toNat! |>.map Nat.toUInt64 |>.toArray
    let opRaw := rawdata[curLine+2]!.drop 23 |>.splitOn
    let op : UInt64 → UInt64 := match opRaw with
                           | ["*", "old"] => fun x => x*x
                           | ["*", n] => fun x => x * n.toNat!.toUInt64
                           | ["+", n] => fun x => x + n.toNat!.toUInt64
                           | _ => unreachable!
    let test := rawdata[curLine+3]!.drop 21 |>.toNat!.toUInt64
    let ifTrue := rawdata[curLine+4]!.drop 29 |>.toNat!
    let ifFalse := rawdata[curLine+5]!.drop 30 |>.toNat!
    monkeys := monkeys.push ⟨items, op, test, ifTrue, ifFalse⟩ 
    curLine := curLine + 7

  let modulo : UInt64 := monkeys.foldl (fun n mon => n * mon.test) 1
  let n := monkeys.size
  let mut monkeyActivity : Array Nat := Array.mkArray n 0
  for rnd in [1:numRounds+1] do
    if rnd % 100 == 0 then dbg_trace s!"Starting round {rnd}, {monkeys.foldl (fun n m => n + m.items.size) 0}"
    for m in [0:n] do 
      for i in monkeys[m]!.items do 
        monkeyActivity := monkeyActivity.modify m (· + 1)
        let i' := (monkeys[m]!.op i) % modulo
        if i' % monkeys[m]!.test == 0 then
          monkeys := monkeys.modify monkeys[m]!.ifTrue (fun _ => monkeys[monkeys[m]!.ifTrue]!.catchItem i')
          monkeys := monkeys.modify m (fun _ => monkeys[m]!.throwItem i)
        else
          monkeys := monkeys.modify monkeys[m]!.ifFalse (fun _ => monkeys[monkeys[m]!.ifFalse]!.catchItem i')
          monkeys := monkeys.modify m (fun _ => monkeys[m]!.throwItem i)
  dbg_trace s!"{monkeyActivity}" 
  let sorted := monkeyActivity.qsort (· > ·)
  return sorted[0]! * sorted[1]!

end Day11
