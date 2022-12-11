import Aoc2022.Utils
import Std.Data.Array.Basic

open System Lean.Parsec

namespace Day11

def input : FilePath := "/home/fred/lean/aoc2022/input_11_test"

structure Monkey (α : Type _) where
  items : Array α
  op : α → α
  test : Nat
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
  for _ in [1:21] do
    for m in [0:n] do 
      let monkey := monkeys[m]!
      for i in monkey.items do 
        monkeyActivity := monkeyActivity.modify m (· + 1)
        let i' := (monkey.op i) / 3
        if i' % monkey.test == 0 then
          monkeys := monkeys.modify monkey.ifTrue (fun _ => monkeys[monkey.ifTrue]!.catchItem i')
          monkeys := monkeys.modify m (fun _ => monkeys[m]!.throwItem i)
        else
          monkeys := monkeys.modify monkey.ifFalse (fun _ => monkeys[monkey.ifFalse]!.catchItem i')
          monkeys := monkeys.modify m (fun _ => monkeys[m]!.throwItem i)
  for m in monkeys do 
    dbg_trace s!"{m.items}" 
  let sorted := monkeyActivity.qsort (· > ·)
  return sorted[0]! * sorted[1]!

/-
PART 2:
-/

def primes : Array UInt16 := #[2, 3, 5, 7, 11, 13, 17, 19, 23]

structure Item where
  comps : Array UInt16
  tag : Nat
deriving BEq, Inhabited, Repr

namespace Item

def _root_.Nat.toItem (n : Nat) (tag : Nat) : Item := ⟨primes.map (fun p => n.toUInt16 % p), tag⟩

instance : Add Item :=
  { add := fun x y => ⟨(x.comps.zip y.comps).zipWith primes (fun (x',y') p => x'+y' % p), x.2⟩ }

instance : Mul Item :=
  { mul := fun x y => ⟨(x.comps.zip y.comps).zipWith primes (fun (x',y') p => x'*y' % p), x.2⟩ }

end Item

def second_part : IO Nat := do
  let rawdata ← IO.FS.lines input
  let mut curLine := 0
  let mut monkeys : Array (Monkey Item) := #[]
  let mut itemCount := 0
  while curLine < rawdata.size do
    let itemsRaw := rawdata[curLine+1]!.drop 18 |>.splitOn ", " |>.map String.toNat! |>.toArray
    let items := itemsRaw.zipWith ((Array.range (itemsRaw.size)).map (· + itemCount)) (fun x t => x.toItem t)
    itemCount := itemCount + items.size
    let opRaw := rawdata[curLine+2]!.drop 23 |>.splitOn
    let op : Item → Item := match opRaw with
                           | ["*", "old"] => fun x => x*x
                           | ["*", n] => fun x => x * n.toNat!.toItem 0
                           | ["+", n] => fun x => x + n.toNat!.toItem 0
                           | _ => unreachable!
    let test := primes.findIdx! (fun p => p == (rawdata[curLine+3]!.drop 21 |>.toNat!.toUInt16))
    let ifTrue := rawdata[curLine+4]!.drop 29 |>.toNat!
    let ifFalse := rawdata[curLine+5]!.drop 30 |>.toNat!
    monkeys := monkeys.push ⟨items, op, test, ifTrue, ifFalse⟩ 
    curLine := curLine + 7

  for m in monkeys do 
    dbg_trace s!"{m.ifFalse}" 

  dbg_trace "\n"

  let n := monkeys.size
  let mut monkeyActivity : Array Nat := Array.mkArray n 0
  for rnd in [1:2000] do
    if rnd % 500 == 0 then dbg_trace s!"Starting round {rnd}"
    for m in [0:n] do 
      for i in monkeys[m]!.items do 
        monkeyActivity := monkeyActivity.modify m (· + 1)
        let i' := monkeys[m]!.op i
        if i'.comps[monkeys[m]!.test]! == 0 then
          monkeys := monkeys.modify monkeys[m]!.ifTrue (fun _ => monkeys[monkeys[m]!.ifTrue]!.catchItem i')
          monkeys := monkeys.modify m (fun _ => monkeys[m]!.throwItem i)
        else
          monkeys := monkeys.modify monkeys[m]!.ifFalse (fun _ => monkeys[monkeys[m]!.ifFalse]!.catchItem i')
          monkeys := monkeys.modify m (fun _ => monkeys[m]!.throwItem i)
  for m in monkeys do 
    dbg_trace s!"{m.items.map (fun i => i.tag)}" 
  let sorted := monkeyActivity.qsort (· > ·)
  dbg_trace s!"{monkeyActivity}"
  return sorted[0]! * sorted[1]!

end Day11
