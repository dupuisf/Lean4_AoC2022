import Aoc2022.Utils
--import Std.Data.Nat.Init.Lemmas
import Std.Data.Nat.Lemmas
import Lean
open Lean

partial def visitBody (f : IR.FunId) : IR.FnBody → StateM (NameSet × Bool) Unit
  | .vdecl x _ e b => do
    let tailcall := match b with | .ret (.var y) => x == y | _ => false
    match e with
    | .fap c _ =>
      if tailcall && c == f then modify fun (s, _) => (s, true)
      else modify fun (s, b) => (s.insert c, b)
    | .pap c _ => modify fun (s, b) => (s.insert c, b)
    | _ => pure ()
    visitBody f b
  | .jdecl _ _ v b => do visitBody f v; visitBody f b
  | .case _ _ _ cs => for c in cs do visitBody f c.body
  | e => if e.isTerminal then pure () else visitBody f e.body

elab "#ir_stats " decl:ident : command => do
  let name := decl.getId
  let some decl := (IR.declMapExt.getState (← getEnv)).find? name
    | throwError "IR declaration {name} not found"
  match decl with
  | .extern .. => logInfo m!"{name} is external"
  | .fdecl (f := f) (body := b) .. =>
    let (_, calls, tailRec) := (visitBody f b).run ({}, false)
    if !calls.isEmpty then
      logInfo m!"{name} calls (recursively) {calls.toList}"
    if tailRec then
      logInfo m!"{name} is tail-recursive"
    else if calls.contains f then
      logInfo m!"{name} is properly recursive"
    else
      logInfo m!"{name} is not recursive"

open System

namespace Day11

def input : FilePath := "/home/fred/lean/aoc2022/input_11_test"

structure Monkey (α : Type _) where
  items : Array α
  numItems : Nat
  op : α → α
  test : α
  ifTrue : Nat
  ifFalse : Nat
deriving Inhabited

def Monkey.catchItem (m : Monkey α) (i : α) : Monkey α :=
  { m with items := m.items.push i }
  --{ m with
  --    items := m.items.set! (m.numItems) i
  --    numItems := m.numItems + 1 }

def Monkey.flushItems [BEq α] [OfNat α 0] (m : Monkey α) : Monkey α :=
  { m with items := #[] }
  --{ m with
  --    items := m.items.map fun _ => 0
  --    numItems := 0 }


/-
PART 1:
-/

def first_part : IO Nat := do
  let rawdata ← IO.FS.lines input
  let mut curLine := 0
  let mut monkeys : Array (Monkey Nat) := #[]
  while curLine < rawdata.size do
    let items := rawdata[curLine+1]!.drop 18
          |>.splitOn ", "
          |>.map String.toNat!
          |>.toArray
    let opRaw := rawdata[curLine+2]!.drop 23 |>.splitOn
    let op : Nat → Nat := match opRaw with
                           | ["*", "old"] => fun x => x*x
                           | ["*", n] => fun x => x * n.toNat!
                           | ["+", n] => fun x => x + n.toNat!
                           | _ => unreachable!
    let test := rawdata[curLine+3]!.drop 21 |>.toNat!
    let ifTrue := rawdata[curLine+4]!.drop 29 |>.toNat!
    let ifFalse := rawdata[curLine+5]!.drop 30 |>.toNat!
    monkeys := monkeys.push ⟨items.append (mkArray 100 0), items.size, op, test, ifTrue, ifFalse⟩
    curLine := curLine + 7

  let n := monkeys.size
  let mut monkeyActivity : Array Nat := Array.mkArray n 0
  for _rnd in [1:21] do
    for m in [0:n] do
      for idx in [0:monkeys[m]!.numItems] do
        let i := monkeys[m]!.items[idx]!
        monkeyActivity := monkeyActivity.modify m (· + 1)
        let i' := (monkeys[m]!.op i) / 3
        if i' % monkeys[m]!.test == 0 then
          monkeys := monkeys.modify monkeys[m]!.ifTrue (fun _ => monkeys[monkeys[m]!.ifTrue]!.catchItem i')
        else
          monkeys := monkeys.modify monkeys[m]!.ifFalse (fun _ => monkeys[monkeys[m]!.ifFalse]!.catchItem i')
      monkeys := monkeys.modify m (fun _ => monkeys[m]!.flushItems)
  let sorted := monkeyActivity.qsort (· > ·)
  return sorted[0]! * sorted[1]!


/-
PART 2:
Doesn't work due to stack overflow (!) at around 9200 rounds
-/

def oneRound (rndsLeft : Nat) (monkeyActivity : Array Nat) (monkeys : Array (Monkey UInt64)) (modulo : UInt64) :
    (Array Nat) × (Array (Monkey UInt64)) := Id.run do
  --if rndsLeft % 100 == 0 then dbg_trace s!"Rounds left: {rndsLeft}, {monkeys.foldl (fun n m => n + m.items.size) 0}"
  if rndsLeft % 100 == 0 then dbg_trace s!"Rounds left: {rndsLeft}, {monkeys[0]!.items}"
  let n := monkeys.size
  let mut monkeyActivity' := monkeyActivity
  let mut monkeys' := monkeys
  for m in [0:n] do
    for idx in [0:monkeys'[m]!.numItems] do
      let i := monkeys'[m]!.items[idx]!
      monkeyActivity' := monkeyActivity'.modify m (· + 1)
      let i' := (monkeys'[m]!.op i) % modulo
      if i' % monkeys'[m]!.test == 0 then
        monkeys' := monkeys'.modify monkeys'[m]!.ifTrue (fun _ => monkeys'[monkeys'[m]!.ifTrue]!.catchItem i')
      else
        monkeys' := monkeys'.modify monkeys'[m]!.ifFalse (fun _ => monkeys'[monkeys'[m]!.ifFalse]!.catchItem i')
    monkeys' := monkeys'.modify m (fun _ => monkeys'[m]!.flushItems)
  pure ⟨monkeyActivity', monkeys'⟩


def monkeyBusiness (rndsLeft : Nat) (monkeyActivity : Array Nat) (monkeys : Array (Monkey UInt64)) (modulo : UInt64) :
    Array Nat :=
  if h : rndsLeft ≤ 0 then monkeyActivity
  else
    have : rndsLeft - 1 < rndsLeft := by
      rw [Nat.not_le] at h
      exact Nat.sub_one_lt_of_le h (Nat.le_refl rndsLeft)
    let ⟨monkeyActivity', monkeys'⟩ := oneRound rndsLeft monkeyActivity monkeys modulo
    monkeyBusiness (rndsLeft-1) monkeyActivity' monkeys' modulo

def second_part (numRounds : Nat) : IO Nat := do
  let rawdata ← IO.FS.lines input
  let mut curLine := 0
  let mut monkeys : Array (Monkey UInt64) := #[]
  while curLine < rawdata.size do
    let items := rawdata[curLine+1]!.drop 18 |>.splitOn ", "
                      |>.map String.toNat!
                      |>.map Nat.toUInt64
                      |>.toArray
    let opRaw := rawdata[curLine+2]!.drop 23 |>.splitOn
    let op : UInt64 → UInt64 := match opRaw with
                           | ["*", "old"] => fun x => x*x
                           | ["*", n] => fun x => x * n.toNat!.toUInt64
                           | ["+", n] => fun x => x + n.toNat!.toUInt64
                           | _ => unreachable!
    let test := rawdata[curLine+3]!.drop 21 |>.toNat!.toUInt64
    let ifTrue := rawdata[curLine+4]!.drop 29 |>.toNat!
    let ifFalse := rawdata[curLine+5]!.drop 30 |>.toNat!
    monkeys := monkeys.push ⟨items ++ (mkArray 100 0), items.size, op, test, ifTrue, ifFalse⟩
    curLine := curLine + 7

  let modulo : UInt64 := monkeys.foldl (fun n mon => n * mon.test) 1
  let n := monkeys.size
  IO.println monkeys[0]!.items
  --let mut monkeyActivity : Array Nat := Array.mkArray n 0
  --for rnd in [1:numRounds+1] do
  --  if rnd % 100 == 0 then dbg_trace s!"Starting round {rnd}, {monkeys.foldl (fun n m => n + m.items.size) 0}"
  --  for m in [0:n] do
  --    for i in monkeys[m]!.items do
  --      monkeyActivity := monkeyActivity.modify m (· + 1)
  --      let i' := (monkeys[m]!.op i) % modulo
  --      if i' % monkeys[m]!.test == 0 then
  --        monkeys := monkeys.modify monkeys[m]!.ifTrue (fun _ => monkeys[monkeys[m]!.ifTrue]!.catchItem i')
  --      else
  --        monkeys := monkeys.modify monkeys[m]!.ifFalse (fun _ => monkeys[monkeys[m]!.ifFalse]!.catchItem i')
  --    monkeys := monkeys.modify m (fun _ => monkeys[m]!.flushItems)
  --dbg_trace s!"{monkeyActivity}"
  let monkeyActivity := monkeyBusiness numRounds (.mkArray n 0) monkeys modulo
  let sorted := monkeyActivity.qsort (· > ·)
  return sorted[0]! * sorted[1]!

--#ir_stats Day11.second_part

end Day11
