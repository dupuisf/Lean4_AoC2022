import Lean.Data.Parsec

namespace Array

def max [Inhabited α] [Max α] (a : Array α) : α :=
  if a.size = 0 then default else Array.foldl Max.max a[0]! a

def findIdx! {α : Type _} (as : Array α) (p : α → Bool) : Nat := 
  match as.findIdx? p with
  | some x => x
  | none => panic!"Element not found"

def filterWithIdx (as : Array α) (p : Nat → α → Bool) : Array α :=
  (·.2) <| as.foldl (init := (0, Array.empty)) fun (idx, r) a =>
    if p idx a then
      (idx+1, r.push a)
    else
      (idx+1, r)

def mkArrayWithNCopies (n : Nat) (x : α) : Array α :=
  match n with
  | 0 => #[]
  | m+1 => (mkArrayWithNCopies m x).push x

end Array

namespace String

def toCharArray (s : String) : Array Char := s.data.toArray

def ofCharArray (a : Array Char) : String := { data := a.toList }

end String

namespace Lean.Parsec

def natNum : Parsec Nat := do
  let some n := (← manyChars digit).toNat? | fail "Not a natural number"
  return n

end Lean.Parsec