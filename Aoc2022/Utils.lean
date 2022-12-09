import Lean.Data.Parsec

namespace Array

def max [Inhabited α] [Max α] (a : Array α) : α :=
  if a.size = 0 then default else Array.foldl Max.max a[0]! a

def findIdx! (as : Array α) (p : α → Bool) : Nat := 
  match as.findIdx? p with
  | some x => x
  | none => panic!"Element not found"

def filterWithIdx (as : Array α) (p : Nat → α → Bool) : Array α :=
  (·.2) <| as.foldl (init := (0, Array.empty)) fun (idx, r) a =>
    if p idx a then
      (idx+1, r.push a)
    else
      (idx+1, r)

def mkArray₂ (m n : Nat) (v : α) : Array (Array α) :=
  Array.mkArray m (Array.mkArray n v) 

def foldtlM [Monad m] (f : β → α → m β) (init : β) (a : Array (Array α)) : m β :=
  a.foldlM (fun x row => row.foldlM f x) init

def foldtl (f : β → α → β) (init : β) (a : Array (Array α)) : β :=
  a.foldl (fun x row => row.foldl f x) init

def transpose [Inhabited α] (a : Array (Array α)) : Array (Array α) := Id.run do
  let dim := a.size
  let mut output : Array (Array α) := #[]
  for i in [0:dim] do
    let curCol := a.map (fun row => row[i]!)
    output := output.push curCol
  return output

def zipWith2D (a : Array (Array α)) (b : Array (Array β)) (f : α → β → γ) : Array (Array γ) :=
  a.zipWith b (fun ra rb => ra.zipWith rb f)

def modify₂ (a : Array (Array α)) (i j : Nat) (f : α → α) : Array (Array α) :=
  a.modify i (fun row => row.modify j f)

end Array

namespace String

def toCharArray (s : String) : Array Char := s.data.toArray

def ofCharArray (a : Array Char) : String := { data := a.toList }

end String

namespace Lean.Parsec

def natNum : Parsec Nat := do
  let some n := (← manyChars digit).toNat? | fail "Not a natural number"
  return n

def newlineChar : Parsec Unit := attempt do
  let c ← anyChar
  if c == '\u000a' || c == '\u000a' then return () else fail s!"Newline not found"

def eol : Parsec Unit := eof <|> (many1 newlineChar *> pure ())

end Lean.Parsec