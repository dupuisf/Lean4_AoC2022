import Aoc2022.Utils

open System Ordering Lean Parsec

namespace Day13

def input : FilePath := "/home/fred/lean/aoc2022/input_13"

inductive Packet
| I (val : Nat)
| L (lst : List Packet)
deriving BEq, Inhabited, Repr

namespace Packet 

partial def cmp (p q : Packet) : Ordering := 
  match p, q with
  | I v, I w    => compare v w
  | L [], L []  => eq
  | L [], L l   => lt
  | L l, L []   => gt
  | I v, L l    => cmp (L [I v]) (L l)
  | L l, I v    => cmp (L l) (L [I v])
  | L (e₁ :: l₁), L (e₂ :: l₂) => match cmp e₁ e₂ with
                                  | lt => lt
                                  | gt => gt
                                  | eq => cmp (L l₁) (L l₂)

partial def toString : Packet → String
| I n => n.repr
| L l => "[" ++ ", ".intercalate (l.map toString) ++ "]"

instance : Ord Packet := ⟨cmp⟩
instance : LE Packet := leOfOrd
instance : ToString Packet := ⟨toString⟩
  
end Packet

open Packet

partial def pPacket : Parsec Packet := (return I (← natNum)) <|> do
  skipString "["
  let lst ← csv pPacket
  skipString "]"
  return L lst

/-
PART 1:
-/

def first_part : IO Nat := do
  let rawdata ← IO.FS.lines input
  let mut cnt := 0
  for i in [1:(rawdata.size+1)/3+1] do
    let packet1 := rawdata[(i-1)*3]!.yoloParse pPacket
    let packet2 := rawdata[(i-1)*3+1]!.yoloParse pPacket
    if packet1 ≤ packet2 then 
      cnt := cnt + i
  return cnt

/-
PART 2:
-/

def second_part : IO Nat := do
  let div1 := "[[2]]".yoloParse pPacket
  let div2 := "[[6]]".yoloParse pPacket
  let packets := (← IO.FS.lines input).filter (· != "")
                                     |>.map (·.yoloParse pPacket)
                                     |>.push div1
                                     |>.push div2
                                     |>.qsort (· ≤ ·) 
  let idx1 := packets.findIdx! (· == div1) + 1
  let idx2 := packets.findIdx! (· == div2) + 1
  return idx1 * idx2

end Day13
