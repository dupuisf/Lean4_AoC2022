import Std.Data.Array.Basic
import Aoc2022.Utils

open System

namespace Day3

def input : FilePath := "/home/fred/lean/aoc2022/input_03"

/-
PART 1:
One Elf has the important job of loading all of the rucksacks with supplies for the jungle journey. Unfortunately, that Elf didn't quite follow the packing instructions, and so a few items now need to be rearranged.

Each rucksack has two large compartments. All items of a given type are meant to go into exactly one of the two compartments. The Elf that did the packing failed to follow this rule for exactly one item type per rucksack.

The Elves have made a list of all of the items currently in each rucksack (your puzzle input), but they need your help finding the errors. Every item type is identified by a single lowercase or uppercase letter (that is, a and A refer to different types of items).

The list of items for each rucksack is given as characters all on a single line. A given rucksack always has the same number of items in each of its two compartments, so the first half of the characters represent items in the first compartment, while the second half of the characters represent items in the second compartment.

For example, suppose you have the following list of contents from six rucksacks:

vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw

    The first rucksack contains the items vJrwpWtwJgWrhcsFMMfFFhFp, which means its first compartment contains the items vJrwpWtwJgWr, while the second compartment contains the items hcsFMMfFFhFp. The only item type that appears in both compartments is lowercase p.
    The second rucksack's compartments contain jqHRNqRjqzjGDLGL and rsFMfFZSrLrFZsSL. The only item type that appears in both compartments is uppercase L.
    The third rucksack's compartments contain PmmdzqPrV and vPwwTWBwg; the only common item type is uppercase P.
    The fourth rucksack's compartments only share item type v.
    The fifth rucksack's compartments only share item type t.
    The sixth rucksack's compartments only share item type s.

To help prioritize item rearrangement, every item type can be converted to a priority:

    Lowercase item types a through z have priorities 1 through 26.
    Uppercase item types A through Z have priorities 27 through 52.

In the above example, the priority of the item type that appears in both compartments of each rucksack is 16 (p), 38 (L), 42 (P), 22 (v), 20 (t), and 19 (s); the sum of these is 157.

Find the item type that appears in both compartments of each rucksack. What is the sum of the priorities of those item types?
-/

def priority (c : Char) : UInt32 :=
  if 'a' ??? c ??? c ??? 'z' then c.val - 'a'.val + 1
    else if 'A' ??? c ??? c ??? 'Z' then c.val - 'A'.val + 27
      else 0

variable {?? : Type _} [Inhabited ??] [DecidableEq ??] [LT ??] [DecidableRel (??:=??) (?? < ??)]

/-- Finding the common element in two sorted lists, with considerable pain to avoid `partial`, 
and I still cheated with `s[??]!`! -/
def find_common (s??? : Array ??) (s??? : Array ??) (p??? p??? : Nat) : ?? :=
  if s???[p???]! = s???[p???]! then s???[p???]!
    else if s???[p???]! < s???[p???]! then 
      (if H : p??? = 0 then default else 
        have h : 0 < p??? := Nat.zero_lt_of_ne_zero H
        have h' : p??? - 1 < p??? := Nat.sub_lt h Nat.zero_lt_one
        have h??? : p??? + (p??? - 1) < p??? + p??? := Nat.add_lt_add_left h' p???
        find_common s??? s??? p??? (p???-1))
      else (if H : p??? = 0 then default else 
        have h : 0 < p??? := Nat.zero_lt_of_ne_zero H
        have h' : p??? - 1 < p??? := Nat.sub_lt h Nat.zero_lt_one
        have h??? : (p??? - 1) + p??? < p??? + p??? := Nat.add_lt_add_right h' p???
        find_common s??? s??? (p???-1) p???)
termination_by _ => p??? + p???

def first_part : IO Nat := do
  let rawdata ??? IO.FS.lines input
  let f : String ??? Nat := 
    fun s =>
      let s??? := ((s.take (s.length / 2)).toList.toArray.map priority).qsort (?? < ??)
      let s??? := ((s.drop (s.length / 2)).toList.toArray.map priority).qsort (?? < ??)
      (find_common s??? s??? (s???.size - 1) (s???.size - 1)).toNat
  let processed := Array.map f rawdata
  return processed.foldl (?? + ??) 0

/-
PART 2:
As you finish identifying the misplaced items, the Elves come to you with another issue.

For safety, the Elves are divided into groups of three. Every Elf carries a badge that identifies their group. For efficiency, within each group of three Elves, the badge is the only item type carried by all three Elves. That is, if a group's badge is item type B, then all three Elves will have item type B somewhere in their rucksack, and at most two of the Elves will be carrying any other item type.

The problem is that someone forgot to put this year's updated authenticity sticker on the badges. All of the badges need to be pulled out of the rucksacks so the new authenticity stickers can be attached.

Additionally, nobody wrote down which item type corresponds to each group's badges. The only way to tell which item type is the right one is by finding the one item type that is common between all three Elves in each group.

Every set of three lines in your list corresponds to a single group, but each group can have a different badge item type. So, in the above example, the first group's rucksacks are the first three lines:

vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg

And the second group's rucksacks are the next three lines:

wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw

In the first group, the only item type that appears in all three rucksacks is lowercase r; this must be their badges. In the second group, their badge item type must be Z.

Priorities for these items must still be found to organize the sticker attachment efforts: here, they are 18 (r) for the first group and 52 (Z) for the second group. The sum of these is 70.

Find the item type that corresponds to the badges of each three-Elf group. What is the sum of the priorities of those item types?
-/

def max??? [Max ??] (x y z : ??) : ?? := max x $ max y z

/-- Not doing this again! -/
partial def find_common??? [Max ??] (s??? s??? s??? : Array ??) (p??? p??? p??? : Nat) : ?? := Id.run do
  let x??? := s???[p???]!
  let x??? := s???[p???]!
  let x??? := s???[p???]!
  if x??? = x??? ??? x??? = x??? then return x???
  let m := max??? x??? x??? x???
  if x??? = m then return (if p??? = 0 then default else find_common??? s??? s??? s??? (p??? - 1) p??? p???)
  if x??? = m then return (if p??? = 0 then default else find_common??? s??? s??? s??? p??? (p??? - 1) p???)
  else return (if p??? = 0 then default else find_common??? s??? s??? s??? p??? p??? (p??? - 1))

def second_part : IO Nat := do
  let rawdata ??? IO.FS.lines input
  let mut result : Nat := 0
  for i in Array.range (rawdata.size / 3) do
    let s??? := ((rawdata[3*i]!).toList.toArray.map priority).qsort (?? < ??)
    let s??? := ((rawdata[3*i + 1]!).toList.toArray.map priority).qsort (?? < ??)
    let s??? := ((rawdata[3*i + 2]!).toList.toArray.map priority).qsort (?? < ??)
    let x := find_common??? s??? s??? s??? (s???.size - 1) (s???.size - 1) (s???.size - 1)
    result := result + x.toNat
  return result

end Day3
