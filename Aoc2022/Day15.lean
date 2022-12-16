import Aoc2022.Utils
import Std.Data.Array.Basic

open System

namespace Day15

def testinput : FilePath := "/home/fred/lean/aoc2022/input_15_test"
def realinput : FilePath := "/home/fred/lean/aoc2022/input_15"

structure Sensor where
  x : Int
  y : Int
  radius : Nat
deriving BEq, Repr, Inhabited

/-
PART 1:
-/

def dist (a b : Int × Int) : Nat := (a.1 - b.1).natAbs + (a.2 - b.2).natAbs

def first_part (input : FilePath) : IO Nat := do
  let rawdata := (← IO.FS.lines input).map (·.map (fun c => if !c.isDigit ∧ c != '-' then ' ' else c))
                                      |>.map String.splitOn
                                      |>.map (·.filter (fun s => s != ""))
                                      |>.map (match · with | [a, b, c, d] => (a.toInt!,b.toInt!,c.toInt!,d.toInt!)
                                                                 | _ => unreachable!)
  let sensors : List Sensor := (rawdata.map (fun (a, b, c, d) => ⟨a, b, dist (a, b) (c, d)⟩)).toList
  let beacons : List (Int × Int) := (rawdata.map (fun (_, _, c, d) => (c, d))).toList.eraseDups
  --let maxXsensors : Int := sensors.foldl (fun acc (x, y) => max acc x) (-2000000000 : Int)
  --let minXsensors : Int := sensors.foldl (fun acc (x, y) => min acc x) (2000000000 : Int)
  --let maxXbeacons : Int := beacons.foldl (fun acc (x, y) => max acc x) (-2000000000 : Int)
  --let minXbeacons : Int := beacons.foldl (fun acc (x, y) => min acc x) (2000000000 : Int)
  --let minX := min minXbeacons minXsensors - 5
  --let maxX := max maxXbeacons maxXsensors + 5

  --let mut cnt := 0
  ----let y := 10
  --let y := 2000000
  --for x in ((Array.range (maxX - minX).toNat).map (fun n => Int.ofNat n + minX)) do 
  --  if beacons.contains (x, y) then continue
  --  let mut flag := false
  --  for (x', y', d) in sensors do 
  --    if dist (x, y) (x', y') ≤ d then flag := true
  --  if flag then 
  --    cnt := cnt + 1

  --return cnt
  return 0

--#eval first_part testinput
--#eval first_part realinput

/-
PART 2 Not working yet
-/

def dimx := 20
def dimy := 20
--def dimx := 4000000
--def dimy := 4000000

namespace Sensor

def findTL (s : Sensor) : Int := s.y + (s.x - s.radius)
def findTR (s : Sensor) : Int := s.y - s.radius - s.x
def findBL (s : Sensor) : Int := s.y - (s.x - s.radius) 
def findBR (s : Sensor) : Int := s.y + s.radius + s.x

end Sensor

def getAnswer (tl tr : Int) : Option (Int × Int) := some ⟨(tl - tr + 1)/2, (tl + tr)/2⟩

def intersect (pos : Int) (s : Sensor) (dir : Fin 4) : Int :=
  match dir with 
  | 0 => max pos s.findBR     -- TL
  | 1 => max pos s.findBL     -- TR
  | 2 => min pos s.findTR     -- BL
  | 3 => min pos s.findTL     -- BR

def sens1 : Sensor := ⟨2, 18, dist (2, 18) (-2, 15)⟩
def sens2 : Sensor := ⟨9, 16, dist (9, 16) (10, 16)⟩
#eval sens1.findTL
#eval sens2.findTL
#eval intersect 27 sens2 24

partial def backtrack (tl tr bl br : Int) (sensors : List Sensor) : Option (Int × Int) := Id.run do 
  if sensors.length == 2 then
    dbg_trace s!"Empty: {tl} {tr} {bl} {br}"
  if tl > br then return none
  if tr > bl then return none
  if tl > dimy + dimx then return none
  if tr > dimy then return none
  if bl < -dimx then return none
  if br < 0 then return none
  if sensors.isEmpty then 
    if (br - tl ≤ 1) ∧ (bl - tr ≤ 1) then return getAnswer tl tr
    panic! "Help! multiple answers possible."
    return none
  let s :: sens := sensors | unreachable!
  --dbg_trace s!"Here: {tl} {tr} {bl} {br}, cursensor : {reprStr sensors.head!}"
  if intersect tl s 0 ≠ tl then
    let anstl := backtrack (intersect tl s 0) tr bl br sens
    if anstl != none then dbg_trace "TL cursensor : {reprStr s}, {tl} {tr} {bl} {br}"; return anstl
  if intersect tr s 1 ≠ tr then
    let anstr := backtrack tl (intersect tr s 1) bl br sens
    if anstr != none then dbg_trace "TR cursensor : {reprStr s}, {tl} {tr} {bl} {br}"; return anstr
  if intersect bl s 2 ≠ bl then
    let ansbl := backtrack tl tr (intersect bl s 2) br sens
    if ansbl != none then dbg_trace s!"BL cursensor : {reprStr s}, {tl} {tr} {bl} {br}"; return ansbl
  let ansbr := backtrack tl tr bl (intersect br s 3) sens
  if ansbr != none then dbg_trace "BR  cursensor : {reprStr s}, {tl} {tr} {bl} {br}"; return ansbr
  return none
  
def second_part (input : FilePath) : IO Int := do
  let rawdata := (← IO.FS.lines input).map (·.map (fun c => if !c.isDigit ∧ c != '-' then ' ' else c))
                                      |>.map String.splitOn
                                      |>.map (·.filter (fun s => s != ""))
                                      |>.map (match · with | [a, b, c, d] => (a.toInt!,b.toInt!,c.toInt!,d.toInt!)
                                                                 | _ => unreachable!)
  let sensors : List Sensor := (rawdata.map (fun (a, b, c, d) => ⟨a, b, 1+ dist (a, b) (c, d)⟩)).toList
  match backtrack 0 (-dimx) dimy (dimy + dimx) sensors with
  | some (x, y) => IO.println s!"({x}, {y})"
                   return 4000000*x + y
  | none => IO.println "None found"
            return 0

--#eval second_part testinput
--#eval second_part realinput

end Day15
