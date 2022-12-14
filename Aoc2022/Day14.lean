import Aoc2022.Utils
import Std.Data.Array.Basic

open System

namespace Day14

def input : FilePath := "/home/fred/lean/aoc2022/input_14"

inductive Material where
| rock
| sand
| air
deriving BEq, Inhabited

instance : ToString Material := ⟨(match · with
                                | .air => "."
                                | .rock => "#"
                                | .sand => "o")⟩

namespace Material

def isSolid : Material → Bool
| .rock => true 
| .sand => true 
| .air => false

def toChar : Material → Char
| .rock => '#'
| .sand => 'o'
| .air => '.'

end Material

open Material

def genLine (s e : Nat × Nat) : Array (Nat × Nat) :=
  let (x₁, y₁) := s
  let (x₂, y₂) := e
  if x₁ = x₂ then 
    if (y₁ ≤ y₂) then Array.range (y₂ - y₁ + 1) |>.map (fun i => (x₁, y₁+i))
    else Array.range (y₁ - y₂ + 1) |>.map (fun i => (x₁, y₂+i))
  else if x₁ < x₂ then Array.range (x₂ - x₁ + 1) |>.map (fun i => (x₁ + i, y₁))
  else Array.range (x₁ - x₂ + 1) |>.map (fun i => (x₂ + i, y₁))

def printMap (a : Array (Array Material)) : IO Unit := do 
  for l in a do 
    let s : String := ⟨(l.map Material.toChar).toList⟩
    IO.println s

-- `none` if it falls off the grid
partial def simSand (src : Nat × Nat) (grid : Array (Array Material)) : Option (Nat × Nat) := Id.run do
  let (x, y) := src
  let maxY := grid.size - 1
  if y = maxY then return none
  if !grid[y+1]![x]!.isSolid then return simSand (x, y+1) grid
  if !grid[y+1]![x-1]!.isSolid then return simSand (x-1, y+1) grid
  if !grid[y+1]![x+1]!.isSolid then return simSand (x+1, y+1) grid
  return (x, y)

/-
PART 1:
-/

def first_part : IO Nat := do
  let rockPaths := (← IO.FS.lines input).map String.splitOn
                                    |>.map (·.filter (· != "->"))
                                    |>.map (·.map (String.splitOn (sep := ",")))
                                    |>.map List.toArray
                                    |>.map (·.map (match · with
                                                  | [x, y] => (x.toNat!, y.toNat!)
                                                  | _ => unreachable! ))
  let minX := rockPaths.foldl (fun acc l => min acc $ l.foldl (fun acc' (x,_) => min acc' x) 1000) 1000
  let maxX := rockPaths.foldl (fun acc l => max acc $ l.foldl (fun acc' (x,_) => max acc' x) 0) 0
  let maxY := rockPaths.foldl (fun acc l => max acc $ l.foldl (fun acc' (_,y) => max acc' y) 0) 0
  let source := (500 - minX + 1, 0)
  let mut rockMap := Array.mkArray (maxY+1) (Array.mkArray (maxX - minX + 3) Material.air)
  for path in rockPaths do 
    let mut curPos := path[0]!
    for i in [1:path.size] do 
      let nextPos := path[i]!
      for (x, y) in genLine curPos nextPos do 
        rockMap := rockMap.modify₂ y (x - minX + 1) (fun _ => .rock)
      curPos := nextPos

  printMap rockMap

  let mut flag := false
  let mut cnt := 0
  while !flag do 
    let nextSand := simSand source rockMap
    if nextSand = none then flag := true
    else 
      let some (x, y) := nextSand | unreachable!
      rockMap := rockMap.modify₂ y x (fun _ => .sand)
      cnt := cnt + 1

  IO.println ""
  IO.println ""
  IO.println ""
  printMap rockMap
  return cnt

/-
PART 2:
-/

def second_part : IO Nat := do
  let rockPaths := (← IO.FS.lines input).map String.splitOn
                                    |>.map (·.filter (· != "->"))
                                    |>.map (·.map (String.splitOn (sep := ",")))
                                    |>.map List.toArray
                                    |>.map (·.map (match · with
                                                  | [x, y] => (x.toNat!, y.toNat!)
                                                  | _ => unreachable! ))
  let minX' := rockPaths.foldl (fun acc l => min acc $ l.foldl (fun acc' (x,_) => min acc' x) 1000) 1000
  let maxX' := rockPaths.foldl (fun acc l => max acc $ l.foldl (fun acc' (x,_) => max acc' x) 0) 0
  let maxY := rockPaths.foldl (fun acc l => max acc $ l.foldl (fun acc' (_,y) => max acc' y) 0) 0
  let minX := minX' - maxY - 10
  let maxX := 500+maxX' + maxY + 10
  let source := (500 - minX + 1, 0)
  let mut rockMap := Array.mkArray (maxY+4) (Array.mkArray (maxX - minX + 3) Material.air)
  let rockPaths' := rockPaths.push #[(0, maxY+2), (maxX, maxY+2)]
  for path in rockPaths' do 
    let mut curPos := path[0]!
    for i in [1:path.size] do 
      let nextPos := path[i]!
      for (x, y) in genLine curPos nextPos do 
        rockMap := rockMap.modify₂ y (x - minX + 1) (fun _ => .rock)
      curPos := nextPos

  let mut flag := false
  let mut cnt := 0
  while !flag do 
    let nextSand := simSand source rockMap
    if nextSand = none then panic! "Falling off!"
    if nextSand = some source then 
      flag := true
      break
    else 
      let some (x, y) := nextSand | unreachable!
      rockMap := rockMap.modify₂ y x (fun _ => .sand)
      cnt := cnt + 1

  IO.println ""
  IO.println ""
  IO.println ""
  return cnt+1

end Day14
