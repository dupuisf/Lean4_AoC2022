import Aoc2022.Utils
import Init.Data.Queue

open System Std Array

namespace Day12

def input : FilePath := "/home/fred/lean/aoc2022/input_12"

/-
PART 1:
-/

def findIdx₂ [Inhabited α] [BEq α] (a : Array (Array α)) (x : α) : Option (Nat × Nat) := Id.run do
  for i in [0:a.size] do
    for j in [0:a[i]!.size] do
      if a[i]![j]! == x then return some (i, j)
  return none

def getNeighbors (grid : Array (Array Int)) (visited : Array (Array Bool)) (pos : Nat × Nat) 
    (chk : Int → Bool): 
    Array (Nat × Nat) := Id.run do
  let mut output := #[]
  let (x,y) := pos
  let numRows := grid.size
  let numCols := grid[0]!.size
  if x ≠ 0 ∧ chk (grid[x-1]![y]! - grid[x]![y]!) ∧ ¬(visited[x-1]![y]!) then 
    output := output.push (x - 1, y)
  if y ≠ 0 ∧ chk (grid[x]![y-1]! - grid[x]![y]!) ∧ ¬(visited[x]![y-1]!) then 
    output := output.push (x, y - 1)
  if x < numRows-1 ∧ chk (grid[x+1]![y]! - grid[x]![y]!) ∧ ¬(visited[x+1]![y]!) then 
    output := output.push (x + 1, y)
  if y < numCols-1 ∧ chk (grid[x]![y+1]! - grid[x]![y]!) ∧ ¬(visited[x]![y+1]!) then 
    output := output.push (x, y + 1)
  output

def printVisited (visited : Array (Array Bool)) : IO Unit := do 
  for row in visited do 
    for col in row do 
      if col then IO.print "#" else IO.print "·"
    IO.print "\n"

def first_part : IO Nat := do
  let gridRaw := (← IO.FS.lines input).map (·.data) |>.map (·.toArray) 
  let some start := findIdx₂ gridRaw 'S' | unreachable!
  let some tgt := findIdx₂ gridRaw 'E' | unreachable!
  let grid := gridRaw.map (·.map (fun c => match c with
                                          | 'S' => 0
                                          | 'E' => 25
                                          | x => (x.toNat - 'a'.toNat : Int)))
  let numRows := grid.size
  let numCols := grid[0]!.size
  let mut visited : Array (Array Bool) := mkArray numRows (mkArray numCols false) 
  let mut next : Queue ((Nat × Nat) × Nat) := Queue.empty
  let mut curPos := start
  let mut curDepth : Nat := 0
  while curPos ≠ tgt do
    let neighbors := getNeighbors grid visited curPos (· ≤ 1)
    next := next.enqueueAll (neighbors.zip (mkArray neighbors.size (curDepth + 1))).toList
    for p in neighbors do
      let (x, y) := p
      visited := visited.set! x (visited[x]!.set! y true)
    let some ((pos, depth), next') := next.dequeue? | panic! "couldn't find E"
    next := next'
    curPos := pos
    curDepth := depth
  --printVisited visited
  return curDepth

/-
PART 2:
-/

def second_part : IO Nat := do
  let gridRaw := (← IO.FS.lines input).map (·.data) |>.map (·.toArray) 
  let some start := findIdx₂ gridRaw 'E' | unreachable!
  let grid := gridRaw.map (·.map (fun c => match c with
                                          | 'S' => 0
                                          | 'E' => 25
                                          | x => (x.toNat - 'a'.toNat : Int)))
  let numRows := grid.size
  let numCols := grid[0]!.size
  let mut visited : Array (Array Bool) := mkArray numRows (mkArray numCols false) 
  let mut next : Queue ((Nat × Nat) × Nat) := Queue.empty
  let mut curPos := start
  let mut curDepth : Nat := 0
  while grid[curPos.1]![curPos.2]! ≠ 0 do
    let neighbors := getNeighbors grid visited curPos (· ≥ -1)
    next := next.enqueueAll (neighbors.zip (mkArray neighbors.size (curDepth + 1))).toList
    for p in neighbors do
      let (x, y) := p
      visited := visited.set! x (visited[x]!.set! y true)
    let some ((pos, depth), next') := next.dequeue? | panic! "couldn't find E"
    next := next'
    curPos := pos
    curDepth := depth
  --printVisited visited
  return curDepth

end Day12
