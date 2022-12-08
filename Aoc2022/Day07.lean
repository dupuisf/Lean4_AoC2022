import Aoc2022.Utils

open System Lean Parsec

namespace Day7

def input : FilePath := "/home/fred/lean/aoc2022/input_07"

/-
PART 1:
-/

inductive DirElem where
| File (name : String) (size : Nat)
| Directory (name : String) (parent : Option DirElem) (content : Array DirElem)
| DirStub (name : String) (parent : Option DirElem)
deriving BEq, Repr

namespace DirElem

instance : Inhabited DirElem := ⟨DirElem.File "" 0⟩

def getParent : DirElem → Option DirElem
| File _ _ => none
| Directory _ parent _ => parent
| DirStub _ parent => parent

def hasParent : DirElem → Bool
| Directory _ (some _) _ => true
| DirStub _ (some _) => true
| _ => false

def addDirToContent (dir : DirElem) (name : String) (content : Array DirElem) : Option DirElem :=
  match dir with
  | Directory nm par ct => some (Directory nm par (ct.push (Directory name none content))) 
  | _ => none

def addDirStubToContent (dir : DirElem) (name : String) : Option DirElem :=
  match dir with
  | Directory nm par ct => some (Directory nm par (ct.push (DirStub name none))) 
  | _ => none

def moveToParent : DirElem → Option DirElem
| Directory name (some parent) content => parent.addDirToContent name content
| DirStub name (some parent) => parent.addDirStubToContent name 
| _ => none

partial def moveToRoot (cur : DirElem) : Option DirElem := do
  if !cur.hasParent then return cur
  let some par := cur.moveToParent | none
  moveToRoot par

def getName : DirElem → String
| File name _ => name
| Directory name _ _ => name
| DirStub name _ => name

def getContent? : DirElem → Option (Array DirElem)
| Directory _ _ content => some content
| _ => none

def isStub : DirElem → Bool
| DirStub _ _ => true
| _ => false

def findChild? (cur : DirElem) (childName : String) : Option Nat :=
  match cur with
  | Directory _nm _par ct => ct.findIdx? (fun x => x.getName == childName)
  | _ => none

def deleteElem (dir : DirElem) (name : String) : Option DirElem :=
  match dir with 
  | Directory nm par ct => match dir.findChild? name with 
                           | some d => Directory nm par (ct.eraseIdx d)
                           | none => none
  | _ => none

def moveToChild (cur : DirElem) (childName : String) : Option DirElem := do
  let Directory curName curParent curCt := cur | none
  let some idx := cur.findChild? childName | none
  let some child := curCt[idx]? | unreachable!
  match child with
  | Directory childName _ childCt => Directory childName (Directory curName curParent (curCt.eraseIdx idx)) childCt
  | DirStub childName _ => DirStub childName (Directory curName curParent (curCt.eraseIdx idx))
  | _ => none

partial def getTotalSize (curSize : Nat) : DirElem → Nat
| File _ size => curSize + size
| Directory _ _ content => curSize + content.foldl getTotalSize 0
| DirStub _ _ => curSize

partial def getTotalSizeAtMost (bound curSize : Nat) : DirElem → Nat
| Directory nm par content => let mySize := getTotalSize 0 (Directory nm par content)
                              if mySize ≤ bound then curSize + mySize + content.foldl (getTotalSizeAtMost bound) 0
                                else curSize + content.foldl (getTotalSizeAtMost bound) 0
| _ => curSize

partial def getSmallestAboveBound (bound curBest : Nat) : DirElem → Nat 
| Directory nm par content => let mySize := getTotalSize 0 (Directory nm par content)
                              if mySize ≥ bound then
                                let newBest := min mySize curBest
                                min newBest (content.foldl (getSmallestAboveBound bound) 70000000)
                              else min curBest (content.foldl (getSmallestAboveBound bound) 70000000)
| _ => curBest

end DirElem

inductive Command where
| cd (dir : String)  
| cd_up
| cd_root
| ls (list : Array DirElem)
deriving BEq, Repr

/- Parser -/

def filenameChar : Parsec Char := attempt do
  let c ← anyChar 
  if ('A' ≤ c ∧ c ≤ 'Z') ∨ ('a' ≤ c ∧ c ≤ 'z') then return c 
  if '0' ≤ c ∧ c ≤ '9' then return c
  if c == '.' then return c
  fail s!"Unexpected character in file name"

def p_file : Parsec DirElem := do
  let size ← natNum
  ws
  let name ← manyChars filenameChar
  eol
  return DirElem.File name size

def p_dir : Parsec DirElem := do
  skipString "dir"
  ws
  let name ← manyChars filenameChar
  eol
  return DirElem.DirStub name none

def pListing : Parsec DirElem := p_file <|> p_dir

def p_cd : Parsec Command := do
  skipString "$ cd "
  let dir ← manyChars filenameChar
  eol
  return Command.cd dir

def p_cd_up : Parsec Command := do
  skipString "$ cd .."
  eol
  return Command.cd_up

def p_cd_root : Parsec Command := do
  skipString "$ cd /"
  eol
  return Command.cd_root

def p_ls : Parsec Command := do
  skipString "$ ls"
  eol
  return Command.ls (← many pListing)

def pCommand : Parsec Command := p_cd_up <|> p_cd_root <|> p_cd <|> p_ls

def pWholething : Parsec (Array Command) := many pCommand

def traverse (cur : DirElem) (cmd : Command) : Except String DirElem := do
  match cmd with
  | Command.cd_up       => let some new := cur.moveToParent | throw "No parent to go back to"
                           return new
  | Command.cd_root     => return cur   -- Is this cheating? Hell yeah.
  | Command.cd childName  => let some new := cur.moveToChild childName | throw "Couldn't move to child"
                             return new
  | Command.ls lst      => if !cur.isStub then return cur
                           return DirElem.Directory cur.getName cur.getParent lst

def first_part : IO Nat := do
  let rawdata ← IO.FS.readFile input
  let Except.ok parsed := pWholething.run rawdata | panic! "Parsing error"
  let finalDirE := parsed.foldlM traverse (DirElem.DirStub "root" none)
  match finalDirE with
  | Except.ok finalDir => let some rootDir := finalDir.moveToRoot | panic! "Can't move to root"
                          return rootDir.getTotalSizeAtMost 100000 0
  | Except.error e => dbg_trace e
                        return 0

/-
PART 2:
-/

def second_part : IO Nat := do
  let rawdata ← IO.FS.readFile input
  let Except.ok parsed := pWholething.run rawdata | panic! "Parsing error"
  let finalDirE := parsed.foldlM traverse (DirElem.DirStub "root" none)
  match finalDirE with
  | Except.ok finalDir => let some rootDir := finalDir.moveToRoot | panic! "Can't move to root"
                          return rootDir.getSmallestAboveBound 8748071 70000000
  | Except.error e => dbg_trace e
                        return 0


end Day7
