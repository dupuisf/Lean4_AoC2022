import Lake
open Lake DSL

package aoc2022 {
  -- add package configuration options here
}

@[default_target]
lean_lib Aoc2022 

@[default_target]
lean_exe aoc2022 {
  root := `Main
}

@[default_target]
lean_exe mwe {
  root := `Aoc2022.Scratchpad
}

require std from git "https://github.com/leanprover/std4" @ "main"
