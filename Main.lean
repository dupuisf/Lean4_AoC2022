import Aoc2022

def main (args : List String) : IO Unit :=
  match args with
  | ["1"] => do
    IO.println "Day 1:"
    IO.println s!"Part 1: {← Day1.first_part}"
    IO.println s!"Part 2: {← Day1.second_part}"
    IO.println ""
  | ["2"] => do
    IO.println "Day 2:"
    IO.println s!"Part 1: {← Day2.first_part}"
    IO.println s!"Part 2: {← Day2.second_part}"
    IO.println ""
  | ["3"] => do
    IO.println "Day 3:"
    IO.println s!"Part 1: {← Day3.first_part}"
    IO.println s!"Part 2: {← Day3.second_part}"
    IO.println ""
  | ["4"] => do
    IO.println "Day 4:"
    IO.println s!"Part 1: {← Day4.first_part}"
    IO.println s!"Part 2: {← Day4.second_part}"
    IO.println ""
  | ["5"] => do
    IO.println "Day 5:"
    IO.println s!"Part 1: {← Day5.first_part}"
    IO.println s!"Part 2: {← Day5.second_part}"
    IO.println ""
  | ["6"] => do
    IO.println "Day 6:"
    IO.println s!"Part 1: {← Day6.first_part}"
    IO.println s!"Part 2: {← Day6.second_part}"
    IO.println ""
  | _ => do
    IO.println "Help, what should I do!?"
