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
  | ["7"] => do
    IO.println "Day 7:"
    IO.println s!"Part 1: {← Day7.first_part}"
    IO.println s!"Part 2: {← Day7.second_part}"
    IO.println ""
  | ["8"] => do
    IO.println "Day 8:"
    IO.println s!"Part 1: {← Day8.first_part}"
    IO.println s!"Part 2: {← Day8.second_part}"
    IO.println ""
  | ["9"] => do
    IO.println "Day 9:"
    IO.println s!"Part 1: {← Day9.first_part}"
    IO.println s!"Part 2: {← Day9.second_part}"
    IO.println ""
  | ["10"] => do
    IO.println "Day 10:"
    IO.println s!"Part 1: {← Day10.first_part}"
    IO.println s!"Part 2: {← Day10.second_part}"
    IO.println ""
  | ["11", n] => do
    IO.println "Day 11:"
    IO.println s!"Part 1: {← Day11.first_part}"
    IO.println s!"Part 2: {← Day11.second_part n.toNat!}"
    IO.println ""
  | ["12"] => do
    IO.println "Day 12:"
    IO.println s!"Part 1: {← Day12.first_part}"
    IO.println s!"Part 2: {← Day12.second_part}"
    IO.println ""
  | ["13"] => do
    IO.println "Day 13:"
    IO.println s!"Part 1: {← Day13.first_part}"
    IO.println s!"Part 2: {← Day13.second_part}"
    IO.println ""
  | _ => do
    IO.println "Help, what should I do!?"
