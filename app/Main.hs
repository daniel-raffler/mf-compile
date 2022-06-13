-- mf-compile
--
-- Licensed under Creative Commons Legal Code
-- Daniel Raffler 2022
--
-- Compiler for a lazy functional language
-- Based on the paper "Übersetzerbau – Abstrakte Maschinen"
-- by François Bry and Norbert Eisinger

module Main where

import Lib

{-
Programm ::= Definition ";" { Definition ";"} .
Definition ::= Variable {Variable} "=" Ausdruck .

Lokaldefinitionen ::= Lokaldefinition { ";" Lokaldefinition } .
Lokaldefinition ::= Variable "=" Ausdruck .

Ausdruck
  ::= "let" Lokaldefinitionen "in" Ausdruck
    | "if" Ausdruck "then" Ausdruck "else" Ausdruck
    | Ausdruck BinärOp Ausdruck
    | UnärOp Ausdruck
    | Ausdruck Ausdruck
    | "(" Ausdruck ")"
    | AtomarerAusdruck.

BinärOp ::= "&" | "|" | "==" | "<" | "+" | "−" | "∗" | "/" .
UnärOp ::=  "not" | "−" .

AtomarerAusdruck ::= Variable | Zahl | Wahrheitswert .
Variable ::= Name .
-}

main :: IO ()
main = someFunc
