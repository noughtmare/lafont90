module Syntax

extend lang::std::Layout;

lexical Type = [a-z _] !<< [a-z _]+ !>> [a-z _];
lexical Port = [a-z] !<< [a-z][a-z \']* !>> [a-z \'];
lexical Sym = [A-Z 0-9 _ +] !<< [A-Z 0-9 _ +][A-Z a-z 0-9 _ +]* !>> [A-Z a-z 0-9 _ +];

start syntax Net
  = "type" { Type "," }* !>> "," "symbol" 
    SymDecl* !>> SymDecl
    IntRule* !>> IntRule;

syntax SymDecl = Sym ":" TypeExpr;
syntax TypeExpr = TypeAtom (";" {Part ","}+)?;
syntax Part = TypeAtom | "{" {TypeAtom ","}* "}";
syntax TypeAtom = Type [+\-];

syntax IntRule = IntTree "\>\<" IntTree;
syntax IntTree = Sym ("[" {IntExpr ","}+ "]")?;
syntax IntExpr = Sym ("(" {IntExpr ","}+ ")")? | Port;