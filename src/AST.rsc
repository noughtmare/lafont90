module AST

data ANet(loc src = |tmp:///|)
  = net(set[AType] typeDecls, set[ASymDecl] symDecls, set[AIntRule] intRules, set[AActivePair] activePairs);

data ASymDecl(loc src = |tmp:///|)
  = symDecl(ASym s, ATypeExpr typeExpr);

data ATypeExpr(loc src = |tmp:///|)
  = typeExpr(ATypeAtom primary, list[list[ATypeAtom]] auxiliary);

data ATypeAtom(loc src = |tmp:///|)
  = atom(AType \type, bool isOutput);

data AIntRule(loc src = |tmp:///|)
  = intRule(AIntTree lhs, AIntTree rhs);

data AIntTree(loc src = |tmp:///|)
  = intTree(ASym s, list[AIntExpr] args);

data AIntExpr(loc src = |tmp:///|)
  = app(ASym s, list[AIntExpr] args)
  | var(APort p);

data AActivePair(loc src = |tmp:///|)
  = activePair(AIntExpr lhs, AIntExpr rhs);

data APort(loc src = |tmp:///|) = port(str name);
data ASym(loc src = |tmp:///|) = sym(str name);
data AType(loc src = |tmp:///|) = \type(str name);