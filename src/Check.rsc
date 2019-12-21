module Check

import AST;
import Message;
import Set;
import List;

// TODO: 
// * check missing interaction rules 
// * check ports & types in initial net definition

 set[Message] check(ANet n) =
   checkDuplicateSymDecls(n.symDecls) +
   checkUnusedTypes(n) +
   checkUnusedSyms(n) +
   checkUndeclaredTypes(n) +
   checkUndeclaredSyms(n) +
   checkIntRuleLeftPrimaryPortTypeIsOutput(n) +
   checkIntRuleRightPrimaryPortTypeIsInput(n) +
   checkIntRulePrimaryTypes(n) +
   union({checkVarOccursTwice(ir) | ir <- n.intRules}) +
   checkSymbolArity(n) +
   checkVarTypes(n) +
   checkPrimaryTypes(n);

set[Message] checkDuplicateSymDecls(set[ASymDecl] sds) =
  { error("Duplicate symbol declaration: \'<s.name>\'", src2)
  | symDecl(ASym s, _, src = loc src1) <- sds
  , symDecl(s, _, src = loc src2) <- sds
  , src1.offset < src2.offset
  };

set[Message] checkUnusedTypes(ANet n) =
  { warning("Unused type: \'<name>\'", src)
  | / t:\type(str name, src = src) := n.typeDecls
  , !(/ t := n.symDecls)
  };

set[Message] checkUnusedSyms(ANet n) =
  { warning("Unused symbol: \'<name>\'", src)
  | / s:sym(str name, src = src) := n.symDecls
  , !(/ s <- n.intRules + n.activePairs)
  };
  
set[Message] checkUndeclaredTypes(ANet n) =
  { error("Undeclared type: \'<name>\'", src)
  | / t:\type(str name, src = src) := n.symDecls
  , !(/ t := n.typeDecls)
  };
  
set[Message] checkUndeclaredSyms(ANet n) =
  { error("Undeclared symbol: \'<name>\'", src)
  | / s:sym(str name, src = src) := n.intRules + n.activePairs
  , !(/ s := n.symDecls)
  , name != "Print" // Print is built-in
  };

set[Message] checkIntRuleLeftPrimaryPortTypeIsOutput(ANet n) =
  { error("Left primary port is not an output port", src)
  | intRule(intTree(sym(str s), _, src = src), _) <- n.intRules
  , symDecl(sym(s), typeExpr(atom(_,bool b), _)) <- n.symDecls && !b
  };

set[Message] checkIntRuleRightPrimaryPortTypeIsInput(ANet n) =
  { error("Right primary port is not an input port", src)
  | intRule(_, intTree(sym(str s), _, src = src)) <- n.intRules
  , symDecl(sym(s), typeExpr(atom(_,bool b), _)) <- n.symDecls && b
  };

set[Message] checkIntRulePrimaryTypes(ANet n) =
  { error("Primary port types do not match: 
          '  \'<s1>\' has primary port type \'<name1>\'
          '  \'<s2>\' has primary port type \'<name2>\'", src)
  | intRule(intTree(sym(str s1), _), intTree(sym(str s2), _), src = src) <- n.intRules
  , symDecl(sym(s1), typeExpr(atom(\type(str name1), _), _)) <- n.symDecls
  , symDecl(sym(s2), typeExpr(atom(\type(str name2), _), _)) <- n.symDecls
  , name1 != name2
  };

set[Message] checkVarOccursTwice(AIntRule ir) =
  { error("Variable \'<name>\' does not occur twice", ir.src)
  | / var(port(str name)) := ir
  , size([() | / var(port(name)) := ir]) != 2
  };

set[Message] checkSymbolArity(ANet n) =
  { error("Symbol applied to too many or too few arguments", src)
  | / intTree(sym(str name), list[AIntExpr] args, src = src) := n
  , symDecl(sym(name), typeExpr(_, list[list[ATypeAtom]] aux)) <- n.symDecls
  , sum([0] + [size(au) | list[ATypeAtom] au <- aux]) != size(args)
  } +
  { error("Symbol applied to too many or too few arguments", src)
  | / intExpr(sym(name), typeExpr(_, list[AIntExpr] args)) := n
  , symDecl(sym(name), typeExpr(_, list[list[ATypeAtom]] aux)) <- n.symDecls
  , sum([0] + [size(au) | list[ATypeAtom] au <- aux]) != size(args)
  };

set[Message] checkVarTypes(ANet n) = union(
  { {error("Var types do not match", src1), error("Var types do not match", src2)}
  | ir <- n.intRules
  , <ASym s1, list[AIntExpr] l1> <- ({ <s, l> | /intTree(ASym s, list[AIntExpr] l) := ir} + { <s, l> | /app(ASym s, list[AIntExpr] l) := ir })
  , <int i1, var(APort v1, src = loc src1)> <- zip(index(l1), l1)
  , <ASym s2, list[AIntExpr] l2> <- ({ <s, l> | /intTree(ASym s, list[AIntExpr] l) := ir} + { <s, l> | /app(ASym s, list[AIntExpr] l) := ir })
  , <int i2, var(APort v2, src = loc src2)> <- zip(index(l2), l2)
  , v1.name == v2.name && v1.src.offset < v2.src.offset
  , symDecl(s1, typeExpr(_, list[list[ATypeAtom]] ts1)) <- n.symDecls
  , symDecl(s2, typeExpr(_, list[list[ATypeAtom]] ts2)) <- n.symDecls
  , concat(ts1)[i1].\type.name != concat(ts2)[i2].\type.name || concat(ts1)[i1].isOutput != concat(ts1)[i1].isOutput
  });
  
set[Message] checkPrimaryTypes(ANet n) =
  { error("Primary types do not match", src)
  | ir <- n.intRules
  , /intTree(ASym s1, list[AIntExpr] l) := ir
  , <int i, app(ASym s2, _, src = loc src)> <- zip(index(l), l)
  , symDecl(s1, typeExpr(_, list[list[ATypeAtom]] ts)) <- n.symDecls
  , symDecl(s2, typeExpr(ATypeAtom t, _)) <- n.symDecls
  , concat(ts)[i].\type.name != t.\type.name || concat(ts)[i].isOutput != t.isOutput
  } +
  { error("Primary types do not match", src)
  | ir <- n.intRules
  , /app(ASym s1, list[AIntExpr] l) := ir
  , <int i, app(ASym s2, _, src = loc src)> <- zip(index(l), l)
  , symDecl(s1, typeExpr(_, list[list[ATypeAtom]] ts)) <- n.symDecls
  , symDecl(s2, typeExpr(ATypeAtom t, _)) <- n.symDecls
  , concat(ts)[i].\type.name != t.\type.name || concat(ts)[i].isOutput == t.isOutput
  };
