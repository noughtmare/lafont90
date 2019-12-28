module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;

ANet cst2ast(start[Net] sn) = cst2ast(sn.top);

ANet cst2ast(Net n) {
  switch (n) {
    case (Net)`type <{Type ","}* ts> symbol <SymDecl* sds> <IntRule* irs> init <ActivePair+ aps>`:
      return net(
        {cst2ast(t) | Type t <- ts},
        {cst2ast(sd) | SymDecl sd <- sds},
        {cst2ast(ir) | IntRule ir <- irs},
        [cst2ast(ap) | ActivePair ap <- aps],
        src = n@\loc);
    default: throw "Error";
  }
}

ASymDecl cst2ast(SymDecl sd) {
  switch (sd) {
    case (SymDecl)`<Sym s> : <TypeExpr typeExpr>`:
      return symDecl(cst2ast(s), cst2ast(typeExpr), src = sd@\loc);
    default: throw "Error";
  }
}

ATypeExpr cst2ast(TypeExpr te) {
  switch (te) {
    case (TypeExpr)`<TypeAtom ta>`:
      return typeExpr(cst2ast(ta), [], src = te@\loc);
    case (TypeExpr)`<TypeAtom ta>; <{Part ","}+ parts>`:
      return typeExpr(cst2ast(ta), [cst2ast(part) | Part part <- parts], src = te@\loc);
    default: throw "Error";
  }
}

list[ATypeAtom] cst2ast(Part p) {
  switch (p) {
    case (Part)`{ <{TypeAtom ","}* ps> }`:
      return [cst2ast(ta) | TypeAtom ta <- ps];
    case (Part)`<TypeAtom ta>`:
      return [cst2ast(ta)];
    default: throw "Error";
  }
}

ATypeAtom cst2ast(TypeAtom ta) {
  switch (ta) {
    case (TypeAtom)`<Type t>+`:
      return atom(cst2ast(t), true, src = ta@\loc);
    case (TypeAtom)`<Type t>-`:
      return atom(cst2ast(t), false, src = ta@\loc);
    default: throw "Error";
  }
}

AIntRule cst2ast(IntRule ir) {
  switch (ir) {
    case (IntRule)`<IntTree lhs> \>\< <IntTree rhs>`:
      return intRule(cst2ast(lhs), cst2ast(rhs), src = ir@\loc);
    default: throw "Error";
  }
}

AIntTree cst2ast(IntTree \it) {
  switch (\it) {
    case (IntTree)`<Sym s>`:
      return intTree(cst2ast(s), [], src = \it@\loc);
    case (IntTree)`<Sym s>[<{IntExpr ","}+ ies>]`:
      return intTree(cst2ast(s), [cst2ast(ie) | IntExpr ie <- ies], src = \it@\loc);
    default: throw "Error";
  }
}

AIntExpr cst2ast(IntExpr ie) {
  switch (ie) {
    case (IntExpr)`<Sym s>`:
      return app(cst2ast(s), [], src = ie@\loc);
    case (IntExpr)`<Sym s>(<{IntExpr ","}+ args>)`:
      return app(cst2ast(s), [cst2ast(arg) | IntExpr arg <- args], src = ie@\loc);
    case (IntExpr)`<Port p>`:
      return var("<p>", src = ie@\loc); 
    default: throw "Error";
  }
}

AIntTree expr2tree(app(ASym s, list[AIntExpr] args)) = intTree(s,args);

AActivePair cst2ast(ActivePair ap) {
  switch (ap) {
    case (ActivePair)`<IntExpr lhs> = <IntExpr rhs>`:
      return activePair(expr2tree(cst2ast(lhs)), expr2tree(cst2ast(rhs)));
    default: throw "Error!";
  }
}

ASym cst2ast(Sym s) {
  switch (s) {
    case (Sym)`<Sym name>`:
      return sym("<name>", src = s@\loc);
    default: throw "Error";
  }
}

AType cst2ast(Type t) {
  switch (t) {
    case (Type)`<Type name>`:
      return \type("<name>", src = t@\loc);
    default: throw "Error";
  }
}