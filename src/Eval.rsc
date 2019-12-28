module Eval

import AST;
import List;
import IO;
import Set;

/*
 * Example usage:
 */
 
/*
import ParseTree;
import Syntax;
import AST;
import Eval;
import CST2AST;
ANet net;
net = cst2ast(parse(#start[Net], |project://Lafont/examples/unary-arithmetics.int|));
eval(net.activePairs, net.intRules);
 */
 
str show(intTree(ASym s, list[AIntExpr] args)) = show(app(s, args));
 
str show(app(ASym s, [])) = "<s.name>";
str show(app(ASym s, list[AIntExpr] args)) = "<s.name>(" + reducer(mapper(args[1..], str (AIntExpr x) { return show(x); }), str (str x, str y) { return x + "," + y; }, show(args[0])) + ")";
str show(var(str name)) = name;

bool isClosed(AIntTree x) {
  for (/ var(str name1, src = src1) := x) {
    if (size({v | / v:var(name1) := x}) != 2) {
      return false;
    }
  }
  return true;
}

AIntTree replaceVars(intTree(ASym s, list[AIntExpr] args), map[str, AIntExpr] m) =
  intTree(s, mapper(args, AIntExpr (AIntExpr arg) {return replaceVars(arg, m);}));

AIntExpr replaceVars(AIntExpr x, map[str, AIntExpr] m) {
  switch (x) {
    case app(ASym s,list[AIntExpr] args):
      return app(s, mapper(args, AIntExpr (AIntExpr arg) {return replaceVars(arg, m);}));
    case var(str name):
      if (name in m) {
        return m[name];
      } else {
        return x;
      }
    default: throw "Unhandled: <x>";
  }
}

AIntExpr uniqify(AIntExpr x, set[str] vars) {
  switch (x) {
    case app(ASym s, list[AIntExpr] aux):
      return app(s, mapper(aux, AIntExpr (AIntExpr y) {return uniqify(y,vars);}));
    case var(str name): {
      int i = 0;
      while (name + "<i>" in vars) {
        i = i + 1;
      }
      return var(name + "<i>");
    }
    default: throw "Unhandled: <X>";
  }
}

void eval(list[AActivePair] activePairs, set[AIntRule] rules) {
  rules = mapper(rules, AIntRule (intRule(AIntTree l:intTree(ASym ls,_), AIntTree r:intTree(ASym rs, _))) {
    if (ls > rs) {
      return intRule(r, l);
    } else {
      return intRule(l, r);
    }
  });
  activePairs = mapper(activePairs, AActivePair (activePair(AIntTree l:intTree(ASym ls,_), AIntTree r:intTree(ASym rs, _))) {
    if (ls > rs) {
      return activePair(r, l);
    } else {
      return activePair(l, r);
    }
  });
  while (!isEmpty(activePairs)) {
    set[str] variables = { name | / var(str name) := activePairs };  
    ASym ls, rs;
    list[AIntExpr] largs, rargs;
    <activePair(intTree(ls,largs), intTree(rs,rargs)), activePairs> = pop(activePairs);
    if (intRule(intTree(ls, list[AIntExpr] laux), intTree(rs, list[AIntExpr] raux)) <- rules) {
      //println("Resolving: <ls.name> \>\< <rs.name>");
      // make names unique
      laux = mapper(laux, AIntExpr (AIntExpr x) { return uniqify(x, variables); });
      raux = mapper(raux, AIntExpr (AIntExpr x) { return uniqify(x, variables); });
      
      // Resolve all the variables that are introduces in the rule.
      map[str, AIntExpr] varMap = ();
      list[tuple[AIntExpr, AIntExpr]] connections = [];
      for (tuple[AIntExpr, AIntExpr] x  <- zip(laux + raux, largs + rargs)) {
        switch (x) {
          case <var(str name),AIntExpr x>:
            if (name in varMap) {
              connections = push(<x, varMap[name]>,connections);
              varMap -= (name: varMap[name]);
            } else {
              varMap += (name: x);
            }
          case <AIntExpr x,AIntExpr y>: connections = push(<x, y>, connections);
          default: throw "Unhandled: <x>";
        }
      }
      connections = mapper(connections, tuple[AIntExpr, AIntExpr] (<AIntExpr x, AIntExpr y>) {return <replaceVars(x, varMap), replaceVars(y, varMap)>;});
      
      // Make new active pairs and external variable map
      varMap = ();
      for (tuple[AIntExpr, AIntExpr] t <- connections) {
        switch (t) {
          case <app(ASym s1, list[AIntExpr] args1),app(ASym s2, list[AIntExpr] args2)>: {
            //println("Adding: <s1.name> \>\< <s2.name>");
            if (s1 > s2) {
              activePairs = push(activePair(intTree(s2, args2), intTree(s1, args1)), activePairs);
            } else {
              activePairs = push(activePair(intTree(s1, args1), intTree(s2, args2)), activePairs);
            }
          }
          case <var(str name), AIntExpr x>:
            varMap += (name: x);
          case <AIntExpr x, var(str name)>:
            varMap += (name: x);
          default: throw "Unhandled: <t>";
        }
      }
      activePairs = mapper(activePairs, AActivePair (activePair(AIntTree lhs, AIntTree rhs)) {return activePair(replaceVars(lhs, varMap), replaceVars(rhs, varMap));});
    } else if (ls.name == "Print" || rs.name == "Print") {
      AIntExpr other;
      if (ls.name == "Print") {
        other = intTree(rs, rargs);
      } else {
        other = intTree(ls, largs);
      }
      if (isClosed(other)) {
        println(show(other));
      } else {
        activePairs = activePairs + [activePair(intTree(sym("Print"), []), other)];
      }
    } else {
      throw "Missing rule: <ls.name> \>\< <rs.name>";
    }
  }
}