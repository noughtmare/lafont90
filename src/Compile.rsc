module Compile

import AST;
import IO;

import util::Maybe;
import List;
import Set;

list[tuple[int, &T1]] indexed(list[&T1] x) {
  return zip(index(x), x);
}

/*
 * Example usage:
import Compile;
import AST;
import CST2AST;
import Syntax;
import ParseTree;
import IO;

println(net2c(cst2ast(parse(#start[Net], |project://Lafont/examples/lists.int|))));
 */

void compile(ANet n) {
  writeFile(n.src[extension="c"].top, net2c(n));
}

str rule2c(set[ASymDecl] symDecls, intRule(intTree(sym(str ls), list[AIntExpr] largs), intTree(sym(str rs), list[AIntExpr] rargs))) {
  str x = "// <ls> \>\< <rs>
          'printf(\"<ls> \>\< <rs>\\n\");
          ";
  map[str,str] vars = ();
  int k = 0;
  list[Maybe[str]] lnames = [];
  list[Maybe[str]] rnames = [];
  <x, vars, k, lnames> = ( <x, vars, k, lnames> | (tuple[str,map[str,str],int,list[Maybe[str]]] (y) { return <it[0] + y[0], y[1], y[2], it[3] + [y[3]]>; })(expr2c1(symDecls, it[1], it[2], ta_, "x.n_target-\>aux[<i>]", arg)) | symDecl(sym(ls), typeExpr(_, list[list[ATypeAtom]] aux)) <- symDecls, <int i, <ATypeAtom ta_, AIntExpr arg>> <- indexed(zip(concat(aux), largs)) );
  <x, vars, k, rnames> = ( <x, vars, k, rnames> | (tuple[str,map[str,str],int,list[Maybe[str]]] (y) { return <it[0] + y[0], y[1], y[2], it[3] + [y[3]]>; })(expr2c1(symDecls, it[1], it[2], ta_, "y.n_target-\>aux[<i>]", arg)) | symDecl(sym(rs), typeExpr(_, list[list[ATypeAtom]] aux)) <- symDecls, <int i, <ATypeAtom ta_, AIntExpr arg>> <- indexed(zip(concat(aux), rargs)) );
  for (symDecl(sym(ls), typeExpr(_, list[list[ATypeAtom]] aux)) <- symDecls, <int i, <just(str name), ATypeAtom ta>> <- indexed(zip(lnames, concat(aux)))) {
    x = x + "connect_<ta.\type.name>(<if (ta.isOutput) {>n_wire(<name>), x.n_target-\>aux[<i>]<} else {>x.n_target-\>aux[<i>], n_wire(<name>)<}>);\n";
  }
  for (symDecl(sym(rs), typeExpr(_, list[list[ATypeAtom]] aux)) <- symDecls, <int i, <just(str name), ATypeAtom ta>> <- indexed(zip(rnames, concat(aux)))) {
    x = x + "connect_<ta.\type.name>(<if (ta.isOutput) {>n_wire(<name>), y.n_target-\>aux[<i>]<} else {>y.n_target-\>aux[<i>], n_wire(<name>)<}>);\n";
  }
  return x;
}

tuple[str,str,map[str,str],int] activePair2c(set[ASymDecl] symDecls, map[str,str] vars, int k, activePair(intTree(ASym ls,list[AIntExpr] largs),intTree(ASym rs,list[AIntExpr] rargs))) {
  str lnode_name = "node_<ls.name>_<k>";
  k = k + 1;
  str rnode_name = "node_<rs.name>_<k>";
  k = k + 1;
  if (symDecl(ls, typeExpr(ATypeAtom ta,_)) <- symDecls) {
    str x =
      "struct Node *<lnode_name> = malloc(sizeof(struct Node) + <size(largs)> * sizeof(struct Wire));
      '<lnode_name>-\>type = SYMBOL_<ls.name>;
      'struct Node *<rnode_name> = malloc(sizeof(struct Node) + <size(rargs)> * sizeof(struct Wire));
      '<rnode_name>-\>type = SYMBOL_<rs.name>;
      '";
    <x, vars, k> = ( <x, vars, k> | (tuple[str,map[str,str],int] (y) { return <it[0] + y[0], y[1], y[2]>; })(expr2c(symDecls, it[1], it[2], ta_, "w_wire(&<lnode_name>-\>aux[<i>])", arg)) | symDecl(ls, typeExpr(_, list[list[ATypeAtom]] aux)) <- symDecls, <int i, <ATypeAtom ta_, AIntExpr arg>> <- indexed(zip(concat(aux), largs)) );
    <x, vars, k> = ( <x, vars, k> | (tuple[str,map[str,str],int] (y) { return <it[0] + y[0], y[1], y[2]>; })(expr2c(symDecls, it[1], it[2], ta_, "w_wire(&<rnode_name>-\>aux[<i>])", arg)) | symDecl(rs, typeExpr(_, list[list[ATypeAtom]] aux)) <- symDecls, <int i, <ATypeAtom ta_, AIntExpr arg>> <- indexed(zip(concat(aux), rargs)) );
    str y =
      "connect_<ta.\type.name>(n_wire(<lnode_name>), n_wire(<rnode_name>));
      '";
    return <x, y, vars, k>;
  } else {
    throw "Error";
  }
}

str activePairs2c(set[ASymDecl] symDecls, list[AActivePair] activePairs) {
  str x = "";
  str x2 = "";
  int k = 0;
  map[str,str] vars = ();
  <x, x2, vars, k> = ( <x, x2, vars, k> | (tuple[str,str,map[str,str],int] (y) { return <it[0] + y[0], it[1] + y[1], y[2], y[3]>;})(activePair2c(symDecls, it[2], it[3], ap)) | AActivePair ap <- activePairs );
  return x + x2;
}

// I hate this part:
tuple[str,map[str,str], int, Maybe[str]] expr2c1(set[ASymDecl] symDecls, map[str,str] vars, int k, ATypeAtom ta, str connect, AIntExpr e) {
  switch (e) {
    case app(s,args): {
      str x = "";
      str node_name = "node_<s.name>_<k>";
      k = k + 1;
      <x, vars, k> = ( <x, vars, k> | (tuple[str,map[str,str],int] (y) { return <it[0] + y[0], y[1], y[2]>; })(expr2c(symDecls, it[1], it[2], ta_, "w_wire(&<node_name>-\>aux[<i>])", arg)) | symDecl(s, typeExpr(_, list[list[ATypeAtom]] aux)) <- symDecls, <int i, <ATypeAtom ta_, AIntExpr arg>> <- indexed(zip(concat(aux), args)) );
      return <
        "struct Node *<node_name> = malloc(sizeof(struct Node) + <size(args)> * sizeof(struct Wire));
        '<node_name>-\>type = SYMBOL_<s.name>;
        '<x>", vars, k, just(node_name)>;
    }
    case var(str name):
      if (name in vars) {
        return <"connect_<ta.\type.name>(<if (!ta.isOutput) {><connect>, <vars[name]><} else {><vars[name]>, <connect><}>);\n", vars, k, nothing()>;
      } else {
        return <"", vars + (name: connect), k, nothing()>;
      }
    default: throw "Unhandled <r>";
  }
}

tuple[str,map[str,str], int] expr2c(set[ASymDecl] symDecls, map[str,str] vars, int k, ATypeAtom ta, str connect, AIntExpr e) {
  switch (e) {
    case app(s,args): {
      str x = "";
      str node_name = "node_<s.name>_<k>";
      k = k + 1;
      <x, vars, k> = ( <x, vars, k> | (tuple[str,map[str,str],int] (y) { return <it[0] + y[0], y[1], y[2]>; })(expr2c(symDecls, it[1], it[2], ta_, "w_wire(&<node_name>-\>aux[<i>])", arg)) | symDecl(s, typeExpr(_, list[list[ATypeAtom]] aux)) <- symDecls, <int i, <ATypeAtom ta_, AIntExpr arg>> <- indexed(zip(concat(aux), args)) );
      return <
        "struct Node *<node_name> = malloc(sizeof(struct Node) + <size(args)> * sizeof(struct Wire));
        '<node_name>-\>type = SYMBOL_<s.name>;
        'connect_<ta.\type.name>(<if (!ta.isOutput) {><connect>, n_wire(<node_name>)<} else {>n_wire(<node_name>), <connect><}>);
        '<x>", vars, k>;
    }
    case var(str name):
      if (name in vars) {
        return <"connect_<ta.\type.name>(<if (!ta.isOutput) {><connect>, <vars[name]><} else {><vars[name]>, <connect><}>);\n", vars, k>;
      } else {
        return <"", vars + (name: connect), k>;
      }
    default: throw "Unhandled <r>";
  }
}

str net2c(ANet n) {
  int j = 0;
  return
    "#include \<stdlib.h\>
    '#include \<stdio.h\>
    '
    'struct Wire {
    '  int isPrimary;
    '  union {
    '    struct Node *n_target;
    '    struct Wire *w_target;
    '  };
    '};
    '
    'struct Node {
    '  int type;
    '  struct Wire aux[];
    '};
    '
    'struct Wire n_wire(struct Node *x) {
    '  struct Wire y;
    '  y.isPrimary = 1;
    '  y.n_target = x;
    '  return y;
    '}
    '
    'struct Wire w_wire(struct Wire *x) {
    '  struct Wire y;
    '  y.isPrimary = 0;
    '  y.w_target = x;
    '  return y;
    '}
    '
    '<for (t <- n.typeDecls) {
      ><for (<int i, symDecl(sym(str name),_)> <- (indexed([x | x:symDecl(_, typeExpr(atom(t, true), _)) <- n.symDecls]))) {
        >#define SYMBOL_<name> <i>
      '<}
      >#define N_<t.name> <size([x | x:symDecl(_,typeExpr(atom(t,true),_)) <- n.symDecls])>
      '<for (<int i, symDecl(sym(str name),_)> <- (indexed([x | x:symDecl(_, typeExpr(atom(t, false), _)) <- n.symDecls]))) {
        >#define SYMBOL_<name> <i>
      '<}
      >void connect_<t.name>(struct Wire x, struct Wire y);
    '<}>
    '<for (t <- n.typeDecls) {
      >void connect_<t.name>(struct Wire x, struct Wire y) {
      '  if (x.isPrimary) {
      '    if (y.isPrimary) {
      '      switch (x.n_target-\>type + N_<t.name> * y.n_target-\>type) {
      '<for (symDecl(sym(str x),typeExpr(atom(t,true),_)) <- n.symDecls) {
        ><for (symDecl(sym(str y),typeExpr(atom(t,false),_)) <- n.symDecls) {
      >        case SYMBOL_<x> + N_<t.name> * SYMBOL_<y>:
      '          {<
            if (rule:intRule(intTree(sym(x),_),intTree(sym(y),_)) <- n.intRules) {>
      '            <rule2c(n.symDecls, rule)>
          '<}
      >            free(x.n_target);
      '            free(y.n_target);
      '            break;
      '          }
        '<}
      ><}
      >      }
      '    } else {
      '      y.w_target-\>isPrimary = 1;
      '      y.w_target-\>n_target = x.n_target;
      '    }
      '  } else {
      '    if (y.isPrimary) {
      '      x.w_target-\>isPrimary = 1;
      '      x.w_target-\>n_target = y.n_target;
      '    } else {
      '      x.w_target-\>isPrimary = 0;
      '      x.w_target-\>w_target = y.w_target;
      '      y.w_target-\>isPrimary = 0;
      '      y.w_target-\>w_target = x.w_target;
      '    }
      '  }
      '}
    '<}>
    'int main() {
    '  <activePairs2c(n.symDecls, n.activePairs)>
    '}
    ";
}