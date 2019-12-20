module IDE

import Syntax;
import AST;
import CST2AST;
//import Resolve;
import Check;
//import Compile;

import util::IDE;
import Message;
import ParseTree;


private str Int ="Interaction Nets";

anno rel[loc, loc] Tree@hyperlinks;

void main() {
  registerLanguage(Int, "int", Tree(str src, loc l) {
    return parse(#start[Net], src, l);
  });

  contribs = {
    annotator(Tree(Tree t) {
      if (start[Net] pt := t) {
        ANet ast = cst2ast(pt);
        // UseDef useDef = resolve(ast).useDef;
        set[Message] msgs = check(ast); // , collect(ast), useDef);
        return t[@messages=msgs];
      }
      return t[@messages={error("Not an interaction net", t@\loc)}];
    }) //,
  //  
  //  builder(set[Message] (Tree t) {
  //    if (start[Form] pt := t) {
  //      AForm ast = cst2ast(pt);
  //      UseDef useDef = resolve(ast).useDef;
  //      set[Message] msgs = check(ast, collect(ast), useDef);
  //      if (msgs == {}) {
  //        compile(ast);
  //      }
  //      return msgs;
  //    }
  //    return {error("Not an interaction net", t@\loc)};
  //  })
  };

  registerContributions(Int, contribs);
}
