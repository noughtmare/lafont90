### 2IMP20 Generic Language Technology Assignment 2

For assignment 2 we chose to implement the interaction nets language first 
described by Yves Lafont in 1989 [1]. Interaction nets are a model of
computation like the lambda calculus, but even lower level. Cellular automata,
rewrite systems and Turing machines can all be considered as a special case of
interaction nets [2]. Another nice property of interaction nets is that they can
easily be mapped onto conventional computer hardware without the need of a 
garbage collector [1].

In this project we implemented a very basic compiler for interaction nets. 
We have tried to adhere as much as possible to the proposal of Lafont in his
original paper [1], but for some parts we took inspiration from a later paper by
Ian Mackie [3] especially for the initial net definition.

This project contains the following Rascal modules:

* **AST** contains the abstract syntax tree.
* **CST2AST** contains the transformation from a concrete syntax tree to an
abstract syntax tree.
* **Check** contains a static semantics checker.
* **Compile** contains a compiler that translates an abstract syntax tree to C
code.
* **Eval** contains an evaluator that can run interaction nets without
compiling.
* **IDE** hooks into eclipse to show warnings and errors and compiles
interaction net files.
* **Syntax** contains the concrete syntax specification.

This project contains the following example interaction net files:

* **ackermann.int** contains an implementation of the Ackermann function with
unary arithmetic. The initial net definition calculates A(3, 8), which is too
large for the evaluator, but the compiled version can run in reasonable time.
* **difference-lists.int** contains an implementation of difference lists [5].
The initial net definition calculates appending a difference list containing
[P,O,P] and another difference list containing [L] to eachother.
* **lists.int** contains an implementation for lists. The initial net
definition calculates appending a list containing [P,O,P] and another list
containing [L] to eachother.
* **parsing.int** contains a simple expression parser that can parse a stream
of the number 1 and the infix symbol P (for plus) into a tree based abstract
syntax. The initial net definition parses the stream (1, P, 1, End) into the
abstract tree P(1, 1). It should be noted that no error handeling is
implemented.  The interaction 'End >< Parse' is not implemented. A malformed
input stream can cause undefined behavior.
* **simple.int** contains one of the simplest possible interaction nets.
It only has a single interaction rule. The initial net definition defines
a net that performs the single interaction rule.
* **test.int** is a test script used during development for testing.
* **turnstile.int** contains a demonstration of an infinitely reducing
interaction net. The initial net definition will execute indefinitely.
* **unary-arithmetics.int** contains an implementation of unary arithmetic,
including addition, multiplication and finding the maximum of two numbers.
The initial net definition calculates the expression max(2 * 3, 2 + 3).

Most of these example interaction net files are based on the examples in the
original paper by Lafont [1].

Currently, the compiled programs do not have any input or output capabilities.
They only print the interactions that occur to the standard output. This can
easily be changed manually after generating the C source files.

[1] Lafont, Yves. "Interaction nets." Proceedings of the 17th ACM
SIGPLAN-SIGACT symposium on Principles of programming languages. ACM, 1989.

[2] Lafont, Yves. "Interaction combinators." Information and Computation
137.1 (1997): 69-101.

[3] Mackie, Ian. "Towards a programming language for interaction nets."
Electronic Notes in Theoretical Computer Science 127.5 (2005): 133-151.

[4] Wikipedia contributors. "Ackermann function." Wikipedia, The Free
Encyclopedia. Wikipedia, The Free Encyclopedia, 10 Sep. 2019. Web. 20 Jan. 2020.

[5] "Difference list." HaskellWiki, . 1 Aug 2018, 16:40 UTC. 20 Jan 2020, 18:25 <https://wiki.haskell.org/index.php?title=Difference_list&oldid=62571>. 