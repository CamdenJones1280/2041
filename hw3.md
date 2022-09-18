# Homework 3:  Advanced Program Manipulation

*CSci 2041: Advanced Programming Principles, Spring 2022 (Section 01)*

**Due:** Friday, April 8 at 11:59pm

## Computing With Program Representations

The `hw3` directory in the public `hw2041` repo contains several files
related to program parsing and evaluation -- you should copy the entire
directory to your personal repository for use on this homework. The file
`program.ml` contains an implementation of the data structure for representing
simple programs that we covered in Videos 8.1-9.1, along with the type-checking
algorithm and evaluation program.  The file `parser.ml` includes a parser that
transforms prefix-form programs into syntax trees and type-checks these trees.
The file `interpreter.ml` contains code to read a program from an input file,
parse it, type-check it, and evaluate the program.  There are also three very
non-descriptively named example programs, `program1.interp`,
`program2.interp`, and `program3.interp` (this last file contains an error). Note: to see `interpreter.ml` or one of the
`program1.interp` or `program2.interp` programs in action, you'll need to build
`interpreter` by running `ocamlopt -o interpreter str.cmxa program.ml parse.ml interpreter.ml` in the `hw3` directory. SubNote: this will produce a lot of binary files with
suffixes matching `.cm*`; it is best if you do not commit these to your repo, as they
take up a lot of space and thus make pushes and pulls take more time.

## Note: prefix form

Programs in our little language are expressed in *prefix form*, where operations precede their operands, and parentheses are required to make parsing unambiguous.  So instead of representing the expression `Add (IntC 1, IntC 1)` as `1+1`, it is represented as `(+ 1 1)`, `if` statements are represented as `(if <cond> <then> <else>)`, `let` statements as `(let <name> <value> <body>)` and so on.  Expressions in this form are often called *S-expressions*.

## Overview of the code

As mentioned above, when we run `interpreter.ml` with a source file to interpret
as input, `interpreter` reads the contents of the file, creating the string
`progString`.  It then passes `progString` as input to the function `wordlist`
in `parser.ml`, which converts the string into a list of all the substrings
separated by "word boundaries" like white space and parentheses, and this
list is given as input to the function `tokens`, which converts these strings
into elements of the type `Parser.token`, which represents the different kinds
of elements that can appear in a program, like arithmetic operators,
parentheses, keywords, and variable identifiers.  The list of tokens is then
given to `Parser.parse_program`, which creates a syntax tree from the type
`Program.expr`.  Then the functions `Program.typeof` and `Program.eval` are used
to type-check and evaluate the syntax tree using the same rules we've already
seen in class.  (Since we have imperative constructs like assignment and while
loops, evaluation expects both an expression and a state, and returns a result
and an updated state)

`Parser.parse_program` is really just a wrapper function that calls the main
working function `_parser`.  `_parser` (and several of its mutually-recursive
helper functions) takes as input a list or stream of tokens, and returns the
complete expression represented by the beginning of the list of tokens, plus any
remaining tokens in the list.  This is necessary because expressions can contain
sub-expressions, and when we convert some of the tokens in a list into a
sub-expression, we need the list of remaining tokens to know where to resume
parsing.  For example, if we want to convert the s-expression `(* (+ 1 2) (- 5
2))` into a syntax tree, we would read the `(` and `*` from the front of the
list, and know that we needed to parse two subexpressions to create a `Mul`
syntax tree. Calling `_parser` on the tokens from `(+ 1 2) (- 5 2))` would
return the `Add(IntC 1, IntC 2)` syntax tree, *and* the tokens corresponding to
the rest of the string, `(- 5 2))`; then we would call `_parser` on these again
to get back `Sub(IntC 5, IntC 2)` and the remaining token, `)`, and finally see
that there was indeed a closing paren next in the token list.

The remainder of this homework will consist of adding new features to the
(currently operational) `interpreter` program.


## 1. (5 points) Adding Input Statements to `program.ml`

So far, programs in our example can do a lot of interesting things:
for instance, they can run loops, define and call (non-recursive) functions, perform
arithmetic and boolean computations, and print out the results of
these computations.  But, there's no way in our language to read an
input from the user.  Let's add a command, `readint` that reads a
single integer into the program.  Programs use this command as if it
were an integer-valued expression, so for example, `(let i readint (print
i))` is a program that reads an integer, binds it to the name `i` and
then prints out `i`.

We'll make this addition in two stages, first adding the code to `program.ml`
that represents this operation and evaluates it in a program; and then in the
next sub-problem, adding the code to `parser.ml` to parse this new operation
from strings that represent user-input programs.

For the first stage, we'll need to make the following change:

+ Add a new constructor to `expr` for a `Readint` expression.  Since invoking `readint` won't involve any sub-expression, the constructor won't need to have any arguments.  The test case here is that if we `#mod_use "program.ml"` then the declaration `let t1 : Program.expr = Program.Readint` should compile without an error.

+ Add a new type-checking rule to `typeof` to infer the correct type of a `Readint` constructor in an expression, using the type judgement ` => Readint : IntT`.  The test case here is that `Program.typeof Program.Readint []` should evaluate to `Program.IntT`

+ Modify the evaluation function to read an integer whenever it encounters a `Readint` expression.  You can use the Ocaml function `read_int : unit -> int` to read an integer from the standard input; `Readint` won't change the state.  **There's no github test for this case, but make sure you include it.**

## 2. (5 points)  Adding readint to `parser.ml`

Now that we can represent and manipulate this new kind of expression, we need to add code to `parser.ml` to allow us to create it from program files.  This will involve both the "lexing" of strings into a new kind of tokens, and the parsing of the corresponding token to a(n) (sub)expression:

+ Add a new constructor `READ` to the `token` type in `parser.ml` for the `readint`  keyword.  The test case is that after first running `#mod_use "program.ml"` and then `#mod_use "parser.ml"` the expression `let rt : Parser.token = Parser.READ` should compile without causing a type error.

+ Add a clause to `tokenize_string` to convert the `readint` keyword into the `READ` token.  The test case is that `tokenize_string "readint"` should evaluate to `READ`.

+ Modify `_parser` to correctly parse the token `READ` (it is analogous to the constant cases, except there's no additional data to wrap in the `Readint`).  An example test case is that `_parser [READ; CP]` should evaluate to `(Program.Readint,[CP])`.


## 3. (35 points)  Adding integer lists to `program.ml`

Another shortcoming of our example programming language is that it lacks a mechanism to store and manipulate sequences of data. For this problem, let's correct that by adding lists of integers as a new kind of value in our programs.  We'll have list literals like `[1 2 3]` and `[]`, the `cons` operation that takes an element `h` and a list `t` and creates a new list with `h` in front of `t`, and the `head` and `tail` functions that return the integer at the head of a list and the tail of a list, respectively.  To keep things simpler, though, we won't use the list literal notation to build lists that involve computation, so `[(+ 1 1) 2 3]` won't be a syntactically valid program in our language (the right way to build this list would be `(cons (+ 1 1) [2 3])`).


#### Adding constructors to the `expr` type

Let's start by making the following changes to the `expr` type in `program.ml`:

+ Add a new constructor for a list literal, `ListC`.  `ListC` should have an argument of type `int list`.

+ Add a new constructor for the `cons` operation, `Cons`.  `Cons` should hold a pair of subexpressions as arguments

+ Add new constructors for the `head` and `tail` operations, `Head` and `Tail`.  These constructors should each have a single subexpression as an argument.

When you're done, if you `#mod_use "program.ml"` and then `open Program` in `utop`, the expression `(Head (Cons (Readint, Tail (ListC [1; 2]))))` should compile without errors and have type `expr`.

#### Adding type-checking rules

Next, we need to modify the `expType` type and `typeof` function in `program.ml` to correctly deal with our new `expr` constructors:

+ Modify the `expType` type to include a new constructor, `ListT`, that will represent the type of list values.  Since `ListT` represents a type like `IntT` or `BoolT`, it won't need to have any arguments.

+ Modify `typeof` to infer the correct type of a `ListC` constructor, using the type judgement ` => ListC _ : ListT`

+ Modify `typeof` to infer the correct type of a `Head` expression, using the type judgement `e : ListT => Head e : Int`.  (raise `TypeError "Head"` if the judgement fails)

+ Modify `typeof` to infer the correct type of a `Tail` expression.  Include a comment stating the type judgement that applies to this case. (raise `TypeError "Tail"` if the judgement fails.)

+ Modify `typeof` to infer the correct type of a `Cons` expression.  Include a comment stating the type judgement that applies to this case. (raise `TypeError "Cons"` if the judgement fails.)

+ You'll also need to add a sub-case for `ListT` in the case that type-checks `Eq` expressions.

When you're done, you should find that `typeof Readint []` evaluates to `IntT`; `typeof (Cons(Head (ListC [1;2]), Tail (ListC [3;4]))) []` evaluates to `ListT`; and `typeof (Cons(BoolC false, ListC [])) []`, `typeof (Tail (IntC 0)) []`, `typeof (Head (IntC 0)) []`, and `typeof (Cons(IntC 0, IntC 1)) []`  all result in TypeError exceptions.

#### Adding evaluation rules

Next, we'll need to modify the `eval` function in `program.ml` to handle our new constructors:


+ Add a new constructor to the `result` type, `ListR`, with an argument of type `int list`

+ Add a case to handle `ListC`, wrapping the list literal in a `ListR` constructor.

+ Add cases to handle `Tail` and `Head`, which recursively evaluate their subexpression, extract the resulting list, and wrap the head or tail in the appropriate `result` constructor.  Don't forget to return the modified state, to handle cases like `Tail(Seq [Set ("x", IntC 0); ListC [1;2]])` that could modify the state of the program through side effects.

+ Add a case to handle `Cons`, which recursively evaluates its first argument, then its second argument, unwraps the values and returns a `ListR` value and the state resulting from the evaluation of the second argument.

+ Add a sub-case to the case that evaluates `Print` constructors to handle `ListR` values.  `Print`ing a list should print the integer members of the list separated by spaces, enclosed in square brackets, e.g. `"[1 2 3]"`.  (You might find the `String.concat` and `string_of_int` functions useful for this.)

When you've finished, you should find, for instance, that `eval (Tail(Cons(Head(Seq [Set ("x", IntC 17); ListC [1;2]]), Cons(Name "x", ListC [])))) [("x", IntR 5); ("y", ListR [1;2])]` evaluates to `(ListR [17], [("x",IntR 17); ("y", ListR [1;2])])`.

## 4. (25 points) Adding integer lists to `parser.ml`

Now that we can represent and manipulate integer list expressions, we need to add code to `parser.ml` to allow us to create them from program files.  This will involve both the "lexing" of strings into new kinds of tokens, and the parsing of lists of tokens to expressions.

#### Tokens

Recall that lexing converts a string into a sequence of tokens from a finite set that we can then use to produce expression trees.  Because we are adding new operations and constants to our programs, we'll need to add several tokens, and code to recognize the tokens in our input:


+ Add new constructors to `token` for list operations: `LB`, `RB` (for `[` and `]` characters), `CONS`, `TAIL`, `HEAD`, and `LIST`.

+ Modify `tokenize_string` to convert `"["` to `LB`, `"]"` to `RB`, `"cons"` to `CONS`, `"tail"` to `TAIL`, `"head"` to `HEAD`, and `"list"` to `LIST`.


#### Parsing

Once we've added these tokens, we next need to add code to build the new expression variants when we encounter related keywords in the token stream.  Recall that the parsing function may recursively call itself (for example, to find the arguments to an operation), so it always returns the list of remaining unused tokens after it parses a (sub)-expression so that the calling site can continue to parse an expression.  In order to handle our new expression variants we'll need to make several modifications:

+ Modify `_parse_type_expr` to handle the `LIST` keyword (in the same way that `INT`,`BOOL`, and `UNIT` are handled)

+ Modify `_parser` to correctly parse the `HEAD` and `TAIL` keywords (in the same way that `NOT` and `PRINT` are handled)

+ Modify `_parser` to correctly parse the `CONS` keyword (in the same way that other two-argument keywords are handled, e.g. `PLUS`, `TIMES`, `DIV`.)

+ Modify `_parser` to correctly parse list literals.  A list literal consists of a `LB` token, followed by zero or more `ICONST` tokens, followed by a `RB` token.  Literals cannot contain other tokens, so for example, calling `_parser [LB; OP; PLUS; ICONST 1; ICONST 1; CP; RB]` (corresponding to `[(1+1)]`) should result in raising `ParseError "list literal"`.  Reaching the end of the token list without finding a `RB` token should result in raising `ParseError "unclosed list"`.

Once you've finished with these modifications, you should find that calling `parse_program (tokens (wordlist example5))` results in

```ocaml
Let ("rev",
  Fun ("in", ListT,
    Let ("out", ListC [],
      Seq
        [While (Not (Eq (Name "in", ListC [])),
          Seq
            [Set ("out", Cons (Head (Name "in"), Name "out"));
              Set ("in", Tail (Name "in"))]);
                Name "out"])),
Apply (Name "rev", ListC [1; 2; 3; 4; 5]))
```

and calling `parse_program (tokens (wordlist "[1 2 "))` should result in an uncaught exception.  (And furthermore, calling `eval (parse_program (tokens (wordlist example5))) []` should result in `(ListR [5;4;3;2;1], [])`.

## 5. (20 points) Adding recursive functions to `program.ml`

The current implementation of functions in `program.ml` does not allow functions to call themselves recursively: to see that this is true, try `#use "program.ml"` followed by this code in utop:

```ocaml
let rfcall = Let("pow2",
  Fun("n",IntT,
    If(Eq (Name "n", IntC 0),
      IntC 1,
      Mul (IntC 2, Apply(Name "pow2", Sub (Name "n", IntC 1))))),
  Apply (Name "pow2", IntC 16));;
typeof rfcall [];;
eval rfcall [];;
```

The problem here is that exactly as in Ocaml, the name `"pow2"` is not in scope
when we define the function, so the type checker doesn't know what type it has,
and the eval function doesn't have a binding for `"pow2"` in the defining state
when evaluating the `Closure`.  So in order to fix this, we'll have to add a new
`expr` constructor for a recursive function that *does* know its name and return
type, and a new `result` constructor for a recursive closure that knows its
name:

+ Add a new constructor to `expr`, `Funrec`, that has *five* arguments:
  - The string that should be bound to the function in the body
  - The string that should be bound to the function argument in the body
  - The type of value that the function returns
  - The type of argument the function expects
  - The `expr` representing the body of the function (like `If(Eq (Name "n", IntC 0), IntC 1,
    Mul (IntC 2, Apply(Name "pow2", Sub (Name "n", IntC 1))))` in the above example)
  A good test case is that `Funrec("pow2","n",IntT,IntT,If(Eq (Name "n", IntC 0), IntC 1,
    Mul (IntC 2, Apply(Name "pow2", Sub (Name "n", IntC 1)))))` should compile and have type `expr`.

+ Add a new constructor to `result`, `ClosureRec`, that has four arguments:
  - The expression representing the body of the function
  - The string that should be found to the function in the body (the function name)
  - The argument name,
  - The state/lexical environment at the point of definition of the function.
  A good test case is that `ClosureRec(If(Eq (Name "ls", ListC []), IntC 0, Apply(Name "lw", Tail (Name "ls"))),
    "lw","l",[])` should compile and have type `result`.

### Type-checking recursive functions

Once we've added these constructors, we're ready to add a clause to `typeof` to handle checking the type of a `Funrec` expression, using the typing rule `(f : t1 -> t2, x : t1) |- e : t2  => Funrec(f,x,t2,t1,e) : t1 -> t2` (So, "if, assuming `f` has type `t1 -> t2` and `x` has type `t1` means the expression `e` has type `t2`, then `Funrec(f,x,t2,t1,e)` has type `t1 -> t2`").  Here are some example evaluations:

+ `typeof (Funrec("pow2","n",IntT,IntT,If(Eq (Name "n", IntC 0), IntC 1,
  Mul (IntC 2, Apply(Name "pow2", Sub (Name "n", IntC 1)))))) []` should evaluate to `FunT(IntT,IntT)`.

+ `typeof (Funrec("lw","ls",IntT,ListT,If(Eq (Name "ls", ListC []), IntC 0, Apply(Name "lw", Tail (Name "ls"))))) []` should evaluate to `FunT(ListT,IntT)`.

+ `typeof (Funrec("tightloop","x",BoolT,IntT,Apply(Name "tightloop", Name "x"))) []` should evaluate to `FunT(IntT,BoolT)`.

+ `typeof (Funrec("f","x",ListT,IntT,Name "x")) []` should result in an uncaught `TypeError` exception (because the body returns an int, rather than a list.)

+ `typeof (Funrec("f","b",IntT,IntT,Apply(Name "f", BoolC false))) []` should result in an uncaught `TypeError` exception, because the body applies `f` to an argument of type bool, rather than an int.

+ `typeof (Funrec("f","n",ListT,IntT,And(BoolC true, Apply(Name "f", IntC 0)))) []` should result in an uncaught `TypeError` exception, because the body treats the result of `f` as a bool, rather than a list.

+ `typeof (Funrec("f","x",IntT,BoolT,Add(IntC 0, Name "x"))) []` should result in an uncaught `TypeError` exception (because the body treats `x` as an int, rather than a bool.)

### Evaluating recursive functions

OK, now we should move on to adding clauses to `eval` and its helper functions to deal with `Funrec` expressions and `ClosureRec` results.  

+ Add a clause to `eval` for the `Funrec` constructor - it should look mostly the same as `Fun`, except that it will return a `ClosureRec` result that captures the function name in addition to the argument name from the `Funrec` constructor.  So for example, `eval (Funrec("f","n",IntT,IntT,Add(Name "n",IntC 1))) [("v",IntR 1)]` should evaluate to `(ClosureRec(Add(Name "n",IntC 1),"f","n",[("v",IntR 1)]),[("v",IntR 1)])`.

+ Add a case to the `Print` clause in eval for `ClosureRec` results: let's have the intepreter print `"<funrec>"` when it's asked to print a recursive closure.

+ The real "magic trick" is in the `evalFunc` function, which currently assumes
that when evaluating `Apply(f,arg)` the subexpression `f` will always evaluate
to a `Closure`.  Since we now have recursive closure results (represented by the
`ClosureRec` constructor) we'll need to update this function, replacing the line
`let (Closure(body,argname,def_st),st') = eval f state in...`, which uses an
implicit pattern match, with an **explicit** match statement that captures the
`body`,`argname`, and `def_st` from the result of calling `(eval f state)`, in
both the non-recursive case (when a `Closure` constructor is returned) and the
recursive case (when a `ClosureRec` constructor is returned).  In the latter
case, when we evaluate the `body` of the closure, we will need to add a new
binding to the lexical environment from its definition: the function name must
be bound to the closure value.  So if calling `(eval f state)` returns the pair
`(closure, st')` and `closure` matches the pattern
`ClosureRec(body,fname,argname,def_st)` then the state argument to the `eval`
call on the function body should include both the `(argname,argval)` binding that
`evalFunc` currently provides, but also the binding `(fname,closure)`.  

Some example evaluations:
+ calling `evalFunc (Funrec("f","b",BoolT,BoolT,If(Name "b",BoolC
true,Apply(Name "f",Name "b")))) (BoolC true) []` should evaluate to `(BoolR
true, [])`, but `evalFunc (Funrec("f","b",BoolT,BoolT,If(Name "b",BoolC
true,Apply(Name "f",Name "b")))) (BoolC false) []` should cause a
`Stack_overflow` exception (due to the looping recursion, and the fact that our
interpreter doesn't perform tail-call elimination).  
+ The expression `evalFunc (Funrec("f","b",IntT,BoolT,If(Name "b",IntC 0,Add(IntC 1,Apply(Name "f",BoolC
true))))) (BoolC true) []` should evaluate to `(IntR 0, [])` and `evalFunc
(Funrec("f","b",IntT,BoolT,If(Name "b",IntC 0,Add(IntC 1,Apply(Name "f",BoolC
true))))) (BoolC false) []` should evaluate to `(IntR 1, [])`.
+ The expression `evalFunc (Funrec("f","n",IntT,IntT,If(Eq(Name "n",IntC 0),IntC 1,Mul(IntC 2, Apply(Name "f",Sub(Name "n",IntC 1)))))) (IntC 7) [("z",UnitR)]` should evaluate to `(IntR 128, [("z",UnitR)])`.  
+ The expression `evalFunc (Funrec("f","n",IntT,IntT,If(Eq(Name "n",IntC 0),IntC 1,Mul(IntC 2, Apply(Name "f",Sub(Name "n",IntC 1)))))) (Name "m") [("m",IntR 5);("z",UnitR)]` should evaluate to `(IntR 32, [("m",IntR 5);("z",UnitR)])`
+ The expression `evalFunc (Funrec("f","n",IntT,IntT,If(Eq(Name "n",IntC 0),IntC 1,Mul(IntC 2, Apply(Name "f",Sub(Name "n",IntC 1)))))) (Seq [Set("z",IntC 3); Name "m"]) [("m",IntR 5);("z",IntR 1)]` should evaluate to `(IntR 32, [("m",IntR 5);("z",IntR 3)])`
+ The expression `evalFunc (Fun("b",BoolT,Name "b")) (BoolC true) [("b",UnitR)]` should (still) evaluate to `(BoolR true, [("b",UnitR)])`.  (i.e., make sure you don't break the non-recursive case!)


## 6. (10 points) Adding recursive functions to `parser.ml`

Finally, we can add code to `parser.ml` to allow programs to define recursive functions.  The goal here is to allow little programs like:
```
(print (letrec fact : int (fun n : int (if (= n 0) 1 (* n (app fact (- n 1))))) (app fact 6)))
```
Here, the `fact : int` portion is telling the interpreter that `fact` is a (recursive) function that returns an integer.  Running this program with `interpreter.ml` would print 6 factorial, or 720, to the standard output. Another example program is this one:
```
(letrec rlen : int (fun ls : list (if (= ls []) 0 (+ 1 (app rlen (tail ls))))) (print (app rlen [1 2 8 9])))
```
Which should print 4 - the length of the list literal `[1 2 8 9]` - to the standard output.

To do this we need to make two changes in `parser.ml`:

+ Add a `LETREC` constructor to the `token` type in `parser`.  The simple test case here is that `let p : token = LETREC` should compile without errors.

+ Add a case for `"letrec"` in `tokenize_string`.  The test case here is that `tokenize_string "letrec"` should evaluate to `LETREC`.

+ Add a mutually recursive helper to `_parser` that parses everything *after* the `LETREC` token in a `letrec` expression.  This should:
  - Check that the next token is an `ID` constructor (it holds the name of the function)
  - Check that the following token is a `COLON`.  If either of these checks fail, raise `SyntaxError "letrec"`.
  - Call `_parse_type_expr` on the token list following the `COLON` to parse the return type of the function
  - Call `_parse_two` to parse the next two sub-expressions `e1` and `e2` in the token list, and consume the closing `CP`.
  - Check that the expression `e1` is a `Fun` constructor: if it's not, raise a `SyntaxError` (with the argument `"letrec must have function value"`).  Otherwise, use pattern matching to extract the argument name, argument type, and function body, and replace `e1` with a `Funrec` expression that has the same argument name, argument type, and body, with the addition of the function name and return type.  The final expression should be a `Let` expression, binding the function name to the new `Funrec` expression and returning the body `e2`.

Here are some example test cases:
- `_parser [OP;LETREC;ICONST 0]` should raise `SyntaxError "letrec"`
- `_parser [OP;LETREC;ID "s";ICONST 1]` should also raise `SyntaxError "letrec"`
- `_parser [OP;LETREC;ID "f";COLON;BCONST true]` should (cause a call to `_parse_type_expr` that will) raise `SyntaxError "unexpected token in type expression"`
-  `_parser [OP;LETREC;ID "f";COLON;INT;ICONST 1;ICONST 2;CP]` should raise `SyntaxError "letrec must have function value"`
- `_parser [OP;LETREC;ID "f";COLON;INT;OP;FUN;ID "x";COLON;INT;ID "x";CP;CP]` should raise `SyntaxError "Unexpected token: unbalanced parentheses or keyword out of call position"`  (There is no "body" for the let expression)
- `_parser [OP;LETREC;ID "f";COLON;INT;OP;FUN;ID "x";COLON;INT;ID "x";CP;ICONST 1]` should (cause a call to `_parse_two` that will) raise `SyntaxError "parser: missing closing paren."`
- `_parser [OP;LETREC;ID "f";COLON;INT;OP;FUN;ID "x";COLON;INT;ID "x";CP;ICONST 1;CP]` should evaluate to `(Let ("f", Funrec ("f", "x", IntT, IntT, Name "x"), IntC 1), [])`
- `_parser [OP;LETREC;ID "f";COLON;BOOL;OP;FUN;ID "x";COLON;INT;BCONST false;CP;ICONST 1;CP]` shoule evaluate to `(Let ("f", Funrec ("f", "x", BoolT, IntT, BoolC false), IntC 1), [])`

## Other considerations

In addition to satisfying the functional specifications given above, your code
should be readable, with comments that explain what you're trying to accomplish.
It must compile with no errors using the command `ocamlc -c -w -8 str.cma program.ml parser.ml interpreter.ml`
Solutions that pay careful attention to resources like running time and stack
space (e.g. using tail recursion wherever feasible) and code reuse are worth
more than solutions that do not have these properties.

## Submission instructions and extension requests.

Once you are satisfied with the status of your submission in github, you can upload the files `program.ml` and `parser.ml` to the "Homework 3" assignment on [Gradescope](https://www.gradescope.com/courses/342332/assignments/1861568).  We will run additional correctness testing to the basic feedback tests described here, and provide some manual feedback on the efficiency, readability, structure and comments of your code, which will be accessible in Gradescope once all submissions have been graded.

**Extension Requests**: Keep in mind that every student is allowed to request up to 6 24-hour deadline extensions for the four homeworks this semester, but no more than 4 may be used on any single homework.  To request an extension for this homework, please use [this form](https://forms.gle/Xoxq9Mu6DSXMRiAHA) by the submission deadline on Friday, 4/8.
