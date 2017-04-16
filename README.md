This is a very basic lambda interpreter, written in Haskell using the Happy parser generator and the Alex lexer.

This was my first ever work in Haskell, so the code is very rough around the edges.

Run using:

        runhaskell < examples/example1.lambda

Valid syntax:

/# = comment, line will be ignored.

\ = lambda symbol substitute

-> = lambda arrow

All combinations of operators * - + using numbers or alphanumeric variables are valid, but the interpreter won't evaluate them.

Wrap all your lambdas and arguments in parentheses.

Please note that I absolutely, definitely, do not guarantee the correctness of this interpreter and/or its' results :)


