# Parser - Ghajaanan Arunachalam

----
## Pexp
The parser takes in a list of tokens from the lexer, and parses them into an abstract-syntax-tree that can be converted and collapsed by the run-time.

`Result <Token list, Token list> -> (AST option * Result <Token list, Token list>) option`


The parser parses successfully according to the following BNF:

`<var> ::= // symbol starting with letter and not a keyword. NB used for all names.`
`<var-list> ::= <var> | <var> <var-list>`
`<int> ::= decimal integer literal`
`<bool> ::= bool`
`<bin-op> ::// a binary operator`
`<unary-op> ::// a unary operator`
`<bra-exp> ::= "(" exp ")"`
`<item-exp> ::= <var> | <int> | <bool> | <bra-exp> | 
<bin-opn> | <defn-exp> | <if-exp> | <lambda-exp>`
`<binary-opn> ::= <bin-op> <item-exp> <item-exp>`
`<unary-opn> ::= <unary-op> <item-exp>`
`<app-exp-list> ::= <item-exp> | <item-exp> <app-exp-list>`
`<defn-exp> ::= "let" <var-list> "be" <exp> "ni" <exp> ";"`
`<if-exp> ::= "if" <exp> "then" <exp> "else" <exp> "fi"`
`<exp> ::= "lamb" <var-list> "->" <exp> "mi"`
`<exp> ::= <app-exp-list>`

A successful parse of a syntactically correct expression gives:

`Some(Some (<AST>), Ok [<remaining tokens list>])`

An unsuccessful parse gives:

`Some (None, Error [<unsuccessful token list>])`

----
## Testing
I tested the parser by using a series of unit tests (Expecto), including a mix of positive tests and error tests allowing me to ensure the parser throw an error when expected.


----
## What doesn't work?

Parsing an incorrect, bracketed expression

e.g. (x y

gives

`Some (Some (Bracketed (FuncApp (Var "x", Var"y")), Error [ ])`

The parser recognises the missing ")" and throws an error, but parses the inside expression successfully.
This wasn't intended but may be desirable, hence why it has been left in for now. *However, this isn't consistent with the rest of the program, so it will either be removed or the other functions changed to provide the same error handling.*

Parsing a binary operation works as intended, but the function is written rather inelegantly, with lines of code copied and pasted. There is a commented out replacement function above that is better, but hasn't yet been integrated with the rest of the parser.

----
## Code written by/for others
None. The workload was fairly well balanced throughout the team.

However, many of my test inputs were given by the lexer and the corresponding output ast were given by the runtime. This helped clarify what the parser had to do.

----
## Debugging/Code review

Initially, for debugging help, I worked closely with the lexer (Tharusha) as he was able to clarify which tokens made up a particular expression and what tokens I should expect next.

For the code review, I worked with combinator runtime (Ramon). This proved useful as we had the opportunity to provide a completely different insight into structuring each others functions and allowed us to confirm our interfacing types.


----
## Deciding types
Since the runtime needed a specific structure for the abstract syntax tree, we first decided the AST type.
And, as we had an idea of the syntax for Gb, we thought it would be most intuitive to output the tokens as simply as possible.

If it proved difficult to parse from a particular list of tokens to its corresponding expression, we revised the Token type, adding extra tokens clarifying where a certain expression began/ended.

----
## How will the code be used by the team?

Parses lexed tokens and gives the resulting abstract syntax tree to the runtime.

`input |> Lexer |> Parser |> runTime`

----
## Additional Features
# Curried Anonymous Functions
Curried anonymous functions are parsed such that the 
body of the left-most variable is another anonymous function with the subsequent variables.

e.g.
`f x -> x + 2`
becomes `Lambda{arg = Var "f"; body = Lambda{arg = Var "x"; body = FuncApp (FuncApp (BinOp PLUSOP,Literal (NumItem 2.0))}}`

This should simplify the runtime.

# Curried FuncDefExp
Curried function definition expressions are parsed such that the body of the left-most variable is an anonymous function with the subsequent variables and body.

e.g.

`let f x y = x + y`

`return f` becomes

`FuncDefExp{identity = Var "f"; body =Lambda{arg = Var "x"; body = Lambda{arg = Var "y"; body =FuncApp(FuncApp(BinOp PLUSOP,Var "x"),Var "y")}}returnexp = Var "f" })`

This should simplify the runtime.
