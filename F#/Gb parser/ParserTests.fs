module ParserTests
open Parser
open Expecto

[<Tests>]
let testNum =
  testCase "Single number" <| fun () ->
    let tokStr = Ok[TNUM 3.0]
    let expected = Some (Some (Literal(NumItem 3.0)), Ok[]) 
    Expect.equal (Parse tokStr) expected "Input: OK[TNUM 3.0]"

let testVar =
  testCase "Single variable" <| fun () ->
    let tokStr = Ok[TVAR "x"]
    let expected = Some (Some (Var "x"), Ok[]) 
    Expect.equal (Parse tokStr) expected "Input: Ok[TVAR \"x\"]"
    
let testVarlist =
  testCase "List of variables" <| fun () ->
    let tokStr = Ok [TVAR "x"; TVAR "y"]
    let expected = Some (Some (FuncApp (Var "x",Var "y")), Ok [])
    Expect.equal (Parse tokStr) expected "Input: Ok [TVAR \"x\"; TVAR \"y\"]"
let testBool =
  testCase "Single bool" <| fun () ->
    let tokStr = Ok[TLITERAL "true"]
    let expected = Some (Some (Literal (BoolItem true)), Ok[])
    Expect.equal (Parse tokStr) expected "Input: Ok[TVAR \"x\"]"

let testMatch =
  testCase "Match Ok [TVAR \"x\"; TVAR \"y\"; TVAR \"z\"] with the token, TVAR \"x\"" <| fun () ->
    let tokStr = Ok [TVAR "x"; TVAR "y"; TVAR "z"]
    let token = TVAR "x"
    let expected = Some (Ok [TVAR "y"; TVAR "z"])
    Expect.equal (Pmatch token tokStr) expected "Input: (TVAR \"x\") (Ok [TVAR \"x\"; TVAR \"y\"; TVAR \"z\"])"

let testBracketedExpression =
  testCase "Test bracketed expression" <| fun () ->
    let tokStr = Ok[TLROUNDBRACKET; TVAR "x"; TVAR "y"; TRROUNDBRACKET; TNUM 6.0]
    let expected = Some(Some (FuncApp (Bracketed (FuncApp (Var "x",Var "y")),Literal (NumItem 6.0))),Ok [])
    Expect.equal (Parse tokStr) expected """"Some(Some (FuncApp (Bracketed (FuncApp (Var "x",Var "y")),Literal (NumItem 6.0))),Ok [])"""
    
let testBracketedExpression2 =
  testCase "Test bracketed expression 2" <| fun () ->
    let tokStr = Ok[TBINOP "*"; TVAR "x"; TLROUNDBRACKET; TBINOP "+"; TVAR "y"; TVAR "z";TRROUNDBRACKET]
    let expected = Some(Some(FuncApp(FuncApp (BinOp TIMESOP,Var "x"),Bracketed (FuncApp (FuncApp (BinOp PLUSOP,Var "y"),Var "z")))), Ok [])
    Expect.equal (Parse tokStr) expected """"Some(Some (FuncApp (Bracketed (FuncApp (Var "x",Var "y")),Literal (NumItem 6.0))),Ok [])"""
    
let testBracketedExpressionError =
  testCase "Test bracketed expression error" <| fun () ->
    let tokStr = Ok [TLROUNDBRACKET; TVAR "x"; TVAR "y";  TNUM 6.0]
    let expected = Some(Some (Bracketed (FuncApp (FuncApp (Var "x",Var "y"),Literal (NumItem 6.0)))),Error [])
    Expect.equal (Parse tokStr) expected """"Some(Some (Bracketed (FuncApp (FuncApp (Var "x",Var "y"),Literal (NumItem 6.0)))),Error [])"""
    
let testRevAssoc =
  testCase "Test reversing tree" <| fun () ->
    let testast = Some(FuncApp(FuncApp (Var "a",FuncApp (Var "b",Var "c")),FuncApp (Var "y",Var "z")))
    let expected = Some(FuncApp(FuncApp (FuncApp (FuncApp (Var "a",Var "b"),Var "c"),Var "y"),Var "z"))
    Expect.equal (RevAssoc testast) expected """"Some(FuncApp(FuncApp (FuncApp (FuncApp (Var "a",Var "b"),Var "c"),Var "y"),Var "z"))"""


let testBinaryOperation =
  testCase "Binary operation" <| fun () ->
    let tokStr = Ok [TBINOP "+"; TVAR "x"; TVAR "y"]
    let expected = Some (Some (FuncApp (FuncApp (BinOp PLUSOP,Var "x"),Var "y")), Ok []) 
    Expect.equal (Parse tokStr) expected """Some (Some (FuncApp (FuncApp (BinOp PLUSOP,Var "x"),Var "y")), Ok [])"""

let testUnaryOperation =
  testCase "Unary operation" <| fun () ->
    let tokStr = Ok [TBINOP "*"; TVAR "x"; TUNARY "fst"; TBINOP "pair"; TVAR "y"; TVAR "z"]
    let expected = Some(Some(FuncApp(FuncApp(FuncApp(FuncApp (FuncApp (BinOp TIMESOP,Var "x"),UnaryOp FST),BinOp PAIROP),Var "y"),Var "z")), Ok [])
    Expect.equal (Parse tokStr) expected """Some(Some(FuncApp(FuncApp(FuncApp(FuncApp (FuncApp (BinOp TIMESOP,Var "x"),UnaryOp FST),BinOp PAIROP),Var "y"),Var "z")), Ok [])"""
   

let testDefinitionExpression =
  testCase "Definition expression" <| fun () ->
    let tokStr = Ok [KLET; TVAR "f"; KBE; TBINOP "^"; TVAR "x"; TNUM 2.0; KNI; TVAR "f"; KRETURN]
    let expected = Some(Some(FuncDefExp{identity = Var "f"
                                        body = FuncApp (FuncApp (BinOp POWOP,Var "x"),Literal (NumItem 2.0))
                                        returnexp = Var "f" }), Ok [])
    Expect.equal (Parse tokStr) expected """Some(Some(FuncDefExp{identity = Var "f"
                                        body = FuncApp (FuncApp (BinOp POWOP,Var "x"),Literal (NumItem 2.0))
                                        returnexp = Var "f" }), Ok [])"""

let testDefinitionExpressionError =
  testCase "Definition expression error" <| fun () ->
    let tokStr = Ok [KLET; TNUM 5.0; KBE; TBINOP "^"; TVAR "x"; TNUM 2.0; KNI; TVAR "f"; KRETURN]
    let expected = Some(None,Error[KLET; TNUM 5.0; KBE; TBINOP "^"; TVAR "x"; TNUM 2.0; KNI; TVAR "f";KRETURN])
    Expect.equal (Parse tokStr) expected """Some(None,Error[KLET; TNUM 5.0; KBE; TBINOP "^"; TVAR "x"; TNUM 2.0; KNI; TVAR "f";KRETURN])"""

let testCurriedDefinitionExpression =
  testCase "Curried definition expression" <| fun () ->
    let tokStr = Ok[KLET; TVAR "f"; TVAR "x"; TVAR "y"; KBE; TBINOP "+"; TVAR "x"; TVAR "y"; KNI; TVAR "f"; KRETURN]
    let expected = Some(Some(FuncDefExp{identity = Var "f"
                                        body =Lambda{ arg = Var "x"
                                                      body = Lambda{ arg = Var "y"
                                                                     body =FuncApp(FuncApp (BinOp PLUSOP,Var "x"),Var "y") } }
                                        returnexp = Var "f" }), Ok [])
    Expect.equal (Parse tokStr) expected """Some(Some(FuncDefExp{identity = Var "f"
                                        body =Lambda{ arg = Var "x"
                                                      body = Lambda{ arg = Var "y"
                                                                     body =FuncApp(FuncApp (BinOp PLUSOP,Var "x"),Var "y") } }
                                        returnexp = Var "f" }), Ok [])"""

let testIfExp =
  testCase "If-else expression" <| fun () ->
    let tokStr = Ok[KIF; TBINOP "="; TVAR "x"; TLITERAL "true"; KTHEN; TBINOP "*"; TVAR "x";TNUM 1.0; KELSE; TBINOP "*"; TVAR "x"; TNUM -1.0; KFI]
    let expected = Some(Some(IfExpr{cond = FuncApp (FuncApp (BinOp EQOP,Var "x"),Literal (BoolItem true))
                                    trueExpr =FuncApp(FuncApp (BinOp TIMESOP,Var "x"),Literal (NumItem 1.0))
                                    falseExpr =FuncApp(FuncApp (BinOp TIMESOP,Var "x"),Literal (NumItem -1.0)) }),Ok [])

    Expect.equal (Parse tokStr) expected """Some(Some(IfExpr{cond = FuncApp (FuncApp (BinOp EQOP,Var "x"),Literal (BoolItem true))
                                    trueExpr =FuncApp(FuncApp (BinOp TIMESOP,Var "x"),Literal (NumItem 1.0))
                                    falseExpr =FuncApp(FuncApp (BinOp TIMESOP,Var "x"),Literal (NumItem -1.0)) }),Ok [])"""

let testIfExpError =
  testCase "If-else expression error" <| fun () ->
    let tokStr = Ok[KIF; TLROUNDBRACKET; TBINOP "="; TVAR "x"; TLITERAL "true"; KTHEN; TBINOP "*"; TVAR "x";TNUM 1.0; KELSE; TBINOP "*"; TVAR "x"; TNUM -1.0; KFI]
    let expected = Some(None,Error[KIF; TLROUNDBRACKET; TBINOP "="; TVAR "x"; TLITERAL "true"; KTHEN;TBINOP "*"; TVAR "x"; TNUM 1.0; KELSE; TBINOP "*"; TVAR "x"; TNUM -1.0;KFI])
    Expect.equal (Parse tokStr) expected """Some(None,Error[KIF; TLROUNDBRACKET; TBINOP "="; TVAR "x"; TLITERAL "true"; KTHEN;TBINOP "*"; TVAR "x"; TNUM 1.0; KELSE; TBINOP "*"; TVAR "x"; TNUM -1.0;KFI])"""

let testAnonymousFunction =
  testCase "Anonymous Function" <| fun () ->
    let tokStr = Ok [KLAMB; TVAR "x"; KARROW; TBINOP "*"; TNUM 2.0; TVAR "x"; KMI]
    let expected = Some(Some(Lambda{ arg = Var "x"
                                     body = FuncApp (FuncApp (BinOp TIMESOP,Literal (NumItem 2.0)),Var "x") }),Ok [])
    Expect.equal (Parse tokStr) expected """Some(Some(Lambda{ arg = Var "x"
                                     body = FuncApp (FuncApp (BinOp TIMESOP,Literal (NumItem 2.0)),Var "x") }),Ok [])"""
                                     
let testAnonymousFunctionError =
  testCase "Anonymous Function error" <| fun () ->
    let tokStr = Ok [KLAMB; TLITERAL "true"; KARROW; TBINOP "*"; TNUM 2.0; TVAR "x"; KMI]
    let expected = Some(None,Error [KLAMB; TLITERAL "true"; KARROW; TBINOP "*"; TNUM 2.0; TVAR "x"; KMI])
    Expect.equal (Parse tokStr) expected """Some(None,Error [KLAMB; TLITERAL "true"; KARROW; TBINOP "*"; TNUM 2.0; TVAR "x"; KMI])"""
    
let testCurriedAnonymousFunction =
  testCase "Curried anonymous Function" <| fun () ->
    let tokStr = Ok  [KLAMB; TVAR "x"; TVAR "y"; TVAR "z"; KARROW; TBINOP "+"; TVAR "x"; TBINOP "*";TVAR "y"; TVAR "z"; KMI]
    let expected = Some(Some(Lambda{ arg = Var "x"
                                     body =Lambda{ arg = Var "y"
                                                   body =Lambda{ arg = Var "z"
                                                                 body =FuncApp(FuncApp(FuncApp(FuncApp (BinOp PLUSOP,Var "x"),BinOp TIMESOP),Var "y"),Var "z") } } }), Ok [])
    Expect.equal (Parse tokStr) expected """Some(Some(Lambda{ arg = Var "x"
                                     body =Lambda{ arg = Var "y"
                                                   body =Lambda{ arg = Var "z"
                                                                 body =FuncApp(FuncApp(FuncApp(FuncApp (BinOp PLUSOP,Var "x"),BinOp TIMESOP),Var "y"),Var "z") } } }), Ok [])"""



let testLst =
    testList "Parser Tests" [
        testNum
        testVar
        testVarlist
        testBool
        testMatch
        testRevAssoc
        testBracketedExpression
        testBracketedExpressionError
        testBracketedExpression2
        testBinaryOperation
        testUnaryOperation
        testDefinitionExpression
        testDefinitionExpressionError
        testCurriedDefinitionExpression
        testAnonymousFunction
        testAnonymousFunctionError
        testIfExp
        testIfExpError
        testCurriedAnonymousFunction
    ]

let testParser() =
    runTests defaultConfig testLst |> ignore