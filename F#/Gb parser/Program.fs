open System

open Parser
open ParserTests

[<EntryPoint>]
let main argv =
    testParser() |> ignore
    Ok [TNUM 3.0] |> print
    Ok [TNUM 3.0] |> Pexp |> print
    
    Ok [TVAR "x"] |> print
    Ok [TVAR "x"] |> Pexp |> print
    
    Ok [TVAR "x"; TVAR "y"] |> print
    Ok [TVAR "x"; TVAR "y"] |> Pexp |> print
    
    Ok [TLITERAL "true"] |> print
    Ok [TLITERAL "true"] |> Pexp |> print
    
    ((TVAR "x"),(Ok [TVAR "x"; TVAR "y"]))  |> print
    Pmatch (TVAR "x") (Ok [TVAR "x"; TVAR "y"])  |> print
    
    Ok [TLROUNDBRACKET; TVAR "x"; TVAR "y"; TRROUNDBRACKET; TNUM 6.0] |> print
    Ok [TLROUNDBRACKET; TVAR "x"; TVAR "y"; TRROUNDBRACKET; TNUM 6.0] |> Pexp |> print

    Some(FuncApp (FuncApp(Var "a", FuncApp(Var "b", Var "c")),FuncApp (Var "y",Var "z"))) |> print
    Some(FuncApp (FuncApp(Var "a", FuncApp(Var "b", Var "c")),FuncApp (Var "y",Var "z"))) |> RevAssoc |> print
    
    Ok[TBINOP "+"; TVAR "x"; TVAR "y"] |> print
    Ok[TBINOP "+"; TVAR "x"; TVAR "y"] |> Pexp |> print
    
    Ok[TBINOP "*"; TVAR "x"; TLROUNDBRACKET; TBINOP "+"; TVAR "y"; TVAR "z"; TRROUNDBRACKET] |> print
    Ok[TBINOP "*"; TVAR "x"; TLROUNDBRACKET; TBINOP "+"; TVAR "y"; TVAR "z"; TRROUNDBRACKET] |> Pexp |> print
    
    Ok[TBINOP "*"; TVAR "x"; TUNARY "fst"; TBINOP "pair"; TVAR "y"; TVAR "z"] |> print
    Ok[TBINOP "*"; TVAR "x"; TUNARY "fst"; TBINOP "pair"; TVAR "y"; TVAR "z"] |> Pexp |> print
    
    Ok[KLET; TVAR "f"; KBE; TBINOP "^"; TVAR "x"; TNUM 2.0; KNI; TVAR "f"; KRETURN] |> print
    Ok[KLET; TVAR "f"; KBE; TBINOP "^"; TVAR "x"; TNUM 2.0; KNI; TVAR "f"; KRETURN] |> Pexp |> print
    
    Ok[KLET; TVAR "f"; TVAR "x"; TVAR "y"; KBE; TBINOP "+"; TVAR "x"; TVAR "y"; KNI; TVAR "f"; KRETURN] |> print
    Ok[KLET; TVAR "f"; TVAR "x"; TVAR "y"; KBE; TBINOP "+"; TVAR "x"; TVAR "y"; KNI; TVAR "f"; KRETURN] |> Pexp |> print

    Ok[KIF; TBINOP "="; TVAR "x"; TLITERAL "true"; KTHEN; TBINOP "*"; TVAR "x"; TNUM 1.0; KELSE; TBINOP "*"; TVAR "x"; TNUM -1.0; KFI] |> print
    Ok[KIF; TBINOP "="; TVAR "x"; TLITERAL "true"; KTHEN; TBINOP "*"; TVAR "x"; TNUM 1.0; KELSE; TBINOP "*"; TVAR "x"; TNUM -1.0; KFI] |> Pexp |> print
    
    Ok[KLAMB; TVAR "x"; KARROW; TBINOP "*"; TNUM 2.0; TVAR "x"; KMI] |> print
    Ok[KLAMB; TVAR "x"; KARROW; TBINOP "*"; TNUM 2.0; TVAR "x"; KMI] |> Pexp |> print
    
    Ok[KLAMB; TVAR "x"; TVAR "y"; TVAR "z"; KARROW; TBINOP "+"; TVAR "x"; TBINOP "*"; TVAR "y"; TVAR "z"; KMI (*;TNUM 3.0*)] |> print
    Ok[KLAMB; TVAR "x"; TVAR "y"; TVAR "z"; KARROW; TBINOP "+"; TVAR "x"; TBINOP "*"; TVAR "y"; TVAR "z"; KMI (*;TNUM 3.0*)] |> Pexp |> print
    
    Ok [KLAMB; TLITERAL "true"; KARROW; TBINOP "*"; TNUM 2.0; TVAR "x"; KMI] |> print
    Ok [KLAMB; TLITERAL "true"; KARROW; TBINOP "*"; TNUM 2.0; TVAR "x"; KMI] |> Pexp |> print
    
//    let input1b = Ok [TVAR "+"; TVAR "y"; TVAR "z"]
//    input1b |> print
//    Pvarlist input1b |> print
    
//    let input1c = FuncApp (FuncApp(Var "a", FuncApp(Var "b", Var "c")),FuncApp (Var "y",Var "z"))
//    input1c |> print
//    RevAssoc input1c |> print
    
//    let input1d = Some (FuncApp (Var "y",Var "z"))
//    input1d |> print
//    RevAssoc input1d |> print


//    let input2 = Ok [TVAR "x"; TVAR "y"; TNUM 3.0]
//    input2 |> print
//    Pvarlist input2 |> print
//    
//    let input3 = Ok [TNUM 1.0; TVAR "y"; TVAR "z"]
//    input3 |> print
//    Pvarlist input3 |> print
//    
//    let input4 = Ok [TNUM 3.0; TNUM 4.0]
//    input4 |> print
//    Pitemexp input4 |> print
//    
//    let input5 = Ok [TNUM 3.0; TVAR "y"; TVAR "z"; TLROUNDBRACKET]
//    input5 |> print
//    Pexp input5 |> print
    
//    let input6a = Ok [TLROUNDBRACKET; TVAR "+"; TVAR "a"; TVAR "b"; TRROUNDBRACKET; TLROUNDBRACKET; TVAR "x"; TVAR "y"; TRROUNDBRACKET; TNUM 6.0]
//    input6a |> print
//    Pexp input6a |> print
//    let input6b = Ok [TVAR "AST 1"; TVAR "AST2"; TNUM 6.0]
//    input6b |> print
//    Pexp input6b |> print

//    let input7a = Ok [TLROUNDBRACKET; TVAR "a"; TVAR "b"; TRROUNDBRACKET]
//    input7a |> print
//    input7a|> Pexp |> print
//    
//    let input7b = Ok [TLROUNDBRACKET; TVAR "a"; TVAR "b"; TRROUNDBRACKET; TLROUNDBRACKET; TBINOP "+"; TVAR "x"; TVAR "y"; TRROUNDBRACKET; TLROUNDBRACKET; TNUM 1.0; TNUM 2.0; TRROUNDBRACKET]
//    input7b |> print
//    input7b|> Pexp |> print
//        
//    let input8a = Ok[TBINOP "+"; TVAR "x"; TVAR "y"]
//    input8a |> print
//    input8a |> Pexp |> print
//    
//    let input9 = Ok[KLET; TVAR "f"; TVAR "x"; KBE; TBINOP "^"; TVAR "x"; TNUM 2.0; KNI; TVAR "f"; KRETURN]
//    input9 |> print
//    input9 |> Pexp |> print
//    
//    let input10 = Ok[KIF; TBINOP "="; TVAR "x"; TLITERAL "true"; KTHEN; TBINOP "*"; TVAR "x"; TNUM 1.0; KELSE; TBINOP "*"; TVAR "x"; TNUM -1.0; KFI ]
//    input10 |> print
//    input10 |> Pexp |> print
//    
//    let input11a = Ok[KLAMB; TVAR "x"; KARROW; TBINOP "*"; TNUM 2.0; TVAR "x"; KMI] //lamb x -> 2 * x
//    input11a |> print
//    input11a |> Pexp |> print //Lambda { id = Var "x" body = FuncApp ( FuncApp ( TIMESOP , TNUM 2.0 ) , Var "x")}
//    
//    let input11b = Ok[KLAMB; TVAR "x"; TVAR "y"; KARROW; TBINOP "+"; TVAR "x"; TVAR "y"; KMI] //lamb x y -> x + y
//    input11b |> print
//    input11b |> Pexp |> print //Lambda { id = Var "x" body = Lambda { id = Var "y" body = FuncApp ( FuncApp ( TIMESOP , TVAR "x" ) , Var "y")}}
//    
//    let input12 = Ok[KLAMB; TVAR "x"; TVAR "y"; TVAR "z"; KARROW; TBINOP "+"; TVAR "x"; TVAR "y"; KMI (*;TNUM 3.0*)]
//    input12 |> print
//    input12 |> Pexp |> print
//    
//    let input13 = Ok[KLET; TVAR "f"; KBE; KLAMB; TVAR "x"; KARROW; TBINOP "+"; TNUM 2.0; TVAR "x"; KMI; KNI; TVAR "f"; KRETURN]
//    input13 |> print
//    input13 |> Pexp |> print
//    
//    let input14 = Ok[KLET; TVAR "f"; TVAR "x"; TVAR "y"; KBE; TBINOP "+"; TVAR "x"; TVAR "y"; KNI; TVAR "f"; KRETURN]
//    input14 |> print
//    input14 |> Pexp |> print
    0 // return an integer exit code
