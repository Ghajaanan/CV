module Parser

open Expecto.Flip
open Common

//for console debugging
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser


let print obj = sprintf "%A" obj |> ignore

print "Hello from parser!"

let pEBind msg = Some (None, Error msg)
let optError msg = (Some << Error) msg
let pMap f inp =
        match inp with
        | Error msg -> optError msg
        | Ok inp -> Some <| f inp

let (|PMATCH|_|) (token: Token) =
        (fun inp ->
           match inp with
           | [] -> Error ([])
           | s::rest when s = token -> Ok (rest)
           | lst -> Error (lst) 
          )
        |> pMap
let (|PMATCHbinop|_|) (token: string -> Token) =
        (fun inp ->
           match inp with
           | [] -> Error ([])
           | TBINOP(str)::rest -> Ok (str, Ok rest)
           | lst -> Error (lst) 
          )
        |> pMap
let Pmatch = (|PMATCH|_|)
let Pmatchbinop = (|PMATCHbinop|_|)

let rec (|EXTRACTRIGHTAPPLIST|_|) ast =
    match ast with
    | FuncApp(EXTRACTRIGHTAPPLIST (ast'), EXTRACTRIGHTAPPLIST (ast'')) -> Some(ast' @ ast'')
    | ast -> Some [ast] 
let extractRightAppList = (|EXTRACTRIGHTAPPLIST|_|)

let associate ast1 ast2 = FuncApp(ast1, ast2)

let (|MAKELEFTAPPLIST|_|) astList =
    match astList with
    | Some(x) -> Some(List.reduce associate x)
    | None -> None
let makeLeftAppList = (|MAKELEFTAPPLIST|_|)

let (|REVASSOC|_|) astoption = //takes input of form Some(ast)
    match astoption with
    | Some (ast) -> ast |> extractRightAppList |> makeLeftAppList
    | None -> None
let RevAssoc = (|REVASSOC|_|)

let rec (|PVARLIST|_|) lst = 
    match lst with
    | Error token -> Some (None, Error token)
    | PVAR (Some ast, (PVARLIST (Some ast2, rest))) -> Some( Some(FuncApp(ast, ast2)) (*|> RevAssoc*) , rest)
    | PVAR (Some(ast), rest) -> Some(Some ast, rest)
    | Ok lst -> pEBind <| lst

and unwrapOk (Ok lst) = lst

and (|PVAR|_|) lst = 
    match lst with
    | Error token -> Some (None, Error token)
    | Ok ((TVAR x)::rest) -> Some(Some(Var x), Ok rest)
    | Ok lst -> pEBind <| lst
let Pvarlist = (|PVARLIST|_|)
let Pvar = (|PVAR|_|)

let (|PNUM|_|) lst = 
    match lst with
    | Error token -> Some (None, Error token)
    | Ok ((TNUM x)::rest) -> Some(Some(Literal(TNum x)), Ok rest)
    | Ok lst -> pEBind <| lst
let Pnum = (|PNUM|_|)

let (|PSTRING|_|) lst = 
    match lst with
    | Error token -> Some (None, Error token)
    | Ok ((TSTRING x)::rest) -> Some(Some(Literal(TString x)), Ok rest)
    | Ok lst -> pEBind <| lst
let Pstring = (|PSTRING|_|)
let (|PBOOL|_|) lst = 
    match lst with
    | Error token -> Some (None, Error token)
    | Ok ((TBOOL true)::rest) -> Some(Some(Literal(TBool true)), Ok rest)
    | Ok ((TBOOL false)::rest) -> Some(Some(Literal(TBool false)), Ok rest)
    | Ok (KNULL::rest) -> Some(Some(Null), Ok rest)
    | Ok lst -> pEBind <| lst
let Pbool = (|PBOOL|_|)

let rec (|PAPPEXPLIST|_|) lst = 
    match lst with
    | Error token -> Some (None, Error token)
    | PITEMEXP (Some ast, (PAPPEXPLIST (Some ast2, rest))) -> Some( Some(FuncApp(ast, ast2)) (*|> RevAssoc*) , rest)
    | PITEMEXP (Some(ast), rest) -> Some(Some ast, rest)
    | Ok lst -> pEBind <| lst
    
and (|CATCHBINOP|_|) (Ok lst) =
    match lst with
    | _ :: TBINOP x :: _ -> Some lst
    | _ -> None

and (|PITEMEXP|_|) lst = 
    match lst with
    | Error token -> Some (None, Error token)
    | PBINOPN (Some (ast), rest)->  Some(Some ast, rest)
    | PVAR (Some(ast), rest)->  print "matched with var"; Some(Some ast, rest)
    | PNUM (Some(ast), rest) ->  print "matched with num"; Some(Some ast, rest)
    | PSTRING (Some(ast), rest) ->  print "matched with string"; Some(Some ast, rest)
    | PBOOL (Some(ast), rest) ->  print "matched with bool"; Some(Some ast, rest)
    | PBRAEXP (Some(ast), rest) ->  print "matched with brackets"; Some(Some ast, rest)
    | PUNARYOPN (Some(ast), rest) ->  print "matched with unaryop"; Some(Some ast, rest)
    | PDEFNRECEXP (Some(ast), rest) ->  print "matched with def of rec"; Some(Some ast, rest)
    | PDEFNEXP (Some(ast), rest) ->  print "matched with def of exp"; Some(Some ast, rest)
    | PIFEXP (Some(ast), rest) ->  print "matched with if else"; Some(Some ast, rest)
    | PLAMBDA (Some(ast), rest) ->  print "matched with lambda"; Some(Some ast, rest)
    | Ok lst ->pEBind <| lst
    
and (|PBINOPEXP|_|) lst = 
    match lst with
    | Error token -> Some (None, Error token)
    | PVAR (Some(ast), rest) -> print "matched with var-lite"; Some(Some ast, rest)
    | PNUM (Some(ast), rest) -> print "matched with num-lite"; Some(Some ast, rest)
    | PSTRING (Some(ast), rest) -> print "matched with string-lite"; Some(Some ast, rest)
    | PBOOL (Some(ast), rest) -> print "matched with bool-lite"; Some(Some ast, rest)
    | PBRAEXP (Some(ast), rest) -> print "matched with brackets-lite"; Some(Some ast, rest)
    | PUNARYOPN (Some(ast), rest) -> print "matched with unaryop-lite"; Some(Some ast, rest)
    //| PDEFNRECEXP (Some(ast), rest)
    // | PDEFNEXP (Some(ast), rest) -> print "matched with def of exp-lite"; Some(Some ast, rest)
    // | PIFEXP (Some(ast), rest) -> print "matched with if else-lite"; Some(Some ast, rest)
    // | PLAMBDA (Some(ast), rest) -> print "matched with lambda-lite"; Some(Some ast, rest)
    | Ok lst ->pEBind <| lst
and (|PEXP|_|) lst = 
    match lst with
    | Error token -> Some (None, Error token)
    | PAPPEXPLIST (Some(ast), rest) -> Some(Some(Some ast |> RevAssoc |> unwrapSome |> traverseTree), rest)
    | Ok lst -> pEBind <| lst
    
and traverseTree (tree_in) =
    match tree_in with
    | FuncApp(left_ast, right_ast) -> FuncApp(traverseTree(left_ast), traverseTree(right_ast))
    | BFuncApp(left_ast, right_ast) -> FuncApp(traverseTree(left_ast), traverseTree(right_ast))
    | Pair(ast_1, ast_2) -> Pair(traverseTree(ast_1), traverseTree(ast_2))
    | FuncDefExp({id = str; body = ast_body; rest = ast_rest}) -> FuncDefExp({id = str; body = traverseTree(ast_body); rest = traverseTree(ast_rest)})
    | Lambda({arg = argument; body = ast_body}) -> Lambda({arg = argument; body = traverseTree(ast_body)})
    | IfExpr({cond = ast_cond; trueExpr = ast_true; falseExpr = ast_false}) -> IfExpr({cond = traverseTree(ast_cond); trueExpr = traverseTree(ast_true); falseExpr = traverseTree(ast_false)})
    | Bracketed(expression_ast) -> traverseTree(expression_ast)
    | Closure({arg = str; body = body_ast; closedEnv = env}) -> Closure({arg = str; body = traverseTree(body_ast); closedEnv = env})
    | other_ast -> other_ast
    
and (|PBRAEXP|_|) lst =
    match lst with
    | Error token -> Some (None, Error token)
    | PMATCH TLROUNDBRACKET (PEXP(Some ast, PMATCH TRROUNDBRACKET (rest))) -> Some(Some(Bracketed(ast)), rest)
    | Ok lst -> pEBind <| lst
    
    
and binaryOperators =
    ["+", PLUSOP; "-", MINUSOP; "*", TIMESOP; "/", DIVOP; "^", POWOP; "%", REMAINDOP; "=", EQOP; "<", LESSOP; ">", MOREOP;
    "<=", LESSEQUALOP; ">=", MOREEQUALOP; ":", PAIROP]
    |> Map.ofList



and unwrapSome input =
    match input with
    | Some ast -> ast
    | None -> print("unwrapSome ERROR. Never happens."); Var ("unwrapSome ERROR")


and (|PBINOPN|_|) lst =
    match lst with
    | Error token -> Some (None, Error token)
    | PBINOPEXP(Some ast, PMATCHbinop(TBINOP) (Ok (binop_str, PITEMEXP(Some ast', rest))))
        -> Some(Some(BFuncApp(BFuncApp(BuiltInFunc(BinLiteral(binaryOperators.[binop_str])), ast), ast')), rest)
    | Ok lst -> pEBind <| lst

and (|PUNARYOPN|_|) lst =
    match lst with
    | Error token -> Some (None, Error token)
    | PMATCH (TUNOP "fst") (PITEMEXP (Some ast, rest)) -> Some (Some (FuncApp (BuiltInFunc(FST), ast)), rest)
    | PMATCH (TUNOP "snd") (PITEMEXP (Some ast, rest)) -> Some (Some (FuncApp (BuiltInFunc(SND), ast)), rest)
    | PMATCH (TUNOP "isPair") (PITEMEXP (Some ast, rest)) -> Some (Some (FuncApp (BuiltInFunc(ISPAIROP), ast)), rest)
    | PMATCH (TUNOP "not") (PITEMEXP (Some ast, rest)) -> Some (Some (FuncApp (BuiltInFunc(NOTOP), ast)), rest)
    | PMATCH (TUNOP "implode") (PITEMEXP (Some ast, rest)) -> Some (Some (FuncApp (BuiltInFunc(IMPLODEOP), ast)), rest)
    | PMATCH (TUNOP "explode") (PITEMEXP (Some ast, rest)) -> Some (Some (FuncApp (BuiltInFunc(EXPLODEOP), ast)), rest)
    | Ok lst -> pEBind <| lst


   
and (|PDEFNEXP|_|) lst =
    match lst with
    | Error token -> Some (None, Error token)
    // Matches with:
    //                <defn-exp> ::= 	 "let" <var> "be" TOPENSCOPE <exp> TCLOSECOPE <exp>
    | PMATCH (KLET) (PVAR (Some (Var name), PMATCH (KBE) (PMATCH (TOPENSCOPE) (PEXP (Some expression_ast, PMATCH (TCLOSESCOPE)(PEXP (Some return_ast, rest)))))))
         -> Some(Some(FuncDefExp ({id = name; body = expression_ast; rest = return_ast}) ), rest)
    // Matches with:
    //                <defn-exp> ::= 	 "let" <var> "be" <exp> ";" <exp>
    | PMATCH (KLET) (PVAR (Some (Var name), PMATCH (KBE)(PEXP (Some expression_ast, PMATCH (TSEMICOLON)(PEXP (Some return_ast, rest))))))
        -> Some(Some(FuncDefExp ({id = name; body = expression_ast; rest = return_ast}) ), rest)
    // Matches with the above cases but where there are more than one <var>. In which case, passes through Pcurrieddefnexp function.
    | PMATCH (KLET) (PVAR (Some (Var name), PVARLIST (Some (varlist_ast), PMATCH (KBE) (PMATCH (TOPENSCOPE) (PEXP (Some expression_ast, PMATCH (TCLOSESCOPE)(PEXP (Some return_ast, rest))))))))
        -> Some(Some(FuncDefExp ({id = name; body = (Pcurrieddefnexp lst expression_ast); rest = return_ast}) ), rest)
    | PMATCH (KLET) (PVAR (Some (Var name), PVARLIST (Some (varlist_ast), PMATCH (KBE)(PEXP (Some expression_ast, PMATCH (TSEMICOLON)(PEXP (Some return_ast, rest)))))))
        -> Some(Some(FuncDefExp ({id = name; body = (Pcurrieddefnexp lst expression_ast); rest = return_ast}) ), rest)
    | Ok lst -> pEBind <| lst
    | _ -> print("PDEFNEXP ERROR. Never happens."); Some(None, Error [])

and (|PDEFNRECEXP|_|) lst =
    match lst with
    | Error token -> Some (None, Error token)
    // Matches with:
    //                <rec-defn-exp> ::= 	 "letr" <var> "be" TOPENSCOPE <exp> TCLOSECOPE <exp>
    | PMATCH (KLETREC) (PVAR (Some (Var name), PMATCH (KBE) (PMATCH (TOPENSCOPE) (PEXP (Some expression_ast, PMATCH (TCLOSESCOPE)(PEXP (Some return_ast, rest)))))))
         -> Some(Some(FuncDefExp ({id = "1"+name; body = Lambda{arg=name; body=expression_ast}; rest = return_ast}) ), rest)
    // Matches with:
    //                <rec-defn-exp> ::= 	 "letr" <var> "be" <exp> ";" <exp>
    | PMATCH (KLETREC) (PVAR (Some (Var name), PMATCH (KBE)(PEXP (Some expression_ast, PMATCH (TSEMICOLON)(PEXP (Some return_ast, rest))))))
        -> Some(Some(FuncDefExp ({id = "1"+name; body = Lambda{arg=name; body=expression_ast}; rest = return_ast}) ), rest)
    // Matches with the above cases but where there are more than one <var>. In which case, passes through Pcurrieddefnexp function.
    | PMATCH (KLETREC) (PVAR (Some (Var name), PVARLIST (Some (varlist_ast), PMATCH (KBE) (PMATCH (TOPENSCOPE) (PEXP (Some expression_ast, PMATCH (TCLOSESCOPE)(PEXP (Some return_ast, rest))))))))
        -> Some(Some(FuncDefExp ({id = "1"+name; body = Lambda{arg=name; body=(Pcurrieddefnexp lst expression_ast)}; rest = return_ast}) ), rest)
    | PMATCH (KLETREC) (PVAR (Some (Var name), PVARLIST (Some (varlist_ast), PMATCH (KBE)(PEXP (Some expression_ast, PMATCH (TSEMICOLON)(PEXP (Some return_ast, rest)))))))
        -> Some(Some(FuncDefExp ({id = "1"+name; body = Lambda{arg=name; body=(Pcurrieddefnexp lst expression_ast)}; rest = return_ast}) ), rest)
    | Ok lst -> pEBind <| lst
    | _ -> print("PDEFRECNEXP ERROR. Never happens."); Some(None, Error [])
   
and extractIDs lst id_list =
    match lst with
    |Ok(KLET:: TVAR x :: tail) -> extractIDs (Ok tail) ([])
    |Ok(KLETREC:: TVAR x :: tail) -> extractIDs (Ok tail) ([])
    |Ok(TVAR x :: tail) -> extractIDs (Ok tail) (id_list @ [x])
    |Ok(KBE :: rest) -> id_list //Eventually end up here, in which case:
                                //    the "TOPENSCOPE <exp> TCLOSECOPE <exp>" or "<exp> ";" <exp>" parts are ignored
                                //    and the id_list (list of variables) are passed through.
    |_ -> failwithf "list empty" //Never happens.

and Pcurrieddefnexp lst expression_ast= //Only called if the next tokens definitely represent a curried defn-exp
    let id_list = extractIDs lst []
    let constructedLambda = constructLambda (expression_ast, id_list)
    constructedLambda
    
and Pcurrieddefnexp2 lst expression_ast= //Only called if the next tokens definitely represent a curried defn-exp
    let id_list = extractIDs lst []
    let constructedLambda = constructLambda (expression_ast, id_list)
    constructedLambda
    

and (|PIFEXP|_|) lst =
    match lst with
    | Error token -> Some (None, Error token)
    // Matches with:
    //          <if-exp> ::=     "if" <exp> "then" TOPENSCOPE <exp> TCLOSECOPE "else" TOPENSCOPE <exp> TCLOSECOPE
    | PMATCH (KIF) (PEXP(Some ifexpr_ast, PMATCH (KTHEN)(PMATCH (TOPENSCOPE)(PEXP(Some thenexpr_ast, PMATCH (TCLOSESCOPE)(PMATCH (KELSE)(PMATCH (TOPENSCOPE)(PEXP (Some elseexpr_ast, PMATCH (TCLOSESCOPE)(rest))))))))))
    // Matches with:
    //          "if" <exp> "then" <exp> "else" <exp>
    | PMATCH (KIF)(PEXP (Some ifexpr_ast, PMATCH (KTHEN)(PEXP (Some thenexpr_ast, PMATCH (KELSE)(PEXP (Some elseexpr_ast, rest))))))
        -> Some(Some(IfExpr({cond = ifexpr_ast; trueExpr = thenexpr_ast; falseExpr = elseexpr_ast})), rest)
    | Ok lst -> pEBind <| lst
        
and (|PLAMBDA|_|) lst =
    match lst with
    | Error token -> Some (None, Error token)
    // Matches with:
    //          <lamb> ::= 	 "lamb" <var> "->" TOPENSCOPE   <exp> TCLOSECOPE
    | PMATCH (KLAMB) (PVAR (Some (Var arg_name), PMATCH (KARROW) (PMATCH (TOPENSCOPE) (PEXP (Some body_ast, PMATCH (TCLOSESCOPE)(rest))))))
        -> Some(Some(Lambda({arg = arg_name; body = body_ast})), rest)
    // Matches with:
    //          <lamb> ::= 	 "lamb" <var> "->" <exp> ";"
    | PMATCH (KLAMB) (PVAR (Some (Var arg_name), PMATCH (KARROW) (PEXP (Some body_ast, PMATCH (TSEMICOLON)(rest)))))
        -> Some(Some(Lambda({arg = arg_name; body = body_ast})), rest)
    // Matches with the above cases but where there are more than one <var>. In which case, passes through Pcurriedlambda function.
    | PMATCH (KLAMB) (PVARLIST (Some varlist_ast, PMATCH (KARROW) (PMATCH (TOPENSCOPE) (PEXP (Some body_ast, PMATCH (TCLOSESCOPE)(rest))))))
        -> Pcurriedlambda lst body_ast rest
    | PMATCH (KLAMB) (PVARLIST (Some varlist_ast, PMATCH (KARROW) (PEXP (Some body_ast, PMATCH (TSEMICOLON)(rest)))))
        -> Pcurriedlambda lst body_ast rest
    | Ok lst -> pEBind <| lst
        
and extractArguments lst argument_list = //lst is list of tokens (which definitely consists of a Lambda function). argument_lst is the list of arguments that will be output. It is initialised as an [].
    match lst with
    |Ok(KLAMB:: TVAR x :: tail) -> extractArguments (Ok tail) (argument_list @ [x])
    |Ok(TVAR x :: tail) -> extractArguments (Ok tail) (argument_list @ [x])
    |Ok(KARROW :: rest) -> argument_list
    |_ -> [] //Never happens.
and constructLambda (body, argument_list: string list) =
    if (argument_list.Length > 1) then Lambda({arg = argument_list.Head; body = constructLambda(body, argument_list.Tail)}) else Lambda({arg = argument_list.Head; body = body})
    
and Pcurriedlambda lst body rest=
    let argument_list = extractArguments lst [] //argument_list is the list of arguments. rest is the list of tokens following KARROW
    let constructedLambda = constructLambda (body, argument_list)
    Some(Some(constructedLambda), rest)

    
let Pappexplist = (|PAPPEXPLIST|_|)
let Pitemexp = (|PITEMEXP|_|)
let Pexp = (|PEXP|_|)
let Pbraexp = (|PBRAEXP|_|)
let Pdefnexp = (|PDEFNEXP|_|)
let Pdefnrecexp = (|PDEFNRECEXP|_|)
let Pifexp = (|PIFEXP|_|)
let Plambda = (|PLAMBDA|_|)

let Parse tokString = tokString |> Pexp

let ParseWrapper (input: Result<Token list, errorType*string>): Result<AST, errorType *string> =
    match input with
    | Error x -> Error x
    | Ok x ->
        let inp' = Parse <| Ok x
        match inp' with
        | Some(_, Error x) -> Error <| (ParseError, sprintf "Parser Error: %A" x)
        | Some (Some _, Ok e) when not <| List.isEmpty e-> Error <| (ParseError, sprintf "Parser Error: leftover %A" e)
        | Some (Some tree, _) -> Ok tree
        | _ -> Error (ParseError,"ParseWrapper ERROR. Never happens.")
       
        

