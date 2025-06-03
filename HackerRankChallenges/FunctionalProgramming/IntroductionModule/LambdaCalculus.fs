module LambdaCalculus

// Define types for lambda calculus expressions
type Expression =
    | Var of string
    | Lambda of string * Expression
    | Apply of Expression * Expression


// Helper function for substitution
let rec substitute (var: string) (replacement: Expression) (expr: Expression) =
    match expr with
    | Var v when v = var -> replacement
    | Var v -> Var v
    | Lambda (v, e) when v = var -> Lambda (v, e) 
    | Lambda (v, e) -> 
        let newVar = v + "'"
        Lambda (newVar, substitute var replacement (substitute v (Var newVar) e))
    |Apply (e1, e2) -> Apply (substitute var replacement e1, substitute var replacement e2)


// Reduction function
let rec reduce expr =
    match expr with
    | Apply (Lambda (var, body), arg) ->
        reduce (substitute var arg body)
    | Apply (e1, e2) ->
        let reduced = reduce e1 // Try to reduce left side first
        if reduced <> e1 then
            reduce (Apply (reduced, e2))
        else
            let reduceRight = reduce e2 // If left can`t be reduced, try right
            if reduceRight <> e2 then
                reduce (Apply (e1, reduceRight))
            else
                expr // Can`t be reduced
    | Lambda (var, body) ->
        let reduceBody = reduce body
        Lambda (var, reduceBody)
    | Var _ -> expr


// Function to convert reduced expression back to string
let rec exprToString expr = 
    match expr with
    | Var v -> v
    | Lambda (v, e) -> sprintf "(λ%s.%s)" v (exprToString e)
    | Apply (e1, e2) -> 
        let left = 
            match e1 with 
                | Lambda _ -> sprintf "(%s)" (exprToString e1) 
                | _ -> exprToString e1
        let right = 
            match e2 with 
                | Var _ -> exprToString e2 
                | _ -> sprintf "(%s)" (exprToString e2)
        sprintf "%s%s" left right

 // Function to parse and reduce
let reduceLambda input =
    // parsing of ((λx.(x y))(λz.z))
    // let parsed1 = 
    //     Apply(
    //         Lambda("x", Apply(Var "x", Var "y")),
    //         Lambda("z", Var "z"))
    
    // parsing of ((λx.((λy.(x y))x))(λz.w))
    //let parsed2 = 
    //    Apply(
    //        Lambda("x", Apply(Lambda("y", Apply(Var "x", Var "y")), Var "x")),
    //        Lambda("z", Var "w"))

    // parsing of ((λx.(x x))(λx.(x x))) 
    //let parsed3 = 
    //    Apply(
    //        Lambda("x", Apply(Var "x", Var "x")),
    //        Lambda("x", Apply(Var "x", Var "x")))

    // parsing of (λg.((λf.((λx.(f (x x))) (λx.(f (x x))))) g)) 
    let parsed4 =
        Apply(
            Lambda("g",
                Apply(
                    Lambda("f",
                        Apply(
                            Lambda("x", Apply(Var "f", Apply(Var "x", Var "x"))),  
                            Lambda("x", Apply(Var "f", Apply(Var "x", Var "x")))
                        )
                    ),
                    Var "g"
                )
            ),
            Var "g"  
        )

    let reduced = reduce parsed4
    
    if reduced = parsed4 then
        "CAN'T REDUCE"
    else
        exprToString reduced