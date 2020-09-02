

type BdiTerm =
    | BdiNumber of float
    | BdiSymbol of string
    | BdiVar of string
    | BdiList of list<BdiTerm>


type Result<'a> =
    | Success of 'a
    | Failure 

let gensym =
    let i = ref 0
    fun () ->
        let result = sprintf "::_%i" !i
        incr i
        result

let rename var =
    match var with
    | BdiVar s ->
        let pos = s.IndexOf("::", 0, s.Length)
        if pos < 0 then BdiVar(s + gensym ()) else BdiVar(s.[0..pos - 1] + gensym ())
    | _ -> BdiVar("")


let b = BdiSymbol "sym1"
let n1 = BdiNumber 3.0
let n2 = BdiNumber 4.0

let v1 = BdiVar("foo")
let v2 = BdiVar("foo1")

let v3 = rename v2



let makeEnv = Map.empty
let binding (var : BdiTerm) (env : Map<BdiTerm, BdiTerm>) = 
    env.ContainsKey var


let rec unify term1 term2 env = 
    if (term1 = term2) then
        Success env
    else if (binding term1 env) then
        unify env.[term1] term2 env
    else if (binding term2 env) then
        unify term1 env.[term2] env
    else match (term1,  term2) with
            | (BdiVar a, term2) ->
                let newEnv = env.Add(term1, term2)
                Success newEnv
            | (term1, BdiVar b) ->
                let newEnv = env.Add(term2, term1)
                Success newEnv
            | (BdiList (x::xs), BdiList (y::ys)) ->
                match unify x y env with
                | Success e ->
                    unify (BdiList xs) (BdiList ys) e
                | _ -> Failure
            | _ -> Failure

// (match '(p ?v b ?x d (?z ?z))'(p a ?w c ?y (e e)) '((?v . a) (?w . b)))
let l1 = BdiList [BdiSymbol "p"; BdiVar "v"; BdiSymbol "b"; BdiVar "x"; BdiSymbol "d"; BdiList [BdiVar "z"; BdiVar "z"]]
let l2 = BdiList [BdiSymbol "p"; BdiSymbol "a"; BdiVar "w";  BdiSymbol "c"; BdiVar "y"; BdiList [BdiSymbol "e"; BdiSymbol "e"]]

let e = makeEnv.Add(BdiVar "v", BdiSymbol "a").Add(BdiVar "w", BdiSymbol "b")

printf "unify: %A" (unify l1 l2 e)

    


