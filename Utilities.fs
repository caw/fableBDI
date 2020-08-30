
type BdiTerm =
    | BdiNumber of float
    | BdiSymbol of string
    | BdiVar of string
    | BdiList of list<BdiTerm>


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

let unify term1 term2 env = if term1 = term2 then true else false

let makeEnv = Map.empty
let binding (BdiVar s) env = 
    printf "s: %s" s

        
printf "Result: %b\n" (unify b b makeEnv)
printf "Result: %b\n" (unify v1 v2 makeEnv)
printf "Result: %b\n" (unify v1 v1 makeEnv)
printf "Result: %b\n" (unify n1 n2 makeEnv)
printf "Result: %b\n" (unify n1 n1 makeEnv)

