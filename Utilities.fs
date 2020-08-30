module Utilities

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


let x = BdiNumber 3.0
let y = BdiVar("foo")

let z = rename y

printf "x: %A\ty: %A\tz: %A\n" x y z
