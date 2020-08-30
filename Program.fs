// Learn more about F# at http://fsharp.org

open System


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let x = Utilities.x
    let y = Utilities.y
    let z = Utilities.z
    let zz = Utilities.rename z
    printfn "x: %A, y: %A, z %A zz %A" x y z zz

    0 // return an integer exit code
