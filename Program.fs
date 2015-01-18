module JoeChakra.Main
open Microsoft.FSharp.Math
open Microsoft.FSharp.Collections
open System.Collections.Generic;
 
type Move= {
   d:int
   x:int
   y:int
}
 
let moves = new System.Collections.Generic.List<Move> ()
//let mutable dir=North
let mutable count=32
let mutable found=true

 
let mutable totalMoves = 0;
let printMove X =  printfn "%d,%d %A" X.x X.y JoeChakra.BrainVita.dirs.[X.d]
 
let rec BVMove mov0 =
    let mov = JoeChakra.BrainVita.findNextMove mov0
    if mov = JoeChakra.BrainVita.invalid then
        false
    else
        JoeChakra.BrainVita.move mov |> ignore
        totalMoves<-totalMoves+1
        count<-count-1
        let (i,j,d) = mov
        let move:Move = {d=d;  x=i; y=j}
        moves.Add(move)
        if Solve 0 then
            true
        else
            JoeChakra.BrainVita.reverse mov |> ignore
            moves.RemoveAt(moves.Count-1)
            count<-count+1
            let mov1 = JoeChakra.BrainVita.incr mov
            BVMove mov1
and Solve  k =
    if count=1 then
        moves |> Seq.iter printMove |>ignore
        printfn ";--------total moves %d----------" totalMoves
        true
    else
        BVMove JoeChakra.BrainVita.zeroMove

 

// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    Solve 0 |> ignore// Learn more about F# at http://fsharp.net
    0 // return an integer exit code
