module JoeChakra.Main
open Microsoft.FSharp.Math
open Microsoft.FSharp.Collections
 
type Move= {
   d:int
   x:int
   y:int
}
 

//let mutable dir=North
let mutable count=32

 
let mutable totalMoves = 0;
let printMove X =  printfn "%d,%d %A" X.x X.y JoeChakra.BrainVita.dirs.[X.d]
 
let rec BVMove mov0 (moves:seq<Move>) =
    let mov = JoeChakra.BrainVita.findNextMove mov0
    if mov = JoeChakra.BrainVita.invalid then
        false
    else
        JoeChakra.BrainVita.move mov |> ignore
        totalMoves<-totalMoves+1
        count<-count-1
        let (i,j,d) = mov
        let move:Move = {d=d;  x=i; y=j}
        if Solve (Seq.append moves [move])  then
            true
        else
            JoeChakra.BrainVita.reverse mov |> ignore
            count<-count+1
            BVMove (JoeChakra.BrainVita.incr mov) moves
and Solve  (moves:seq<Move>) =
    if count=1 then
        moves |> Seq.iter printMove |>ignore
        printfn ";--------total moves %d----------" totalMoves
        true
    else
        BVMove JoeChakra.BrainVita.zeroMove moves

 

// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    Solve Seq.empty |> ignore// Learn more about F# at http://fsharp.net
    0 // return an integer exit code
