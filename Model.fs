// Mdel
module JoeChakra.BrainVita
open Microsoft.FSharp.Math
open Microsoft.FSharp.Collections
open System.Collections.Generic;
let IsValid i j =
     (i>=2 && i<=4 && j>=0 && j<7) ||
     (j>=2 && j<=4 && i>=0 && i<7)
 
let board = Array2D.init 7 7 (fun i j -> 
            (IsValid i j) && not (i=3 && j=3) )
 
let movableX i x y =
   board.[x,y] && board.[x+i,y] && not board.[x+2*i,y]
 
let movableY j x y =
   board.[x,y] && board.[x,y+j] && not board.[x,y+2*j]
 
let moveX i x y =
    board.[x,y] <- false
    board.[x+i,y] <- false
    board.[x+2*i,y] <- true
    true
 
let moveXY i j x y =
    board.[x,y] <- false
    board.[x+i,y+j] <- false
    board.[x+2*i,y+2*j] <- true
    true
 
let moveY j x y =
    board.[x,y] <- false
    board.[x,y+j] <- false
    board.[x,y+2*j] <- true
    true
 
type Direction =
    | East
    | South
    | West
    | North
let isMovable d x y =
    if IsValid x y then
        match d with
            | East -> (IsValid (x+2) y) && (movableX 1 x y)
            | West -> (IsValid (x-2) y) && (movableX -1 x y)
            | South -> (IsValid x (y+2)) && (movableY 1 x y)
            | North -> (IsValid x (y-2))  && (movableY -1 x y)
    else
    false

let dirs = [East;South;West;North]
 
let move mov =
    let (x,y,d) = mov
    match dirs.[d] with
        | East ->  (moveX 1 x y)
        | West ->  (moveX -1 x y)
        | South -> (moveY 1 x y)
        | North -> (moveY -1 x y)
 
let reverseXY i j x y =
    board.[x,y] <- true
    board.[x+i,y+j] <- true
    board.[x+2*i,y+2*j] <- false
    true
 
let reverseX i x y =
    board.[x,y] <- true
    board.[x+i,y] <- true
    board.[x+2*i,y] <- false
    true
 
let reverseY i x y =
    board.[x,y] <- true
    board.[x,y+i] <- true
    board.[x,y+2*i] <- false
    true
     
let reverse mov =
    let (x,y,d) = mov
    match dirs.[d] with
        | East -> reverseX 1 x y
        | West -> reverseX -1 x y
        | South -> reverseY 1 x y
        | North -> reverseY -1 x y
         

let invalid = (-1, -1, -1)
let zeroMove = 0,0,0
 
let incr mov =
    let (i,j,d) = mov
    if d<3 then
        i, j, (d+1)
    else if i<6 then
        (i+1), j, 0
    else if j<7 then
        0, (j+1), 0
    else
        invalid



let rec findNextMove mov =
    if mov = invalid then
        mov
    else
        let (i,j,d) = mov
        if isMovable (dirs.[d]) i j then
            mov
        else
             findNextMove (incr mov)


let findFirstMove =
    findNextMove zeroMove

