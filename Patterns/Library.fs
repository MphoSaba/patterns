#if !INTERACTIVE
module Patterns
#endif

type Cell = 
    |Black
    |White
    |Unknown 

type Pattern = 
    |BlackP//A pattern that matches a black/positive cell
    |WhiteP//A pattern that matches a white/negative cell
    |UnknownP//A pattern that matches a yellow/unknown cell
    |ZeroOrMore of Pattern //A pattern that matches zero or more patterns
    |OneOrMore of Pattern //A pattern that matches one or more patterns
    |Exactly of int*Pattern //A pattern that matches exactly n instances of a pattern
    |FewerThan of int*Pattern //A pattern that matches fewer than n instances of a pattern
    |Sequence of Pattern list //A pattern that matches a sequence of patterns
    |Either of Pattern*Pattern //A pattern that matches either of two patterns,
    |Anything //A pattern that matches any cell
    |EndOfCells //A pattern that matches the end of a sequence

let toCells v = 
    let rec tCells xs ys =
        match xs with
        | [] -> ys
        | x::rest -> 
            match x with
            | 'b' | 'B' -> tCells rest (ys@["b"])
            | 'w' | 'W' -> tCells rest (ys@["w"])
            | _ -> tCells rest (ys@["."])
    tCells (v |> Seq.toList) []

let fromCells v = 
    let rec fCells xs str =
        match xs with
        | [] -> str
        | x::rest -> fCells rest (str+x)
    fCells v ""

let changer x = 
    match x with 
    | BlackP -> "b"
    | WhiteP -> "w"
    | UnknownP -> "."
    | _ -> ""

let func cells pattern =
    let mpho = changer pattern
    let ans = List.exists (fun el -> el=mpho) cells
    match ans with
    | true -> Some [mpho]
    | false -> None

let rec zom pattern cells (xs:string list) count= 
    match cells,(xs.Length=count) with
    | _,true -> xs
    | [], _ -> xs
    | x::rest,false -> 
        match pattern, x with
        | BlackP,"b" -> zom pattern rest (xs@["b"]) count
        | WhiteP,"w" -> zom pattern rest (xs@["w"]) count
        | UnknownP,"." -> zom pattern rest (xs@["."]) count
        | _ -> xs

let oom pattern cells count rest =
    match rest with
    | "more" ->
        let mpho = zom pattern cells [] cells.Length
        match mpho.Length>=count with
        | true -> Some mpho
        | false -> None
    | "fewer" -> 
        let mpho = zom pattern cells [] (count-1)
        match mpho.Length < count with
        | true -> Some mpho
        | false -> None
    | "exactly" ->
        let mpho = zom pattern cells [] count
        match mpho.Length=count with
        | true -> Some mpho
        | false -> None

let patternMatch pattern cells = 
    let l = fromCells cells |> toCells
    match pattern with
    | BlackP | WhiteP | UnknownP ->func l pattern 
    | ZeroOrMore a -> oom a l 0 "more"
    | OneOrMore a -> oom a l 1 "more"
    | Exactly (a, b) -> oom b l a "exactly"
    | FewerThan (a,b) -> 
        match a>0 with
        | true -> oom b l a "fewer"
        | false -> None
    | _ -> None

let find pattern cells = failwith "Not implemented"

let map func pattern cells = failwith "Not implemented"