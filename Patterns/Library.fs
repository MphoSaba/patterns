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

let rec changed xs ys = 
    match xs with
    | [] -> ys
    | x::rest -> 
        match x with
        | BlackP -> changed rest (ys@["b"])
        | WhiteP -> changed rest (ys@["w"])
        | UnknownP -> changed rest (ys@["."])
        | _ -> failwith "Not implemented"

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
        match count>0 with
        | true -> 
            let mpho = zom pattern cells [] (count-1)
            match mpho.Length<count with
            | true -> Some mpho
            | false -> None
        | false -> None
    | "exactly" ->
        let mpho = zom pattern cells [] count
        match mpho.Length = count with
        | true -> Some mpho
        | false -> None
    | _ -> failwith "Not implemented"

let rec seq cells pattern lst = 
    match cells,pattern with
    | _, [] -> lst
    | x::xrest,y::yrest -> seq xrest yrest (lst@[x])

let seqChecker cells pattern = 
    let mpho = seq cells (changed pattern []) []
    match mpho = (changed pattern []) with
    | true -> Some mpho
    | false -> None

let anyChecker cells pattern = 
    match cells with
    | [] -> None
    | x::rest ->
        match x with
        | "b" -> oom BlackP cells 1 "exactly"
        | "w" -> oom WhiteP cells 1 "exactly"
        | "." -> oom UnknownP cells 1 "exactly"
        | _ -> failwith "Not implemented"

let rec checkPattern cells pattern = 
    match pattern with
    | BlackP | WhiteP | UnknownP ->oom pattern cells 1 "exactly"
    | ZeroOrMore a -> oom a cells 0 "more"
    | OneOrMore a -> oom a cells 1 "more"
    | Exactly (a, b) -> oom b cells a "exactly"
    | FewerThan (a,b) -> oom b cells a "fewer"
    | Sequence a -> seqChecker cells a
    | Either (a,b) ->
        match (oom a cells 1 "exactly"),(oom b cells 1 "exactly") with
        | Some a, _ -> Some a 
        | None, Some b -> Some b
        | None, None -> None
    | Anything -> anyChecker cells pattern
    | EndOfCells ->
        match cells with
        | [] -> Some []
        | _ -> None

let patternMatch pattern cells = 
    let l = fromCells cells |> toCells
    checkPattern l pattern

let find pattern cells = failwith "Not implemented"

let map func pattern cells = //failwith "Not implemented"
    let l = fromCells cells |> toCells
    List.map (func) l