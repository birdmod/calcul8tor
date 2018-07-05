type Token = 
| Plus | Minus | Mul | Div | Val of int

let rec tokenize (s:string) : Token = 
    match s with
    | "+" -> Plus
    | "-" -> Minus
    | "*" -> Mul
    | "/" -> Div
    | e -> Val(System.Int32.Parse(e))

let rec eatTokens (tkns: Token list) = 
    match tkns with
    | Val(a)::Plus::l -> a + eatTokens l
    | Val(a)::Minus::l -> a - eatTokens l
    | Val(a)::Mul::Val(b)::l -> eatTokens (Val(a*b)::l)
    | Val(a)::Div::Val(b)::l -> eatTokens (Val(a*b)::l)
    | Val(a)::[] -> a
    | _ -> failwith "parse error"

[<EntryPoint>]
let main argv = 
    let inputWithSpaces:string = System.Console.ReadLine()
    let tokens = 
        inputWithSpaces.Split(' ') 
        |> Array.map tokenize 
        |> Array.toList

    let res = eatTokens tokens

    printfn "%A" res
    System.Console.ReadLine |> ignore
    0 // return an integer exit code
