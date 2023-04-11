module JSONParser

open FParsec

type Json =
    | JString of string
    | JNumber of float
    | JBool of bool
    | JNull
    | JList of Json list
    | JObject of Map<string, Json>

type Parser<'a> = Parser<'a, unit>


let hexDigits: Parser<string> =
    pipe4 hex hex hex hex (fun x1 x2 x3 x4 -> floatOfHexString $"{x1}{x2}{x3}{x4}" |> char |> string)


let jstring: Parser<Json> =
    let normalChar = manySatisfy (fun c -> c <> '\\' && c <> '"')

    let escapedChar =
        pstring "\\"
        >>. (pstring "u" >>. hexDigits
             <|> (anyOf "\\nrt\""
                  |>> function
                      | 'n' -> "\n"
                      | 'r' -> "\r"
                      | 't' -> "\t"
                      | c -> string c))

    between (pstring "\"") (pstring "\"") (stringsSepBy normalChar escapedChar)
    |>> JString


let number: Parser<Json> = pfloat |>> JNumber

let whitespace: Parser<unit> = spaces

let boolean: Parser<Json> =
    pstring "true" <|> pstring "false"
    |>> function
        | "false" -> false
        | "true" -> true
        | a -> false
    |>> JBool

let declare<'a> = ref Unchecked.defaultof<'a>

let mutable value, valueRef = createParserForwardedToRef<Json, unit> ()

let rec object: Parser<Json> =
    let pKVP: Parser<string * Json> =
        pipe2
            ((whitespace >>. jstring)
             |>> function
                 | JString(str) -> str)
            (whitespace >>. pstring ":" >>. (value))
            (fun x1 x2 -> (x1, x2))

    between (pstring "{") (pstring "}") (sepBy pKVP (pstring ",") |>> Map |>> JObject)

let array: Parser<Json> =
    between (pstring "[") (pstring "]") (sepBy (value) (pstring ",") |>> JList)

valueRef.Value <-
    between
        whitespace
        whitespace
        (jstring
         <|> boolean
         <|> number
         <|> (pstring "null" |>> fun _ -> JNull)
         <|> array
         <|> object)
