module JSONParser

open FParsec

type public Json =
    | JString of string
    | JNumber of float
    | JBool of bool
    | JNull
    | JList of Json list
    | JObject of Map<string, Json>

type private Parser<'a> = Parser<'a, unit>


let private parseUnicode: Parser<string> =
    pipe4 hex hex hex hex (fun x1 x2 x3 x4 -> floatOfHexString $"{x1}{x2}{x3}{x4}" |> char |> string)


let jnull = pstring "null" |>> fun _ -> JNull

let private jstring: Parser<Json> =
    let normalChar = manySatisfy (fun c -> c <> '\\' && c <> '"')

    let escape =
        anyOf "\\nf/brt\""
        |>> function
            | 'n' -> "\n"
            | 'b' -> "\b"
            | 'f' -> "\u000C"
            | 'r' -> "\r"
            | 't' -> "\t"
            | c -> string c

    let escapeUnicode = pstring "u" >>. parseUnicode

    let escapedChar = pstring "\\" >>. (escapeUnicode <|> escape)

    between (pstring "\"") (pstring "\"") (stringsSepBy normalChar escapedChar)
    |>> JString


let private number: Parser<Json> = pfloat |>> JNumber

let private whitespace: Parser<unit> = spaces

let private boolean: Parser<Json> =
    pstring "true" <|> pstring "false"
    |>> function
        | "false" -> false
        | "true" -> true
        | a -> false
    |>> JBool

let mutable private value, valueRef = createParserForwardedToRef<Json, unit> ()

let private object: Parser<Json> =
    let pKVP: Parser<string * Json> =
        pipe2
            ((whitespace >>. jstring)
             |>> function
                 | JString(str) -> str)
            (whitespace >>. pstring ":" >>. (value))
            (fun x1 x2 -> (x1, x2))

    between (pstring "{") (pstring "}") (sepBy pKVP (pstring ",") |>> Map |>> JObject)

let private array: Parser<Json> =
    between (pstring "[") (pstring "]") (sepBy (value) (pstring ",") |>> JList)

valueRef.Value <-
        whitespace >>. choice [
            object
            array
            jstring
            number
            boolean
            jnull
        ] .>> whitespace

let public parseJSON src = run (object <|> array) src
