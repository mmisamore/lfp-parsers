module Parser

open Maybe

/// Traditional string parser
type Parser<'a> = string -> ('a * string) list

/// Binding parsers together
let bind: Parser<'a> -> ('a -> Parser<'b>) -> Parser<'b> =
    fun pa f ->
        fun s ->
            let as' = pa s
            let bs = seq { for (a',s') in as' -> (f a') s' }
            List.concat bs

/// Infix shorthand for bind
let (>>=) = bind

/// A trivial Parser that ignores the input string and produces the given symbol
let retParser: 'a -> Parser<'a> =
    fun a -> fun s -> [(a, s)]

/// Apply for parsers
let ap: Parser<'a -> 'b> -> Parser<'a> -> Parser<'b> =
    fun pf pa -> pf >>= fun f -> pa >>= fun a -> retParser (f a)

/// Infix shorthand for ap
let (<*>) = ap


