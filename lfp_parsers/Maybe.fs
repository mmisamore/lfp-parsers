module Maybe

/// The Maybe type constructor
type Maybe<'a> = Nothing | Just of 'a

/// Mapping over Maybe
let map: ('a -> 'b) -> Maybe<'a> -> Maybe<'b> =
    fun f ma -> match ma with
                | Nothing -> Nothing
                | Just a  -> Just (f a)

/// Infix shorthand for map
let (<!>) = map

/// Bind for Maybe
let bind: Maybe<'a> -> ('a -> Maybe<'b>) -> Maybe<'b> =
    fun ma f -> match ma with
                | Nothing -> Nothing
                | Just a  -> f a

/// Infix shorthand for bind
let (>>=) = bind

/// Return for Maybe
let retMaybe: 'a -> Maybe<'a> = Just

/// Apply for Maybe
let ap: Maybe<'a -> 'b> -> Maybe<'a> -> Maybe<'b> =
    fun mf ma -> mf >>= fun f -> ma >>= fun a -> retMaybe (f a)

/// Infix shorthand for ap
let (<*>) = ap
