module Route exposing (Route(..), parse, parser)

import Url
import Url.Parser as Parser exposing (Parser, (</>), map, s, string, top)


type Route
    = Top
    | Analysis String


parse : Url.Url -> Maybe Route
parse url =
    Parser.parse parser url


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ s "Analysis-F1"
            </> Parser.oneOf
                    [ map Top top
                    , map Analysis string
                    ]
        , Parser.oneOf
            [ map Top top
            , map Analysis string
            ]
        ]
