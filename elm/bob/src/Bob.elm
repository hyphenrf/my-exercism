module Bob exposing (hey)


hey : String -> String
hey remark =
    case String.trim remark of
        "" ->
            "Fine. Be that way!"

        cs ->
            case ( isQuestion cs, notYelling cs ) of
                ( True, True ) ->
                    "Sure."

                ( True, False ) ->
                    "Calm down, I know what I'm doing!"

                ( False, True ) ->
                    "Whatever."

                ( False, False ) ->
                    "Whoa, chill out!"


isQuestion : String -> Bool
isQuestion =
    String.endsWith "?"


notYelling : String -> Bool
notYelling string =
    not (String.any Char.isUpper string)
        || String.any Char.isLower string
