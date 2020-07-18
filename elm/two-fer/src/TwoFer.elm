module TwoFer exposing (twoFer)


twoFer : Maybe String -> String
twoFer name =
    case name of
        Nothing ->
            "One for you, one for me."

        Just str ->
            "One for " ++ str ++ ", one for me."
