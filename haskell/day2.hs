module Main where
    -- String functions
    whitespaces = " \t"
    lstripWith chars = dropWhile (\x -> any (== x) chars)
    rstripWith chars = reverse . lstripWith chars . reverse
    stripWith chars = lstripWith chars . rstripWith chars
    strip :: String -> String
    strip = stripWith whitespaces
    --lstrip = lstripWith whitespaces
    --rstrip = rstripWith whitespaces

    replace :: Char -> String -> String -> String
    replace old new (c:s) = (if c == old then new else [c]) ++ replace old new s
    replace old new [] = []

    -- Converting a human form of currency
    -- "$2,345,678.99" is converted to 2345679.0
    currency2Float :: String -> Float
    currency2Float s = read (currency2SFloat s) :: Float
        where currency2SFloat = strip . lstripWith "$ " . replace ',' ""
