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
    currency2Float = s2f . replace ',' "" . lstripWith "$ " . strip
        where s2f s = read s :: Float

    -- Haskell quicksort simple version
    -- See http://en.wikipedia.org/wiki/Quicksort#Simple_version
    mysort :: Ord a => [a] -> [a]
    mysort l = quicksort l

    quicksort :: Ord a => [a] -> [a]
    quicksort (pivot:l) =
        quicksortStep l [] [] pivot
    quicksort [] =
        []

    quicksortStep :: Ord a => [a] -> [a] -> [a] -> a -> [a]
    quicksortStep (a:l) less greater pivot = do
        if a <= pivot
            then quicksortStep l (a:less) greater pivot
            else quicksortStep l less (a:greater) pivot
    quicksortStep [] less greater pivot = quicksort less ++ [pivot] ++ quicksort greater

    -- Partially applied function(s)
    half x = div x 2
    add s t = s ++ t
    eol s = s ++ "\n"
