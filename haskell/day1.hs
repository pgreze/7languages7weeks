module Main where

    -- Different ways to write allEven
    -- With tail recursion
    allEvenRecurs :: [Integer] -> [Integer]
    allEvenRecurs [] = []
    allEvenRecurs (h:t) = if even h then h:allEvenRecurs t else allEvenRecurs t
    -- With list comprehension
    allEventLC l = [x | x <- l, even x]
    -- With built-in filter function
    allEvenFilter = filter even

    -- builds two-tuples with all possible combinations of colors
    colors = ["black", "white", "blue", "yellow", "red"]
    pairs :: [String] -> [(String, String)]
    pairs l = [(x, y) | x <- l, y <- l, x < y]

    -- childhood multiplication table :3
    mulTableLimit = 12
    mulTable = [(x, y, x*y) | x <- [1..mulTableLimit], y <- [1..mulTableLimit]]

    -- Map coloring problem
    -- Regions:
    --            Tennessee (neighbour of all southern regions)
    --     Mississippi   Alabama   Georgia
    --                          Florida (neighbour of Alabama/Georgia)
    frontiers = [("Mississippi", "Tennessee"),("Mississippi", "Alabama"),("Alabama", "Tennessee"),("Alabama", "Mississippi"),("Alabama", "Georgia"),("Alabama", "Florida"),("Georgia", "Florida"),("Georgia", "Tennessee")]
    regions = ["Tennessee","Mississippi","Alabama","Georgia","Florida"]

    regionsColors :: [[String]]
    regionsColors = coloring regions [] colors []

    -- Coloring resolution
    -- 1st arg: notColoredRegions
    -- 2nd arg: coloredRegions
    -- 3rd arg: availableColors
    -- 4th arg: currentColorOwners
    coloring :: [String] -> [[String]] -> [String] -> [[String]] -> [[String]]
    -- When all regions are colored
    coloring [] colored _ currentColorOwners = (colored ++ currentColorOwners)
    -- When a new color is required (first iteration for example)
    coloring (currentRegion:nextRegions) coloredRegions availableColors [] =
        coloring nextRegions [] colors (coloredRegions ++ [[currentRegion]])
    -- Core resolution
    coloring (currentRegion:nextRegions) coloredRegions (color:nextColors) (owners:nextOwners) = do
        -- If current region is a neighbour of current color's owners
        if neighbour currentRegion owners
            -- Try a new color
            then coloring (currentRegion:nextRegions) (coloredRegions ++ [owners]) nextColors nextOwners

            -- Not a neighbour, affect this color
            -- And restart resolution
            else coloring nextRegions [] colors (coloredRegions ++ (currentRegion:owners):nextOwners)

    -- Returning if r1 is a neighbour of region list
    -- Note: elem returns if first argument is in list (second argument)
    neighbour :: String -> [String] -> Bool
    neighbour r1 [] = False
    neighbour r1 l =
        let
            r2 = head l
        in
            elem (r1, r2) frontiers || elem (r2, r1) frontiers || neighbour r1 (tail  l)
