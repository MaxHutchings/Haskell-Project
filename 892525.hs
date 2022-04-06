--
-- MATHFUN
-- 892525
-- 

--How it works
--Demos:
    -- Type 'demo' followed by the demo number, for example 'demo 1'
--UI:
    --Type 'main' to start the UI functionality
    --Follow instructions on the menu
    
--Functionality Completed:
--  [x]  i
--  [x]  ii
--  [x]  iii
--  [x]  iv
--  [x]  v
--  [x]  vi
--  [x]  vii
--  []   Rainfall map
--  [x]  Demos
--  [x]  UI and File I/O

----------- IMPORTANT NOTE FOR MARKER -------------
-- The spacing for tasks 3, 5 and 6 works properly on the GUI version of haskell, but the tabs (\t) work differently in the CMD version of Haskell
-- All functionality is complete except the rainfall map

import Data.List
import Data.List.Split
import Text.Printf

--
-- Types (define Place type here)
--

type Place = (String, Float, Float, [Int])

testData :: [Place]
testData = [ 
            ("London", 51.5, -0.1, [0, 0, 5, 8, 8, 0, 0]), 
            ("Cardiff", 51.5, -3.2, [12, 8, 15, 0, 0, 0, 2]),
            ("Norwich", 25.6, 1.3, [0, 6, 5, 0, 0, 0, 3]),
            ("Birmingham", 52.5, -1.9, [0, 2, 10, 7, 8, 2, 2]),
            ("Liverpool", 53.4, -3.0, [8, 16, 20, 3, 4, 9, 2]),
            ("Hull", 53.8, -0.3, [0, 6, 5, 0, 0, 0, 4]),
            ("Newcastle", 55.0, -1.6, [0, 0, 8, 3, 6, 7, 5]),
            ("Belfast", 54.6, -5.9, [10, 18, 14, 0, 6, 5, 2]),
            ("Glasgow", 55.9, -4.3, [7, 5, 3, 0, 6, 5, 0]),
            ("Plymouth", 50.4, -4.1, [4, 9, 0, 0, 0, 6, 5]),
            ("Aberdeen", 57.1, -2.1, [0, 0, 6, 5, 8, 2, 0]),
            ("Stornoway", 58.2, -6.4, [15, 6, 15, 0, 0, 4, 2]),
            ("Lerwick", 60.2, -1.1, [8, 10, 5, 5, 0, 0, 3]),
            ("St Helier", 49.2, -2.1, [0, 0, 0, 0, 6, 10, 0])
            ]
            
--
--  Your functional code goes here
--

-- Get functions that are commonly used
getPlaceName :: Place -> String
getPlaceName (name, _, _, _) = name

getPlaceCoordsNorth :: Place -> Float
getPlaceCoordsNorth (_, coords, _, _) = coords

getPlaceCoordsEast :: Place -> Float
getPlaceCoordsEast (_, _, coords, _) = coords

getPlaceRainfall :: Place -> [Int]
getPlaceRainfall (_, _, _, rainfall) = rainfall



-- I
-- Return a list of names of all the places as a list of Strings
taskI :: [Place] -> [String]
taskI placeList = [getPlaceName x | x <- placeList]



-- II
-- Works out the mean average of the rainfall list given a place name as a String
taskII :: [Place] -> String -> Float
taskII placeList placeName = fromIntegral (sum rainfallList) / fromIntegral (length rainfallList)
    where
        foundPlace = getPlaceByName placeName placeList
        rainfallList = getPlaceRainfall foundPlace

-- Will find a Place from the list of Places given a Place name
getPlaceByName :: String -> [Place] -> Place
getPlaceByName _ [] = error "No Place with that name"
getPlaceByName placeName (x:xs)
    | placeName == getPlaceName x = x
    | otherwise = getPlaceByName placeName xs

    
    
-- III
-- Creates a single String the presents the name and rainfall values of every Place from a list of Places 
taskIII :: [Place] -> String
taskIII [] = ""
taskIII (x:xs) = nameSpace ++ "\t " ++ (formatRainList rainfallList) ++ "\n" ++ taskIII xs
    where
        rainfallList = getPlaceRainfall x
        maxLength = maximum [length (getPlaceName p) | p <- (x:xs)]
        nameSpace =  nameSpacing (getPlaceName x) maxLength

-- Formats the list of rainfall into a string, adding spaces to line up the numbers 
formatRainList :: [Int] -> String
formatRainList [] = ""
formatRainList (x:xs)
    | x < 10 = " |  " ++ show x ++ formatRainList xs
    | otherwise = " | " ++ show x ++ formatRainList xs

-- Adds spaces to the end of the place name depending on the length of the name, so that there is equal space after the names
nameSpacing :: String -> Int -> String
nameSpacing name maxLength
    | length name < maxLength = (nameSpacing name (maxLength - 1)) ++ " "
    | otherwise = name

    

-- IV
-- Cycles through a list of Places and checks the rainfall value of a given day, returning a String of all Places that have a 0 on the given day
taskIV :: Int -> [Place] -> [String]
taskIV _ [] = []    
taskIV dayToCheck (x:xs)
    | (rainfallIndexFor0 dayToCheck x) == True = (getPlaceName x) : (taskIV dayToCheck xs)
    | otherwise = (taskIV dayToCheck xs)

-- When given a index number and a Place, will check the rainfall values to see if there is a 0 in that index, return a Bool to reflect if that is True
rainfallIndexFor0 :: Int -> Place -> Bool
rainfallIndexFor0 index placeCheck
    | getPlaceRainfall (placeCheck) !! (index - 1) == 0 = True
    | otherwise = False

    
    
-- V
-- Will create a list of new Places given their new updated rainfall list
taskV :: [Place] -> [Int] -> [Place]
taskV [] _ = []
taskV (x:xs) rainList = createPlace oldName oldNorth oldEast newRain : taskV xs (tail rainList)
    where
        oldName = getPlaceName x
        oldNorth = getPlaceCoordsNorth x
        oldEast = getPlaceCoordsEast x
        newRain = updateRainfallList (rainList !! 0) (getPlaceRainfall x)

-- Will create a new Place, given all the data
createPlace :: String -> Float -> Float -> [Int] -> Place
createPlace name coordsNorth coordsEast rainList = (name, coordsNorth, coordsEast, rainList)

-- Will add the new Int to the front of the list, whilst removing the last value of the list
updateRainfallList :: Int -> [Int] -> [Int]
updateRainfallList newRain oldList = newRain : init oldList 
   

   
-- VI
-- Will replace a Place given the name, with a new Place and return the new updated Place list
taskVI :: [Place] -> Place -> String -> [Place]
taskVI placeList newPlace oldPlaceName = replaceNth indexPos newPlace placeList
    where
        indexPos = findPlaceIndex oldPlaceName 0 placeList

-- Will find the index of a Place in a Place list given the name of the Place
findPlaceIndex :: String -> Int -> [Place] -> Int
findPlaceIndex nameCheck indexPos (x:xs)
    | nameCheck == placeName = indexPos
    | otherwise = findPlaceIndex nameCheck (indexPos + 1) xs
    where
        placeName = getPlaceName x

-- Will split the original Place list into two depending on the index value, whilst deleting the Place in that index
-- and replacing it with the new Place, then will recombine the list to return a single list
replaceNth :: Int -> Place -> [Place] -> [Place]
replaceNth _ _ [] = []
replaceNth indexPos newPlace (x:xs)
    | indexPos == 0 = newPlace : xs
    | otherwise = x : replaceNth (indexPos - 1) newPlace xs

    
    
-- VII
-- Given coordinates, works out the closest place that was dry yesterday and returns the Place that was closest
taskVII :: [Place] -> Float -> Float -> Place
taskVII placeList x1 y1 = shortestDistance x1 y1 (listOf0RainYesterday placeList)

-- Works out the distance between two points, given 4 Floats, returning the distance as a Float
pythagorasDistance :: Float -> Float -> Float -> Float -> Float
pythagorasDistance x1 y1 x2 y2 = sqrt ((diffX) + (diffY))
    where
        diffX = ((x2) - (x1)) * ((x2) - (x1))
        diffY = ((y2) - (y1)) * ((y2) - (y1))

-- Cycles through a list of Places and finds the Place with the shortest distance from the (x1, y1) point
shortestDistance :: Float -> Float -> [Place] -> Place
shortestDistance _ _ [x] = x
shortestDistance x1 y1 (x:xs) = compPlaces x1 y1 x (shortestDistance x1 y1 xs)

-- Compares two distances, returning the Place with the shortest distance to the (x1, y1) point
compPlaces :: Float -> Float -> Place -> Place -> Place
compPlaces x1 y1 place1 place2
    | dist1 <= dist2 = place1
    | dist1 == 0 = place1
    | dist2 == 0 = place2
    | otherwise = place2
    where
        dist1 = pythagorasDistance x1 y1 (getPlaceCoordsNorth place1) (getPlaceCoordsEast place1)
        dist2 = pythagorasDistance x1 y1 (getPlaceCoordsNorth place2) (getPlaceCoordsEast place2)

-- Returns a list of Places that had no rain yesterday 
listOf0RainYesterday :: [Place] -> [Place]
listOf0RainYesterday [] = []
listOf0RainYesterday (x:xs)
    | rainfallIndexFor0 1 x = x : listOf0RainYesterday xs
    | otherwise = listOf0RainYesterday xs

    
    
--
--  Demo
--

demo :: Int -> IO ()
demo 1 = putStrLn (intercalate ", " (taskI testData) ++ "\n")
demo 2 = putStrLn (show (taskII testData "Cardiff") ++ "\n") 
demo 3 = putStrLn (taskIII testData) 
demo 4 = putStrLn (intercalate ", " (taskIV 2 testData) ++ "\n") 
demo 5 = putStrLn (formatPlaceInfoString (taskV testData [0,8,0,0,5,0,0,3,4,2,0,8,0,0]))
demo 6 = putStrLn (formatPlaceInfoString (taskVI testData ("Portsmouth", 50.8, -1.1, [0, 0, 3, 2, 5, 2, 1]) "Plymouth"))
demo 7 = putStrLn (placeToString (taskVII testData (50.9) (-1.3)) ++ "\n")
--demo 8 = taskVIII testData

-- Exactly the same as task III but includes the coords of the place
formatPlaceInfoString :: [Place] -> String
formatPlaceInfoString [] = ""
formatPlaceInfoString (x:xs) = nameSpace ++ "\t " ++ northCoords ++ eastCoords ++ (formatRainList rainfallList) ++ "\n" ++ formatPlaceInfoString xs
    where
        maxLength = maximum [length (getPlaceName p) | p <- (x:xs)]
        nameSpace =  nameSpacing (getPlaceName x) maxLength
        rainfallList = getPlaceRainfall x
        northCoords = printf " %.2f\t " (getPlaceCoordsNorth x)
        eastCoords = printf " %.2f\t " (getPlaceCoordsEast x)

        
--
-- Screen Utilities (use these to do the rainfall map - note that these do 
-- not work in WinGHCi on Windows, so use GHCi.)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text
 

--
-- Your rainfall map code goes here
--

-- NOT DONE
  
--
-- Your user interface (and loading/saving) code goes here
--
main = do
    contents <- readFile "places.txt"
    let placeList = read contents :: [Place]
    runProg placeList
    
runProg :: [Place] -> IO ()
runProg placeList = do
        putStrLn "\n\n0 - Save and exit\n1 - Return a list of all place names\n2 - Give the average rainfall of a given place\n3 - Return a formatted view of rainfall for each place\n4 - Return all places that were dry a given amount of days ago\n5 - Update the most recent rainfall data of all places\n6 - Replace a location with a new given place\n7 - Return the place that is closest to given coordinates, that was totally dry yesterday\nEnter a task number (0-7):"
        menuNumber <- getInt
        if (menuNumber == 0) then do
                let finalForm = show placeList
                writeFile "places.txt" finalForm
        else if (menuNumber == 1) then do
                putStrLn (intercalate ", " (taskI placeList) ++ "\n")
                runProg placeList
        else if (menuNumber == 2) then do
                putStrLn "Enter the name of place: "
                placeName <- getLine
                if isStringInList placeName (taskI placeList)
                    then do
                        printf "The average rainfall is: %.2f\n" (taskII placeList placeName)
                        runProg placeList
                    else do
                        putStrLn ("\n" ++ placeName ++ " is not a place in the current place list\n")
                        runProg placeList
        else if (menuNumber == 3) then do
                putStrLn ("\n" ++ taskIII placeList)
                runProg placeList
        else if (menuNumber == 4) then do
                putStrLn "Enter the number of days ago to check"
                daysAgo <- getInt
                if daysAgo < 7 then do
                    putStrLn (intercalate ", " (taskIV daysAgo placeList) ++ "\n")
                    runProg placeList
                else do 
                    putStrLn "Please enter a day between 1-7, as the number you entered was not"
                    runProg placeList
        else if (menuNumber == 5) then do
                putStrLn "Enter the rainfall values for each of the places in the format '1 2 3 4 5 6' and so on\nThere are 14 places, so please enter 14 rainfall values\nThe places are in the order:"
                putStrLn (intercalate ", " (taskI placeList) ++ "\n")
                rainListString <- getLine
                let rainList = map read $ words rainListString :: [Int]
                if length rainList == 14
                    then do
                        putStrLn (formatPlaceInfoString (taskV placeList rainList))
                        runProg placeList
                    else do
                        putStrLn ("This is the wrong amount rainfall values for the amount of places. You entered " ++ show (length rainList) ++ " number(s), however there are 14 places\n")
                        runProg placeList
        else if (menuNumber == 6) then do
                putStrLn "Enter the place name to replace"
                placeName <- getLine
                if isStringInList placeName (taskI placeList)
                    then do
                        putStrLn "Enter the name of the new place"
                        newName <- getLine
                        putStrLn "\nEnter the north coordinates of the new place"
                        newNorth <- getFloat
                        putStrLn "\nEnter the east coordinates of the new place"
                        newEast <- getFloat
                        putStrLn "\nEnter the rainfall list in the format 1 2 3 4 5 6 7"
                        rainListString <- getLine
                        let newRainList = map read $ words rainListString :: [Int]
                        if length newRainList == 7
                            then do
                                let newPlace = createPlace newName newNorth newEast newRainList
                                let newPlaceList = taskVI placeList newPlace placeName
                                putStrLn (formatPlaceInfoString newPlaceList)
                                runProg newPlaceList
                            else do
                                putStrLn "Make sure that there are 7 rainfall figures"
                                runProg placeList
                    else do
                        putStrLn "The place name given does not exist"
                        runProg placeList
        else if (menuNumber == 7) then do
                putStrLn "Enter the North coordinates"
                north <- getFloat
                putStrLn "Enter the East coordinates"
                east <- getFloat
                putStrLn ("\nThe closest place to " ++ show north ++ " (N), " ++ show east ++ " (E) is:")
                putStrLn (placeToString (taskVII placeList north east))
                runProg placeList
        else do
                putStrLn "\nPlease enter a number from the menu"
                runProg placeList

                
-- FUNCTIONS ONLY USED BY UI / DEMO

-- Returns all information about a Place into a String
placeToString :: Place -> String
placeToString place = (name ++ ", " ++ show coord1 ++ ", " ++ show coord2 ++ ", " ++ " [" ++ (intercalate ", " (map show rainfallList)) ++ "]")
    where
        name = getPlaceName place
        coord1 = getPlaceCoordsNorth place
        coord2 = getPlaceCoordsEast place
        rainfallList = getPlaceRainfall place

-- Returns Bool value based on where a String is within a list of Strings
isStringInList :: String -> [String] -> Bool
isStringInList _ [] = False
isStringInList placeName (x:xs)
    | placeName == x = True
    | otherwise = isStringInList placeName xs

-- Gets a user input and converts it to a Int
getInt :: IO Int
getInt = do
    str <- getLine
    return (read str :: Int)
    
-- Gets a user input and converts it to a Float
getFloat :: IO Float
getFloat = do
    str <- getLine
    return (read str :: Float)