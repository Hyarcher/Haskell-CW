-- ############################ --
--                              --
--  HASKELL CW 2019 / UP839653  --
--                              --
-- ############################ --


-- Imported modules.
import Text.Printf
import Data.List
import Data.Char


-- ######################### --
--                           --
--  DATA TYPES AND DATABASE  --
--                           --
-- ######################### --


-- Type Definitions.
type Title = String
type Artist = String
type Year = Int
type Sales = Int
type Album = (Title, Artist, Year, Sales)

-- A list of tuples defining the Album type.
testData :: [Album]
testData = [("Greatest Hits", "Queen", 1981, 6300000),
            ("Gold: Greatest Hits", "ABBA", 1992, 5400000),
            ("Sgt. Pepper's Lonely Hearts Club Band", "The Beatles", 1967, 5340000),
            ("21", "Adele", 2011, 5110000),
            ("(What's the Story) Morning Glory?", "Oasis", 1995, 4940000),
            ("Thriller", "Michael Jackson", 1982, 4470000),
            ("The Dark Side of the Moon", "Pink Floyd", 1973, 4470000),
            ("Brothers in Arms", "Dire Straits", 1985, 4350000),
            ("Bad", "Michael Jackson", 1987, 4140000),
            ("Rumours", "Fleetwood Mac", 1977, 4090000),
            ("Greatest Hits II", "Queen", 1991, 3990000),
            ("Back to Black", "Amy Winehouse", 2006, 3940000),
            ("The Immaculate Collection", "Madonna", 1990, 3700000),
            ("25", "Adele", 2015, 3500000),
            ("Stars", "Simply Red", 1991, 3450000),
            ("Come On Over", "Shania Twain", 1998, 3430000),
            ("x", "Ed Sheeran", 2014, 3380000),
            ("Legend", "Bob Marley", 1984, 3380000),
            ("Bat Out of Hell", "Meat Loaf", 1977, 3370000),
            ("Back to Bedlam", "James Blunt", 2004, 3360000),
            ("Urban Hymns", "The Verve", 1997, 3340000),
            ("Bridge over Troubled Water", "Simon & Garfunkel", 1970, 3260000),
            ("1", "The Beatles", 2000, 3230000),
            ("Spirit", "Leona Lewis", 2007, 3170000),
            ("Crazy Love", "Michael Bublé", 2009, 3130000),
            ("No Angel", "Dido", 2000, 3090000),
            ("White Ladder", "David Gray", 1998, 3020000),
            ("The Fame", "Lady Gaga", 2009, 2990000),
            ("Only by the Night", "Kings of Leon", 2008, 2980000),
            ("A Rush of Blood to the Head", "Coldplay", 2002, 2960000),
            ("Talk on Corners", "The Corrs", 1997, 2960000),
            ("Spice", "Spice Girls", 1996, 2960000),
            ("Life for Rent", "Dido", 2003, 2900000),
            ("Beautiful World", "Take That", 2006, 2880000),
            ("The Joshua Tree", "U2", 1987, 2880000),
            ("Hopes and Fears", "Keane", 2004, 2860000),
            ("The War of the Worlds", "Jeff Wayne", 1978, 2800000),
            ("X&Y", "Coldplay", 2005, 2790000),
            ("Jagged Little Pill", "Alanis Morissette", 1995, 2780000),
            ("Tubular Bells", "Mike Oldfield", 1973, 2760000),
            ("Scissor Sisters", "Scissor Sisters", 2004, 2760000),
            ("...But Seriously", "Phil Collins", 1989, 2750000),
            ("Tracy Chapman", "Tracy Chapman", 1988, 2710000),
            ("Parachutes", "Coldplay", 2000, 2710000),
            ("The Man Who", "Travis", 1999, 2687500),
            ("Greatest Hits", "ABBA", 1975, 2606000),
            ("I've Been Expecting You", "Robbie Williams", 1998, 2586500),
            ("Come Away with Me", "Norah Jones", 2002, 2556650),
            ("Graceland", "Paul Simon", 1986, 2500000),
            ("Ladies & Gentlemen: The Best of", "George Michael", 1998, 2500000)]



-- ##################### --
--                       --
--   Main functionality  --
--                       --
-- ##################### --


getAlbums :: [Album]
getAlbums = testData


-- i
-- All the albums are returned in a single string. 'unlines' allows each tuple to start of a different line.
-- 'map' returns the list of tuples applying the 'unlines' function to all tuples.
albumsToString :: [Album] -> String
albumsToString testData = unlines(map printAlbums testData)

-- Prints the tuples from the list with labels in front. This function is parsed into 'unlines'.
printAlbums :: (String, String, Int, Int) -> String
printAlbums (title, artist, year, sales) = "Title: " ++ title ++ ", Artist: " ++ artist ++ ", Year: " ++ show(year) ++ ", Sales: " ++show(sales)

-- ii
-- The top ten albums are printed out. 'take' takes the top ten tuples from the list.
top10 :: [Album] -> [Album]
top10 tenAlbums = take 10 getAlbums

-- iii
-- Compares two years given by the user and checks whether 'yearX' and 'yearY' are smaller or larger
-- than year respectively. Also by using '=' we get the inclusive years inputted by the user.
albumsBetweenYears :: Int -> Int -> [Album] -> [Album]
albumsBetweenYears yearX yearY testData = [(title,artist,year,sales) | (title,artist,year,sales) <- getAlbums, yearX <= year && yearY >= year]

-- iv
-- Gets the prefix inputted by the user and uses 'isPrefixOf' to produce a list of albums with this prefix.
prefixAlbums :: String -> [Album] -> [Album]
prefixAlbums prefix testData = [(title,artist,year,sales) | (title,artist,year,sales) <- getAlbums, isPrefixOf prefix title]

-- v
-- Takes the artists name and then sum adds the sales together and prints the total sales figure.
totalArtistSales :: String -> [Album] -> String
totalArtistSales artistName testData = "This artists total sales: " ++ show(sum[sales | (name,artist,year,sales) <- getAlbums, artist == artistName])

-- vi

-- vii
-- Removes the last album from the list.
removeLastAlbum :: [Album] -> [Album]
removeLastAlbum album = init album

-- Appends a new album into the list.
addNewAlbum :: String -> String -> Int -> Int -> [Album]
addNewAlbum title artist year sales = insert(title,artist,year,sales) testData

-- viii
-- Takes the name and artist and then increases the sales based on the user's input. Then prints the
-- albums with the new increased sales.
addSales :: String -> String -> Int -> [Album] -> [Album]
addSales addTitle addArtist extraSales testData = [(name,artist,year,sales + extraSales) | (name,artist,year,sales) <- getAlbums, addTitle == name && addArtist == artist] ++ getAlbums



-- ##################### --
--                       --
--     DEMO TESTING      --
--                       --
-- ##################### --


-- Demo functions for testing the functionality of the code.
demo :: Int -> IO ()
demo 1 = putStrLn (albumsToString testData)
demo 2 = putStrLn (albumsToString (top10 testData))
demo 3 = putStrLn (albumsToString (albumsBetweenYears 2000 2008 testData))
demo 4 = putStrLn (albumsToString (prefixAlbums "Th" testData))
demo 5 = putStrLn (totalArtistSales "Queen" testData)
--demo 6 = putStrLn ()
demo 7 = putStrLn (albumsToString (removeLastAlbum (addNewAlbum "Progress" "Take That" 2010 2700000)))
demo 8 = putStrLn (albumsToString (addSales "21" "Adele" 400000 testData))



-- ##################### --
--                       --
--    USER INTERFACE     --
--                       --
-- ##################### --


-- The main function reads the file and gets the content from the file.
main :: IO ()
main = do
  fileContent <- readFile "albums.txt"
  let albums = read fileContent :: [Album]
  putStrLn (albumsToString albums)
  optionMenu albums


-- The menu that is displayed to the user with a
-- list of the options the user can choose from.
menuDisplay :: IO ()
menuDisplay = do
  putStrLn("")
  putStrLn("##   ##   #####   #   #   #   #")
  putStrLn("# # # #   #       ##  #   #   #")
  putStrLn("#  #  #   ####    # # #   #   #")
  putStrLn("#     #   #       #  ##   #   #")
  putStrLn("#     #   #####   #   #   #####")
  putStrLn("")
  putStrLn("Please type an option from the list below :)")
  putStrLn("")
  putStrLn("")
  putStrLn("( 1 ) = Print the list of Albums")
  putStrLn("( 2 ) = Print the top 10 albums in terms of sales")
  putStrLn("( 3 ) = Print all the inclusive albums between two given years")
  putStrLn("( 4 ) = Print all albums with a given prefix")
  putStrLn("( 5 ) = Print the total sales figure for a given artist")
  putStrLn("( 6 ) = (DOESN'T WORK) Print a list showing how many times each artist is in the top 50")
  putStrLn("( 7 ) = Remove the 50th (lowest selling) album and adds a given (new) album into the list")
  putStrLn("( 8 ) = Increase the sales figure for an album given its title, artist and additional sales")
  putStrLn("( save & exit ) = Saves changes to the file and exits the program")
  putStrLn("")


-- After the main menu is displayed, this function gets the input from the user with 'getLine'.
-- This input is then checked against the possible options from optionSelected.
optionMenu :: [Album] -> IO ()
optionMenu albums = do
  menuDisplay
  putStrLn("Type your option: ")
  option <- getLine
  optionSelected option albums
  putStrLn("")


-- The optionMenu parses the user's choice into this function then the corresponding choice
-- is displayed on the user interface.
optionSelected :: String -> [Album] -> IO ()
optionSelected "1" albums = do
  putStrLn (albumsToString albums)
  optionMenu albums

optionSelected "2" albums = do
  putStrLn (albumsToString (top10 albums))
  optionMenu albums

optionSelected "3" albums = do
  putStrLn("Enter the first year (input an integer): ")
  yearX <- getLine
  let yearA = read yearX :: Int
  putStrLn("Enter the second year (input an integer): ")
  yearY <- getLine
  let yearB = read yearY :: Int
-- Conditional to check if the first inputted year is larger than the second inputted year.
-- If the first year is larger then the program returns an error statement and displays the menu again.
  if (yearA > yearB)
    then do
      putStrLn("#################################")
      putStrLn("#                               #")
      putStrLn("#  The first year cannot be     #")
      putStrLn("#  larger than the second year  #")
      putStrLn("#                               #")
      putStrLn("#################################")
      optionMenu albums
    else do
      putStrLn (albumsToString (albumsBetweenYears yearA yearB albums))
      optionMenu albums

optionSelected "4" albums = do
  putStrLn("Enter a prefix of an album name (input a string): ")
  prefix <- getLine
  putStrLn (albumsToString (prefixAlbums prefix albums))
  optionMenu albums

optionSelected "5" albums = do
  putStrLn("Enter an artists' name (input a string): ")
  artistName <- getLine
  putStrLn (totalArtistSales artistName albums)
  optionMenu albums

optionSelected "6" albums = do
  optionMenu albums

optionSelected "7" albums = do
  putStrLn("Enter a new album name (input a string): ")
  newAlbumName <- getLine
  putStrLn("Enter a new artist (input a string): ")
  newArtist <- getLine
  putStrLn("Enter a new year (input an integer): ")
  year <- getLine
  let newYear = read year :: Int
  putStrLn("Enter new sales (input an integer): ")
  sales <- getLine
  let newSales = read sales :: Int
  let updatedAlbums = removeLastAlbum (addNewAlbum newAlbumName newArtist newYear newSales)
  putStrLn (albumsToString updatedAlbums)
  optionMenu updatedAlbums

optionSelected "8" albums = do
  putStrLn("Enter an album name (input a string): ")
  albumName <- getLine
  putStrLn("Enter an artist (input a string): ")
  artist <- getLine
  putStrLn("Enter the increase in sales (input an integer): ")
  sales <- getLine
  let salesIncrease = read sales :: Int
  let updatedSales = addSales albumName artist salesIncrease albums
  putStrLn (albumsToString updatedSales)
  optionMenu updatedSales

-- This option allows the user to exit the application and when exited will save the file
-- and write any new data to the 'albums.txt' file.
optionSelected "save & exit" albums = do
  writeFile "albums.txt" (show albums)
  putStrLn("#############################################################")
  putStrLn("#                                                           #")
  putStrLn("#    FILE HAS BEEN SAVED AND THE PROGRAM HAS BEEN EXITED    #")
  putStrLn("#                                                           #")
  putStrLn("# you can start the program again by entering the word main #")
  putStrLn("#                                                           #")
  putStrLn("#############################################################")

optionSelected _ albums = do
  optionMenu albums
