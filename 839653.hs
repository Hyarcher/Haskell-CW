-- Haskell CW 2019
-- UP839653

import Text.Printf
import Data.List


-- Type Definitions
type Title = String
type Artist = String
type Year = Int
type Sales = Int
type Album = (Title, Artist, Year, Sales)

-- Define Album type here

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




--
--
--  FUNCTIONS FOR DEMOS
--
--

getAlbums :: [Album]
getAlbums = testData


-- i (too similar to Jai and Leo)
-- All the albums are returned in a single string.
albumsToString :: [Album] -> String
albumsToString [] = ""
albumsToString ((title, artist, releaseYear, sales):xs) = "Title: " ++ title ++ "     " ++ "Artist: " ++ artist ++ "     " ++ "Release year: " ++ show(releaseYear) ++ "     " ++ "Sales: " ++ show(sales) ++ "\n" ++ albumsToString xs

-- ii (too similar to Jai)
top10 :: [Album] -> [Album]
top10 tenAlbums = take 10 getAlbums

-- iii (too similar to Jai)
albumsBetweenYears :: Int -> Int -> [Album] -> [Album]
albumsBetweenYears yearX yearY testData = filter (\(_,_,year,_) -> year >= yearX && year <= yearY) getAlbums

-- iv (too similar to Jai)
prefixAlbums :: String -> [Album] -> [Album]
prefixAlbums prefix testData = filter (\(name,_,_,_) -> isPrefixOf prefix name) getAlbums

-- v
totalArtistSales :: String -> [Album] -> String
totalArtistSales artistName testData = "This artists total sales: " ++ show(sum[sales | (name,artist,year,sales) <- getAlbums, artist == artistName])

-- vi

-- vii
removeLastAlbum :: [Album] -> [Album]
removeLastAlbum album = init album

addNewAlbum :: String -> String -> Int -> Int -> [Album]
addNewAlbum title artist year sales = insert(title,artist,year,sales) testData

-- viii
addSales :: String -> String -> Int -> [Album] -> [Album]
addSales addTitle addArtist extraSales testData = [(name,artist,year,sales + extraSales) | (name,artist,year,sales) <- getAlbums, addTitle == name && addArtist == artist]



-- Demo function to test basic functionality (without persistence - i.e.
-- testData doesn't change and nothing is saved/loaded to/from albums file).

demo :: Int -> IO ()
demo 1 = putStrLn (albumsToString testData)
demo 2 = putStrLn (albumsToString (top10 testData))
demo 3 = putStrLn (albumsToString (albumsBetweenYears 2000 2008 testData))
demo 4 = putStrLn (albumsToString (prefixAlbums "Th" testData))
demo 5 = putStrLn (totalArtistSales "Queen" testData)
--demo 6 = putStrLn (
demo 7 = putStrLn (albumsToString (removeLastAlbum (addNewAlbum "Progress" "Take That" 2010 2700000)))
demo 8 = putStrLn (albumsToString (addSales "21" "Adele" 890000 testData))


--
--
-- USER INTERFACE
--
--

main :: IO ()
main = do
  fileContent <- readFile "albums.txt"
  let albums = read fileContent :: [Album]
  putStrLn (albumsToString albums)
  optionMenu albums


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
  putStrLn("( 6 ) = Print a list showing how many times each artist is in the top 50")
  putStrLn("( 7 ) = Remove the 50th (lowest selling) album and adds a given (new) album into the list")
  putStrLn("( 8 ) = Increase the sales figure for an album given its title, artist and additional sales")
  putStrLn("( save ) = Saves changes to the file")
  putStrLn("( exit ) = Exits the program with no changes to the file")
  putStrLn("")


optionMenu :: [Album] -> IO ()
optionMenu albums = do
  menuDisplay
  putStrLn("Type your option: ")
  option <- getLine
  optionSelected option albums
  putStrLn("")


optionSelected :: String -> [Album] -> IO ()
optionSelected "1" albums = do
  putStrLn (albumsToString albums)
  optionMenu albums

optionSelected "2" albums = do
  putStrLn (albumsToString (top10 albums))
  optionMenu albums

optionSelected "3" albums = do
  yearX <- getLine
  let yearA = read yearX :: Int
  yearY <- getLine
  let yearB = read yearY :: Int
  putStrLn (albumsToString (albumsBetweenYears yearA yearB albums))

optionSelected "4" albums = do
  prefix <- getLine
  putStrLn (albumsToString (prefixAlbums prefix albums))

optionSelected "5" albums = do
  artistName <- getLine
  putStrLn (totalArtistSales artistName albums)

optionSelected "6" albums = do
  menuDisplay

optionSelected "7" albums = do
  newAlbumName <- getLine
  newArtist <- getLine
  year <- getLine
  let newYear = read year :: Int
  sales <- getLine
  let newSales = read sales :: Int
  putStrLn (albumsToString (removeLastAlbum (addNewAlbum newAlbumName newArtist newYear newSales)))
  menuDisplay

optionSelected "8" albums = do
  albumName <- getLine
  artist <- getLine
  sales <- getLine
  let salesIncrease = read sales :: Int
  putStrLn (albumsToString (addSales albumName artist salesIncrease albums))
  menuDisplay

--optionSelected "save"

optionSelected "exit" albums = do
  putStrLn("Exited the program, file was not saved")
  
optionSelected _ albums = do
  menuDisplay




















--
