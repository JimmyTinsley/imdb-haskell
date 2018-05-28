module ParseInformation where

import System.IO
import Data.List as D
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as S
import DownloadHTML
-- | Movie is the data type we create for this program, it contains rank, title, director name, year, rating, first actor name, and second actor name.
-- And mrank mtitle mdir myear mrating mactor1 mactor2 are also functions to create a Movie data type 
data Movie = Movie {mrank :: Int, mtitle :: String, mdir :: String, myear :: Int, mrating :: Double, mactor1 :: String, mactor2 :: String} deriving (Eq, Show)
-- | isRank is to check whether a line contains rank information
isRank s = (take 31 s) == "    <span name=\"rk\" data-value="
-- | This function is to convert a list of number in String format into a list of number in Integer format using read function
str_to_Int :: [String] -> [Int]
str_to_Int = map read
-- | This function is to convert a list of number in String format into a list of double in Double format using read function
str_to_Double :: [String] -> [Double]
str_to_Double = map read


-- | listBreak generalises break recursively. It takes a predicate and a list and breaks 
-- the list where the elements satisfy the predicates   
listBreak :: (a -> Bool) -> [a] -> [[a]]
listBreak pred [] = []
listBreak pred [x] = [[x]] 
listBreak pred (x0:x1:xs) = 
  if pred x1 then [x0]:(listBreak pred (x1:xs))
  else let (l1:ls) = listBreak pred (x1:xs) in ((x0:l1):ls)

-- | This function converts the information from imdb.html and store them into the Movie data type we create
getAllmovie data0 = allmovie
   where
    data1 = S.unpack data0
    all_lines = lines data1
    rank_lines = filter isRank all_lines
    movie_section = tail $ (listBreak isRank all_lines)
    title_lines = map head $ map (drop 11) movie_section
    year_lines = map head $ map (drop 12) movie_section
    rating_lines = map head $ map (drop 15) movie_section
    rank = map (takeWhile (/= '"')) $ map (drop 32) rank_lines
    dir_name = map tail $ map (reverse . dropWhile (== ' ') . reverse . takeWhile (/= '(')) $ map (drop 6) title_lines
    actor_name1 = map (takeWhile (/= ',')) $ map (drop 2) $ map (dropWhile (/= ',')) title_lines
    actor_name2 = map (takeWhile (/= '"')) $ map (drop 2) $ map (dropWhile (/= ',')) $ map (drop 1) $ map (dropWhile (/= ',')) title_lines
    allactor = (++) [actor_name1] [actor_name2]
    movie_title = map (takeWhile (/= '<')) $ map tail $ map (dropWhile (/= '>')) title_lines
    year = map init $ map (takeWhile (/= '<')) $ map (drop 2) $ map (dropWhile (/= '>')) year_lines
    rating = map (takeWhile (/= '<')) $ map tail $ map (dropWhile (/= '>')) rating_lines
    rank_Int = str_to_Int rank
    year_Int = str_to_Int year
    rating_Double = str_to_Double rating
    newmovie a = ((((((((Movie $ rank_Int!!(a-1)) $ movie_title!!(a-1)) $ dir_name!!(a-1)) $ year_Int!!(a-1)) $ rating_Double!!(a-1)) $ actor_name1!!(a-1)) $ actor_name2!!(a-1)))
    allmovie = map newmovie rank_Int

-- | This function is to run the download HTML and read imdb.html in ByteString
main2 :: IO [Movie]
main2 = do
    downloadHTML "imdb.html" "https://www.imdb.com/chart/top"
    data0 <- B.readFile "imdb.html"
    let allmovie1 = getAllmovie data0
    return allmovie1
    --print . mrank allmovie1	


 
    