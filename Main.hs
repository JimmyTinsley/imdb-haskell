module Main where

import DownloadHTML
import ParseInformation
import MovieDB
import System.Environment

main = do args <- getArgs
          case args of
             ["createDB"] -> storeInfo
             ["DisplayMovies"]   -> getAll
             ["DisplayActorMovies", name] -> getActorMovies name
             ["DisplayMovieActors", title] -> getMovieActors title
             ["DisplayActorsMovie", name1, name2] -> getActorsMovie name1 name2
             ["DisplayTopActors", limit] -> getHighestActors limit2
			 where limit2 = read limit::Integer
             ["DisplayActorCount", name] -> getActorCount name
             ["DisplayDirectorMovies", name] -> getDirectorMovies name
             ["DisplayMovieDirector", movie] -> getMovieDirector movie
             ["DisplayRatingMovies", rating] -> getRatingMovies rating2
			 where rating2 = read rating::Double
             ["DisplayMovieRating", movie] -> getMovieRating movie
             ["DisplayRankMovie", rank] -> getRankMovie rank2
			 where rank2 = read rank::Integer
             ["DisplayMovieRank", movie] -> getMovieRank movie
             ["DisplayYearMovies", year] -> getYearMovies year2
			 where year2 = read year::Integer
             ["DisplayMovieYear", movie] -> getMovieYear movie
             _ -> syntaxError

syntaxError = putStrLn
  "Usage: IMDB App command [args]\n\
  \\n\
  \createDB                              Create database movieDB.db\n\
  \DisplayMovies                         Shows all the movies from the database\n\
  \DisplayActorMovies actor              Shows all the movies of a given actor\n\
  \DisplayMovieActors movie              Shows actors of a given movie\n\
  \DisplayActorsMovie actor1 actor2      Shows movie of given two actors\n\
  \DisplayTopActors limit                Shows top actors appearing on the list with a given limit\n\
  \DisplayActorCount name                Shows the number of times an actor appearing on the list\n\
  \DisplayDirectorMovies name            Shows all the movies of a given director\n\
  \DisplayMovieDirector name             Shows the director name of a given movie\n\
  \DisplayRatingMovies rating            Shows all the movies with a given rating\n\
  \DisplayMovieRating movie              Shows the rating of a given movie\n\
  \DisplayRankMovie rank                 Shows the movie of a given rank\n\
  \DisplayMovieRank movie                Shows the rank of a given movie\n\
  \DisplayYearMovies year                Shows the movies of a given year\n\
  \DisplayMovieYear movie                Shows the year of a given movie\n"
