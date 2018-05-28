# imdb-haskell
A Haskell app that save IMDB TOP 250 information in haskell data type and sqlite3 database with some query functions.

## Compilation
Open terminal at the project's root directory, and type 'ghc -o imdb Main.hs'. Then you could see the executable file 'imdb'.

## Dependencies
Haskell package: Database.HDBC
	cabal install HDBC
Haskell package: Database.HDBC.Sqlite3
	cabal install HDBC-Sqlite3

## Usage

### Unix
	./imdb <args>

### Windows
	imdb.exe <args>

### Args options:
    createDB                             
 Create database movieDB.db
 
    DisplayMovies                         
Shows all the movies from the database

    DisplayActorMovies actor              
Shows all the movies of a given actor

    DisplayMovieActors movie              
Shows actors of a given movie

    DisplayActorsMovie actor1 actor2      
Shows movie of given two actors

    DisplayTopActors limit                
Shows top actors appearing on the list with a given limit

    DisplayActorCount name                
Shows the number of times an actor appearing on the list

    DisplayDirectorMovies name            
Shows all the movies of a given director

    DisplayMovieDirector name             
Shows the director name of a given movie

    DisplayRatingMovies rating            
Shows all the movies with a given rating

    DisplayMovieRating movie              
Shows the rating of a given movie

    DisplayRankMovie rank                 
Shows the movie of a given rank

    DisplayMovieRank movie                
Shows the rank of a given movie

    DisplayYearMovies year                
Shows the movies of a given year

    DisplayMovieYear movie                
Shows the year of a given movie
