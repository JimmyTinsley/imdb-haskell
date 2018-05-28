module MovieDB where

import DownloadHTML
import ParseInformation

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List
 
storeInfo = do  
   createDB
   storeMovies
   storeActors
   storeActorsMovies
   print "Connection Successful"
-- | This function is to create the database for the program
createDB :: IO Connection 
createDB = do 
   conn <- connectSqlite3 "movieDB.db"
   --runRaw conn "pragma foreign_keys = on"
   run conn "CREATE TABLE movies (mId INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, rank INTEGER NOT NULL UNIQUE , title VARCHAR(80) NOT NULL , year INTEGER NOT NULL , rating REAL NOT NULL,dfName VARCHAR(40) NOT NULL, dlName VARCHAR(40) NOT NULL)" []  
   run conn "CREATE TABLE actors (aId INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, afName VARCHAR(40) NOT NULL, alName VARCHAR(40) NOT NULL)" []
   run conn "CREATE TABLE movies_actors (m_Id INTEGER, a_Id INTEGER, FOREIGN KEY(m_Id) REFERENCES movies(mId), FOREIGN KEY(a_Id) REFERENCES actors(aId) )" []
   commit conn
   --disconnect conn
   return conn  
-- | This function is to store the movie name information downloaded from IMDB website into a table
storeMovies :: IO ()
storeMovies = do
     data2 <- main2
     conn <- connectSqlite3 "movieDB.db"
     --runRaw conn "pragma foreign_keys = on"
     let storeRank = mrank 
     let storeTitle = mtitle 
     let storeYear = myear 
     let storeRating = mrating 
     let storedirFName = takeWhile (/= ' ') . mdir 
     let storedirLName = reverse . takeWhile (/= ' ') . reverse . mdir 
     stmt <- prepare conn "INSERT OR IGNORE INTO movies(rank,title,year,rating,dfName,dlName) VALUES (?,?,?,?,?,?)"
     let storeAllMovies a = [toSql (storeRank $ data2 !! (a-1)), toSql (storeTitle $ data2 !! (a-1)), toSql (storeYear $ data2 !!(a-1)), toSql (storeRating $ data2 !!(a-1)),toSql (storedirFName $ data2 !!(a-1)), toSql (storedirLName $ data2 !!(a-1))]
     let inc = map storeAllMovies [1..250]
     executeMany stmt inc 
     commit conn
-- | This function is to store the actor name information downloaded from IMDB website
storeActors :: IO ()
storeActors = do 
     data2 <- main2
     conn <- connectSqlite3 "movieDB.db"
     let aofName = takeWhile (/= ' ') . mactor1
     let aolName = reverse .  takeWhile (/= ' ') . reverse . mactor1
     let atfName = takeWhile (/= ' ') . mactor2
     let atlName = reverse .  takeWhile (/= ' ') . reverse . mactor2
     stmt <- prepare conn "INSERT OR IGNORE INTO actors (afName, alName) VALUES (?,?)"
     let storeAllActors1 a = [toSql (aofName $ data2!!(a-1)), toSql (aolName $ data2!!(a-1))]
     let storeAllActors2 a = [toSql (atfName $ data2!!(a-1)), toSql (atlName $ data2!!(a-1))]
     let inc = map storeAllActors1 [1..250]
     let inc2 = map storeAllActors2 [1..250] 
     executeMany stmt inc
     executeMany stmt inc2
     commit conn
-- | This function is to create the relationship table for actors and movies
storeActorsMovies :: IO ()
storeActorsMovies = do 
     data2 <- main2
     conn <- connectSqlite3 "movieDB.db"
     stmt <- prepare conn "INSERT OR IGNORE INTO movies_actors (m_id, a_id) VALUES (?,?)"
     let storeAllActorsMovies1 a = [toSql ((a-1)::Int), toSql ((a-1)::Int) ]
     let storeAllActorsMovies2 a = [toSql ((a-251)::Int), toSql ((a-1)::Int) ]
     let inc = map storeAllActorsMovies1 [2..251]
     let inc2 = map storeAllActorsMovies2 [252..501] 
     executeMany stmt inc
     executeMany stmt inc2
     commit conn
-- | Get and display the Movies table
getTableMovies = do
    conn <- connectSqlite3 "movieDB.db"
    query <- quickQuery' conn "SELECT * FROM movies" []
    print query
    disconnect conn
-- | Get and display the Actors table
getTableActors = do
    conn <- connectSqlite3 "movieDB.db"
    query <- quickQuery' conn "SELECT * FROM actors" []
    print query
    disconnect conn
-- | Get and display the ActorMovies table
getTableActorsMovies = do
    conn <- connectSqlite3 "movieDB.db"
    query <- quickQuery' conn "SELECT * FROM movies_actors" []
    print query
    disconnect conn
-- | Display all the movie information downloaded from IMDB TOP 250 website
getAll = do
    conn <- connectSqlite3 "movieDB.db"
    query <- quickQuery' conn "SELECT rank,title,year,rating,(dfName ||' '|| dlName),GROUP_CONCAT(DISTINCT (afName ||' '|| alName)) FROM movies,actors,movies_actors WHERE movies.mId = movies_actors.m_Id AND actors.aId = movies_actors.a_Id GROUP BY movies.mId" []
    print query
    disconnect conn
-- | Display movies based on given actor name
getActorMovies name = do          
   conn <- connectSqlite3 "movieDB.db"
   query <- quickQuery' conn "SELECT title FROM movies,actors,movies_actors WHERE movies.mId = movies_actors.m_Id AND actors.aId = movies_actors.a_Id AND actors.afName = ? AND actors.alName = ? " [toSql ((takeWhile (/= ' ') $ name)::String), toSql ((reverse . takeWhile (/= ' ') $ reverse $ name)::String)]
   print query
   disconnect conn
-- | Displays two actors of the given title
getMovieActors title = do          
   conn <- connectSqlite3 "movieDB.db"
   query <- quickQuery' conn "SELECT GROUP_CONCAT(DISTINCT (afName ||' '|| alName)) AS aName FROM actors,movies,movies_actors WHERE movies.mId = movies_actors.m_Id AND actors.aId = movies_actors.a_Id AND title = ? GROUP BY movies.mId" [toSql (title::String)]
   print query
   disconnect conn
-- | Displays the movie name based on two given actor names
getActorsMovie name1 name2 = do       
   conn <- connectSqlite3 "movieDB.db"
   query <- quickQuery' conn "SELECT title FROM movies,actors,movies_actors WHERE movies.mId = movies_actors.m_Id AND actors.aId = movies_actors.a_Id AND actors.afName IN (? , ?) AND actors.alName IN (? , ?) GROUP BY movies.title HAVING COUNT(*) = 2" [toSql ((takeWhile (/= ' ') $ name1)::String), toSql ((takeWhile (/= ' ') $ name2)::String), toSql ((reverse . takeWhile (/= ' ') $ reverse $ name1)::String), toSql ((reverse . takeWhile (/= ' ') $ reverse $ name2)::String)]
   print query
   disconnect conn
-- | Displays top 5 actors who appears the most on the list
getHighestActors limit = do                 
   conn <- connectSqlite3 "movieDB.db"
   query <- quickQuery' conn "SELECT (afName ||' '|| alName) AS aName, COUNT ((afName ||' '|| alName)) FROM movies,actors,movies_actors WHERE movies.mId = movies_actors.m_Id AND actors.aId = movies_actors.a_Id GROUP BY aName HAVING COUNT((afName ||' '|| alName)) > 0 ORDER BY COUNT ((afName ||' '|| alName)) DESC LIMIT ?" [toSql (limit::Integer)]
   print query
   disconnect conn
-- | Displays number of times actor is appearing on the list 
getActorCount name = do               
   conn <- connectSqlite3 "movieDB.db"
   query <- quickQuery' conn "SELECT COUNT (*) FROM movies,actors,movies_actors WHERE movies.mId = movies_actors.m_Id AND actors.aId = movies_actors.a_Id AND actors.afName = ? AND actors.alName = ? " [toSql ((takeWhile (/= ' ') $ name)::String), toSql ((reverse . takeWhile (/= ' ') $ reverse $ name)::String)]
   print query
   disconnect conn
-- | Displays all the movies by given director
getDirectorMovies name = do          
   conn <- connectSqlite3 "movieDB.db"
   query <- quickQuery' conn "SELECT title,rank,year,rating FROM movies WHERE dfName = ? AND dlName = ? " [toSql ((takeWhile (/= ' ') $ name)::String), toSql ((reverse . takeWhile (/= ' ') $ reverse $ name)::String)]
   print query
   disconnect conn
-- | Display director name based on movie name
getMovieDirector movie = do          
   conn <- connectSqlite3 "movieDB.db"
   query <- quickQuery' conn "SELECT (dfName ||' '|| dlName) FROM movies WHERE title = ? " [toSql (movie::String)]
   print query
   disconnect conn
-- | Displays all the movies by given rating
getRatingMovies rating = do          
   conn <- connectSqlite3 "movieDB.db"
   query <- quickQuery' conn "SELECT title FROM movies WHERE rating = ? " [toSql (rating::Double)]
   print query
   disconnect conn
-- | Displays rating based on given movie
getMovieRating movie = do           
   conn <- connectSqlite3 "movieDB.db"
   query <- quickQuery' conn "SELECT rating FROM movies WHERE title = ? " [toSql (movie::String)]
   print query
   disconnect conn
-- | Displays movie based on given rank
getRankMovie rank = do              
   conn <- connectSqlite3 "movieDB.db"
   query <- quickQuery' conn "SELECT title FROM movies WHERE rank = ? " [toSql (rank::Integer)]
   print query
   disconnect conn
-- | Displays rank based on given movie
getMovieRank movie = do            
   conn <- connectSqlite3 "movieDB.db"
   query <- quickQuery' conn "SELECT rank FROM movies WHERE title = ? " [toSql (movie::String)]
   print query
   disconnect conn
-- | Displays movies based on given year
getYearMovies year = do           
   conn <- connectSqlite3 "movieDB.db"
   query <- quickQuery' conn "SELECT title FROM movies WHERE year = ? " [toSql (year::Integer)]
   print query
   disconnect conn
-- | Displays year based on given movie
getMovieYear movie = do          
   conn <- connectSqlite3 "movieDB.db"
   query <- quickQuery' conn "SELECT year FROM movies WHERE title = ? " [toSql (movie::String)]
   print query
   disconnect conn

