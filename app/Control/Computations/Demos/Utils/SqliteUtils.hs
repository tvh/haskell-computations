{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Thin conveniences over "Database.SQLite3": opening/closing a database
 ('withSqliteDb', 'initSqliteDb', 'closeSqliteDb'), running a query and
 collecting its rows as name\/value pairs ('query', 'exec'), transactions
 ('withTransaction'), and retrying on @SQLITE_BUSY@ ('retryIfBusy'), which
 every other function here already does internally. Used to build a custom
 sqlite-backed 'Control.Computations.CompEngine.CompSrc.CompSrc' or
 'Control.Computations.CompEngine.CompSink.CompSink'.
-}
module Control.Computations.Demos.Utils.SqliteUtils (
  TableName,
  ColumnName,
  SQLRow,
  SQLData (..),
  Sqlite.Database,
  Sqlite.Statement,
  columnNames,
  exec,
  query,
  execStmt,
  withStatement,
  retryIfBusy,
  getColumnValue,
  initSqliteDb,
  closeSqliteDb,
  withSqliteDb,
  withTransaction,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.Clock
import Control.Computations.Utils.Logging
import Control.Computations.Utils.TimeSpan

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Exception
import Control.Monad
import Data.Hashable
import Data.LargeHashable
import qualified Data.List as L
import qualified Data.Text as T
import Database.SQLite3 (SQLData)
import qualified Database.SQLite3 as Sqlite
import qualified Database.SQLite3.Direct as Direct
import System.Random

type TableName = T.Text
type ColumnName = T.Text

type SQLRow = [(ColumnName, SQLData)]

instance Hashable SQLData
instance LargeHashable SQLData

instance Ord SQLData where
  compare d1 d2 =
    case (d1, d2) of
      -- null
      (Sqlite.SQLNull, Sqlite.SQLNull) -> EQ
      (Sqlite.SQLNull, _) -> LT
      -- int
      (Sqlite.SQLInteger _, Sqlite.SQLNull) -> GT
      (Sqlite.SQLInteger i1, Sqlite.SQLInteger i2) -> compare i1 i2
      (Sqlite.SQLInteger _, _) -> LT
      -- float
      (Sqlite.SQLFloat _, Sqlite.SQLNull) -> GT
      (Sqlite.SQLFloat _, Sqlite.SQLInteger _) -> GT
      (Sqlite.SQLFloat f1, Sqlite.SQLFloat f2) -> compare f1 f2
      (Sqlite.SQLFloat _, _) -> LT
      -- text
      (Sqlite.SQLText _, Sqlite.SQLNull) -> GT
      (Sqlite.SQLText _, Sqlite.SQLInteger _) -> GT
      (Sqlite.SQLText _, Sqlite.SQLFloat _) -> GT
      (Sqlite.SQLText t1, Sqlite.SQLText t2) -> compare t1 t2
      (Sqlite.SQLText _, Sqlite.SQLBlob _) -> LT
      -- blob
      (Sqlite.SQLBlob b1, Sqlite.SQLBlob b2) -> compare b1 b2
      (Sqlite.SQLBlob _, _) -> GT

columnNames :: Sqlite.Statement -> IO [T.Text]
columnNames stmt =
  do
    n <- Sqlite.columnCount stmt
    loop (Sqlite.ColumnIndex 0) n []
 where
  loop i n acc =
    if i >= n
      then pure (reverse acc)
      else do
        mName <- Sqlite.columnName stmt i
        case mName of
          Nothing -> fail ("columnNames: columName return Nothing")
          Just name -> loop (i + 1) n (name : acc)

collectRows :: Sqlite.Statement -> IO [SQLRow]
collectRows stmt = loop Nothing []
 where
  loop mNames acc =
    do
      stepRes <- retryIfBusy "collectRows.step" $ Sqlite.step stmt
      case stepRes of
        Sqlite.Done -> pure (reverse acc)
        Sqlite.Row -> do
          cols <- Sqlite.columns stmt
          names <-
            case mNames of
              Just x -> pure x
              Nothing -> columnNames stmt
          loop (Just names) (zip names cols : acc)

-- | Run an 'IO' action, retrying with a short random backoff (up to 50
-- attempts) if it fails with sqlite's @SQLITE_BUSY@ error; @what@ is a label
-- used only in the log message if retries are exhausted. Every other
-- database-touching function in this module already goes through this.
retryIfBusy :: String -> IO a -> IO a
retryIfBusy what action = loop 0
 where
  loop n =
    do
      res <- try action
      case res of
        Left (err :: Sqlite.SQLError)
          | Sqlite.sqlError err == Sqlite.ErrorBusy ->
              if n < maxRetries
                then do
                  sleepBeforeRetry
                  loop (n + 1)
                else do
                  logWarn (what ++ ": no retry on SQLITE_BUSY after " ++ show n ++ " attempts")
                  throwIO err
          | otherwise -> throwIO err
        Right x -> pure x
  sleepBeforeRetry =
    do
      i <- randomRIO (1 :: Int, 20)
      c_sleep realClock (milliseconds i)
  maxRetries = 50 :: Int

-- | Run a SQL statement with no parameters and no result rows expected.
exec :: Sqlite.Database -> T.Text -> IO ()
exec db code =
  retryIfBusy "exec" (Sqlite.exec db code)

-- | Bind named parameters to a prepared 'Sqlite.Statement', run it, and
-- collect every result row as a 'SQLRow' (column name\/value pairs). Resets
-- the statement and clears its bindings afterwards either way, so the same
-- 'Sqlite.Statement' can be reused for the next call.
query :: Sqlite.Statement -> [(T.Text, SQLData)] -> IO [SQLRow]
query stmt bindings =
  do
    Sqlite.bindNamed stmt bindings
    rows <- retryIfBusy "query" (collectRows stmt) `finally` (Sqlite.reset stmt >> Sqlite.clearBindings stmt)
    pure rows

execStmt :: Sqlite.Statement -> [(T.Text, SQLData)] -> IO ()
execStmt stmt bindings = do
  res <- query stmt bindings
  case res of
    [] -> pure ()
    _ -> fail ("unexpected result for execStmt: " ++ show res)

withStatement :: Sqlite.Database -> T.Text -> (Sqlite.Statement -> IO a) -> IO a
withStatement db sql action =
  bracket (Sqlite.prepare db sql) Sqlite.finalize action

-- | Look up a column's value in a 'SQLRow', failing (in any 'MonadFail') if
-- the row has no such column.
getColumnValue :: MonadFail m => SQLRow -> ColumnName -> m SQLData
getColumnValue row colName =
  case L.lookup colName row of
    Nothing ->
      do
        let msg =
              "row request does not contain column " ++ show colName
        fail msg
    Just sqlData ->
      pure sqlData

-- | Open (creating if necessary) a sqlite database at @dbPath@ in WAL mode.
-- Prefer 'withSqliteDb' unless you need to manage the database's lifetime
-- yourself.
initSqliteDb :: T.Text -> IO Sqlite.Database
initSqliteDb dbPath =
  do
    db <-
      Sqlite.open2
        dbPath
        [Sqlite.SQLOpenCreate, Sqlite.SQLOpenReadWrite, Sqlite.SQLOpenFullMutex]
        Sqlite.SQLVFSDefault
    Sqlite.exec db "PRAGMA journal_mode=WAL;"
    pure db

-- | Close a database opened with 'initSqliteDb'\/'withSqliteDb'.
closeSqliteDb :: Sqlite.Database -> IO ()
closeSqliteDb db =
  Sqlite.close db `catch` \(_ :: Sqlite.SQLError) ->
    void (Direct.close db)

-- | Open a sqlite database (see 'initSqliteDb'), run @action@, and close it
-- afterwards even if @action@ throws.
withSqliteDb :: T.Text -> (Sqlite.Database -> IO a) -> IO a
withSqliteDb path action =
  bracket
    (initSqliteDb path)
    closeSqliteDb
    action

-- | Run @action@ inside a @BEGIN TRANSACTION@\/@COMMIT@, rolling back on
-- exception instead of committing.
withTransaction :: Sqlite.Database -> IO a -> IO a
withTransaction db action =
  bracketOnError
    (exec db "BEGIN TRANSACTION;")
    (\_ -> exec db "ROLLBACK;")
    (\_ -> action >>= \x -> exec db "COMMIT;" >> pure x)
