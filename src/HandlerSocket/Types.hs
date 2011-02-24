module HandlerSocket.Types where

data Result = Ok
            | ErrorResult { errno :: Int, msg :: String }
            | OpenResult  { idxId :: IndexId }
            | GetResult   { rrows :: [Row] }
     deriving (Show)

data Op = Eq | Gt | Lt | Gte | Lte
instance Show Op where
  show Eq  = "="
  show Gt  = ">"
  show Lt  = "<"
  show Gte = ">="
  show Lte = "<="

type IndexId = Int
type Column  = String
type Value   = String
type Row     = [Value]
type Limit   = Int
type Offset  = Int

class HandlerSocket a where
  open   :: a -> IndexId
              -> String    -- ^ database
              -> String    -- ^ table
              -> String    -- ^ index
              -> [String]  -- ^ columns
              -> IO Result
  get    :: a -> IndexId
              -> Op
              -> [Column]
              -> Limit
              -> Offset
              -> IO Result
  update :: a -> IndexId
              -> Op
              -> [Column]
              -> [Value]
              -> Limit
              -> Offset
              -> IO Result
  insert :: a -> IndexId
              -> [Value]
              -> IO Result
  remove :: a -> IndexId
              -> Op
              -> [Column]
              -> Limit
              -> Offset
              -> IO Result

-- vim: set ts=2 sw=2 et :