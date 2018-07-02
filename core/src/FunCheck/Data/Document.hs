module FunCheck.Data.Document
  ( Document(..)
  )
where

data Document a
 = Leaf a
 | Many [Document a]
 | KeyedMany [(String, Document a)]
 | Null
