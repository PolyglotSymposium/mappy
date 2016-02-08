module Language.Primitives.IoAble where

class IoAble a where
  classifyInput :: a -> Maybe InputClassification
  classifyOutput :: a -> Maybe OutputClassification
  pluckInner :: a -> IoSelector -> a
  fromString :: String -> a

data OutputClassification =
  IoPrint
  | IoWriteFile

data InputClassification =
  IoReadFile

data IoSelector =
  IoFilename
  | IoContents
  | IoReadFileSel
