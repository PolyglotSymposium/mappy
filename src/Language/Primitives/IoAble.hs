module Language.Primitives.IoAble where

class IoAble a where
  classifyIo :: a -> Maybe IoOperationClassification
  pluckInner :: a -> IoSelector -> a

data IoOperationClassification =
  IoPrint
  | IoWriteFile

data IoSelector =
  IoFilename
  | IoContents
