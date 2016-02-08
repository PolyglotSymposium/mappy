module Language.Primitives.Io where

import Debug.Trace
import System.IO.Unsafe

import Language.PrettyPrintable
import Language.Primitives.IoAble

data Io = Io
  deriving (Eq, Show, Ord)

ioLookup :: (IoAble a, PrettyPrintable a) => a -> Maybe a
ioLookup key =
  case classifyInput key of
    Just IoReadFile -> Just $ unsafeReadFile key
    _ -> Nothing

ioInsert :: (IoAble a, PrettyPrintable a) => a -> a -> ()
ioInsert key value =
  case classifyOutput key of
    Just IoPrint -> trace (pretty value) ()
    Just IoWriteFile -> unsafeWriteFile value
    _ -> ()

unsafeWriteFile :: (IoAble a, PrettyPrintable a) => a -> ()
unsafeWriteFile value =
  let
    file = destring $ pretty $ pluckInner value IoFilename
    contents = destring $ pretty $ pluckInner value IoContents
  in
    (unsafePerformIO $ writeFile file contents *> return ())

unsafeReadFile :: (IoAble a, PrettyPrintable a) => a -> a
unsafeReadFile map' = unsafePerformIO $ do
  contents <- readFile $ destring $ pretty $ pluckInner map' IoReadFileSel
  return $ fromString contents

destring = init . drop 1
