module Language.Primitives.Io where

import Debug.Trace
import System.IO.Unsafe

import Language.PrettyPrintable
import Language.Primitives.IoAble

data Io = Io
  deriving (Eq, Show, Ord)

ioInsert :: (IoAble a, PrettyPrintable a) => a -> a -> ()
ioInsert key value =
  case classifyIo key of
    Just IoPrint -> trace (pretty value) ()
    Just IoWriteFile -> unsafeWriteFile value
    _ -> ()

unsafeWriteFile :: (IoAble a, PrettyPrintable a) => a -> ()
unsafeWriteFile value =
  let
    destring = init . drop 1
    file = destring $ pretty $ pluckInner value IoFilename
    contents = destring $ pretty $ pluckInner value IoContents
  in
    (unsafePerformIO $ writeFile file contents *> return ())
