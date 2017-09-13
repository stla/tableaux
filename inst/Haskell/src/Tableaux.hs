{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Tableaux where
import           Data.Bool                        (bool)
import           Data.Singletons                  (sing)
import qualified Data.Vector.SEXP                 as VS
import           Foreign
import           Foreign.C
import           Foreign.R                        (SEXP)
import qualified Foreign.R.Type                   as R
import           Language.R.Literal               (mkProtectedSEXPVectorIO)
import           Math.Combinat.Partitions.Integer
import           Math.Combinat.Tableaux

importTableau :: Ptr (SEXP s R.Int) -> Ptr CInt -> IO (Tableau Int32)
importTableau tableau l = do
  l <- peek l
  tableau <- peekArray (fromIntegral l :: Int) tableau
  return $ map (VS.toList . VS.fromSEXP) tableau

foreign export ccall asciiTableauR :: Ptr (SEXP s R.Int) -> Ptr CInt -> Ptr CString -> IO ()
asciiTableauR :: Ptr (SEXP s R.Int) -> Ptr CInt -> Ptr CString -> IO ()
asciiTableauR tableau l result = do
  tableau <- importTableau tableau l
  let ascii = show (asciiTableau tableau)
  (>>=) (newCString ascii) (poke result)

foreign export ccall dualTableauR :: Ptr (SEXP s R.Int) -> Ptr CInt ->
                                                  Ptr (SEXP s R.Vector) -> IO ()
dualTableauR :: Ptr (SEXP s R.Int) -> Ptr CInt -> Ptr (SEXP s R.Vector) -> IO ()
dualTableauR tableau l result = do
  tableau <- importTableau tableau l
  dtableau <- mkProtectedSEXPVectorIO sing $
    (map (VS.toSEXP . VS.fromList) (dualTableau tableau) :: [SEXP s R.Int])
  poke result dtableau

foreign export ccall isPartitionR :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO()
isPartitionR :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO()
isPartitionR partition l result = do
  l <- peek l
  partition <- peekArray (fromIntegral l :: Int) partition
  poke result $ bool 0 1 (isPartition (map fromIntegral partition :: [Int]))

foreign export ccall countStandardYoungTableauxR :: Ptr CInt -> Ptr CInt ->
                                                                Ptr CInt -> IO()
countStandardYoungTableauxR :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO()
countStandardYoungTableauxR partition l result = do
  l <- peek l
  partition <- peekArray (fromIntegral l :: Int) partition
  poke result $ fromIntegral $
                  countStandardYoungTableaux (mkPartition $
                                                map fromIntegral partition)

foreign export ccall standardYoungTableauxR :: Ptr CInt -> Ptr CInt ->
                                                   Ptr (SEXP s R.Vector) -> IO()
standardYoungTableauxR :: Ptr CInt -> Ptr CInt -> Ptr (SEXP s R.Vector) -> IO()
standardYoungTableauxR partition l result = do
  l <- peek l
  partition <- peekArray (fromIntegral l :: Int) partition
  let tableaux = standardYoungTableaux (mkPartition $ map fromIntegral partition)
  let tableaux32 = map (map (map fromIntegral)) tableaux :: [[[Int32]]]
  tableauxAsSEXP <- mapM (\x -> (mkProtectedSEXPVectorIO sing $
                            (map (VS.toSEXP . VS.fromList) x :: [SEXP s R.Int]))
                              :: IO (SEXP s R.Vector)) tableaux32
  (>>=) (mkProtectedSEXPVectorIO sing tableauxAsSEXP) (poke result)

foreign export ccall hookLengthsR :: Ptr CInt -> Ptr CInt ->
                                                  Ptr (SEXP s R.Vector) -> IO ()
hookLengthsR :: Ptr CInt -> Ptr CInt -> Ptr (SEXP s R.Vector) -> IO ()
hookLengthsR partition l result = do
  l <- peek l
  partition <- peekArray (fromIntegral l :: Int) partition
  let lengths = map (map fromIntegral) $
                  hookLengths (mkPartition $ map fromIntegral partition)
                    :: [[Int32]]
  (>>=) (mkProtectedSEXPVectorIO sing $
          (map (VS.toSEXP . VS.fromList) lengths :: [SEXP s R.Int]))
            (poke result)
