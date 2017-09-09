{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds #-}
module Tableaux where
import Foreign
import Foreign.C
import Foreign.R (SEXP)
import qualified Foreign.R.Type as R
import qualified Data.Vector.SEXP as DV
import Math.Combinat.Tableaux
-- import Math.Combinat.ASCII
import Math.Combinat.Partitions.Integer
import Language.R.Literal (mkProtectedSEXPVector)
import Data.Singletons (sing)

importTableau :: Ptr (SEXP s R.Int) -> Ptr CInt -> IO (Tableau Int32)
importTableau tableau l = do
  l <- peek l
  tableau <- peekArray (fromIntegral l :: Int) tableau
  return $ map (DV.toList . DV.fromSEXP) tableau

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
  poke result $ mkProtectedSEXPVector sing $
    (map (DV.toSEXP . DV.fromList) (dualTableau tableau) :: [SEXP s R.Int])

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
  let tableauxAsSEXP = map (\x -> (mkProtectedSEXPVector sing $
                            (map (DV.toSEXP . DV.fromList) x :: [SEXP s R.Int]))
                              :: SEXP s R.Vector) tableaux32
  poke result $ mkProtectedSEXPVector sing tableauxAsSEXP
