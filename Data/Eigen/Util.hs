module Data.Eigen.Util (
    -- Stack list of matrices horizontally
    hstack
    -- Stack list of matrices vertically
    , vstack
    -- Kronecker product of two matrix
    , kronecker
) where

import Data.Eigen.Matrix as E
import Data.Maybe (fromJust)
import Data.List as L
import Data.Vector.Storable as V


-- | Stack matrices horizontallly
hstack :: E.Elem a b => [ E.Matrix a b ] -> E.Matrix a b
hstack mats = E.generate allrows allcols generateFunc
  where
    allcols = L.sum ncols
    allrows = E.rows $ L.head mats
    ncols = L.map E.cols mats
    whichMat c = fromJust $ L.findIndex (>c) $ L.scanl1 (+) ncols 
    generateFunc i j = (mats!!matId) E.! (i, j - (L.sum $ L.take matId ncols) )
      where 
        matId = whichMat j

-- | Stack given matrices vertically. It uses the following property
-- vstack [a, b, c ..] = ( hstack [a',b',c'.. ] )' where M' is transpose of
-- matrix M. 
--
-- TODO: This is computationally inefficient than implementing is directly like
-- hstack.
vstack :: E.Elem a b => [ E.Matrix a b ] -> E.Matrix a b
vstack mats = E.transpose $ hstack $ L.map E.transpose mats

-- | Kronecker matric multiplication.
kronecker mat1 mat2 = vstack $ L.map hstack result
  where
    result = listToMat c1 $ E.fold' ( \c e -> ((E.map (*e) mat2):c) ) [] mat1 
    [ (r1,c1), (r2,c2) ] = [ E.dims mat1, E.dims mat2 ]
    (r, c) = ( r1*r2, c1*c2 )

    -- Give a flatten list the structure of matrix.
    listToMat _ [] = []
    listToMat ncols xs = row : listToMat ncols rest 
      where
        ( row, rest ) = L.splitAt ncols xs


