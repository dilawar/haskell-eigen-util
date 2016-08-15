{-# LANGUAGE FlexibleContexts #-}

module Data.Eigen.Util (
    -- Basic operations
    rowOper 
    , colOper
    -- creation 
    , fromList'
    -- stacking functions 
    , hstack
    , vstack
    -- Kronecker product of two matrix
    , kronecker
) where

import Data.Eigen.Matrix as E
import Data.Maybe (fromJust)
import Data.List as L
import Data.Vector.Storable as V

-- | to2DList turns a 1-d list to 2D list.
-- to2DList :: E.Elem a b => Int -> [ b ] -> [[ b ]]
to2DList _ [] = []
to2DList n es = row : to2DList n rest
  where
    ( row, rest ) = L.splitAt n es

-- | Alternative implementation of fromList. It accepts a flatten list of
-- elements and number of columns. 
-- No tests are performed to check if number of elements in list are sufficient.
fromList' :: E.Elem a b => Int -> [ a ] -> E.Matrix a b
fromList' n elems = E.fromList $ to2DList n elems

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
    result = to2DList c1 $ E.fold' ( \c e -> ((E.map (*e) mat2):c) ) [] mat1 
    [ (r1,c1), (r2,c2) ] = [ E.dims mat1, E.dims mat2 ]
    (r, c) = ( r1*r2, c1*c2 )

-- | rowOper
--   row r1 = row r1 + c * row r2
rowOper :: E.Elem a b => Int -> Int -> a -> E.Matrix a b -> E.Matrix a b
rowOper r1 r2 c mat = E.imap ( 
    \i j v -> if i == r1 then v + c * ( mat E.! (r2,j) ) else v ) mat

-- | colOper 
-- col c1 = col c1 + c * col c2 
colOper :: E.Elem a b => Int -> Int -> a -> E.Matrix a b -> E.Matrix a b
colOper c1 c2 c mat = E.imap ( 
    \i j v -> if j == c1 then v + c * ( mat E.! (i,c2) ) else v ) mat

