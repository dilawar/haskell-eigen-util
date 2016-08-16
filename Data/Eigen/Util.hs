{-# LANGUAGE FlexibleContexts #-}

module Data.Eigen.Util (
    -- Basic operations
    rowAdd 
    , colAdd 
    , rowScale
    , colScale
    -- creation 
    , fromList'
    -- stacking functions 
    , hstack
    , vstack
    -- Function to manipulate matrices
    , delRow 
    , delRows
    , delCol
    , delCols
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

-- | rowAdd
--   row r1 = k1 * row r1 + k2 * row r2
rowAdd :: E.Elem a b => (a, Int) -> (a,  Int) -> E.Matrix a b -> E.Matrix a b
rowAdd (k1,r1) (k2,r2) mat = E.imap ( 
    \i j v -> if i == r1 then k1 * v + k2 * ( mat E.! (r2,j) ) else v ) mat

-- | colAdd 
-- col c1 = k1 * col c1 + k2 * col c2 
colAdd :: E.Elem a b => (a, Int) -> (a, Int ) -> E.Matrix a b -> E.Matrix a b
colAdd (k1, c1) (k2, c2) mat = E.imap ( 
    \i j v -> if j == c1 then k1 * v + k2 * ( mat E.! (i,c2) ) else v ) mat

-- | scale a row by a factor
rowScale :: E.Elem a b => Int -> a -> E.Matrix a b -> E.Matrix a b
rowScale row c mat = E.imap ( \i j v -> if i == row then c * v else v ) mat

-- | scale a column by a factor
colScale :: E.Elem a b => Int -> a -> E.Matrix a b -> E.Matrix a b
colScale col c mat = E.imap ( \i j v -> if j == col then c * v else v ) mat

-- TODO: Following two functions are inefficient since they don't manipulate
-- matrix in place.
-- | delete a row 
delRow :: E.Elem a b => Int -> E.Matrix a b -> E.Matrix a b
delRow r mat = E.fromList $ deleteAt r $ E.toList mat

-- Utility function to delete an element from the at index n
deleteAt :: Int -> [a] -> [a]
deleteAt n ls = let (ys,zs) = L.splitAt n ls in ys L.++ (L.tail zs)

-- | delete a column
delCol :: E.Elem a b => Int -> E.Matrix a b -> E.Matrix a b
delCol c mat = E.fromList $ L.map (\row -> deleteAt c row) $ E.toList mat

-- | delete list of given rows
delRows :: E.Elem a b => [ Int ] -> E.Matrix a b -> E.Matrix a b
delRows rows mat = delRows' (L.sort rows) mat 
  where 
    delRows' [] mat = mat
    delRows' (r:rs) mat = delRows' (L.map (\e->e-1) rs) (delRow r mat) 

-- | delete list of given columns
delCols :: E.Elem a b => [ Int ] -> E.Matrix a b -> E.Matrix a b
delCols cols mat = delCols' (L.sort cols) mat 
  where 
    delCols' [] mat = mat
    delCols' (c:cs) mat = delCols' (L.map (\e->e-1) cs) (delCol c mat) 
