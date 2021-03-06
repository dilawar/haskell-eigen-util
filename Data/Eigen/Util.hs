{-# LANGUAGE FlexibleContexts #-}

module Data.Eigen.Util (
    -- | Basic operations on rows and columns on rows and columns on rows and
    -- columns on rows and columns
    rowAdd 
    , rowsAdd
    , colAdd 
    , colsAdd 
    , scaleRow
    , scaleCol
    -- | Matrix creation from list 
    , fromList'
    -- | stacking functions 
    , hstack
    , vstack
    -- | Function to manipulate matrices
    , delRow 
    , delRows
    , delCol
    , delCols
    -- | Kronecker product of two matrix
    , kronecker
    -- | Display matrix 
    , pprint
    , pprintIO
) where

import Data.Eigen.Matrix as E
import Data.Maybe (fromJust)
import Data.List as L
import Data.Vector.Storable as V
import Text.Printf (printf, PrintfArg)

-- | to2DList turns a 1-d list to 2D list.
to2DList _ [] = []
to2DList n es = row : to2DList n rest
  where
    ( row, rest ) = L.splitAt n es

{- | Alternative implementation of fromList. It accepts a flatten list of
-- elements and number of columns. 
-- No tests are performed to check if number of elements in list are sufficient.
--}
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

{- | Stack given matrices vertically. It uses the following property
-- vstack [a, b, c ..] = ( hstack [a',b',c'.. ] )' where M' is transpose of
-- matrix M. 
--
-- TODO: This is computationally inefficient than implementing is directly like
-- hstack.
-}
vstack :: E.Elem a b => [ E.Matrix a b ] -> E.Matrix a b
vstack mats = E.transpose $ hstack $ L.map E.transpose mats

-- | Kronecker matric multiplication.
kronecker mat1 mat2 = E.transpose $ vstack $ L.map hstack result
  where
    result = to2DList c1 $ L.reverse $ E.fold ( \c e -> ((E.map (*e) mat2):c) ) [] mat1 
    [ (r1,c1), (r2,c2) ] = [ E.dims mat1, E.dims mat2 ]
    (r, c) = ( r1*r2, c1*c2 )

-- | rowAdd r1 = r1 + k * r2
rowAdd :: E.Elem a b => Int -> (a,  Int) -> E.Matrix a b -> E.Matrix a b
rowAdd r1 (k,r2) mat = E.imap ( 
    \i j v -> if i == r1 then v + k * ( mat E.! (r2,j) ) else v ) mat

-- | colAdd c1 = c1 + k * c2
colAdd :: E.Elem a b => Int -> (a, Int ) -> E.Matrix a b -> E.Matrix a b
colAdd c1 (k, c2) mat = E.imap ( 
    \i j v -> if j == c1 then v + k * ( mat E.! (i,c2) ) else v ) mat

{- | Adds a list of given columns with a list weights to the first column in the list. 
 - Note that first value in the list of weights is ignored 
 -}
colsAdd :: E.Elem a b => [ Int ] -> [ a ] -> E.Matrix a b -> E.Matrix a b
colsAdd (c:[]) _ m = m
colsAdd (c:c1:cols) (w:w1:ws) m = colsAdd (c:cols) (w:ws) $ colAdd c (w1,c1) m

{- | Adds a list of given rows with a list weights to the first row in the list. 
 - Note that first value in the list of weights is ignored 
 -}
rowsAdd :: E.Elem a b => [ Int ] -> [ a ] -> E.Matrix a b -> E.Matrix a b
rowsAdd (r:[]) _ m = m
rowsAdd (r:r1:rows) (w:w1:ws) m = rowsAdd (r:rows) (w:ws) $ rowAdd r (w1,r1) m

-- | scale a row by a factor
scaleRow :: E.Elem a b => Int -> a -> E.Matrix a b -> E.Matrix a b
scaleRow row c mat = E.imap ( \i j v -> if i == row then c * v else v ) mat

-- | scale a column by a factor
scaleCol :: E.Elem a b => Int -> a -> E.Matrix a b -> E.Matrix a b
scaleCol col c mat = E.imap ( \i j v -> if j == col then c * v else v ) mat

-- Utility function to delete given element from the list
deleteAt :: Int -> [a] -> [a]
deleteAt n ls = let (ys,zs) = L.splitAt n ls in ys L.++ (L.tail zs)


-- | delete a row 
delRow :: E.Elem a b => Int -> E.Matrix a b -> E.Matrix a b
delRow r mat = E.fromList $ deleteAt r $ E.toList mat

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

-- | Pretty print the matrix
pprint :: (PrintfArg a,  E.Elem a b) => E.Matrix a b -> String 
pprint mat = L.unlines $ 
    -- construct each row
    L.map (\i -> L.unwords $ L.map (\e -> printf "%.5f" e) (E.row i mat)) $
    -- all rows
    L.take (E.rows mat) [0,1..]

-- | print matrix in IO monad
pprintIO :: (PrintfArg a, E.Elem a b) => E.Matrix a b -> IO () 
pprintIO mat = do 
    putStrLn $ pprint mat
