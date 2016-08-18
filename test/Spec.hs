import qualified Data.Eigen.Matrix as E
import Data.Eigen.Util

testKronecker m1 m2 = kronecker m1 m2

mat1 = E.fromList [ [1, 2], [3, 4] ] :: E.MatrixXd 
mat2 = E.fromList [ [5, 6], [7, 8] ] :: E.MatrixXd 

main :: IO ()
main = do 
    pprintIO mat1
    pprintIO mat2
    let res = kronecker mat1 mat2
    {-pprintIO res-}
    print res
    putStrLn "All done"
