
import qualified Data.Eigen.Matrix as E
import Data.Eigen.Util

testKronecker m1 m2 = kronecker m1 m2

mat1 = E.fromList [ [0.85, 0.15], [0.1, 0.9] ] :: E.MatrixXd 
mat2 = E.fromList [ [0.8, 0.2], [0.05, 0.95] ] :: E.MatrixXd 

main :: IO ()
main = do 
    pprintIO mat1
    pprintIO mat2
    let res = kronecker mat1 mat2
    {-pprintIO res-}
    print $ res
    putStrLn "All done"
