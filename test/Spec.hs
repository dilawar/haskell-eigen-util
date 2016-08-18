import qualified Data.Eigen.Matrix as E
import Data.Eigen.Util


mat1 = E.fromList [ [1, 2], [3, 4] ] :: E.MatrixXd 
mat2 = E.fromList [ [5, 6], [7, 8] ] :: E.MatrixXd 
res = E.fromList [ [5,6,10,12], [7,8,14,16], [15, 18, 20, 24], [21, 24, 28, 32 ] ] :: E.MatrixXd

testKronecker m1 m2 = do 
    pprintIO m1
    pprintIO m2
    pprintIO res
    pprintIO $ kronecker m1 m2
    putStrLn "Test done"

main :: IO ()
main = do 
    testKronecker mat1 mat2 
    putStrLn "All done"
