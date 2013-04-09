-- Change the program so it performs a simple arithmetic operation on the two
-- arguments and prints out the result. You can use read to convert a string to
-- a number, and show to convert a number back into a string. Play around with
-- different operations.

module Ex2 where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let x = read $ args !! 0 :: Int
    let y = read $ args !! 1 :: Int
    putStrLn (show x ++ " * " ++ show y ++ " = " ++ show (x * y))
