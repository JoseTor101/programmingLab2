import Data.List

condnumero :: Float -> Float
condnumero num
  | num == 3 = 100
  | num == 5 = 25
  | otherwise = 0

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
  where foldingFunction (x:y:ys)"*"= (x * y):  ys
        foldingFunction (x:y:ys)"+"= (x + y):  ys
        foldingFunction (x:y:ys)"-"= (y - x):  ys
        foldingFunction (x:y:ys)"/"= (y / x):  ys
        foldingFunction (x:xs) "neg"=  negate x:xs
        foldingFunction (x:xs) "raiz"=  sqrt x:xs
        foldingFunction (x:xs) "condnumero" = condnumero x:xs
        foldingFunction xs "sum"= [sum xs]
        foldingFunction xs "product"= [product xs]
        foldingFunction xs "promedio"= (sum xs / fromIntegral (length xs)): xs
        foldingFunction xs numberString= read numberString:xs


main = do  
    putStrLn "\nThis is a Reverse Polish Notation calculator. \n \nHere are a few examples we've made for you, \nbut you can write your own operations through the terminal!\n" 

    putStrLn "> Multiply: 2.5 3 5 * *"
    print (solveRPN "2.5 3 5 * *")

    putStrLn "\n> Add: 22 16 7 + +"
    print (solveRPN "22 16 7 + +")

    putStrLn "\n> Substract: 22 9 7 - -"
    print (solveRPN "22 9 7 - -")

    putStrLn "\n> Divide: 59 6 /"
    print (solveRPN "59 6 /")

    putStrLn "\n> Negate: 10 67 neg +"
    print (solveRPN "10 67 neg +")

    putStrLn "\n> Square root: 5 8 16 raiz +"
    print (solveRPN "5 8 16 raiz +")

    putStrLn "\n> Conditional numbers: 10 3 condnumero +"
    print (solveRPN "10 3 condnumero +")

    putStrLn "\n> Total sum: 45 3 12 20 sum"
    print (solveRPN "45 3 12 20 sum")

    putStrLn "\n> Total product: 9 16 7 2 3 product"
    print (solveRPN "9 16 7 2 3 product")

    putStrLn "\n> Average: 120 40 20 120 promedio"
    print (solveRPN "120 40 20 120 promedio")

