import Data.List

condnumero:: Float -> Float
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
        foldingFunction (x:xs)"ln"= log x:xs
        foldingFunction xs "neg"=  [head xs*(-1)]
        foldingFunction (x:xs) "raiz"=  (sqrt x):xs
        foldingFunction (x:xs) "condnumero" = (condnumero x):xs
        foldingFunction xs "sum"= [sum xs]
        foldingFunction xs "product"= [product xs]
        foldingFunction xs "promedio"= (sum xs / fromIntegral (length xs)): xs
        foldingFunction xs numberString= read numberString:xs

main :: IO()
main = return ()

{--
FUNCIONES
->Producto total

--}