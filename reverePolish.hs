solveRPN :: String -> Float
solveRPN = head . foldl foldingfunc [] . words
    where foldingfunc (x:y:ys) "*" = x*y:ys
          foldingfunc (x:y:ys) "+" = x+y:ys
          foldingfunc (x:y:ys) "-" = x-y:ys
          foldingfunc (x:y:ys) "/" = x/y:ys
          foldingfunc (x:y:ys) "^" = x**y:ys
          foldingfunc (x:xs) "ln" = log x:xs
          foldingfunc xs "sum" = [sum xs]
          foldingfunc xs numberString = read numberString:xs