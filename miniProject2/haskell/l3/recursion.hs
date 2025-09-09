-- Recursion

-- Lets implement factorial func

-- base case
factorial 0 = 1

-- recursive case
factorial n = n * factorial (n - 1)

main = do
    putStrLn "Factorial of your number is: "
    print(factorial 5)

-- Putting big number will result memory insufficient and will give 0!!