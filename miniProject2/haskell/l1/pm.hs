-- Pattern Matching
coffeeType :: String -> String
coffeeType "americano" = "Best"
coffeeType "cappuccino" = "Rich"
coffeeType "latte" = "Milky and Weird"
coffeeType _ = "Unknow coffee type"

main = do
    putStrLn "Your Coffee description is: "
    print(coffeeType "americano")
    print(coffeeType "latte")
    print(coffeeType "bread")