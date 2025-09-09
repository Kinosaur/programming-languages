-- Where Clause

popDensity :: Fractional a => (a, a) -> a
popDensity (population, area) = density where density = population / area

main = do 
    print(popDensity(1100000000.25, 850.78))