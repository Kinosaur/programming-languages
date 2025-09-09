-- List
import Data.List

main = do
    print(intersperse '|' "ILovePython") -- "I|L|o|v|e|P|y|t|h|o|n"
    print(intersperse " " ["Haskell", "is", "weird"]) -- ["Haskell"," ","is"," ","weird"]
    print(splitAt 3 "Orange") --("Ora","nge")
    print(sort [5.00, 34, 8])