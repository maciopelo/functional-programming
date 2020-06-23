-- Koch Maciej
-- WEAIiIB Inforamtyka II rok

-- Program 'automat z przekąskami' wedlug wlasnego pomyslu.
-- main - glowna funkcja programu z interaktywna czescia
-- W pliku 'vendingMachine.txt' znadują sie dane dotyczace naszego
-- portfela z iloscia dostepnych monet (pierwsza linia) oraz dane
-- dotyczace produktow w automacie. Po uruchomieniu program pobiera
-- dane z pliku i wyswietla informacje w konsoli, a nastepnie postepując 
-- zgodnie z komunikatami mozemy dokonac zakupu przekasek lub napojow.
-- Program obsluguje wydawaie reszty. Po wykonaniu operacji dane sa 
-- zapisywane do pliku wiec jest na biezaco aktualizowany. 
-- Zostaly obsluzone opcje wyjscia oraz ewentualne wyjatki podczas
-- wprowadzenia niepoprawnych danych.



import System.IO  
import System.Directory  
import Data.List
import System.Exit
import Data.Typeable


type Coin = (Float,Float,String)

data Product = Product { idNum::Int, name::String, amount::Int, price::Float } deriving (Show, Read, Eq)
data Wallet = Wallet { coins::[Coin] } deriving (Show, Read, Eq)


coinValues = [500.0, 200.0, 100.0, 50.0, 20.0, 10.0]
coinValuesString = ["5zl","2zl","1zl","50gr","20gr","10gr"]


first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z  


makeProductsList :: [Char] -> [Product]
makeProductsList list = [ Product (read a :: Int) b (read c :: Int) (read d :: Float) | [a, b, c, d] <- allLines ]
                        where allLines = [ words x | x <- tail (lines list) ]


addSpaces :: [Char] -> [Char]
addSpaces word = word ++ (take (12-(length word)) (cycle [' ']))


checkAmount :: [Char] -> [Char]
checkAmount amount = if amount == "1" then " 1 piece  left"
                   else if length amount < 2 then " " ++ amount ++ " pieces left"
                   else amount ++ " pieces left"

 
updateListOfProducts :: Int -> [Product] -> [Product]
updateListOfProducts num list = [ if idNum prod == num then (Product (idNum prod) (name prod) (amount prod -1) (price prod)) else (Product (idNum prod) (name prod) (amount prod) (price prod)) | prod <- list]


showProduct :: Product -> [Char]
showProduct = \x -> "no."++ show (idNum x)++" | "++(addSpaces (name x))++" | price: "++show (price x)++"0 zl"++" | "++ checkAmount (show (amount x))


prepareToSaveInFile :: [Product] -> [[Char]]
prepareToSaveInFile list = [ show (idNum prod)++" "++(name prod)++" "++show(amount prod)++" "++show(price prod) | prod <- list]


makeWallet :: [Char] -> Wallet
makeWallet list = Wallet (zip3 amountOfCoins coinValues coinValuesString)
                  where amountOfCoins = [ read x :: Float | x <- words (head (lines list))]


sumOfWallet :: Wallet -> Float
sumOfWallet wallet = sum list
                     where list = map (\(x,y,z) -> x*y) (coins wallet)
 

showWallet :: Wallet -> [Char]
showWallet wallet =  concat [ " | "++ show x ++" * " ++ z | (x,y,z)<- (coins wallet)]



readInts :: IO [Int]
readInts = fmap (map read.words) getLine


insertCoins :: String -> [Float]
insertCoins x = [if (read x :: Float) == 5.0 || (read x :: Float) == 2.0 || (read x :: Float) == 1.0 then (read x :: Float)*100 else (read x :: Float)| x <- words x]


getPrice :: Int -> [Product] -> [Product]
getPrice number list = filter (\(Product a b c d) -> a == number) list


updateWallet :: [Float] -> Wallet -> [Int]
updateWallet givenCoins wallet = [ length x | x <- group difference]
                        where difference = money \\ givenCoins   
                              money = concat (map (\(a,b,c) -> take (round a) (repeat b)) (coins wallet))


change :: Int -> [Int]
change n = reverse . snd $ foldl next (n, []) (map (round) coinValues)
    where next (remaining, cs) coin
            | coin <= remaining = (r', cnt:cs)
            | otherwise         = (remaining, 0:cs)
            where r' = remaining `mod` coin
                  cnt = remaining `div` coin


getNumberOfProducts :: [Product] -> [Int]
getNumberOfProducts list = [ idNum prod| prod <- list]




main :: IO ()
main = do
    handle <- openFile "vendingMachine.txt" ReadMode 
    contents <- hGetContents handle 
    let listOfProducts = makeProductsList contents
        wallet = makeWallet contents
        walletSum = sumOfWallet wallet
        numbers = getNumberOfProducts listOfProducts
        productsToShow = map showProduct listOfProducts
    putStrLn  ("In wallet: "++(show ((sumOfWallet wallet)/100) ++ "zl" ++ (showWallet wallet)) ++ "\n")
    putStr $ unlines productsToShow
    putStr "\nPlease choose a number of product (or leave - q): "     
    numberString <- getLine 
    if numberString /= "q" 
        then do
            let number = read numberString 
            if (elem number numbers)
                then do 
                    let chosenPrice = (price (getPrice number listOfProducts !! 0))*100
                    if walletSum < chosenPrice
                        then do
                            putStrLn "Not enough money ! "
                            return()
                        else do

                            putStr "Insert values of coins after spaces (e.g. 2 10 10 - 2.20zl): "
                            x <- getLine
                            let givenCoins = insertCoins x
                            
                            if sum givenCoins >= chosenPrice
                                then do
                                    putStrLn  ("\nYou gave: "++(show ((sum givenCoins) /100))++"zl")
                                    putStrLn  ("Your change: "++(show (((sum givenCoins) -  chosenPrice)/100))++"zl")
                                    let rest = round ((sum givenCoins) -  chosenPrice)
                                        restToWallet = change rest
                                        updatedWallet = zipWith (+) (updateWallet givenCoins wallet) restToWallet
                                        walletToString = concat [ show a ++ " " | a <- updatedWallet]
                                        newWallet = makeWallet walletToString
                                    (tempName, tempHandle) <- openTempFile "." "temp"
                                    putStrLn  ("In wallet: "++(show ((sumOfWallet newWallet)/100) ++ "zl" ++ (showWallet newWallet)) ++ "\n")
                        
                                    let updatedList = updateListOfProducts number listOfProducts
                                        listToSave = walletToString++"\n"++(unlines (prepareToSaveInFile updatedList))
                                        updatedProductsToShow = map showProduct updatedList
                                    --putStr $ unlines updatedProductsToShow
                                    hPutStr tempHandle $ listToSave 
                                    hClose handle  
                                    hClose tempHandle  
                                    removeFile "vendingMachine.txt"  
                                    renameFile tempName "vendingMachine.txt" 
                                    putStr "Do you want to buy anything else ? [Y/N] "
                                    answer <- getLine
                                    if answer == "Y" || answer == "y" then do main else do return()
                                
                                else do
                                    putStrLn "\nYou have inserted to few coins !" 
                                    main
            else do 
                putStrLn "\nWrong number of product !\n" 
                main
        else do
            return()
    
     
  
 
  