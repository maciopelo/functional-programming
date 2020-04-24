import Data.List
{-
Zadanie z trójkątem
1. [(x,y,z) | x<-[3..17], y<-[3..17], z<-[3..17], (max(max x y) z) < (min x y)+(min (min x y) z),x>=y,y>=z]
2. [(x,y,z) | x<-[3..17], y<-[3..17], z<-[3..17], (max(max x y) z) < (min x y)+(min (min x y) z), ((max(max x y) z)^2) == ((min x y)^2) + ((min (min x y) z)^2),x>=y,y>=z]

-}

incAndtriple v = ( v + 1 ) * 3
specialCases 1 = "Hello"
specialCases 2 = " "
specialCases 3 = "World"
specialCases 4 = "!"
specialCases x = "???"



headOfList list = list !! 0
lengthOfList list = sum[1 | _ <- list]
takeFromList 0 list = []
takeFromList howMany list = [ list!!x | x <-[0..howMany-1]]
mapList func list = [ func x | x  <- list]
(+++) x y = x ++ y 


bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname       
    
max' :: (Ord a )=> a -> a -> a
max' a b 
    | a > b = a
    | otherwise = b

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r^2
