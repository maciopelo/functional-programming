data Product = Product {name::String, amount::Int, price::Float} deriving (Show, Read, Eq)


vendingMachine = [Product "Coke" 15 2.50,
                  Product "Fanta" 10 2.30,
                  Product "Water" 20 2.00,
                  Product "Orange Juice" 8 3.00,
                  Product "Lay's" 20 2.00,
                  Product "Snickers" 30 2.00,
                  Product "Price-Polo" 15 1.80,
                  Product "7Days" 11 2.80 ]


changeAmount name list = [ if name prod == name then Product (name prod) (amount prod)-1 (price prod) else Product (name prod) (amount prod) (price prod)  | prod <- list]