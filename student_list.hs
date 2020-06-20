-- Koch Maciej
-- WEAIiIB 
-- 303120
-- Informatyka II rok



import System.IO



-- Student datatype
data Student = Student { firstName::String,
                         lastName::String, 
                         age::Int
                         }deriving (Show, Read, Eq)

data StudentsFirstNameChangeEvent  = StudentsFirstNameChangeEvent { oldName::String,
                                                                    newName::String
                                                                    }deriving (Show, Read, Eq)
                                                                  



-- 1. Utworzyć listę zawierającą pełne imiona i nazwiska studentów w postaci łańcuchów znaków.
listToProcess = [   Student "Alicja" "Akla" 21,
                    Student "Batrek" "Bodo" 20,
                    Student "Celina" "Czyzyk" 21,
                    Student "Damian" "Dab"  22, 
                    Student "Eustachy" "Elo" 20]

modifiedList = [    Student "AlicjaX" "Akla" 21,
                    Student "BatrekX" "Bodo" 20, 
                    Student "Celina" "Czyzyk" 21, 
                    Student "DamianX" "Dab"  22, 
                    Student "Eustachy" "Elo" 20]


                    

-- 2. Utworzyć listę zawierającą pary w postaci krotek: numer porządkowy, student.
addSerialNumbers list = zip [1..] list
numberedList = addSerialNumbers listToProcess




-- 3. Przetworzyć listę z powyższego punktu na raport tekstowy
studentToString :: Student -> String
studentToString (Student {firstName = f, lastName = s, age = a}) = "student: " ++ s ++ " " ++ f ++ " wiek: " ++ show a ++"\n"

studentTupleToString :: Show a => (a, Student) -> [Char]
studentTupleToString tuple  = show (fst tuple) ++ ". "  ++  studentToString (snd tuple)




--4. Odczytywanie danych z pliku i tworzenie tabelki w html
makeHtmlTable :: FilePath -> FilePath -> IO ()
makeHtmlTable input output = do
    file <- readFile input
    let allLines = [words x | x <- lines file]
    let listToProcess = [ Student a b (read c :: Int) | [a, b, c] <- allLines ]
    let numberedList = addSerialNumbers listToProcess
    let header = "<table style='width:100%'> <tr> <th>Number</th> <th>Firstname</th> <th>Lastname</th> <th>Age</th> </tr>"
    let footer = "</table>"
    let cellsOftable = ["<tr> <td>" ++ show (fst x) ++ "</td>" ++ "<td>" ++ (firstName (snd x)) ++ "</td>" ++ "<td>" ++ (lastName (snd x)) ++ "</td>" ++ "<td>" ++ show (age (snd x)) ++ "</td> </tr>" | x <- numberedList]
    let final = header : cellsOftable ++ [footer]
    writeFile output (unlines final)



-- 5. Wygenerować listę zmian w postaci typu wydarzenia
compareLists:: Monad m => [Student] -> [Student] -> m [StudentsFirstNameChangeEvent]
compareLists firstList secondList = do 
    let tmp = zip firstList secondList
    let result = [if firstName (fst x)  == firstName (snd x) then StudentsFirstNameChangeEvent (firstName (fst x)) (firstName (snd x)) else StudentsFirstNameChangeEvent (firstName (fst x)) (firstName (snd x)) | x <- tmp]
    return result



--6. Odczytywanie danych z pliku i ich zapisywanie w innym 
makeTxtRaport :: FilePath -> FilePath -> IO ()
makeTxtRaport input output  = do
    file <- readFile input
    let allLines = [words x | x <- lines file]
    let listToProcess = [ Student a b (read c :: Int) | [a, b, c] <- allLines ]
    let numberedList = addSerialNumbers listToProcess
    let content = concat [studentTupleToString x | x <- numberedList]
    writeFile output content

    


main = do 
    print "1. Utworzyc liste zawierajaca pelne imiona i nazwiska studentow w postaci lancuchów znakow."
    print listToProcess
    print "2. Utworzyc liste zawierajaca pary w postaci krotek: numer porzadkowy, student."
    print numberedList
    print "3. Przetworzyc liste z powyzszego punktu na raport tekstowy w formacie"
    makeTxtRaport "students.txt" "output_students.txt"
    print "4. Wygenerowac tabelke HTML"
    makeHtmlTable "students.txt" "students.html"
    print "5. Wygenerowac liste zmian w postaci typu wydarzenia"
    x <- compareLists listToProcess modifiedList
    print x
    print "6. Dane zaczytano z students.txt"


















