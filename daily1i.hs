import System.Exit
import Data.List


data Event = Event { num :: Int,
                     label :: String } 

instance Eq Event where
    (Event x1 str1) == (Event x2 str2) = x1 == x2

instance Show Event where
    show (Event x xs) = (show x) ++ ") " ++ (show xs)

showEvent :: Event -> String
showEvent evt = (show (num evt)) ++ ") " ++ (label evt) ++ "\n"

flatten :: [[a]] -> [a]
flatten = foldl (++) []

menu :: [Event] -> String
menu eventList = "\n\n" ++ (flatten (map showEvent eventList)) ++ "\n\na) Add an event\nd) Delete an event\nm) Modify an event\nq) Exit\n"

reorderI :: [Event] -> Int -> [Event] -> [Event]
reorderI [] _ ys = ys
reorderI (x:xs) n ys = (reorderI xs (n+1) (ys ++ [(Event (n+1) (label x))]))

reorder :: [Event] -> [Event]
reorder xs = reorderI xs 0 []

main :: IO ()
main = doMenu []

addEvent :: String -> [Event] -> [Event]
addEvent xs [] = [(Event 1 xs)]
addEvent xs ys = ys ++ [(Event ((num (last ys)) + 1) xs)]

deleteEvent :: String -> [Event] -> [Event]
deleteEvent str [] = [] 
deleteEvent str ys = reorder (delete (Event (read str ::Int) "") ys) 

processSplitCommand :: [String] -> [Event] -> [Event]
processSplitCommand ["a", xs] ys = addEvent xs ys
processSplitCommand ["d", xs] ys = deleteEvent xs ys

processCommand :: String -> [Event] -> [Event]
processCommand xs ys = processSplitCommand (words xs) ys 

doMenu :: [Event] -> IO ()
doMenu eventList = do
  putStr (menu eventList)
  cmd <- getLine
  if cmd == "q"
    then exitWith ExitSuccess
    else doMenu (processCommand cmd eventList)
