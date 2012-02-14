import System.Exit


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

main :: IO ()
main = doMenu []

addEvent :: String -> [Event] -> [Event]
addEvent xs [] = [(Event 1 xs)]
addEvent xs ys = ys ++ [(Event ((num (last ys)) + 1) xs)]

deleteEvent :: String -> [Event] -> [Event]
deleteEvent xs [] = undefined

processSplitCommand :: [String] -> [Event] -> [Event]
processSplitCommand ["a", xs] ys = addEvent xs ys

processCommand :: String -> [Event] -> [Event]
processCommand xs ys = processSplitCommand (words xs) ys 

doMenu :: [Event] -> IO ()
doMenu eventList = do
  putStr (menu eventList)
  cmd <- getLine
  if cmd == "q"
    then exitWith ExitSuccess
    else doMenu (processCommand cmd eventList)
