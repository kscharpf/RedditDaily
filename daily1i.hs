import System.Exit


data Event = Event Int String

eventNum :: Event -> Int
eventNum (Event n _) = n

eventLabel :: Event -> String
eventLabel (Event _ eventLabel) = eventLabel

showEvent :: Event -> String
showEvent evt = (show (eventNum evt)) ++ ") " ++ (eventLabel evt) ++ "\n"

flatten :: [[a]] -> [a]
flatten = foldl (++) []

menu :: [Event] -> String
menu eventList = "\n\n" ++ (flatten (map showEvent eventList)) ++ "\n\na) Add an event\nd) Delete an event\nm) Modify an event\nq) Exit\n"

main :: IO ()
main = doMenu []

addEvent :: String -> [Event] -> [Event]
addEvent xs [] = [(Event 1 xs)]
addEvent xs ys = ys ++ [(Event ((eventNum (last ys)) + 1) xs)]

deleteEvent :: String -> [Event] -> [Event]


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
