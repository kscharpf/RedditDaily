import Data.Ratio

weekDayName :: Int -> String
weekDayName 0 = "Sunday" 
weekDayName 1 = "Monday" 
weekDayName 2 = "Tuesday" 
weekDayName 3 = "Wednesday" 
weekDayName 4 = "Thursday" 
weekDayName 5 = "Friday" 
weekDayName 6 = "Saturday" 

gaussDayOfWeek :: Int -> Int -> Int -> Int -> Int
gaussDayOfWeek y m d c = (d + (floor ( (2.6*(toRational m)) - 0.2)) + y + (floor (y%4)) + (floor (c%4)) - (2*c)) `mod` 7

firstTwoDigits x = (read (take 2 (show x)))::Int
lastTwoDigits x = (read (drop 2 (show x)))::Int

yearConv :: Int -> Int -> Int
yearConv x y
  | y <= 2 = x - 1
  | otherwise = x

monthConv :: Int -> Int
monthConv x = ((x + 9) `mod` 12) + 1

dayOfWeek :: Int -> Int -> Int -> Int
dayOfWeek inY inM inD = 
       let yr = yearConv inY inM
       in (gaussDayOfWeek (lastTwoDigits yr) (monthConv inM) inD (firstTwoDigits yr))


dayOfWeekName :: [String] -> String
dayOfWeekName [dayStr, monthStr, yearStr] = weekDayName (dayOfWeek (read dayStr::Int) (read monthStr::Int) (read yearStr::Int))

