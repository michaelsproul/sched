import Data.Array
import Data.Ix

newtype Person = Person Int deriving (Eq, Ord, Ix, Show)

type Table = Array (Person, Int) Bool
type Trip = (Int, Int, [Person])

-- Compute the best trips for the given minimum number of people and minimum trip length.
bestTrips :: Table -> Int -> Int -> [Trip]
bestTrips table minPeople minLength = filter (\(_, _, ppl) -> length ppl >= minPeople) allTrips
    where
        allTrips = [bestTripBetween table start end |
                        start <- [1..numDays],
                        end <- [(start + minLength - 1)..numDays]]
        ((_, 1), (_, numDays)) = bounds table

-- Compute the best trip between two dates.
bestTripBetween :: Table -> Int -> Int -> Trip
bestTripBetween table start end = (start, end, [p | (p, _) <- availablePeople])
    where
        availablePeople = filter isAvailable availabilities
        isAvailable (p, av) = all id av
        availabilities = [(p, slice table start end p) | p <- allPeople table]

-- End-point inclusive array slice.
slice :: Table -> Int -> Int -> Person -> [Bool]
slice table start end p = [table ! (p, t) | t <- [start..end]]

-- All people from the table.
allPeople :: Table -> [Person]
allPeople table = [Person i | i <- [1..numPeople]]
    where
        ((Person 1, _), (Person numPeople, _)) = bounds table
