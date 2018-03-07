import Data.Char
import Data.List
import Data.Bits

-- Test Values --
ip :: [Char]
ip = "192.168.2.8"

netmask :: [Char]
netmask = "24"

-- Utils --

pairProduct :: (Int, Int) -> Int
pairProduct (a,b) = a * b

notDot :: Char -> Bool
notDot = (/= '.')

bitmask :: Int -> Int -> [Int]
bitmask v n = take 32 $ (replicate n v) ++ (repeat complement)
    where
        complement = (v + 1) `mod` 2

-- IP to 32bit array conversion --

makeByte :: [Int] -> [Int]
makeByte bits = (replicate n 0) ++ bits
    where
        n = 8 - (length bits)

decToBits :: Int -> [Int]
decToBits 0 = []
decToBits x = (decToBits dividend) ++ [remainder]
    where
        (dividend, remainder) = divMod x 2

stoi :: [Char] -> Int
stoi xs = sum (map pairProduct (zip digits powersOfTen))
    where 
        digits = reverse (map digitToInt xs)
        powersOfTen = iterate (*10) 1

parseOctets :: [Char] -> [[Char]]
parseOctets [] = []
parseOctets xs = [takeWhile notDot xs] ++ (parseOctets rest)
    where
        nextOctet = dropWhile notDot xs
        rest = if (length nextOctet) > 0
            then (tail nextOctet)
            else []

ipToBits :: [Char] -> [Int]
ipToBits ip = concat $ map (makeByte . decToBits) (map stoi (parseOctets ip)) 

--- Netmask ----

intToNetmask :: Int -> [Int] 
intToNetmask nm = bitmask 1 nm

-- Wildcard --
-- Network --
network :: [Int] -> Int -> [Int]
network ip netmask = bitmask 1 netmask
-- Broadcast --
-- HostMin --
-- HostMax --

--- Hosts Per Network ---
hosts :: Int -> Int
hosts nm = 2^(32 -nm)

