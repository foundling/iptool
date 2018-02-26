import Data.Char

notDot :: Char -> Bool
notDot = (/= '.')

charsToDigit :: [Char] -> [Int]
charsToDigit = map digitToInt 

pairProduct :: (Int, Int) -> Int  
pairProduct (a,b) = a * b

digitsToDecimal :: [Int] -> Int
digitsToDecimal digits = sum $ map pairProduct $ zip reversedDigits powersOfTen
    where 
        reversedDigits = reverse digits
        powersOfTen = iterate (*10) 1


-- replace octets with this, reorganize names and conversions of string to int
parseOctetStrings :: String -> [String]
parseOctetStrings [] = []
parseOctetStrings xs = [takeWhile notDot xs] ++ (octets rest)
    where
        nextOctet = dropWhile notDot xs 
        rest = if (length nextOctet) > 0  
                then (tail nextOctet)
                else []


octets :: String -> [Int]
octets [] = []
octets xs = octetInt : octets (drop (nCollected + 1) xs)
    where
        octetString = takeWhile notDot xs
        nCollected = length octetString
        octetInt = digitsToDecimal $ charsToDigit $ octetString

decToBin :: Int -> [Int]
decToBin 0 = []
decToBin x = (decToBin dividend) ++ [remainder]
    where
        (dividend, remainder) = divMod x 2 

binToDec :: [Int] -> Int
binToDec bits = sum $ map pairProduct (zip (reverse bits) $ iterate (*2) 1)

makeByte :: [Int] -> [Int] 
makeByte bits = (replicate n 0) ++ bits 
    where
        n = 8 - (length bits)

toBits :: String -> [Int]
toBits ip = concat $ map makeByte $ map decToBin (octets ip)

bitmask :: Int -> Int -> [Int]
bitmask n maskValue = take 32 $ (replicate n maskValue) ++ (repeat complement)  
    where
        complement = (maskValue + 1) `mod` 2

gateway :: String -> Int -> [Int]
gateway ip networkSize = map pairProduct $ zip (toBits ip) (bitmask networkSize 1)

hostNumber :: String -> Int  -> Int
hostNumber ip networkSize = binToDec $ map pairProduct $ zip (toBits ip) (bitmask networkSize 0)  

fn :: String -> String
fn s = s ++ "!"

main = do
    interact fn

