module P13 (run1, run2, inputLocation) where
import Data.List.Split (chunksOf)
import Data.Char (isNumber)
import Data.List (sortBy, elemIndex, findIndices)
import Data.Maybe (mapMaybe)

data PacketValue = PacketInt Int | PacketList [PacketValue] deriving Eq
type Input = [(PacketValue, PacketValue)]

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input13"

parse :: String -> Input
parse = map parsePair . chunksOf 3 . lines

parsePair :: [String] -> (PacketValue, PacketValue)
parsePair (pair1:pair2:_) = (fst $ parsePacketValue pair1, fst $ parsePacketValue pair2)
parsePair _ = error "Parsing wrong number of lines for pair"

parsePacketValue :: String -> (PacketValue, String)
parsePacketValue ('[':xs) = parsePacketList xs
parsePacketValue xs = parsePacketInt xs

parsePacketInt :: String -> (PacketValue, String)
parsePacketInt xs =
    let (intStr, res) = span isNumber xs
    in  (PacketInt (read intStr), res)

parsePacketList :: String -> (PacketValue, String)
parsePacketList ('[':xs) =
    let (value, xs') = parsePacketList xs
        (list, xs'') = parsePacketList xs'
    in  (appendPacketList value list, xs'')
parsePacketList (']':xs) = (PacketList [], xs)
parsePacketList (',':xs) = parsePacketList xs
parsePacketList xs =
    let (value, xs') = parsePacketInt xs
        (list, xs'') = parsePacketList xs'
    in  (appendPacketList value list, xs'')

appendPacketList :: PacketValue -> PacketValue -> PacketValue
appendPacketList value (PacketList list) = PacketList (value : list)
appendPacketList _ _ = error "Can't append to int"

solve1 :: Input -> Int
solve1 = sum . map (+1) . findIndices isPairOrdered

isPairOrdered :: (PacketValue, PacketValue) -> Bool
isPairOrdered = (==LT) . uncurry comparePacket

comparePacket :: PacketValue -> PacketValue -> Ordering
comparePacket (PacketInt left) (PacketInt right) = compare left right
comparePacket left@(PacketInt _) (PacketList right) = comparePacketList [left] right
comparePacket (PacketList left) right@(PacketInt _) = comparePacketList left [right]
comparePacket (PacketList left) (PacketList right) = comparePacketList left right

comparePacketList :: [PacketValue] -> [PacketValue] -> Ordering
comparePacketList [] [] = EQ
comparePacketList [] _ = LT
comparePacketList _ [] = GT
comparePacketList (left:left') (right:right')
    | compareFirstElems == EQ = comparePacket (PacketList left') (PacketList right')
    | otherwise = compareFirstElems
    where compareFirstElems = comparePacket left right

solve2 :: Input -> Int
solve2 = decoderKey . concatMap unpair

unpair :: (a,a) -> [a]
unpair (x,y) = [x,y]

dividerPackets :: [PacketValue]
dividerPackets = [PacketList [PacketList [PacketInt 2]], PacketList [PacketList [PacketInt 6]]]

decoderKey :: [PacketValue] -> Int
decoderKey = product . dividerIndices . sortWithDividers

sortWithDividers :: [PacketValue] -> [PacketValue]
sortWithDividers = sortBy comparePacket . (dividerPackets ++)

dividerIndices :: [PacketValue] -> [Int]
dividerIndices xs = map (+1) $ mapMaybe (`elemIndex` xs) dividerPackets