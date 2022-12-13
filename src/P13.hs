module P13 (run1, run2, inputLocation) where
import Data.List.Split (chunksOf)
import Data.Char (isNumber)
import Data.List (sortBy, elemIndex)
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
solve1 = sum . map fst . filter ((==LT) . uncurry orderPacket . snd) . zip [1..]

orderPacket :: PacketValue -> PacketValue -> Ordering
orderPacket (PacketInt left) (PacketInt right) = compare left right
orderPacket left@(PacketInt _) right = orderPacket (PacketList [left]) right
orderPacket left right@(PacketInt _) = orderPacket left (PacketList [right])
orderPacket (PacketList []) (PacketList []) = EQ
orderPacket (PacketList []) _ = LT
orderPacket _ (PacketList []) = GT
orderPacket (PacketList (left:left')) (PacketList (right:right')) =
    case orderPacket left right of
        EQ -> orderPacket (PacketList left') (PacketList right')
        x -> x

solve2 :: Input -> Int
solve2 = decoderKey . concatMap unpair

unpair :: (a,a) -> [a]
unpair (x,y) = [x,y]

decoderKey :: [PacketValue] -> Int
decoderKey packetList =
    let dividerPackets = [PacketList [PacketList [PacketInt 2]], PacketList [PacketList [PacketInt 6]]]
        sortedList = sortBy orderPacket (packetList ++ dividerPackets)
    in  product $ map (+1) $ mapMaybe (`elemIndex` sortedList) dividerPackets