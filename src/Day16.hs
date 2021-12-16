module Day16 (part16a, part16b) where
import           Data.Foldable (Foldable (foldl'))
import           Debug.Trace
import           Util          (readLines)

--------------------------------------------------------------------------------
-- Day 16 -
--------------------------------------------------------------------------------
data Bit = Zero | One deriving (Eq, Show, Enum)

data Payload = Literal { typeID :: Int, value :: Int }
             | Operator { typeID :: Int, subpackets :: [Packet] } deriving (Eq, Show)

data Packet = Packet { version :: Int
                     , payload :: Payload
                     } deriving (Eq, Show)

hex2Bits :: String -> [Bit]
hex2Bits = concatMap parseBit
  where
    parseBit '0' = [Zero, Zero, Zero, Zero]
    parseBit '1' = [Zero, Zero, Zero, One]
    parseBit '2' = [Zero, Zero, One, Zero]
    parseBit '3' = [Zero, Zero, One, One]
    parseBit '4' = [Zero, One, Zero, Zero]
    parseBit '5' = [Zero, One, Zero, One]
    parseBit '6' = [Zero, One, One, Zero]
    parseBit '7' = [Zero, One, One, One]
    parseBit '8' = [One, Zero, Zero, Zero]
    parseBit '9' = [One, Zero, Zero, One]
    parseBit 'A' = [One, Zero, One, Zero]
    parseBit 'B' = [One, Zero, One, One]
    parseBit 'C' = [One, One, Zero, Zero]
    parseBit 'D' = [One, One, Zero, One]
    parseBit 'E' = [One, One, One, Zero]
    parseBit 'F' = [One, One, One, One]
    parseBit _   = error "Invalid hex character"

bin2Dec :: [Bit] -> Int
bin2Dec = foldl' (\acc x -> acc * 2 + fromEnum x) 0

parseValue :: [Bit] -> (Int, [Bit])
parseValue bits = (bin2Dec unchunked, rest')
  where
    (unchunked, rest') = chunk bits
    chunk (One:a:b:c:d:rest) =
      let (cs, rest') = chunk rest
      in (a:b:c:d:cs, rest')
    chunk (Zero:a:b:c:d:rest) = ([a, b, c, d], rest)
    chunk _ = error "Invalid value"

parseSubpackets :: [Bit] -> ([Packet], [Bit])
parseSubpackets (Zero:rest) =
  let subPacketLength = bin2Dec $ take 15 rest
      afterLength = drop 15 rest
      foldPacket 0 ps bits = (ps, bits)
      foldPacket n ps bits =
        let (p, rest') = parsePacket bits
        in foldPacket (n - (length bits - length rest')) (ps ++ [p]) rest'
  in foldPacket subPacketLength [] afterLength
parseSubpackets (One:rest) =
  let subPacketLength = bin2Dec $ take 11 rest
      afterLength = drop 11 rest
      foldPacket 0 ps bits = (ps, bits)
      foldPacket n ps bits =
        let (p, rest') = parsePacket bits
        in foldPacket (n-1) (ps ++ [p]) rest'
  in foldPacket subPacketLength [] afterLength
parseSubpackets _ = error "Invalid subpacket"

parsePayload :: [Bit] -> (Payload, [Bit])
parsePayload (One:Zero:Zero:rest) = (Literal { typeID = 4, value = value }, rest')
  where (value, rest') = parseValue rest
parsePayload (t1:t2:t3:rest) = (Operator { typeID = bin2Dec [t1, t2, t3], subpackets = subpackets }, rest')
  where
    (subpackets, rest') = parseSubpackets rest
parsePayload rs = (Literal { typeID = 4, value = -99 }, rs)

parsePacket :: [Bit] -> (Packet, [Bit])
parsePacket (v1:v2:v3:rest) =
  let (payload, rest') = parsePayload rest
  in (Packet { version = bin2Dec [v1, v2, v3]
             , payload = payload
             }, rest')
parsePacket _ = error "Invalid packet"

addAllVersionNumbers :: Packet -> Int
addAllVersionNumbers Packet { version = v, payload = Literal {} } = v
addAllVersionNumbers Packet { version = v, payload = Operator { typeID = t, subpackets = ps } } = v + sum (map addAllVersionNumbers ps)

valueOf :: Packet -> Int
valueOf Packet { payload = Literal { value = v' } } = v'
valueOf Packet { payload = Operator { typeID = t, subpackets = ps } } =
  let subValues = map valueOf ps
  in case t of
    0 -> sum subValues
    1 -> product subValues
    2 -> minimum subValues
    3 -> maximum subValues
    -- type 4 is literal
    5 -> fromEnum (head subValues > (head . tail) subValues)
    6 -> fromEnum (head subValues < (head . tail) subValues)
    7 -> fromEnum (head subValues == (head . tail) subValues)
    _ -> 0

part16a :: IO Int
part16a = do
  input <- head <$> readLines "inputs/day16.txt"
  let packet = hex2Bits input
  return $ addAllVersionNumbers $ fst $ parsePacket packet

part16b :: IO Int
part16b = do
  input <- head <$> readLines "inputs/day16.txt"
  let packet = hex2Bits input
  return $ valueOf $ fst $ parsePacket packet
