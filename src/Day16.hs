module Day16 (part16a, part16b) where

import Control.Monad.State (MonadState (get), State, evalState, get, put, runState)
import Data.Foldable (Foldable (foldl'))
import Debug.Trace
import Util (readLines)

--------------------------------------------------------------------------------
-- Day 16 - Packet Decoder
--------------------------------------------------------------------------------
data Bit = Zero | One deriving (Eq, Show, Enum)

data Payload
  = Literal {typeID :: Int, value :: Int}
  | Operator {typeID :: Int, subpackets :: [Packet]}
  deriving (Eq, Show)

data Packet = Packet
  { version :: Int,
    payload :: Payload
  }
  deriving (Eq, Show)

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
    parseBit _ = error "Invalid hex character"

bin2Dec :: [Bit] -> Int
bin2Dec = foldl' (\acc x -> acc * 2 + fromEnum x) 0

popBits :: Int -> State [Bit] [Bit]
popBits n = do
  bits <- get
  let (result, rest) = splitAt n bits
  put rest
  return result

popInt :: Int -> State [Bit] Int
popInt n = bin2Dec <$> popBits n

popBit :: State [Bit] Bit
popBit = do
  bits <- get
  put $ tail bits
  return $ head bits

parseValue :: State [Bit] Int
parseValue =
  let chunk = do
        continue <- popBit
        quad <- popBits 4
        if continue == One
          then (quad ++) <$> chunk
          else return quad
   in bin2Dec <$> chunk

parseNPackets :: Int -> State [Bit] [Packet]
parseNPackets 0 = return []
parseNPackets n = do
  packet <- parsePacket
  packets <- parseNPackets (n - 1)
  return (packet : packets)

parseBoundedPackets :: Int -> State [Bit] [Packet]
parseBoundedPackets 0 = return []
parseBoundedPackets l = do
  before <- get
  packet <- parsePacket
  after <- get
  packets <- parseBoundedPackets (l - (length before - length after))
  return (packet : packets)

parseSubpackets :: State [Bit] [Packet]
parseSubpackets = do
  ltype <- popBit
  if ltype == Zero
    then do
      length <- popInt 15
      parseBoundedPackets length
    else do
      length <- popInt 11
      parseNPackets length

parsePayload :: State [Bit] Payload
parsePayload = do
  typeID <- popInt 3
  if typeID == 4
    then do
      Literal typeID <$> parseValue
    else do
      Operator typeID <$> parseSubpackets

parsePacket :: State [Bit] Packet
parsePacket = do
  version <- popInt 3
  Packet version <$> parsePayload

sumVersions :: Packet -> Int
sumVersions Packet {version = v, payload = p} = v + sumVersions' p
  where
    sumVersions' (Literal _ _) = 0
    sumVersions' (Operator _ ps) = sum (map sumVersions ps)

valueOf :: Packet -> Int
valueOf Packet {payload = Literal {value = v'}} = v'
valueOf Packet {payload = Operator {typeID = t, subpackets = ps}} =
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
        _ -> error ("Unknown operator: " ++ show t)

part16a :: IO Int
part16a = do
  input <- head <$> readLines "inputs/day16.txt"
  let packet = hex2Bits input
  return $ sumVersions $ evalState parsePacket packet

part16b :: IO Int
part16b = do
  input <- head <$> readLines "inputs/day16.txt"
  let packet = hex2Bits input
  return $ valueOf $ evalState parsePacket packet
