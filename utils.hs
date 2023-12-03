{-# LANGUAGE BinaryLiterals #-}
import Data.Bits
import Foreign.C.Types (CULong)
import Data.Int (Int64)

type Bitboard = CULong
data Side = White | Black

-- Definitions of empty bitboard, files, and ranks bitboards

emptyBoard :: Bitboard
emptyBoard = zeroBits

fileBitboard :: Int -> Bitboard
fileBitboard = shift 0b100000001000000010000000100000001000000010000000100000001

rankBitboard :: Int -> Bitboard
rankBitboard = shift 0b11111111

getFile :: Bitboard -> Maybe Int
getFile x
    | popCount x > 1 = Nothing
    | otherwise = Just $ popCount (x - 1) .&. 7

getRank :: Bitboard -> Maybe Int
getRank x
    | popCount x > 1 = Nothing
    | otherwise = Just $ shift (popCount $ x - 1) (-3)

-- Shifts and neighbours

getShiftWithMask :: Bitboard -> Int -> Bitboard -> Bitboard
getShiftWithMask board shiftAmount mask = shift board shiftAmount .&. complement mask

northNeighbour :: Bitboard -> Bitboard
northNeighbour board = getShiftWithMask board 8 (rankBitboard 0)

southNeighbour :: Bitboard -> Bitboard
southNeighbour board = getShiftWithMask board (-8) (rankBitboard 7)

eastNeighbour :: Bitboard -> Bitboard
eastNeighbour board = getShiftWithMask board 1 (fileBitboard 0)

westNeighbour :: Bitboard -> Bitboard
westNeighbour board = getShiftWithMask board (-1) (fileBitboard 7)

-- Piece attack bitboards

getSingleKnightAttackBitboard :: Bitboard -> Maybe Bitboard
getSingleKnightAttackBitboard board
    | popCount board > 1 = Nothing
    | otherwise = Just $
        getShiftWithMask board 10 (fileBitboard 0 .|. fileBitboard 1) .|.
        getShiftWithMask board 17 (fileBitboard 0) .|.
        getShiftWithMask board 15 (fileBitboard 7) .|.
        getShiftWithMask board 6 (fileBitboard 6 .|. fileBitboard 7) .|.
        getShiftWithMask board (-10) (fileBitboard 6 .|. fileBitboard 7) .|.
        getShiftWithMask board (-17) (fileBitboard 7) .|.
        getShiftWithMask board (-15) (fileBitboard 0) .|.
        getShiftWithMask board (-6) (fileBitboard 0 .|. fileBitboard 1)

getSingleKingAttackBitboard :: Bitboard -> Maybe Bitboard
getSingleKingAttackBitboard board
    | popCount board > 1 = Nothing
    | otherwise = Just $ xor board $
        eastNeighbour northSouthNeighbours .|.
        westNeighbour northSouthNeighbours .|.
        northSouthNeighbours
    where northSouthNeighbours =
            northNeighbour board .|.
            southNeighbour board .|.
            board

-- getPawnPushMoves :: Bitboard -> Side -> Maybe Bitboard
-- getPawnPushMoves board side = 