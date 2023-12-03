{-# LANGUAGE BinaryLiterals #-}
import Data.Bits

type Bitboard = Word
data Side = White | Black
type Board = [Bitboard]

-- Definitions of empty bitboard, files, ranks, and occupied bitboards

emptyBitboard :: Bitboard
emptyBitboard = zeroBits

initialBoard :: Board
initialBoard = [whitePawnBitboard, whiteRookBitboard, whiteKnightBitboard, whiteBishopBitboard, whiteQueenBitboard, whiteKingBitboard,
                blackPawnBitboard, blackRookBitboard, blackKnightBitboard, blackBishopBitboard, blackQueenBitboard, blackKingBitboard]
    where
        whitePawnBitboard = 0b1111111100000000
        whiteRookBitboard = 0b10000001
        whiteKnightBitboard = 0b1000010
        whiteBishopBitboard = 0b100100
        whiteQueenBitboard = 0b1000
        whiteKingBitboard = 0b10000
        blackPawnBitboard = 0b11111111000000000000000000000000000000000000000000000000
        blackRookBitboard = 0b1000000100000000000000000000000000000000000000000000000000000000
        blackKnightBitboard = 0b100001000000000000000000000000000000000000000000000000000000000
        blackBishopBitboard = 0b10010000000000000000000000000000000000000000000000000000000000
        blackQueenBitboard = 0b100000000000000000000000000000000000000000000000000000000000
        blackKingBitboard = 0b1000000000000000000000000000000000000000000000000000000000000
    

fileBitboard :: Int -> Bitboard
fileBitboard = shift 0b100000001000000010000000100000001000000010000000100000001

rankBitboard :: Int -> Bitboard
rankBitboard x = shift 0b11111111 (x * 8)

getFile :: Bitboard -> Maybe Int
getFile x
    | popCount x > 1 = Nothing
    | otherwise = Just $ popCount (x - 1) .&. 7

getRank :: Bitboard -> Maybe Int
getRank x
    | popCount x > 1 = Nothing
    | otherwise = Just $ shift (popCount $ x - 1) (-3)

occupiedBitboard :: Board -> Bitboard
occupiedBitboard = foldr (.|.) 0b0

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

getSingleKnightAttackTargets :: Bitboard -> Maybe Bitboard
getSingleKnightAttackTargets board
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

getSingleKingAttackTargets :: Bitboard -> Maybe Bitboard
getSingleKingAttackTargets board
    | popCount board > 1 = Nothing
    | otherwise = Just $ xor board $
        eastNeighbour northSouthNeighbours .|.
        westNeighbour northSouthNeighbours .|.
        northSouthNeighbours
    where northSouthNeighbours =
            northNeighbour board .|.
            southNeighbour board .|.
            board

getSinglePawnSinglePushTargets :: Bitboard -> Side -> Board -> Maybe Bitboard
getSinglePawnSinglePushTargets board side currentBoard
    | popCount board > 1 = Nothing
    | otherwise = Just $
        upNeighbour board .&.
        unoccupiedBitboard
    where
        upNeighbour = case side of
            White -> northNeighbour
            Black -> southNeighbour
        unoccupiedBitboard = complement $ occupiedBitboard currentBoard

getSinglePawnDoublePushTargets :: Bitboard -> Side -> Board -> Maybe Bitboard
getSinglePawnDoublePushTargets board side currentBoard
    | popCount board > 1 = Nothing
    | otherwise = Just $
        upNeighbour (upNeighbour board .&. unoccupiedBitboard) .&.
        unoccupiedBitboard .&.
        rankBitboard 3
    where
        upNeighbour = case side of
            White -> northNeighbour
            Black -> southNeighbour
        unoccupiedBitboard = complement $ occupiedBitboard currentBoard