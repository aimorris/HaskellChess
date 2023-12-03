{-# LANGUAGE BinaryLiterals #-}
import Data.Bits ( Bits((.|.), (.&.), shift, complement) )
import Foreign.C.Types (CULong)
import Data.Int (Int64)

type Bitboard = CULong

fileBitboard :: Int -> Bitboard
fileBitboard = shift 0b100000001000000010000000100000001000000010000000100000001

getKnightAttackBitboardSingle :: Bitboard -> Int -> Bitboard -> Bitboard
getKnightAttackBitboardSingle board shiftAmount mask = (.&.) (shift board shiftAmount) (complement mask)

getKnightAttackBitboard :: Bitboard -> Bitboard
getKnightAttackBitboard board =
    getKnightAttackBitboardSingle board 10 (fileBitboard 0 .|. fileBitboard 1) .|.
    getKnightAttackBitboardSingle board 17 (fileBitboard 0) .|.
    getKnightAttackBitboardSingle board 15 (fileBitboard 7) .|.
    getKnightAttackBitboardSingle board 6 (fileBitboard 6 .|. fileBitboard 7) .|.
    getKnightAttackBitboardSingle board (-10) (fileBitboard 6 .|. fileBitboard 7) .|.
    getKnightAttackBitboardSingle board (-17) (fileBitboard 7) .|.
    getKnightAttackBitboardSingle board (-15) (fileBitboard 0) .|.
    getKnightAttackBitboardSingle board (-6) (fileBitboard 0 .|. fileBitboard 1)