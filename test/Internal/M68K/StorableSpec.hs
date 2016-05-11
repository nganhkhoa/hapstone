module Internal.M68K.StorableSpec where

import Foreign
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Internal.M68K

import Internal.M68K.Default

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.M68K" $ do
    m68kOpMemStructSpec
    csM68KOpSpec
    --csM68KOpSizeSpec
    --csM68KSpec

getM68KOpMemStruct :: IO M68KOpMemStruct
getM68KOpMemStruct = do
    ptr <- mallocArray (sizeOf m68kOpMemStruct) :: IO (Ptr Word8)
    poke (castPtr ptr) (fromIntegral $ fromEnum M68kRegA0 :: Word32)
    poke (plusPtr ptr 4) (fromIntegral $ fromEnum M68kRegD4 :: Word32)
    poke (plusPtr ptr 8) (fromIntegral $ fromEnum M68kRegA2 :: Word32)
    poke (plusPtr ptr 12) (0x01234567 :: Word32)
    poke (plusPtr ptr 16) (0x10325476 :: Word32)
    poke (plusPtr ptr 20) (0xffaa :: Word16)
    poke (plusPtr ptr 22) (0x8 :: Word8)
    poke (plusPtr ptr 23) (0x1 :: Word8)
    poke (plusPtr ptr 24) (0x4 :: Word8)
    poke (plusPtr ptr 25) (0x8 :: Word8)
    poke (plusPtr ptr 26) (0x1 :: Word8)
    peek (castPtr ptr) <* free ptr

m68kOpMemStruct :: M68KOpMemStruct
m68kOpMemStruct = M68KOpMemStruct M68kRegA0 M68kRegD4 M68kRegA2 0x01234567
    0x10325476 0xffaa 0x8 0x1 0x4 0x8 0x1

-- | M68KOpMemStruct spec
m68kOpMemStructSpec :: Spec
m68kOpMemStructSpec = describe "Storable M68KOpMemStruct" $ do
    it "is a packed struct" $
        sizeOf (undefined :: M68KOpMemStruct) == 4 * 5 + 2 + 5 + 1
    it "has matching peek- and poke-implementations" $ property $
        \s@M68KOpMemStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getM68KOpMemStruct `shouldReturn` m68kOpMemStruct

getCsM68KOp :: IO CsM68KOp
getCsM68KOp = do
    ptr <- mallocArray (sizeOf csM68KOp) :: IO (Ptr Word8)
    poke (castPtr ptr) (72324 :: Word64)
    poke (plusPtr ptr 28) (fromIntegral $ fromEnum M68kOpImm :: Int32) 
    poke (plusPtr ptr 32) (fromIntegral $ fromEnum M68kAmImmidiate :: Int32) 
    peek (castPtr ptr) <* free ptr

csM68KOp :: CsM68KOp
csM68KOp = CsM68KOp (Imm 72324) M68kAmImmidiate

-- | CsM68KOp spec
csM68KOpSpec :: Spec
csM68KOpSpec = describe "Storable CsM68KOp" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsM68KOp) == sizeOf m68kOpMemStruct + 3 * 4
    it "has matching peek- and poke-implementations" $ property $
        \s ->
            alloca (\p -> poke p s >> (peek p :: IO CsM68KOp)) `shouldReturn` s
    it "parses correctly" $ getCsM68KOp `shouldReturn` csM68KOp

{-
getCsM68K :: IO CsM68K
getCsM68K = do
    ptr <- mallocArray (sizeOf csM68K) :: IO (Ptr Word8)
    poke (castPtr ptr) (1 :: Word8)
    poke (plusPtr ptr 8) csM68KOp
    peek (castPtr ptr) <* free ptr

csM68K :: CsM68K
csM68K = CsM68K [csM68KOp]

-- | CsM68K spec
csM68KSpec :: Spec
csM68KSpec = describe "Storable CsM68K" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsM68K) ==
            1 + 7 + 8 * sizeOf (undefined :: CsM68KOp)
    it "has matching peek- and poke-implementations" $ property $
        \s@CsM68K{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getCsM68K `shouldReturn` csM68K
    -}