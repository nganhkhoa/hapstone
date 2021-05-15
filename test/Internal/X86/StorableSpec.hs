module Internal.X86.StorableSpec where

import Foreign
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Capstone (disasmSimpleIO, Disassembler(..), defaultAction)
import Hapstone.Internal.Capstone
import Hapstone.Internal.X86

import Internal.X86.Default

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.X86" $ do
    x86OpMemStructSpec
    csX86OpSpec
    csX86Spec
    x86DisasmSpec

getX86OpMemStruct :: IO X86OpMemStruct
getX86OpMemStruct = do
    ptr <- mallocArray (sizeOf x86OpMemStruct) :: IO (Ptr Word8)
    poke (castPtr ptr) $ fromEnum X86RegEax
    poke (plusPtr ptr 4) $ fromEnum X86RegEdi
    poke (plusPtr ptr 8) $ fromEnum X86RegDx
    poke (plusPtr ptr 12) (0x31507264 :: Int32)
    poke (plusPtr ptr 16) (0x3df1507264 :: Int64)
    peek (castPtr ptr) <* free ptr

x86OpMemStruct :: X86OpMemStruct
x86OpMemStruct =
    X86OpMemStruct X86RegEax X86RegEdi X86RegDx 0x31507264 0x3df1507264

-- | X86OpMemStruct spec
x86OpMemStructSpec :: Spec
x86OpMemStructSpec = describe "Storable X86OpMemStruct" $ do
    it "has a memory layout we can handle" $
        sizeOf (undefined :: X86OpMemStruct) == 4 * 4 + 8
    it "has matching peek- and poke-implementations" $ property $
        \s@X86OpMemStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getX86OpMemStruct `shouldReturn` x86OpMemStruct

getCsX86Op :: IO CsX86Op
getCsX86Op = do
    ptr <- mallocArray (sizeOf csX86Op) :: IO (Ptr Word8)
    poke (castPtr ptr) $ fromEnum X86OpImm
    poke (plusPtr ptr 8) (0x0123456789abcdef :: Int64)
    poke (plusPtr ptr 32) (2 :: Word8)
    poke (plusPtr ptr 33) (0 :: Word8)
    poke (plusPtr ptr 36) $ fromEnum X86AvxBcast4
    poke (plusPtr ptr 40) (1 :: Word8)
    peek (castPtr ptr) <* free ptr

csX86Op :: CsX86Op
csX86Op = CsX86Op (Imm 0x0123456789abcdef) 2 0 X86AvxBcast4 True

-- | CsX86ipsOp spec
csX86OpSpec :: Spec
csX86OpSpec = describe "Storable CsX86Op" $ do
    it "has a memory layout we can handle" $
        sizeOf (undefined :: CsX86Op) == 4 + 4 + 24 + 2 + 2 + 4 + 1 + 7
    it "has matching peek- and poke-implementations" $ property $
        \s ->
            alloca (\p -> poke p s >> (peek p :: IO CsX86Op)) `shouldReturn` s
    it "parses correctly" $ getCsX86Op `shouldReturn` csX86Op

getCsX86 :: IO CsX86
getCsX86 = do
    ptr <- mallocArray (sizeOf csX86) :: IO (Ptr Word8)
    pokeArray ptr ([ 0, 1, 2, 3 -- prefix
                   , 4, 5, 6, 7 -- opcode
                   , 0 -- rex
                   , 0x20 -- address_size
                   , 0x21 -- modrm
                   , 0 -- sib
                   ] :: [Word8])
    poke (plusPtr ptr 16) (0x01234567 :: Int64) -- disp
    poke (plusPtr ptr 24) $ fromEnum X86RegAl
    poke (plusPtr ptr 28) (2 :: Int8) -- sibScale
    poke (plusPtr ptr 32) $ fromEnum X86RegEdx
    poke (plusPtr ptr 36) $ fromEnum X86XopCcEq
    poke (plusPtr ptr 40) $ fromEnum X86SseCcEq
    poke (plusPtr ptr 44) $ fromEnum X86AvxCcEq
    poke (plusPtr ptr 48) (1 :: Word8) -- avxSae
    poke (plusPtr ptr 52) $ fromEnum X86AvxRmRu
    poke (plusPtr ptr 56) (0xDEADBEEF :: Word64) -- flags
    poke (plusPtr ptr 64) (1 :: Word8) -- op_count
    poke (plusPtr ptr 72) csX86Op
    poke (plusPtr ptr 456) $ CsX86Encoding 1 2 3 4 5
    peek (castPtr ptr) <* free ptr

csX86 :: CsX86
csX86 = CsX86 (Nothing, Just 1, Just 2, Just 3) [4, 5, 6, 7]
    0x0 0x20 0x21 Nothing (Just 0x01234567) X86RegAl 0x2 X86RegEdx
    X86XopCcEq X86SseCcEq X86AvxCcEq True X86AvxRmRu 0xDEADBEEF [csX86Op]
    $ CsX86Encoding 1 2 3 4 5

-- | CsX86 spec
csX86Spec :: Spec
csX86Spec = describe "Storable CsX86" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsX86) ==
            4 + 4 + 1 + 1 + 1 + 1 + 4 + 4 + 8 + 4 + 1 + 3 + 4 + 4 + 4 + 1 + 3 + 4 + 1 + 3 + 4 + 8 + 48 * 8 + 8
    it "has matching peek- and poke-implementations" $ property $
        \s@CsX86{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getCsX86 `shouldReturn` csX86

pushRBP = [0x55]
readRBP = CsX86Op {
    value = Reg X86RegRbp,
    size = 8,
    access = 1, -- READ
    avxBcast = X86AvxBcastInvalid,
    avxZeroOpmask = False
}
pushX86 = CsX86 {
    prefix = (Nothing, Nothing, Nothing, Nothing),
    opcode = pushRBP,
    rex = 0,
    addrSize = 8,
    modRM = 0,
    sib = Nothing,
    disp = Nothing, -- Is this right?
    sibIndex = X86RegInvalid, -- Scale is 0, What if this is supposed to be X86RegRbp?
    sibScale = 0,
    sibBase = X86RegInvalid,
    xopCc = X86XopCcInvalid,
    sseCc = X86SseCcInvalid,
    avxCc = X86AvxCcInvalid,
    avxSae = False, -- Should this be true?
    avxRm = X86AvxRmInvalid,
    flags = 0,
    operands = [readRBP],
    encoding = CsX86Encoding 0 0 0 0 0
}
pushDetail = CsDetail {
    regsRead = [fromIntegral $ fromEnum X86RegRsp],
    regsWrite = [fromIntegral $ fromEnum X86RegRsp],
    groups = [145],
    archInfo = Just $ X86 pushX86
}
pushInsn = Right [CsInsn {
    insnId = 0x24c, -- the internal ID of push
    address = 0x1000,
    bytes = pushRBP,
    mnemonic = "push",
    opStr = "rbp",
    Hapstone.Internal.Capstone.detail = Just pushDetail
}]

pushDis = Disassembler CsArchX86 [CsMode64] pushRBP 0x1000 1 True Nothing defaultAction

x86DisasmSpec :: Spec
x86DisasmSpec = describe "X86-64 Disasm" $ do
    it "disassembles `push rbp' correctly" $
        disasmSimpleIO pushDis `shouldReturn` pushInsn
