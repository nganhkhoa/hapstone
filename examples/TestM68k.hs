module Main where

import           Data.Word
import           Text.Printf
import           Numeric                        ( showHex )

import           Hapstone.Capstone
import           Hapstone.Internal.Capstone    as Capstone

m68k_code =
  [ 0x4c, 0x00, 0x54, 0x04, 0x48, 0xe7, 0xe0, 0x30, 0x4c
  , 0xdf, 0x0c, 0x07, 0xd4, 0x40, 0x87, 0x5a, 0x4e, 0x71
  , 0x02, 0xb4, 0xc0, 0xde, 0xc0, 0xde, 0x5c, 0x00, 0x1d
  , 0x80, 0x71, 0x12, 0x01, 0x23, 0xf2, 0x3c, 0x44, 0x22
  , 0x40, 0x49, 0x0e, 0x56, 0x54, 0xc5, 0xf2, 0x3c, 0x44
  , 0x00, 0x44, 0x7a, 0x00, 0x00, 0xf2, 0x00, 0x0a, 0x28
  , 0x4e, 0xb9, 0x00, 0x00, 0x00, 0x12, 0x4e, 0x75
  ]


print_insn_detail :: Capstone.Csh -> Capstone.CsInsn -> IO ()
print_insn_detail handle insn = putStrLn ("0x" ++ a ++ ":\t" ++ m ++ "\t" ++ o)
 where
  m = mnemonic insn
  o = opStr insn
  a = (showHex $ address insn) ""

all_tests =
  [ ( Disassembler { arch                     = Capstone.CsArchM68k
                   , modes                    = [CsModeBigEndian, CsModeM68k040]
                   , buffer                   = m68k_code
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "M68K"
    )
  ]

main :: IO ()
main = do
  mapM test_disasm all_tests
  pure ()
 where
  test_disasm (dis, platform) = do
    putStrLn $ replicate 16 '*'
    putStrLn $ "Platform: " ++ platform
    putStrLn $ "Code: " ++ to_hex (buffer dis)
    putStrLn "Disasm:"
    disasmIO $ dis

  to_hex code = unwords (map (printf "0x%02X") code)
