module Main where

import           Data.Word
import           Text.Printf
import           Numeric                        ( showHex )

import           Hapstone.Capstone
import           Hapstone.Internal.Capstone    as Capstone

arm_code =
  [ 0x09, 0x00, 0x38, 0xd5, 0xbf, 0x40, 0x00, 0xd5, 0x0c
  , 0x05, 0x13, 0xd5, 0x20, 0x50, 0x02, 0x0e, 0x20, 0xe4
  , 0x3d, 0x0f, 0x00, 0x18, 0xa0, 0x5f, 0xa2, 0x00, 0xae
  , 0x9e, 0x9f, 0x37, 0x03, 0xd5, 0xbf, 0x33, 0x03, 0xd5
  , 0xdf, 0x3f, 0x03, 0xd5, 0x21, 0x7c, 0x02, 0x9b, 0x21
  , 0x7c, 0x00, 0x53, 0x00, 0x40, 0x21, 0x4b, 0xe1, 0x0b
  , 0x40, 0xb9, 0x20, 0x04, 0x81, 0xda, 0x20, 0x08, 0x02
  , 0x8b, 0x10, 0x5b, 0xe8, 0x3c
  ]

print_insn_detail :: Capstone.Csh -> Capstone.CsInsn -> IO ()
print_insn_detail handle insn = putStrLn ("0x" ++ a ++ ":\t" ++ m ++ "\t" ++ o)
 where
  m = mnemonic insn
  o = opStr insn
  a = (showHex $ address insn) ""

all_tests =
  [ ( Disassembler { arch                     = Capstone.CsArchArm64
                   , modes                    = [Capstone.CsModeArm]
                   , buffer                   = arm_code
                   , addr                     = 0x80001000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "ARM-64"
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
