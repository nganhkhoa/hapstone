module Internal.Default where

import Data.Char (chr)

import Foreign
import Foreign.C.Types

import Hapstone.Internal.Capstone
import Internal.Arm64.Default ()
import Internal.Arm.Default ()
import Internal.Mips.Default ()
import Internal.Ppc.Default ()
import Internal.Sparc.Default ()
import Internal.SystemZ.Default ()
import Internal.X86.Default ()
import Internal.XCore.Default ()

import Test.QuickCheck
import Test.QuickCheck.Instances

-- generators for our datatypes

instance Arbitrary CsArch where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary CsSupport where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary CsMode where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary CsOption where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary CsOptionState where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary CsOperand where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary CsGroup where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary CsSkipdataStruct where
    arbitrary = CsSkipdataStruct <$> (filter (/='\NUL') <$> arbitrary) <*>
        pure nullFunPtr <*> pure nullPtr

instance Arbitrary ArchInfo where
    arbitrary = oneof
        [ X86 <$> arbitrary
        , Arm64 <$> arbitrary
        , Arm <$> arbitrary
        , Mips <$> arbitrary
        , Ppc <$> arbitrary
        , Sparc <$> arbitrary
        , SysZ <$> arbitrary
        , XCore <$> arbitrary
        ]

instance Arbitrary CsDetail where
    arbitrary = CsDetail <$> (take 12 <$> arbitrary) <*>
        (take 20 <$> arbitrary) <*> (take 8 <$> arbitrary) <*> pure Nothing

instance Arbitrary CsInsn where
    arbitrary = CsInsn <$> arbitrary <*> arbitrary <*>
        (take 16 <$> arbitrary) <*>
        (take 31 <$> listOf (chr <$> elements [1..255])) <*>
        (take 159 <$> listOf (chr <$> elements [1..255])) <*> pure Nothing
