import Test.QuickCheck
import Test.Framework(defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2(testProperty)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Control.Applicative
import Control.Monad

import Data.Git.Storage.Object
import Data.Git.Storage.Loose
import Data.Git.Ref
import Data.Git.Types
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Calendar

import Data.Maybe

-- for arbitrary instance to generate only data that are writable
-- to disk. i.e. no deltas.
data ObjNoDelta = ObjNoDelta Object
    deriving (Eq)

instance Show ObjNoDelta where
    show (ObjNoDelta o) = show o

arbitraryBS size = B.pack . map fromIntegral <$> replicateM size (choose (0,255) :: Gen Int)
arbitraryBSno0 size = B.pack . map fromIntegral <$> replicateM size (choose (1,255) :: Gen Int)

arbitraryBSascii size = B.pack . map fromIntegral <$> replicateM size (choose (0x20,0x7f) :: Gen Int)
arbitraryBSnoangle size = B.pack . map fromIntegral <$> replicateM size (choose (0x40,0x7f) :: Gen Int)

instance Arbitrary Ref where
    arbitrary = fromBinary <$> arbitraryBS 20

arbitraryMsg = arbitraryBSno0 1
arbitraryLazy = L.fromChunks . (:[]) <$> arbitraryBS 40

arbitraryRefList :: Gen [Ref]
arbitraryRefList = replicateM 2 arbitrary

arbitraryEnt = liftM3 (,,) arbitrary (arbitraryBSno0 48) arbitrary
arbitraryEnts = choose (1,2) >>= \i -> replicateM i arbitraryEnt

instance Arbitrary TimeZone where
    arbitrary = hoursToTimeZone . rel <$> arbitrary
        where rel a = (a `mod` 24) - 12

instance Arbitrary UTCTime where
    arbitrary = UTCTime <$> (flip addDays b <$> choose (0, 365 * 40))
                        <*> (secondsToDiffTime <$> arbitrary)
        where b = fromGregorian 1970 1 1

arbitraryName = liftM4 (,,,) (arbitraryBSnoangle 16)
                             (arbitraryBSnoangle 16)
                             arbitrary
                             arbitrary

arbitraryObjTypeNoDelta = oneof [return TypeTree,return TypeBlob,return TypeCommit,return TypeTag]

arbitrarySmallList = frequency [ (2, return []), (1, resize 3 arbitrary) ]

instance Arbitrary Commit where
    arbitrary = Commit <$> arbitrary <*> arbitraryRefList <*> arbitraryName <*> arbitraryName <*> return Nothing <*> arbitrarySmallList <*> arbitraryMsg

instance Arbitrary CommitExtra where
    arbitrary = CommitExtra <$> arbitraryBSascii 80 <*> arbitraryMsg

instance Arbitrary Tree where
    arbitrary = Tree <$> arbitraryEnts

instance Arbitrary Blob where
    arbitrary = Blob <$> arbitraryLazy

instance Arbitrary Tag where
    arbitrary = Tag <$> arbitrary <*> arbitraryObjTypeNoDelta <*> arbitraryBSascii 20 <*> arbitraryName <*> arbitraryMsg

instance Arbitrary ObjNoDelta where
    arbitrary = ObjNoDelta <$> oneof
        [ toObject <$> (arbitrary :: Gen Commit)
        , toObject <$> (arbitrary :: Gen Tree)
        , toObject <$> (arbitrary :: Gen Blob)
        , toObject <$> (arbitrary :: Gen Tag)
        ]

prop_object_marshalling_id (ObjNoDelta obj) = obj `assertEq` (looseUnmarshall $ looseMarshall obj)
    where assertEq a b
            | show a == show b    = True
            | otherwise = error ("not equal:\n"  ++ show a ++ "\ngot: " ++ show b)

refTests =
    [ testProperty "hexadecimal" (marshEqual (fromHex . toHex))
    , testProperty "binary" (marshEqual (fromBinary . toBinary))
    ]
    where
        marshEqual t ref = ref == t ref

objTests =
    [ testProperty "unmarshall.marshall==id" prop_object_marshalling_id
    ]

main = defaultMain
    [ testGroup "ref marshalling" refTests
    , testGroup "object marshalling" objTests
    ]
