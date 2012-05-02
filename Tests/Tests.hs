import Test.QuickCheck
import Test.Framework(defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2(testProperty)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Control.Applicative
import Control.Monad

import Data.Git.Object
import Data.Git.Loose
import Data.Git.Ref
import Data.Git.Types
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Calendar

-- for arbitrary instance to generate only data that are writable
-- to disk. i.e. no deltas.
--data ObjNoDelta = ObjNoDelta Object

--instance Show ObjNoDelta where
--	show (ObjNoDelta o) = show o

arbitraryBS size = B.pack . map fromIntegral <$> replicateM size (choose (0,255) :: Gen Int)
arbitraryBSno0 size = B.pack . map fromIntegral <$> replicateM size (choose (1,255) :: Gen Int)

arbitraryBSascii size = B.pack . map fromIntegral <$> replicateM size (choose (0x20,0x7f) :: Gen Int)
arbitraryBSnoangle size = B.pack . map fromIntegral <$> replicateM size (choose (0x40,0x7f) :: Gen Int)

instance Arbitrary Ref where
	arbitrary = fromBinary <$> arbitraryBS 20

arbitraryMsg = arbitraryBSno0 128
arbitraryLazy = L.fromChunks . (:[]) <$> arbitraryBS 4096

arbitraryRefList :: Gen [Ref]
arbitraryRefList = replicateM 2 arbitrary

arbitraryEnt = liftM3 (,,) arbitrary (arbitraryBSno0 48) arbitrary
arbitraryEnts = choose (1,100) >>= \i -> replicateM i arbitraryEnt

instance Arbitrary TimeZone where
    arbitrary = hoursToTimeZone <$> arbitrary 

instance Arbitrary UTCTime where
    arbitrary = UTCTime <$> (flip addDays b <$> choose (0, 365 * 40))
                        <*> (secondsToDiffTime <$> arbitrary)
        where b = fromGregorian 1970 1 1
arbitraryName = liftM4 (,,,) (arbitraryBSnoangle 16)
                             (arbitraryBSnoangle 16)
                             arbitrary
                             arbitrary

arbitraryObjTypeNoDelta = oneof [return TypeTree,return TypeBlob,return TypeCommit,return TypeTag]

instance Arbitrary Commit where
	arbitrary = Commit <$> arbitrary <*> arbitraryRefList <*> arbitraryName <*> arbitraryName <*> arbitraryMsg

instance Arbitrary Tree where
	arbitrary = Tree <$> arbitraryEnts

instance Arbitrary Blob where
	arbitrary = Blob <$> arbitraryLazy

instance Arbitrary Tag where
	arbitrary = Tag <$> arbitrary <*> arbitraryObjTypeNoDelta <*> arbitraryBSascii 20 <*> arbitraryName <*> arbitraryMsg

{-
instance Arbitrary Object where
	arbitrary = undefined

instance Arbitrary ObjNoDelta where
	arbitrary = ObjNoDelta <$> oneof
		[ liftM5 Commit arbitrary arbitraryRefList arbitraryName arbitraryName arbitraryMsg
		, liftM Tree arbitraryEnts
		, liftM Blob arbitraryLazy
		, liftM5 Tag
		]
-}

--prop_object_marshalling_id (ObjNoDelta obj) = obj == (looseUnmarshall $ looseMarshall obj)

refTests =
	[ testProperty "hexadecimal" (marshEqual (fromHex . toHex))
	, testProperty "binary" (marshEqual (fromBinary . toBinary))
	]
	where
		marshEqual t ref = ref == t ref

objTests =
	[ -- testProperty "unmarshall.marshall==id" prop_object_marshalling_id
	]

main = defaultMain
	[ testGroup "ref marshalling" refTests
	, testGroup "object marshalling" objTests
	]
