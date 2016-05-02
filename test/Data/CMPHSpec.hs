{-# LANGUAGE OverloadedStrings #-}
module Data.CMPHSpec where

import           Control.Monad         (guard)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Char
import qualified Data.CMPH             as CMPH
import qualified Data.List             as DL
import qualified Data.List.NonEmpty    as NEL
import           Data.Maybe            (isNothing)
import           Data.Monoid           ((<>))
import           Data.Ord              (comparing)
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Text             (Text)
import           Data.Text.Encoding    (decodeUtf8)
import           Data.Word
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck

main = hspec spec

letterGen = arbitrary `suchThat` (\c -> c /= '\NUL')
stringGen = BS8.pack <$> listOf letterGen
stringListGen = listOf stringGen

unique s = Set.size (Set.fromList s) == length s

spec = modifyMaxSuccess (const 1000) $  describe "cmph" $ do
  it "gives up early if there's a NUL in there" $ do
    property
    $ forAll (stringListGen `suchThat` (not . null)) $ \strings' -> do
      let strings = map (\a  -> a <> "\NUL" <> a) strings'
      ph <- CMPH.fromList strings
      fmap (const "cmph") ph `shouldSatisfy` isNothing

  it "does not generate dupes if the input contains no nulls " $ do
    property
    $ forAll stringListGen $ \strings -> do
      --      print ("strings", strings)
      Just ph <-  CMPH.fromList strings
      --      print ("builtstrings", strings)
      -- we unique them here: it's ok to pass in dupes, but for
      -- correctness purposes, it's the uniqued size we care about
      results <- mapM (CMPH.hash ph) (Set.toList $ Set.fromList strings)
      --      print ("results", DL.sortBy (comparing snd) $ zip strings results)
      DL.sort results `shouldSatisfy` unique
