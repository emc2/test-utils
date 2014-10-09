-- Copyright (c) 2014 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
{-# OPTIONS_GHC -Wall -Werror #-}

module Test.Utils.Enumeration(
       fromEncoding,
       fromEncodingWithLimit,
       assertionFromEncoding,
       assertionFromEncodingWithLimit,
       fromEncodings2,
       fromEncodingsWithLimit2,
       assertionFromEncodings2,
       assertionFromEncodingsWithLimit2,
       fromEncodings3,
       fromEncodingsWithLimit3,
       assertionFromEncodings3,
       assertionFromEncodingsWithLimit3,
       fromEncodings4,
       fromEncodingsWithLimit4,
       assertionFromEncodings4,
       assertionFromEncodingsWithLimit4,
       fromEncodings5,
       fromEncodingsWithLimit5,
       assertionFromEncodings5,
       assertionFromEncodingsWithLimit5
       ) where

import Data.ArithEncode
import Test.HUnitPlus.Base

-- | Generate tests from an @Encoding@ of argument values.
fromEncoding :: (ty -> Test)
             -- ^ Test-generation function.
             -> Encoding ty
             -- ^ An @Encoding@ for the argument to the
             -- test-generation function.
             -> [Test]
fromEncoding mkTest enc =
  case size enc of
    Just finitesize -> map (mkTest . decode enc) [0..finitesize - 1]
    Nothing -> error "Creating tests from infinite encoding"

-- | Generate tests from an @Encoding@ of argument values, up to a
-- maximum number.
fromEncodingWithLimit :: (ty -> Test)
                      -- ^ Test-generation function.
                      -> Encoding ty
                      -- ^ An @Encoding@ for the argument to the
                      -- test-generation function.
                      -> Integer
                      -- ^ The maximum number of tests to generate.
                      -> [Test]
fromEncodingWithLimit mkTest enc limit =
  case size enc of
    Just finitesize | finitesize < limit ->
      map (mkTest . decode enc) [0..finitesize - 1]
    _ -> map (mkTest . decode enc) [0..limit]

-- | Generate test assertions from an @Encoding@ of argument values.
assertionFromEncoding :: (ty -> Assertion)
                      -- ^ Test-generation function.
                      -> Encoding ty
                      -- ^ An @Encoding@ for the argument to the
                      -- test-generation function.
                      -> Assertion
assertionFromEncoding mkTest enc =
  case size enc of
    Just finitesize -> mapM_ (mkTest . decode enc) [0..finitesize - 1]
    Nothing -> error "Creating tests from infinite encoding"

-- | Generate test assertions from an @Encoding@ of argument values.
assertionFromEncodingWithLimit :: (ty -> Assertion)
                               -- ^ Test-generation function.
                               -> Encoding ty
                               -- ^ An @Encoding@ for the argument to the
                               -- test-generation function.
                               -> Integer
                               -- ^ The maximum number of tests to generate.
                               -> Assertion
assertionFromEncodingWithLimit mkTest enc limit =
  case size enc of
    Just finitesize | finitesize < limit ->
      mapM_ (mkTest . decode enc) [0..finitesize - 1]
    _ -> mapM_ (mkTest . decode enc) [0..limit]

-- | Generate tests from @Encoding@s of two argument values.
fromEncodings2 :: (ty1 -> ty2 -> Test)
               -- ^ Test-generation function.
               -> Encoding ty1
               -- ^ An @Encoding@ for the first argument to the
               -- test-generation function.
               -> Encoding ty2
               -- ^ An @Encoding@ for the second argument to the
               -- test-generation function.
               -> [Test]
fromEncodings2 mkTest enc1 enc2 =
  fromEncoding (uncurry mkTest) (pair enc1 enc2)

-- | Generate tests from @Encoding@s of two argument values, up to a
-- maximum number.
fromEncodingsWithLimit2 :: (ty1 -> ty2 -> Test)
                        -- ^ Test-generation function.
                        -> Encoding ty1
                        -- ^ An @Encoding@ for the first argument to the
                        -- test-generation function.
                        -> Encoding ty2
                        -- ^ An @Encoding@ for the second argument to the
                        -- test-generation function.
                        -> Integer
                        -- ^ The maximum number of tests to generate.
                        -> [Test]
fromEncodingsWithLimit2 mkTest enc1 enc2 =
  fromEncodingWithLimit (uncurry mkTest) (pair enc1 enc2)

-- | Generate assertions from @Encoding@s of two argument values.
assertionFromEncodings2 :: (ty1 -> ty2 -> Assertion)
                        -- ^ Test-generation function.
                        -> Encoding ty1
                        -- ^ An @Encoding@ for the first argument to the
                        -- test-generation function.
                        -> Encoding ty2
                        -- ^ An @Encoding@ for the second argument to the
                        -- test-generation function.
                        -> Assertion
assertionFromEncodings2 mkTest enc1 enc2 =
  assertionFromEncoding (uncurry mkTest) (pair enc1 enc2)

-- | Generate tests from @Encoding@s of two argument values, up to a
-- maximum number.
assertionFromEncodingsWithLimit2 :: (ty1 -> ty2 -> Assertion)
                                 -- ^ Test-generation function.
                                 -> Encoding ty1
                                 -- ^ An @Encoding@ for the first
                                 -- argument to the test-generation
                                 -- function.
                                 -> Encoding ty2
                                 -- ^ An @Encoding@ for the second
                                 -- argument to the test-generation
                                 -- function.
                                 -> Integer
                                 -- ^ The maximum number of tests to generate.
                                 -> Assertion
assertionFromEncodingsWithLimit2 mkTest enc1 enc2 =
  assertionFromEncodingWithLimit (uncurry mkTest) (pair enc1 enc2)

-- | Generate tests from @Encoding@s of three argument values.
fromEncodings3 :: (ty1 -> ty2 -> ty3 -> Test)
               -- ^ Test-generation function.
               -> Encoding ty1
               -- ^ An @Encoding@ for the first argument to the
               -- test-generation function.
               -> Encoding ty2
               -- ^ An @Encoding@ for the second argument to the
               -- test-generation function.
               -> Encoding ty3
               -- ^ An @Encoding@ for the third argument to the
               -- test-generation function.
               -> [Test]
fromEncodings3 mkTest enc1 enc2 enc3 =
  fromEncoding (\(a, b, c) -> mkTest a b c) (triple enc1 enc2 enc3)

-- | Generate tests from @Encoding@s of three argument values, up to a
-- maximum number.
fromEncodingsWithLimit3 :: (ty1 -> ty2 -> ty3 -> Test)
                        -- ^ Test-generation function.
                        -> Encoding ty1
                        -- ^ An @Encoding@ for the first argument to the
                        -- test-generation function.
                        -> Encoding ty2
                        -- ^ An @Encoding@ for the second argument to the
                        -- test-generation function.
                        -> Encoding ty3
                        -- ^ An @Encoding@ for the third argument to the
                        -- test-generation function.
                        -> Integer
                        -- ^ The maximum number of tests to generate.
                        -> [Test]
fromEncodingsWithLimit3 mkTest enc1 enc2 enc3 =
  fromEncodingWithLimit (\(a, b, c) -> mkTest a b c) (triple enc1 enc2 enc3)

-- | Generate assertions from @Encoding@s of three argument values.
assertionFromEncodings3 :: (ty1 -> ty2 -> ty3 -> Assertion)
                        -- ^ Test-generation function.
                        -> Encoding ty1
                        -- ^ An @Encoding@ for the first argument to the
                        -- test-generation function.
                        -> Encoding ty2
                        -- ^ An @Encoding@ for the second argument to the
                        -- test-generation function.
                        -> Encoding ty3
                        -- ^ An @Encoding@ for the third argument to the
                        -- test-generation function.
                        -> Assertion
assertionFromEncodings3 mkTest enc1 enc2 enc3 =
  assertionFromEncoding (\(a, b, c) -> mkTest a b c) (triple enc1 enc2 enc3)

-- | Generate tests from @Encoding@s of three argument values, up to a
-- maximum number.
assertionFromEncodingsWithLimit3 :: (ty1 -> ty2 -> ty3 -> Assertion)
                                 -- ^ Test-generation function.
                                 -> Encoding ty1
                                 -- ^ An @Encoding@ for the first
                                 -- argument to the test-generation
                                 -- function.
                                 -> Encoding ty2
                                 -- ^ An @Encoding@ for the second
                                 -- argument to the test-generation
                                 -- function.
                                 -> Encoding ty3
                                 -- ^ An @Encoding@ for the third
                                 -- argument to the test-generation
                                 -- function.
                                 -> Integer
                                 -- ^ The maximum number of tests to generate.
                                 -> Assertion
assertionFromEncodingsWithLimit3 mkTest enc1 enc2 enc3 =
  assertionFromEncodingWithLimit (\(a, b, c) -> mkTest a b c)
                                 (triple enc1 enc2 enc3)

-- | Generate tests from @Encoding@s of four argument values.
fromEncodings4 :: (ty1 -> ty2 -> ty3 -> ty4 -> Test)
               -- ^ Test-generation function.
               -> Encoding ty1
               -- ^ An @Encoding@ for the first argument to the
               -- test-generation function.
               -> Encoding ty2
               -- ^ An @Encoding@ for the second argument to the
               -- test-generation function.
               -> Encoding ty3
               -- ^ An @Encoding@ for the third argument to the
               -- test-generation function.
               -> Encoding ty4
               -- ^ An @Encoding@ for the fourth argument to the
               -- test-generation function.
               -> [Test]
fromEncodings4 mkTest enc1 enc2 enc3 enc4 =
  fromEncoding (\(a, b, c, d) -> mkTest a b c d) (quad enc1 enc2 enc3 enc4)

-- | Generate tests from @Encoding@s of four argument values, up to a
-- maximum number.
fromEncodingsWithLimit4 :: (ty1 -> ty2 -> ty3 -> ty4 -> Test)
                        -- ^ Test-generation function.
                        -> Encoding ty1
                        -- ^ An @Encoding@ for the first argument to the
                        -- test-generation function.
                        -> Encoding ty2
                        -- ^ An @Encoding@ for the second argument to the
                        -- test-generation function.
                        -> Encoding ty3
                        -- ^ An @Encoding@ for the third argument to the
                        -- test-generation function.
                        -> Encoding ty4
                        -- ^ An @Encoding@ for the fourth argument to the
                        -- test-generation function.
                        -> Integer
                        -- ^ The maximum number of tests to generate.
                        -> [Test]
fromEncodingsWithLimit4 mkTest enc1 enc2 enc3 enc4 =
  fromEncodingWithLimit (\(a, b, c, d) -> mkTest a b c d)
                        (quad enc1 enc2 enc3 enc4)

-- | Generate assertions from @Encoding@s of four argument values.
assertionFromEncodings4 :: (ty1 -> ty2 -> ty3 -> ty4 -> Assertion)
                        -- ^ Test-generation function.
                        -> Encoding ty1
                        -- ^ An @Encoding@ for the first argument to the
                        -- test-generation function.
                        -> Encoding ty2
                        -- ^ An @Encoding@ for the second argument to the
                        -- test-generation function.
                        -> Encoding ty3
                        -- ^ An @Encoding@ for the third argument to the
                        -- test-generation function.
                        -> Encoding ty4
                        -- ^ An @Encoding@ for the fourth argument to the
                        -- test-generation function.
                        -> Assertion
assertionFromEncodings4 mkTest enc1 enc2 enc3 enc4 =
  assertionFromEncoding (\(a, b, c, d) -> mkTest a b c d)
                        (quad enc1 enc2 enc3 enc4)

-- | Generate tests from @Encoding@s of four argument values, up to a
-- maximum number.
assertionFromEncodingsWithLimit4 :: (ty1 -> ty2 -> ty3 -> ty4 -> Assertion)
                                 -- ^ Test-generation function.
                                 -> Encoding ty1
                                 -- ^ An @Encoding@ for the first
                                 -- argument to the test-generation
                                 -- function.
                                 -> Encoding ty2
                                 -- ^ An @Encoding@ for the second
                                 -- argument to the test-generation
                                 -- function.
                                 -> Encoding ty3
                                 -- ^ An @Encoding@ for the third
                                 -- argument to the test-generation
                                 -- function.
                                 -> Encoding ty4
                                 -- ^ An @Encoding@ for the fourth
                                 -- argument to the test-generation
                                 -- function.
                                 -> Integer
                                 -- ^ The maximum number of tests to generate.
                                 -> Assertion
assertionFromEncodingsWithLimit4 mkTest enc1 enc2 enc3 enc4 =
  assertionFromEncodingWithLimit (\(a, b, c, d) -> mkTest a b c d)
                                 (quad enc1 enc2 enc3 enc4)

-- | Generate tests from @Encoding@s of five argument values.
fromEncodings5 :: (ty1 -> ty2 -> ty3 -> ty4 -> ty5 -> Test)
               -- ^ Test-generation function.
               -> Encoding ty1
               -- ^ An @Encoding@ for the first argument to the
               -- test-generation function.
               -> Encoding ty2
               -- ^ An @Encoding@ for the second argument to the
               -- test-generation function.
               -> Encoding ty3
               -- ^ An @Encoding@ for the third argument to the
               -- test-generation function.
               -> Encoding ty4
               -- ^ An @Encoding@ for the fourth argument to the
               -- test-generation function.
               -> Encoding ty5
               -- ^ An @Encoding@ for the fifth argument to the
               -- test-generation function.
               -> [Test]
fromEncodings5 mkTest enc1 enc2 enc3 enc4 enc5 =
  fromEncoding (\(a, b, c, d, e) -> mkTest a b c d e)
               (quint enc1 enc2 enc3 enc4 enc5)

-- | Generate tests from @Encoding@s of five argument values, up to a
-- maximum number.
fromEncodingsWithLimit5 :: (ty1 -> ty2 -> ty3 -> ty4 -> ty5 -> Test)
                        -- ^ Test-generation function.
                        -> Encoding ty1
                        -- ^ An @Encoding@ for the first argument to the
                        -- test-generation function.
                        -> Encoding ty2
                        -- ^ An @Encoding@ for the second argument to the
                        -- test-generation function.
                        -> Encoding ty3
                        -- ^ An @Encoding@ for the third argument to the
                        -- test-generation function.
                        -> Encoding ty4
                        -- ^ An @Encoding@ for the fourth argument to the
                        -- test-generation function.
                        -> Encoding ty5
                        -- ^ An @Encoding@ for the fifth argument to the
                        -- test-generation function.
                        -> Integer
                        -- ^ The maximum number of tests to generate.
                        -> [Test]
fromEncodingsWithLimit5 mkTest enc1 enc2 enc3 enc4 enc5 =
  fromEncodingWithLimit (\(a, b, c, d, e) -> mkTest a b c d e)
                        (quint enc1 enc2 enc3 enc4 enc5)

-- | Generate assertions from @Encoding@s of five argument values.
assertionFromEncodings5 :: (ty1 -> ty2 -> ty3 -> ty4 -> ty5 -> Assertion)
                        -- ^ Test-generation function.
                        -> Encoding ty1
                        -- ^ An @Encoding@ for the first argument to the
                        -- test-generation function.
                        -> Encoding ty2
                        -- ^ An @Encoding@ for the second argument to the
                        -- test-generation function.
                        -> Encoding ty3
                        -- ^ An @Encoding@ for the third argument to the
                        -- test-generation function.
                        -> Encoding ty4
                        -- ^ An @Encoding@ for the fourth argument to the
                        -- test-generation function.
                        -> Encoding ty5
                        -- ^ An @Encoding@ for the fifth argument to the
                        -- test-generation function.
                        -> Assertion
assertionFromEncodings5 mkTest enc1 enc2 enc3 enc4 enc5 =
  assertionFromEncoding (\(a, b, c, d, e) -> mkTest a b c d e)
                        (quint enc1 enc2 enc3 enc4 enc5)

-- | Generate tests from @Encoding@s of five argument values, up to a
-- maximum number.
assertionFromEncodingsWithLimit5 :: (ty1 -> ty2 -> ty3 -> ty4 -> ty5 ->
                                     Assertion)
                                 -- ^ Test-generation function.
                                 -> Encoding ty1
                                 -- ^ An @Encoding@ for the first
                                 -- argument to the test-generation
                                 -- function.
                                 -> Encoding ty2
                                 -- ^ An @Encoding@ for the second
                                 -- argument to the test-generation
                                 -- function.
                                 -> Encoding ty3
                                 -- ^ An @Encoding@ for the third
                                 -- argument to the test-generation
                                 -- function.
                                 -> Encoding ty4
                                 -- ^ An @Encoding@ for the fourth
                                 -- argument to the test-generation
                                 -- function.
                                 -> Encoding ty5
                                 -- ^ An @Encoding@ for the fifth
                                 -- argument to the test-generation
                                 -- function.
                                 -> Integer
                                 -- ^ The maximum number of tests to generate.
                                 -> Assertion
assertionFromEncodingsWithLimit5 mkTest enc1 enc2 enc3 enc4 enc5 =
  assertionFromEncodingWithLimit (\(a, b, c, d, e) -> mkTest a b c d e)
                                 (quint enc1 enc2 enc3 enc4 enc5)
