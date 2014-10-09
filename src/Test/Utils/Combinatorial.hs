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

-- | A module providing combinatorial testing utilities.
module Test.Utils.Combinatorial(
       combinatorial2,
       combinatorial3,
       combinatorial4,
       combinatorial5,
       combinatorial6
       ) where

import Test.HUnitPlus.Base

-- | Generate a set of tests from a generator function and two lists
-- of possible values of arguments.
combinatorial2 :: (ty1 -> ty2 -> Test)
               -- ^ Function that produces a @Test@ from two arguments.
               -> [ty1]
               -- ^ Possible values of the first argument.
               -> [ty2]
               -- ^ Possible values of the second argument.
               -> [Test]
               -- ^ A list of tests
combinatorial2 mkTest args1 args2 =
  foldr (\arg1 tests1 ->
          foldr (\arg2 tests2 -> mkTest arg1 arg2 : tests2)
                tests1 args2)
        [] args1

-- | Generate a set of tests from a generator function and three lists
-- of possible values of arguments.
combinatorial3 :: (ty1 -> ty2 -> ty3 -> Test)
               -- ^ Function that produces a @Test@ from three arguments.
               -> [ty1]
               -- ^ Possible values of the first argument.
               -> [ty2]
               -- ^ Possible values of the second argument.
               -> [ty3]
               -- ^ Possible values of the third argument.
               -> [Test]
               -- ^ A list of tests
combinatorial3 mkTest args1 args2 args3 =
  foldr (\arg1 tests1 ->
          foldr (\arg2 tests2 ->
                  foldr (\arg3 tests3 -> mkTest arg1 arg2 arg3 : tests3)
                        tests2 args3)
                tests1 args2)
        [] args1

-- | Generate a set of tests from a generator function and four lists
-- of possible values of arguments.
combinatorial4 :: (ty1 -> ty2 -> ty3 -> ty4 -> Test)
               -- ^ Function that produces a @Test@ from four arguments.
               -> [ty1]
               -- ^ Possible values of the first argument.
               -> [ty2]
               -- ^ Possible values of the second argument.
               -> [ty3]
               -- ^ Possible values of the third argument.
               -> [ty4]
               -- ^ Possible values of the fourth argument.
               -> [Test]
               -- ^ A list of tests
combinatorial4 mkTest args1 args2 args3 args4 =
  foldr (\arg1 tests1 ->
          foldr (\arg2 tests2 ->
                  foldr (\arg3 tests3 ->
                          foldr (\arg4 tests4 ->
                                  mkTest arg1 arg2 arg3 arg4 : tests4)
                                tests3 args4)
                        tests2 args3)
                tests1 args2)
        [] args1

-- | Generate a set of tests from a generator function and five lists
-- of possible values of arguments.
combinatorial5 :: (ty1 -> ty2 -> ty3 -> ty4 -> ty5 -> Test)
               -- ^ Function that produces a @Test@ from five arguments.
               -> [ty1]
               -- ^ Possible values of the first argument.
               -> [ty2]
               -- ^ Possible values of the second argument.
               -> [ty3]
               -- ^ Possible values of the third argument.
               -> [ty4]
               -- ^ Possible values of the fourth argument.
               -> [ty5]
               -- ^ Possible values of the fifth argument.
               -> [Test]
               -- ^ A list of tests
combinatorial5 mkTest args1 args2 args3 args4 args5 =
  foldr (\arg1 tests1 ->
          foldr (\arg2 tests2 ->
                  foldr (\arg3 tests3 ->
                          foldr (\arg4 tests4 ->
                                  foldr (\arg5 tests5 ->
                                          mkTest arg1 arg2 arg3 arg4 arg5 :
                                          tests5)
                                        tests4 args5)
                                tests3 args4)
                        tests2 args3)
                tests1 args2)
        [] args1

-- | Generate a set of tests from a generator function and six lists
-- of possible values of arguments.
combinatorial6 :: (ty1 -> ty2 -> ty3 -> ty4 -> ty5 -> ty6 -> Test)
               -- ^ Function that produces a @Test@ from five arguments.
               -> [ty1]
               -- ^ Possible values of the first argument.
               -> [ty2]
               -- ^ Possible values of the second argument.
               -> [ty3]
               -- ^ Possible values of the third argument.
               -> [ty4]
               -- ^ Possible values of the fourth argument.
               -> [ty5]
               -- ^ Possible values of the fifth argument.
               -> [ty6]
               -- ^ Possible values of the sixth argument.
               -> [Test]
               -- ^ A list of tests
combinatorial6 mkTest args1 args2 args3 args4 args5 args6 =
  foldr (\arg1 tests1 ->
          foldr (\arg2 tests2 ->
                  foldr (\arg3 tests3 ->
                          foldr (\arg4 tests4 ->
                                  foldr (\arg5 tests5 ->
                                          foldr (\arg6 tests6 ->
                                                  mkTest arg1 arg2 arg3
                                                         arg4 arg5 arg6 :
                                                         tests6)
                                                tests5 args6)
                                        tests4 args5)
                                tests3 args4)
                        tests2 args3)
                tests1 args2)
        [] args1
