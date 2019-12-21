{-# LANGUAGE QuasiQuotes #-}

{- |
Copyright: (c) 2017-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Templates for generated Haskell source code files.
-}

module Summoner.Template.Haskell
       ( haskellFiles
       ) where

import NeatInterpolation (text)

import Summoner.Settings (Settings (..))
import Summoner.Tree (TreeFs (..))


haskellFiles :: Settings -> [TreeFs]
haskellFiles Settings{..} = concat
    [ [ Dir "src"       []              | settingsIsLib ]
    , [ Dir "x"         [exeFile]       | settingsIsExe ]
    , [ Dir "test"      [testFile]      | settingsTest  ]
    , [ Dir "bench"     [benchmarkFile] | settingsBench ]
    ]
  where
    exeFile :: TreeFs
    exeFile = File "Main.hs" $ if settingsIsLib then createExe else createOnlyExe

    createOnlyExe :: Text
    createOnlyExe =
        [text|
        module Main (main) where


        main :: IO ()
        main = putStrLn ("Hello, world!" :: String)
        |]

    createExe :: Text
    createExe =
        [text|
        module Main (main) where

        main :: IO ()
        main = pure ()
        |]

    testFile :: TreeFs
    testFile = File "Spec.hs"
        [text|
        module Main (main) where


        main :: IO ()
        main = putStrLn ("Test suite is not implemented" :: String)
        |]

    benchmarkFile :: TreeFs
    benchmarkFile = File "Main.hs"
        [text|
        module Main (main) where

        import Gauge.Main


        main :: IO ()
        main = defaultMain [bench "const" (whnf const ())]
        |]
