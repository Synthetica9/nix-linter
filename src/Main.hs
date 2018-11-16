{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Prelude                          hiding (log)

import           Control.Arrow                    ((&&&))
import           Control.Monad                    (forever)
import           Data.Foldable                    (for_, toList)
import           Data.Maybe                       (catMaybes)
import           Data.Traversable                 (for)
import           System.Environment
import           System.IO                        (hPutStrLn, isEOF, stderr)
import           System.IO.Unsafe                 (unsafeInterleaveIO)

import qualified Data.ByteString.Lazy             as B


import           Nix.Parser
import           Nix.Pretty

import           Data.Set                         (Set (..), (\\))
import qualified Data.Set                         as S

import           Data.Aeson                       (encode, toJSON)

import           Nix.Linter
import           Nix.Linter.Types
import           Nix.Linter.Utils

import           System.Console.CmdArgs
import           System.Console.CmdArgs.Verbosity

data NixLinter = NixLinter
  {
    check     :: [OffenseCategory]
  , noCheck   :: [OffenseCategory]
  , json      :: Bool
  , file_list :: Bool
  , out       :: FilePath
  , files     :: [FilePath]
  } deriving (Show, Data, Typeable)

nixLinter = NixLinter
  {
    check = def &= help "checks to enable"
  , noCheck = def &= help "checks to disable"
  , json  = def &= help "Use JSON output"
  , file_list = def &= help "Read files to process (like xargs)"
  , out = def &= help "File to output to" &= typ "FILE"
  , files = def &= args &= typ "FILES"
  } &= verbosity &= details (mkChecksHelp Nix.Linter.checks)

getChecks :: [AvailableCheck] -> NixLinter -> Either String Check
getChecks avail (NixLinter{..}) = let
    defaults = S.fromList $ category <$> filter defaultEnabled avail
    explitEnabled = S.fromList check
    explitDisabled = S.fromList noCheck
    conflicting = S.intersection explitEnabled explitDisabled
    finalEnabled = defaults `S.union` explitDisabled `S.difference` explitDisabled
    lookupTable = (category &&& baseCheck) <$> avail
    getCheck = flip lookup lookupTable
    checks = catMaybes (getCheck <$> toList finalEnabled)
  in if conflicting /= S.empty
    then Left $ "Checks both enabled and disabled: " ++ show conflicting
    else Right $ combineChecks checks


mkChecksHelp :: [AvailableCheck] -> [String]
mkChecksHelp xs = "Available checks:" : (mkDetails <$> xs) where
  mkDetails (AvailableCheck{..}) = "    " ++ show category ++ mkDis defaultEnabled
  mkDis False = " (disabled by default)"
  mkDis _     = ""

main =  cmdArgs nixLinter >>= runChecks

log :: String -> IO ()
log = hPutStrLn stderr

stdinContents :: IO [String]
stdinContents = do
  eof <- isEOF
  if eof
    then pure []
    else (:) <$> getLine <*> (unsafeInterleaveIO $ stdinContents)

runChecks :: NixLinter -> IO ()
runChecks (opts@NixLinter{..}) = do
  let enabledChecks = getChecks checks opts
  combined <- case enabledChecks of
    Left err -> fail err
    Right cs -> pure cs

  files' <- if file_list
    then
      if null files
        then stdinContents
        else (concat :: [[String]] -> [FilePath]) <$> lines <$$> for files readFile
    else
      pure files

  (results :: [Offense]) <- fmap concat $ for files' $ \f -> unsafeInterleaveIO $ do
    whenLoud $ log $ "Parsing file... " ++ f
    parsed <- parseNixFileLoc f
    case parsed of
      Success result -> pure $ combined result
      Failure why    -> (log $ "Parse failed for " ++ f ++ "\n" ++ show why) >> pure []

  if json
    then B.putStr $ encode $ toJSON results
    else for_ results (putStrLn . prettyOffense)
