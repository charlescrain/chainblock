{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ChainBlock.Logging where

import           Control.Monad.Logger
import qualified Data.ByteString.Char8 as BC
import           Data.Monoid           ((<>))
-- import           Data.Monoid           ((<>))
import           Data.Text             (unpack)
import           System.Log.FastLogger (fromLogStr)
import           Text.PrettyPrint      hiding ((<>))

padString :: Int -> Char -> String -> String
padString targetLen c str = str <> replicate (max 0 (targetLen - length str)) c

logMsg :: Loc
       -> LogSource
       -> LogLevel
       -> LogStr
       -> IO ()
logMsg Loc{..} lsrc lvl logstr =
  putStrLn $ renderStyle style{lineLength=300} $ hsep
    [ (text . padString 10 ' ' . show $ lvl)
    , (text " | ")
    , (text $ padString 20 ' ' loc_module)
    , (text " | ")
    , (text $ padString 25 ' ' loc_filename <> " " <> (show . fst $ loc_start))
    , (text " | ")
    , (text . padString 15 ' ' . unpack $ lsrc)
    , (text " | ")
    , (text . padString 20 ' ' $ BC.unpack . fromLogStr $ logstr)
    -- <> "  |  "
    -- <> loc_filename
    -- <> "  |  "
    -- <> show . fst $ loc_start
    -- <> "  |  "
    -- <> unpack lsrc
    -- <> "  |  "
    -- <> (BC.unpack . fromLogStr $ logstr)
    ]
