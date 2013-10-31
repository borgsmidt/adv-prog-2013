--
-- Skeleton for Salsa parser
-- To be used at the exam for Advanced Programming, B1-2013
--

module SalsaParser where

import SalsaAst

parseString :: String -> Either Error Program

parseFile :: FilePath -> IO (Either Error Program)
