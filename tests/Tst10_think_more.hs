{-# LANGUAGE ViewPatterns #-}

import Data.Monoid

type Option = String
data Optioner = Op {getOpt::[(String, String)]}

instance Monoid Optioner where
    mempty = Op []
    Op l `mappend` Op r = Op $ l ++ r

mkOpt :: Option -> String -> Optioner
mkOpt o v = Op $ [(o,v)]

notSupported :: Option -> Either String Option
notSupported  o = Left $ show o ++ " not supported"

getOption :: Option -> Optioner -> Either String Option
getOption o (getOpt -> []) = notSupported o
getOption o op = maybe (notSupported o) Right . lookup o . getOpt $ op

tstOp = mkOpt "a" "a-opt" <> mkOpt "b" "b-opt"

-- try to make typeclass "Optionable" and TF?