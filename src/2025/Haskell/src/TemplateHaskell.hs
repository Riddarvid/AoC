{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TemplateHaskell () where
import           AoCUtils.Geometry     (Point2)
import           Data.Function.Memoize (deriveMemoizable)

deriveMemoizable ''Point2
