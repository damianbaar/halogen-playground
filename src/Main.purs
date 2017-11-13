module Main where

import Prelude

import Book.Validation (Person, PhoneType(..), address, showFullName, fullNameEither, person, phoneNumber, validatePerson)
import Component (component)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, error, catchException, throwException)
import Data.Maybe (Maybe(..))
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen as H

examplePerson :: Person
examplePerson =
  person "John" "Smith"
         (address "123 Fake St." "FakeTown" "CA")
         [ phoneNumber HomePhone "555-555-5555"
         , phoneNumber CellPhone "555-555-0000"
         ]

emptyPerson :: Person
emptyPerson = person "" "" (address "" "" "") []

-- Notice that there is no EXCEPTION effect
-- main :: forall eff. Eff (console :: CONSOLE, fs :: FS | eff) Unit
-- main = do
--   result <- try (readTextFile UTF8 "README.md")
--   case result of
--     Right lines ->
--       Console.log ("README: \n" <> lines )
--     Left error ->
--       Console.error ("Couldn't open README.md. Error was: " <> show error)

safeDivide :: forall eff. Int -> Int -> Eff (exception :: EXCEPTION | eff) Int
safeDivide a b = case a `mod` b of
  0 -> pure (a/b)
  _ -> throwException $ error "Denominator does not divide numerator"

-- if eff is not extensible then liftEff does not work! yay!
-- safeDivideTest :: Number -> Number -> Eff (console:: CONSOLE) Unit

safeDivideTest :: forall eff. Int -> Int -> Eff (console:: CONSOLE | eff) Unit
safeDivideTest a b = catchException (log <<< show) $ safeDivide a b >>= (log <<< show)

-- log $ showFullName "a" "b" "c"
-- showFullName "a" "b" "c" # log
-- showFullName <$> Just "a" 
--               <*> Just "b" 
--               <*> Just "c" 
--               # show 
--               # log

main :: Eff (HA.HalogenEffects (console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  H.liftEff $ fullNameEither (Just "a") (Just "d") (Just "c") # show # log

  safeDivideTest 1 1 # H.liftEff
  H.liftEff $ safeDivideTest 0 0

  validatePerson examplePerson # show # log # H.liftEff
  validatePerson emptyPerson # show # log # H.liftEff

  runUI component unit body
