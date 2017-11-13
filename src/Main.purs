module Main where

import Prelude

import Book.Validation (Person, PhoneType(..), address, showFullName, fullNameEither, person, phoneNumber, validatePerson)
import Component (component)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, error, catchException, throwException)
import DOM.HTML.HTMLFormElement (method)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)

examplePerson :: Person
examplePerson =
  person "John" "Smith"
         (address "123 Fake St." "FakeTown" "CA")
         [ phoneNumber HomePhone "555-555-5555"
         , phoneNumber CellPhone "555-555-0000"
         ]

emptyPerson :: Person
emptyPerson = person "" "" (address "" "" "") []

safeDivide :: forall eff. Int -> Int -> Eff (exception :: EXCEPTION, ajax :: AJAX | eff) Int
safeDivide a b = case a `mod` b of
  0 -> pure (a/b)
  _ -> throwException $ error "Denominator does not divide numerator"

-- if eff is not extensible then liftEff does not work! yay!
-- safeDivideTest :: Number -> Number -> Eff (console:: CONSOLE) Unit

safeDivideTest :: forall eff. Int -> Int -> Eff (console:: CONSOLE, ajax :: AJAX | eff) Unit
safeDivideTest a b = catchException (log <<< show) $ safeDivide a b >>= (log <<< show)

-- log $ showFullName "a" "b" "c"
-- showFullName "a" "b" "c" # log

main :: forall e. Eff (HA.HalogenEffects (console :: CONSOLE, ajax :: AJAX | e)) Unit
main = HA.runHalogenAff do
  res <- affjax $ defaultRequest { url = "/api", method = Left GET }
  body <- HA.awaitBody

  H.liftEff $ fullNameEither (Just "a") (Just "d") (Just "c") # show # log
  H.liftEff $ log $ "GET /api response: " <> res.response

  showFullName <$> Just "a" 
               <*> Just "b" 
               <*> Just "234234234" 
               # show 
               # log
               # H.liftEff

  safeDivideTest 1 1 # H.liftEff
  H.liftEff $ safeDivideTest 0 0

  validatePerson examplePerson # show # log # H.liftEff
  validatePerson emptyPerson # show # log # H.liftEff

  runUI component unit body
