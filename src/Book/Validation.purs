module Book.Validation (
  showFullName,
  fullNameEither,
  person,
  validatePerson,
  phoneNumber,
  address,
  Person,
  Address,
  PhoneType(..),
  PhoneNumber
) where

import Prelude

import Data.Either (Either(..))
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))

type Error = String

newtype Address = Address 
  { street :: String
  , city :: String
  , state :: String
  }

derive instance genericAddress :: Generic Address
instance showAddress :: Show Address where show = gShow

data PhoneType = HomePhone | WorkPhone | CellPhone | OtherPhone

instance showPhoneType :: Show PhoneType where
  show HomePhone = "HomePhone"
  show WorkPhone = "WorkPhone"
  show CellPhone = "CellPhone"
  show OtherPhone = "OtherPhone"

newtype PhoneNumber = PhoneNumber 
  { phoneType :: PhoneType
  , number :: String
  }

instance showPhoneNumber :: Show PhoneNumber where
  show (PhoneNumber p) = "Phone number" <> show p.phoneType <> p.number

newtype Person = Person 
  { firstName :: String
  , lastName :: String
  , address :: Address
  , phones :: Array PhoneNumber
  }

instance showPerson :: Show Person where
  show (Person o) = "Person " <>
    "{ firstName: " <> show o.firstName <>
    ", lastName: "  <> show o.lastName <>
    ", address: "   <> show o.address <>
    ", phones: "    <> show o.phones <>
    " }"

showFullName :: String -> String -> String -> String
showFullName first middle last = last <> ", " <> first <> " " <> middle

withError :: forall a b. Maybe a -> b -> Either b a
withError Nothing err = Left err
withError (Just a) _ = Right a

fullNameEither :: Maybe String -> Maybe String -> Maybe String -> Either String String
fullNameEither first middle last =
  showFullName <$> (first `withError` "First name is missing")
           <*> (middle `withError` "Middle name is missing")
           <*> (last `withError` "Lat name is missing")

nonEmpty :: String -> Either String Unit
nonEmpty "" = Left "Field cannot be empty"
nonEmpty _ = Right unit

address :: String -> String -> String -> Address
address street city state = Address { street, city, state }

phoneNumber :: PhoneType -> String -> PhoneNumber
phoneNumber phoneType number = PhoneNumber { phoneType, number }

person :: String -> String -> Address -> Array PhoneNumber -> Person
person firstName lastName address phones = Person 
  { firstName,
    lastName,
    address,
    phones
  }

validatePerson :: Person -> Either Error Person
validatePerson (Person o) =
  person <$> (nonEmpty o.firstName *> pure o.firstName)
         <*> (nonEmpty o.lastName  *> pure o.lastName)
         <*> pure o.address
         <*> pure o.phones


