{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ExampleLoginLogout.UI.Widgets.Auth
 ( def

 , Auth
 , aUserId
 , aUser

 , Op(..)
 , Req(..)

 , model
 , controller
 , viewStatus
 ) where

import           BasePrelude
import           Control.Lens
import qualified Lei
import qualified Ohm.HTML as Ohm

import           ExampleLoginLogout.Types.User (UserId, User)
import qualified ExampleLoginLogout.Types.User as User

--------------------------------------------------------------------------------

data Auth = Auth
  { _aUserId :: !UserId
  , _aUser   :: !User
  } deriving (Eq, Show)

aUserId :: Lens' Auth UserId
aUserId f s@Auth{_aUserId=a} = f a <&> \b -> s{_aUserId=b}

aUser :: Lens' Auth User
aUser f s@Auth{_aUser=a} = f a <&> \b -> s{_aUser=b}

--------------------------------------------------------------------------------

def :: UserId -> User -> Auth
def = Auth

--------------------------------------------------------------------------------

data Op = OpSetUser !UserId !User
  deriving (Show)

model :: Lei.Model Op Auth
model = Lei.mkModel $ \case
    OpSetUser a b -> set aUserId a . set aUser b

--------------------------------------------------------------------------------

data Req = ReqLogout
  deriving (Show)

controller
  :: Lei.C r o Req Op m () -- ^ Logout
  -> Lei.Controller r o Req Op Auth m
controller logout = Lei.mkController $ \_ r -> case r of
    ReqLogout -> logout

--------------------------------------------------------------------------------

viewStatus :: Monad m => Lei.View () Req Auth m Ohm.HTML
viewStatus = Lei.mkView_ $ \_ req s ->
  Ohm.into Ohm.p
    [ Ohm.text ("You are logged in as " :: String)
    , Ohm.text (s ^. aUser . User.name . to Ohm.toJSString)
    , Ohm.text (" — " :: String)
    , Ohm.with Ohm.button
        (Ohm.onClick (req ReqLogout))
        [Ohm.text ("Logout" :: String)]
    ]
