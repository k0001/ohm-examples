{-# LANGUAGE OverloadedStrings #-}

module ExampleLoginLogout.UI.Widgets.Auth
 ( -- * Model
   modelDef

 , Model
 , mUserId
 , mUser

   -- * Controller
 , controller

 , Req(..)
 , _ReqLogout

   -- * View
 , viewStatus
 ) where

import           BasePrelude
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO)
import qualified Lei
import qualified Ohm.HTML as Ohm

import           ExampleLoginLogout.Types.User (UserId, User)
import qualified ExampleLoginLogout.Types.User as User

--------------------------------------------------------------------------------

data Model = Model
  { _mUserId :: !UserId
  , _mUser   :: !User
  } deriving (Eq, Show)

mUserId :: Lens' Model UserId
mUserId f s@Model{_mUserId=a} = f a <&> \b -> s{_mUserId=b}

mUser :: Lens' Model User
mUser f s@Model{_mUser=a} = f a <&> \b -> s{_mUser=b}

--------------------------------------------------------------------------------

modelDef :: UserId -> User -> Model
modelDef = Model

--------------------------------------------------------------------------------

data Req = ReqLogout
  deriving (Show)

_ReqLogout :: Prism' Req ()
_ReqLogout = prism' (const ReqLogout) (\ReqLogout -> Just ())

--------------------------------------------------------------------------------


controller
  :: Lei.C s r Req Model m () -- ^ Logout
  -> Lei.Controller s r Req Model m
controller logout = Lei.mkController $ \ReqLogout -> logout

--------------------------------------------------------------------------------

viewStatus :: MonadIO m => Lei.View () Req Model m Ohm.HTML
viewStatus = Lei.mkViewSimple $ \req s ->
  Ohm.into Ohm.p
    [ Ohm.text ("You are logged in as " :: String)
    , Ohm.text (s ^. mUser . User.name . to Ohm.toJSString)
    , Ohm.text (" — " :: String)
    , Ohm.with Ohm.button
        (Ohm.onClick (req ReqLogout))
        [Ohm.text ("Logout" :: String)]
    ]
