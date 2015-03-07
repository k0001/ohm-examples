{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ExampleLoginLogout.UI.Widgets.AuthLoginPassword
 ( -- * Model
   modelDef

 , Model
 , mEmail
 , mPassword
 , mErrors

 , Error(..)

   -- * Controller
 , controller

 , Req(..)
 , _ReqSetEmail
 , _ReqSetPassword
 , _ReqLogin

   -- * View
 , viewForm
 ) where

import           BasePrelude
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as Text
import qualified Lei
import qualified Ohm.HTML as Ohm

import           ExampleLoginLogout.Types.Texty (Texty, _Texty)

--------------------------------------------------------------------------------

data Error = ErrorBadCredentials
  deriving (Eq, Show)

errorDescription :: Error -> Text.Text
errorDescription ErrorBadCredentials = "Bad email or password"

--------------------------------------------------------------------------------

data Model = Model
  { _mEmail    :: !(Maybe Texty)
  , _mPassword :: !(Maybe Texty)
  , _mErrors   :: ![Error]
  } deriving (Eq, Show)

mEmail :: Lens' Model (Maybe Texty)
mEmail f s@Model{_mEmail=a} = f a <&> \b -> s{_mEmail=b}

mPassword :: Lens' Model (Maybe Texty)
mPassword f s@Model{_mPassword=a} = f a <&> \b -> s{_mPassword=b}

mErrors :: Lens' Model [Error]
mErrors f s@Model{_mErrors=a} = f a <&> \b -> s{_mErrors=b}

--------------------------------------------------------------------------------

modelDef :: Model
modelDef = Model Nothing Nothing []

--------------------------------------------------------------------------------

data Req
  = ReqSetEmail !(Maybe Texty)
  | ReqSetPassword !(Maybe Texty)
  | ReqLogin !Texty !Texty
  deriving (Show)

_ReqSetEmail :: Prism' Req (Maybe Texty)
_ReqSetEmail = prism' ReqSetEmail $ \case ReqSetEmail a -> Just a
                                          _ -> Nothing

_ReqSetPassword :: Prism' Req (Maybe Texty)
_ReqSetPassword = prism' ReqSetPassword $ \case ReqSetPassword a -> Just a
                                                _ -> Nothing

_ReqLogin :: Prism' Req (Texty, Texty)
_ReqLogin = prism' (uncurry ReqLogin) $ \case ReqLogin a b -> Just (a, b)
                                              _ -> Nothing

--------------------------------------------------------------------------------

controller
  :: Monad m
  => (Texty -> Texty -> Lei.C s r Req Model m (Maybe user))
  -- ^ Login by email and password
  -> Lei.Controller s r Req Model m
controller login = Lei.mkController $ \r -> do
    mErrors .= []
    case r of
       ReqSetEmail a -> mEmail .= a
       ReqSetPassword a -> mPassword .= a
       ReqLogin uname pwd -> do
          mu <- login uname pwd
          when (isNothing mu) $ do
             mErrors %= (ErrorBadCredentials:)

--------------------------------------------------------------------------------

viewForm :: MonadIO m => Lei.View () Req Model m Ohm.HTML
viewForm = Lei.mkViewSimple $ \req s ->
    Ohm.into Ohm.form
      [ Ohm.text ("Email" :: String)
      , Ohm.with Ohm.input (do
          Ohm.attrs . at "value" ?= (s ^. mEmail . to Ohm.toJSString)
          Ohm.onInput $ req . ReqSetEmail . preview _Texty . Text.pack)
          []
      , Ohm.text ("Password" :: String)
      , Ohm.with Ohm.input (do
          Ohm.attrs . at "value" ?= (s ^. mPassword . to Ohm.toJSString)
          Ohm.onInput $ req . ReqSetPassword . preview _Texty . Text.pack)
          []
      , Ohm.with Ohm.button
          (case (s ^. mEmail, s ^. mPassword) of
             (Just e_, Just p_) -> Ohm.onClick $ req (ReqLogin e_ p_)
             _ -> Ohm.attrs . at "disabled" ?= "disabled")
          [Ohm.text ("Login" :: String)]
      , case view mErrors s of
          [] -> Ohm.text ("" :: String)
          xs -> Ohm.with Ohm.ul
                  (Ohm.classes .= ["errors"])
                  (xs <&> \x -> let err = Ohm.toJSString (errorDescription x)
                                in Ohm.into Ohm.li [Ohm.text err])
      ]
