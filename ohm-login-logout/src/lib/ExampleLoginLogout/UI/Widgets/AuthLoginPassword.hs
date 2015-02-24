{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ExampleLoginLogout.UI.Widgets.AuthLoginPassword
 ( def

 , AuthLoginPassword
 , alpEmail
 , alpPassword
 , alpErrors

 , Op(..)
 , model

 , Req(..)
 , controller

 , viewForm
 ) where

import           BasePrelude
import           Control.Lens
import qualified Data.Text as Text
import qualified Lei
import qualified Ohm.HTML as Ohm

import           ExampleLoginLogout.Types.Texty (Texty, _Texty)

--------------------------------------------------------------------------------

data AuthLoginPassword = AuthLoginPassword
  { _alpEmail    :: !(Maybe Texty)
  , _alpPassword :: !(Maybe Texty)
  , _alpErrors   :: ![Error]
  } deriving (Eq, Show)

alpEmail :: Lens' AuthLoginPassword (Maybe Texty)
alpEmail f s@AuthLoginPassword{_alpEmail=a} = f a <&> \b -> s{_alpEmail=b}

alpPassword :: Lens' AuthLoginPassword (Maybe Texty)
alpPassword f s@AuthLoginPassword{_alpPassword=a} = f a <&> \b -> s{_alpPassword=b}

alpErrors :: Lens' AuthLoginPassword [Error]
alpErrors f s@AuthLoginPassword{_alpErrors=a} = f a <&> \b -> s{_alpErrors=b}

--------------------------------------------------------------------------------

def :: AuthLoginPassword
def = AuthLoginPassword Nothing Nothing []

--------------------------------------------------------------------------------

data Op
  = OpSetEmail !(Maybe Texty)
  | OpSetPassword !(Maybe Texty)
  | OpClearErrors
  | OpAddError !Error
  deriving (Show)

model :: Lei.Model Op AuthLoginPassword
model = Lei.mkModel $ \case
    OpSetEmail a -> set alpEmail a
    OpSetPassword a -> set alpPassword a
    OpClearErrors -> set alpErrors []
    OpAddError a -> over alpErrors (a:)

--------------------------------------------------------------------------------

data Req
  = ReqSetEmail !(Maybe Texty)
  | ReqSetPassword !(Maybe Texty)
  | ReqLogin !Texty !Texty
  deriving (Show)

controller
  :: Monad m
  => (Texty -> Texty -> Lei.C r o Req Op m (Maybe user))
  -- ^ Login by email and password
  -> Lei.Controller r o Req Op AuthLoginPassword m
controller login = Lei.mkController $ \_ r -> do
    Lei.op $ OpClearErrors
    case r of
       ReqSetEmail a -> Lei.op $ OpSetEmail a
       ReqSetPassword a -> Lei.op $ OpSetPassword a
       ReqLogin uname pwd -> do
          mu <- login uname pwd
          when (isNothing mu) $ do
             Lei.op $ OpAddError ErrorBadCredentials

--------------------------------------------------------------------------------

data Error = ErrorBadCredentials
  deriving (Eq, Show)

errorDescription :: Error -> Text.Text
errorDescription ErrorBadCredentials = "Bad email or password"

--------------------------------------------------------------------------------

viewForm :: Monad m => Lei.View () Req AuthLoginPassword m Ohm.HTML
viewForm = Lei.mkView_ $ \_ req s ->
    Ohm.into Ohm.form
      [ Ohm.text ("Email" :: String)
      , Ohm.with Ohm.input (do
          Ohm.attrs . at "value" ?= (s ^. alpEmail . to Ohm.toJSString)
          Ohm.onInput $ req . ReqSetEmail . preview _Texty . Text.pack)
          []
      , Ohm.text ("Password" :: String)
      , Ohm.with Ohm.input (do
          Ohm.attrs . at "value" ?= (s ^. alpPassword . to Ohm.toJSString)
          Ohm.onInput $ req . ReqSetPassword . preview _Texty . Text.pack)
          []
      , Ohm.with Ohm.button
          (case (s ^. alpEmail, s ^. alpPassword) of
             (Just e_, Just p_) -> Ohm.onClick $ req (ReqLogin e_ p_)
             _ -> Ohm.attrs . at "disabled" ?= "disabled")
          [Ohm.text ("Login" :: String)]
      , case view alpErrors s of
          [] -> Ohm.text ("" :: String)
          xs -> Ohm.with Ohm.ul
                  (Ohm.classes .= ["errors"])
                  (xs <&> \x -> let err = Ohm.toJSString (errorDescription x)
                                in Ohm.into Ohm.li [Ohm.text err])
      ]
