{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ExampleLoginLogout.UI.Widgets.Root
 ( -- * Model
   modelDef

 , Model
 , mAuth

   -- * Controller
 , controller

 , Req(..)
 , _ReqAuth
 , _ReqAuthLoginPassword

   -- * View
 , viewRoot
 ) where

import           BasePrelude
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Lei
import qualified Ohm.HTML as Ohm

import           ExampleLoginLogout.Types.Texty (Texty, _Texty)
import           ExampleLoginLogout.Types.User (UserId(UserId), User(User))
import qualified ExampleLoginLogout.UI.Widgets.Auth as WAuth
import qualified ExampleLoginLogout.UI.Widgets.AuthLoginPassword as WAuthLoginPassword

--------------------------------------------------------------------------------

data Model = Model
  { _mAuth :: !(Either WAuthLoginPassword.Model WAuth.Model)
  } deriving (Eq, Show)

mAuth :: Lens' Model (Either WAuthLoginPassword.Model WAuth.Model)
mAuth f s@Model{_mAuth=a} = f a <&> \b -> s{_mAuth=b}

-------------------------------------------------------------------------------

modelDef :: Model
modelDef = Model { _mAuth = Left WAuthLoginPassword.modelDef }

--------------------------------------------------------------------------------

data Req
  = ReqAuth !WAuth.Req
  | ReqAuthLoginPassword !WAuthLoginPassword.Req
  deriving (Show)


_ReqAuth :: Prism' Req WAuth.Req
_ReqAuth = prism' ReqAuth $ \case ReqAuth a -> Just a
                                  _ -> Nothing

_ReqAuthLoginPassword :: Prism' Req WAuthLoginPassword.Req
_ReqAuthLoginPassword = prism' ReqAuthLoginPassword $ \case
     ReqAuthLoginPassword a -> Just a
     _ -> Nothing

--------------------------------------------------------------------------------


controller :: MonadIO m => Lei.Controller r s Req Model m
controller = mconcat
   [ Lei.controlling _ReqAuthLoginPassword
       (preview (mAuth . _Left), set (mAuth . _Left))
       (\nat -> WAuthLoginPassword.controller (fmap (fmap nat) login))
   , Lei.controlling _ReqAuth
       (preview (mAuth . _Right), set (mAuth . _Right))
       (\nat -> WAuth.controller (nat logout))
   ]
  where
    logout = mAuth .= Left WAuthLoginPassword.modelDef
    login email password = do
       mu <- liftIO $ getUserByEmailAndPassword email password
       for_ mu $ \(uId, u) -> mAuth .= Right (WAuth.modelDef uId u)
       return mu

-- | Just pretending we do some database lookup here
getUserByEmailAndPassword :: Texty -> Texty -> IO (Maybe (UserId, User))
getUserByEmailAndPassword email password = return $
    case (review _Texty email, review _Texty password) of
       ("bob@example.com", "bob") -> Just $ (UserId 19, User email ("Bobby" ^?! _Texty))
       ("alice@example.com", "alice") -> Just $ (UserId 32, User email ("Alice" ^?! _Texty))
       _ -> Nothing

-------------------------------------------------------------------------------

viewRoot :: MonadIO m => Lei.View () Req Model m Ohm.HTML
viewRoot = Lei.mkView $ do
    ((), vmAuth) <- Lei.nestView (fmap id .) ReqAuth WAuth.viewStatus
    ((), vmAuthLoginPassword) <-
       Lei.nestView (fmap id .) ReqAuthLoginPassword WAuthLoginPassword.viewForm
    let kvr = \_ s -> do
           body <- case _mAuth s of
              Left  s' -> Lei.render vmAuthLoginPassword s'
              Right s' -> Lei.render vmAuth s'
           return $ Ohm.into Ohm.div
              [ body
              , Ohm.into Ohm.p [Ohm.text ("Valid login credentials: alice@example.com/alice, bob@example.com/bob" :: String)]
              , Ohm.into Ohm.h4 [Ohm.text ("Current model state: " :: String)]
              , Ohm.into Ohm.pre [Ohm.text (show s)]
              ]
    return ((), return, kvr)
