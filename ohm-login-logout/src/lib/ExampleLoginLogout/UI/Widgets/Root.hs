{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ExampleLoginLogout.UI.Widgets.Root
 ( def

 , Root
 , rAuth

 , Op(..)

 , Req(..)

 , model
 , controller
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

data Root = Root
  { _rAuth :: !(Either WAuthLoginPassword.AuthLoginPassword WAuth.Auth)
  } deriving (Eq, Show)

rAuth :: Lens' Root (Either WAuthLoginPassword.AuthLoginPassword WAuth.Auth)
rAuth f s@Root{_rAuth=a} = f a <&> \b ->s{_rAuth=b}

--------------------------------------------------------------------------------

def :: Root
def = Root { _rAuth = Left WAuthLoginPassword.def }


--------------------------------------------------------------------------------

data Op
  = OpUIAuthSwitch !(Either WAuthLoginPassword.AuthLoginPassword WAuth.Auth)
  | OpAuth !WAuth.Op
  | OpAuthLoginPassword !WAuthLoginPassword.Op
  deriving (Show)

model :: Lei.Model Op Root
model = Lei.mkModel $ \case
    OpUIAuthSwitch a -> set rAuth a
    OpAuth a -> over (rAuth . _Right) (Lei.runModel WAuth.model a)
    OpAuthLoginPassword a ->
       over (rAuth. _Left) (Lei.runModel WAuthLoginPassword.model a)

--------------------------------------------------------------------------------

data Req
  = ReqAuth !WAuth.Req
  | ReqAuthLoginPassword !WAuthLoginPassword.Req
  deriving (Show)

controller :: MonadIO m => Lei.Controller r o Req Op Root m
controller = Lei.mkController $ \s r -> case r of
    ReqAuthLoginPassword r' -> do
       case _rAuth s of
          Right _ -> return () -- bad request in this state
          Left s' -> do
             -- NOTE: 'Lei.bury2' is ridiculous and needs to be replaced with
             -- a smarter use of 'Lei.bury'. The important thing to notice
             -- is that we will be passing a 'C' defined in this layer to a
             -- nested controller, and it will be used from there.
             login' <- Lei.bury2 login
             Lei.nestController s' r' ReqAuthLoginPassword OpAuthLoginPassword $
                WAuthLoginPassword.controller login'
    ReqAuth r' -> do
       case _rAuth s of
          Left _ -> return () -- bad request in this state
          Right s' -> do
             logout' <- Lei.bury logout
             Lei.nestController s' r' ReqAuth OpAuth $ WAuth.controller logout'
  where
    logout = Lei.op $ OpUIAuthSwitch (Left WAuthLoginPassword.def)
    login email password = do
      mu <- liftIO $ getUserByEmailAndPassword email password
      for_ mu $ \(uId, u) -> do
         Lei.op $ OpUIAuthSwitch $ Right $ WAuth.def uId u
      return mu

-- | Just pretending we do some database lookup here
getUserByEmailAndPassword :: Texty -> Texty -> IO (Maybe (UserId, User))
getUserByEmailAndPassword email password = return $
    case (review _Texty email, review _Texty password) of
       ("bob@example.com", "bob") -> Just $ (UserId 19, User email ("Bobby" ^?! _Texty))
       ("alice@example.com", "alice") -> Just $ (UserId 32, User email ("Alice" ^?! _Texty))
       _ -> Nothing

-------------------------------------------------------------------------------

viewRoot :: Monad m => Lei.View () Req Root m Ohm.HTML
viewRoot = Lei.mkView $ do
    ((), vrAuth) <- Lei.nestView (fmap id .) ReqAuth WAuth.viewStatus
    ((), vrAuthLoginPassword) <-
       Lei.nestView (fmap id .) ReqAuthLoginPassword WAuthLoginPassword.viewForm
    let vs () = return ()
        vr stop req s = do
           body <- case _rAuth s of
              Left walp -> Lei.runViewRender vrAuthLoginPassword stop req walp
              Right wauth -> Lei.runViewRender vrAuth stop req wauth
           return $ Ohm.into Ohm.div
             [ body
             , Ohm.into Ohm.p [Ohm.text ("Valid login credentials: alice@example.com/alice, bob@example.com/bob" :: String)]
             , Ohm.into Ohm.h4 [Ohm.text ("Current model state: " :: String)]
             , Ohm.into Ohm.pre [Ohm.text (show s)]
             ]
    return ((), vs, vr)
