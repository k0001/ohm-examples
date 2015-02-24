module ExampleLoginLogout.Types.User
 ( UserId(..)

 , User(User)
 , email
 , name
 ) where

import BasePrelude
import Control.Lens

import ExampleLoginLogout.Types.Texty (Texty)

--------------------------------------------------------------------------------

newtype UserId = UserId { unUserId :: Int32 }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

data User = User
  { _email :: !Texty
  , _name  :: !Texty
  } deriving (Eq, Show)

email :: Lens' User Texty
email f s@User{_email=a} = f a <&> \b -> s{_email=b}

name :: Lens' User Texty
name f s@User{_name=a} = f a <&> \b -> s{_name=b}
