{-# LANGUAGE FlexibleInstances #-}

module ExampleLoginLogout.Types.Texty
 ( Texty
 , _Texty
 ) where

import           BasePrelude
import           Control.Lens
import qualified Data.Text as Text
import qualified GHCJS.Foreign

--------------------------------------------------------------------------------

-- | A 'Texty' is 'Text.Text' that is guaranteed to be non-empty.
--
-- Use the '_Texty' 'Prism'' to introduce and eliminate 'Texty's.
newtype Texty = Texty { unTexty :: Text.Text }
  deriving (Eq, Show)

instance GHCJS.Foreign.ToJSString Texty where
  toJSString = GHCJS.Foreign.toJSString . unTexty

instance GHCJS.Foreign.ToJSString (Maybe Texty) where
  toJSString = GHCJS.Foreign.toJSString . maybe mempty unTexty

_Texty :: Prism' Text.Text Texty
_Texty = prism' (\(Texty a) -> a) $ \a -> do
    guard $ not $ Text.null a
    return $ Texty a
