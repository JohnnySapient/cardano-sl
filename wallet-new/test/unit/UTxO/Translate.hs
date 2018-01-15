-- | Monad in which we can the translation from the DSL to Cardano types
module UTxO.Translate (
    Translate
  , runTranslate
  , lift
  , liftPure
  , liftMaybe
  ) where

import Universum hiding (lift)
import Control.Exception (throw)
import Control.Monad.Except (MonadError)
import System.IO.Error (userError)

import Pos.Core hiding (genesisData, generatedSecrets)
import Pos.Update

{-------------------------------------------------------------------------------
  Testing infrastructure from cardano-sl-core

  The genesis block comes from defaultTestConf, which in turn uses
  configuration.yaml. It is specified by a 'GenesisSpec'.
-------------------------------------------------------------------------------}

import Test.Pos.Util (
    withDefConfiguration
  , withDefUpdateConfiguration
  )

{-------------------------------------------------------------------------------
  Translation context
-------------------------------------------------------------------------------}

data Translate a = Translate {
      unTranslate :: (HasConfiguration, HasUpdateConfiguration) => Either Text a
    }

instance Functor Translate where
  fmap = liftM

instance Applicative Translate where
  pure  = return
  (<*>) = ap

instance Monad Translate where
  return a = Translate $ Right a
  x >>= f  = Translate $ case unTranslate x of
                           Left err -> Left err
                           Right a  -> unTranslate (f a)

lift :: (forall m. (HasConfiguration, HasUpdateConfiguration, MonadError Text m) => m a)
     -> Translate a
lift act = Translate act

liftPure :: ((HasConfiguration, HasUpdateConfiguration) => a) -> Translate a
liftPure a = Translate (Right a)

liftMaybe :: Text -> ((HasConfiguration, HasUpdateConfiguration) => Maybe a) -> Translate a
liftMaybe err ma = Translate $ case ma of
                                 Just a  -> Right a
                                 Nothing -> Left err

runTranslate :: Translate a -> a
runTranslate (Translate act) =
   withDefConfiguration $
   withDefUpdateConfiguration $
     case act of
       Left  e -> throw (userError (show e))
       Right a -> a
