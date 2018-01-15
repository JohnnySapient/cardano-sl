module UTxO.Interpreter (
    -- * Additional types
    Block(..)
    -- * Interpretation proper
  , Interpret(..)
  , int'
  ) where

import Universum

import Pos.Core hiding (Block)
import Pos.Crypto

import UTxO.Simplified (Simplified(..))
import UTxO.Translate
import qualified UTxO            as DSL
import qualified UTxO.Simplified as Simpl

{-------------------------------------------------------------------------------
  Additional types not in the basic UTxO formalization
-------------------------------------------------------------------------------}

type Addr = Simpl Address

-- | A block of transactions
--
-- The block must be signed by the slot leader for the given slot.
data Block = Block {
      blockPrev  :: Maybe Block
    , blockSId   :: SlotId
    , blockTrans :: [DSL.Transaction Addr]
    }

{-------------------------------------------------------------------------------
  Translate from the DSL to Core types

  This goes through the 'Simplified' core module as an intermediate step.

  TODO: Remove intermediate types. They are now isomorphic to the
  'Simplified' ones.
-------------------------------------------------------------------------------}

class Interpret a where
  type Interpreted a :: *

  int :: a -> Simpl (Interpreted a)

int' :: (Interpret a, Simplified (Interpreted a))
     => a -> Translate (Interpreted a)
int' = fromSimpl . int

instance Interpret (DSL.Input Addr) where
  type Interpreted (DSL.Input Addr) = (SecretKey, TxIn)

  int inp@DSL.Input{..} =
    case DSL.outAddr (DSL.out inp) of
      DSL.AddrOrdinary owner -> Simpl.Input {
          inpOwner = owner
        , inpTrans = int inpTrans
        , inpIndex = inpIndex
        }
      _otherwise -> error "TODO"

instance Interpret (DSL.Output Addr) where
  type Interpreted (DSL.Output Addr) = TxOutAux

  int DSL.Output{outAddr = DSL.AddrOrdinary addr, ..} = Simpl.Output {
        outAddr = addr
      , outVal  = outVal
      }

instance Interpret (DSL.Transaction Addr) where
  type Interpreted (DSL.Transaction Addr) = TxAux

  int DSL.Transaction{..} = Simpl.Transaction {
        trIns  = map int trIns
      , trOuts = map int trOuts
      }

instance Interpret Block where
  type Interpreted Block = MainBlock

  int Block{..} = Simpl.Block {
        blockPrev  = int <$> blockPrev
      , blockSId   = blockSId
      , blockTrans = map int blockTrans
      }
