-- | Resolved blocks and transactions
module Cardano.Wallet.Kernel.DB.Resolved (
    -- * Resolved blocks and transactions
    ResolvedInput
  , ResolvedTx(..)
  , ResolvedBlock(..)
    -- ** Lenses
  , rtxInputs
  , rtxOutputs
  , rbTxs
  ) where

import           Control.Lens.TH (makeLenses)
import           Data.SafeCopy (base, deriveSafeCopy)

import qualified Pos.Core as Core
import qualified Pos.Txp as Core

import           Cardano.Wallet.Kernel.DB.InDb

{-------------------------------------------------------------------------------
  Resolved blocks and transactions
-------------------------------------------------------------------------------}

-- | Resolved input
--
-- A transaction input @(h, i)@ points to the @i@th output of the transaction
-- with hash @h@, which is not particularly informative. The corresponding
-- 'ResolvedInput' is obtained by looking up what that output actually is.
type ResolvedInput = Core.TxOutAux

-- | (Unsigned) transaction with inputs resolved
data ResolvedTx = ResolvedTx {
      -- | Transaction inputs
      _rtxInputs  :: InDb [(Core.TxIn, ResolvedInput)]

      -- | Transaction outputs
    , _rtxOutputs :: InDb Core.Utxo
    }

-- | (Unsigned block) containing resolved transactions
--
-- TODO: Add metadata (see spec)
data ResolvedBlock = ResolvedBlock {
      -- | Transactions in the block
      _rbTxs  :: [ResolvedTx]
    }

makeLenses ''ResolvedTx
makeLenses ''ResolvedBlock

deriveSafeCopy 1 'base ''ResolvedTx
deriveSafeCopy 1 'base ''ResolvedBlock
