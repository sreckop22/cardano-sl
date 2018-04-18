{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.WalletLayer.Kernel
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum
import           System.Wlog (Severity)
import           Pos.Core (HasConfiguration)

import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..), PassiveWalletLayer (..))

import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Pos.Util.Chrono (NE, OldestFirst (..))
import           Pos.Block.Types(Blund, Undo (..))

import           Cardano.Wallet.Kernel.Types (ResolvedBlock, fromRawResolvedBlock)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromJust)

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall m n a. (HasConfiguration, MonadIO n, MonadIO m, MonadMask m, Monad n)
    => (Severity -> Text -> IO ())
    -> (PassiveWalletLayer n -> m a) -> m a
bracketPassiveWallet logFunction f =
    Kernel.bracketPassiveWallet logFunction $ \w ->
                f (passiveWalletLayer w)

  where
    passiveWalletLayer :: forall n'. (MonadIO n', HasConfiguration)
                       => Kernel.PassiveWallet -> PassiveWalletLayer n'
    passiveWalletLayer wallet =
        PassiveWalletLayer
            { pwlGetWalletIds  = error "Not implemented!" -- TODO will be fixed once we move to the AcidState design
            , pwlApplyBlocks   = applyBlocks' wallet
            }

    applyBlocks' :: forall n''. (HasConfiguration, MonadIO n'')
                 => Kernel.PassiveWallet -> OldestFirst NE Blund -> n'' ()
    applyBlocks' w blunds
        = do
            let resolvedBlocks = map blundToResolvedBlock blunds
            _ <- liftIO $ Kernel.applyBlocks w resolvedBlocks
            return ()

    blundToResolvedBlock :: Blund -> ResolvedBlock
    blundToResolvedBlock (b,u)
        = case b of
            Left _ -> error "genesis block, expecting a MainBlock"
            Right mainBlock ->
                fromRawResolvedBlock (mainBlock, spentOutputs')
        where
            spentOutputs' = map (map fromJust . NE.toList) $ undoTx u

-- | Initialize the active wallet.
-- The active wallet is allowed all.
bracketActiveWallet
    :: forall m n a. (MonadIO n, MonadIO m, MonadMask m, Monad n)
    => PassiveWalletLayer n
    -> WalletDiffusion
    -> (ActiveWalletLayer n -> m a) -> m a
bracketActiveWallet walletPassiveLayer _walletDiffusion =
    bracket
      (return ActiveWalletLayer{..})
      (\_ -> return ())
