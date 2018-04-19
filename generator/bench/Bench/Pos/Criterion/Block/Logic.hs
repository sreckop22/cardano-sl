module Bench.Pos.Criterion.Block.Logic
    ( runBenchmark
    ) where

import           Universum

import           Control.Lens (makeLensesWith)
import           Control.Monad.Reader (withReaderT)
import           Criterion.Main (Benchmarkable, bench, defaultConfig, defaultMainWith, perRunEnv)
import           Criterion.Types (Config (..))
import           Ether.Internal (HasLens (..))
import           Mockable.Production (Production (..))
import           System.Wlog (LoggerName (..))

import           Pos.Block.Error (VerifyBlocksException)
import           Pos.Block.Logic.VAR (verifyBlocksPrefix)
import           Pos.Block.Types (Undo)
import           Pos.Core.Block (Block)
import           Pos.Core.Common (BlockCount (..))
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.DB.Block (dbGetSerBlockPureDefault, dbGetSerUndoPureDefault)
import           Pos.DB.Class (MonadDBRead (..), MonadGState (..))
import           Pos.DB.DB (gsAdoptedBVDataDefault)
import           Pos.DB.Pure (DBPureVar, dbGetPureDefault, dbIterSourcePureDefault, newDBPureVar)
import           Pos.Delegation.Class (DelegationVar)
import           Pos.KnownPeers (MonadFormatPeers (..))
import           Pos.Launcher.Configuration (HasConfigurations, withConfigurations_)
import           Pos.Lrc.Context (LrcContext)
import           Pos.Network.Types (HasNodeType (..))
import           Pos.Reporting (HasReportingContext (..))
import           Pos.Slotting
import           Pos.Ssc (SscState, SscMemTag)
import           Pos.Txp.Settings.Global (TxpGlobalSettings)
import           Pos.Txp.Logic.Global (txpGlobalSettings)
import           Pos.Update.Poll.Modifier (PollModifier)
import           Pos.Util.Chrono (OldestFirst, NE)
import           Pos.Util.Lens (postfixLFields)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo, retrieveCompileTimeInfo)
import           Pos.WorkMode (RealModeContext, EmptyMempoolExt)

config :: Config
config = defaultConfig
    { reportFile = Just "verification.html"
    }

data BenchContext = BenchContext {
      bcRealModeContext :: !(RealModeContext EmptyMempoolExt)
    , bcDBPureVar :: DBPureVar
    , bcDelegationVar :: DelegationVar
    , bcSscState :: SscState
    , bcLrcContext :: LrcContext
    , bcTxpGlobalSettings :: TxpGlobalSettings
    }

makeLensesWith postfixLFields ''BenchContext

newBenchContext :: IO BenchContext
newBenchContext = do
    let bcRealModeContext = undefined
    bsDBPureVar <- newDBPureVar
    let bcDelegationVar = undefined
    let bcSscState = undefined
    let bcLrcContext = undefined
    let bcTxpGlobalSettings = txpGlobalSettings
    return BenchContext {..}

type BenchMode = ReaderT BenchContext Production

instance HasLens SimpleSlottingStateVar BenchContext SimpleSlottingStateVar where
    lensOf = bcRealModeContext_L . lensOf @SimpleSlottingStateVar

instance HasSlottingVar BenchContext where
    slottingTimestamp = bcRealModeContext_L . slottingTimestamp
    slottingVar       = bcRealModeContext_L . slottingVar

instance ( HasConfiguration
         , MonadSlotsData ctx BenchMode
         ) => MonadSlots ctx BenchMode where
  getCurrentSlot           = getCurrentSlotSimple
  getCurrentSlotBlocking   = getCurrentSlotBlockingSimple
  getCurrentSlotInaccurate = getCurrentSlotInaccurateSimple
  currentTimeSlotting      = currentTimeSlottingSimple

instance HasConfiguration => MonadDBRead BenchMode where
    dbGet = dbGetPureDefault
    dbIterSource = dbIterSourcePureDefault
    dbGetSerBlock = dbGetSerBlockPureDefault
    dbGetSerUndo = dbGetSerUndoPureDefault

instance HasLens DBPureVar BenchContext DBPureVar where
    lensOf = bcDBPureVar_L

instance HasConfiguration => MonadGState BenchMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance HasLens DelegationVar BenchContext DelegationVar where
    lensOf = bcDelegationVar_L

instance HasLens SscMemTag BenchContext SscState where
    lensOf = bcSscState_L

instance HasLens LrcContext BenchContext LrcContext where
    lensOf = bcLrcContext_L

instance HasLens TxpGlobalSettings BenchContext TxpGlobalSettings where
    lensOf = bcTxpGlobalSettings_L

instance MonadFormatPeers BenchMode where
    formatKnownPeers f = withReaderT bcRealModeContext $ formatKnownPeers f

instance HasReportingContext BenchContext where
  reportingContext  = bcRealModeContext_L . reportingContext

instance HasNodeType BenchContext where
    getNodeType = getNodeType . bcRealModeContext

verifyBlocksPrefixBench
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => BenchContext
    -> BlockCount
    -> Benchmarkable
verifyBlocksPrefixBench ctx bCount = perRunEnv genEnv benchBlockVerification
    where
    genEnv :: IO (OldestFirst NE Block)
    genEnv = undefined

    benchBlockVerification
        :: ( HasConfigurations
           , HasCompileInfo
           )
        => OldestFirst NE Block
        -> IO (Either VerifyBlocksException (OldestFirst NE Undo, PollModifier))
    benchBlockVerification blocks =
        runProduction $ runReaderT (verifyBlocksPrefix blocks) ctx
        
runBenchmark :: IO ()
runBenchmark =
    withCompileInfo $(retrieveCompileTimeInfo) $
    withConfigurations_ (LoggerName "verifyBlocksPrefixBench") undefined $ \_ -> do
        ctx <- newBenchContext
        defaultMainWith config
            [ bench "verifyBlocksPrefixBench" $ verifyBlocksPrefixBench ctx (BlockCount 2000) ]
