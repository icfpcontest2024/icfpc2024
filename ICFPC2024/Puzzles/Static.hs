module ICFPC2024.Puzzles.Static (
  readStaticData,
  StaticData (..),
) where

import ICFPC2024.Config ( Config(..) )
import ICFPC2024.Puzzles.Helpers.Static ( readTemplate, Template )

import qualified ICFPC2024.Puzzles.Hello.Static as HELLO
import qualified ICFPC2024.Puzzles.Efficiency.Static as EFF
import qualified ICFPC2024.Puzzles.Lambdaman.Static as LAM
import qualified ICFPC2024.Puzzles.Spaceship.Static as SPA
import qualified ICFPC2024.Puzzles.ThreeD.Static as THD

-- | All static puzzle data
data StaticData = StaticData
  { staticHello      :: HELLO.StaticData
  , staticEfficiency :: EFF.StaticData
  , staticLambdaman  :: LAM.StaticData
  , staticSpaceship  :: SPA.StaticData
  , static3D         :: THD.StaticData
  , staticSolved     :: Template
  , staticNotEnrolled :: Template
  }


-- | Read the static puzzle data
readStaticData :: Config -> IO StaticData
readStaticData conf = do
  hello <- HELLO.readStaticData conf
  eff   <- EFF.readStaticData conf
  lam   <- LAM.readStaticData conf
  spa   <- SPA.readStaticData conf
  thd   <- THD.readStaticData conf
  solvd <- readTemplate conf "solved.md"
  nenr  <- readTemplate conf "not_enrolled.md"
  pure $ StaticData
    { staticHello      = hello
    , staticEfficiency = eff
    , staticLambdaman  = lam
    , staticSpaceship  = spa
    , static3D         = thd
    , staticSolved     = solvd
    , staticNotEnrolled = nenr
    }
