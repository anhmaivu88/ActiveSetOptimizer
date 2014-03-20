module ActiveSetSolve where

import Calculus
import Polynomial
import Numeric.LinearAlgebra
import Control.Monad.Trans.Either
import Control.Monad.Trans
import Control.Monad.Loops
import Utils
import Constraint
import LineSearch
import ModifiedCholesky
import EqualityConstrainedSolve
import Control.Monad


activeSetSolve  :: (Objective f) =>
                   (Double,Double) -> Double    -- params
                   -> f                         -- objective
                   -> ActiveSet                 -- constraints
                   -> IO (Vector Double)
activeSetSolve cs eps f as = eitherT return (error "wat") $ flip iterateM_ as $ \(AS eqc ix iqc) -> do
--  liftIO $ putStrLn $ "TOP OF ACTIVE SET SOLVE"
--  liftIO $ putStrLn $ "  Active set now size " ++ (show $ eqcSize eqc)
  (iqc',eqc') <- liftIO $ newtonEqualityConstrainedSolve cs eps f iqc eqc
  let (x0,x1) = (eval f $ initialPoint eqc,eval f $ initialPoint eqc')
  when ((x0 - x1)/x0 < eps) $ left $ initialPoint eqc'
  hoistEither $ updateActiveSet eps f (AS eqc' ix iqc)

                   