-- @+leo-ver=4-thin
-- @+node:gcross.20100128100529.1239:@thin simulate-compass.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091120111528.1238:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20091120111528.1238:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091120111528.1235:<< Import needed modules >>
import Acme.Dont

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans

import Data.Complex
import Data.ConfigFile
import Data.IORef
import Data.UUID

import System.Environment
import System.Posix.Clock
import System.Exit

import Text.Printf

import VMPS.Algorithms
import VMPS.EnergyMinimizationChain
import VMPS.Models
import VMPS.Operators
import VMPS.Paulis
import VMPS.States
import VMPS.Tensors

import Debug.Trace
-- @-node:gcross.20091120111528.1235:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091120111528.1234:Operator tensors
makeModelOperatorSiteTensors :: Double -> Int -> Int -> [OperatorSiteTensor]
makeModelOperatorSiteTensors angle width height =
    let makeRow left middle right =
            [left] ++ replicate (width-2) middle ++ [right]

        width_ = fromIntegral width

        bandwidth = (4+width-1)

        no_op =
            [(1 --> 1) pI
            ,(2 --> 2) pI
            ]

        start_horizontal =
            [(1 --> 3) ((cos angle :+ 0) *: pX)]
        end_horizontal =
            [(3 --> 2) pX]

        start_vertical =
            [(1 --> 4) ((sin angle :+ 0) *: pZ)]
        run_vertical =
            [(i --> (i+1)) pI | i <- [4..4+width_-2]]
        end_vertical =
            [((4+width_-1) --> 2) pZ]

        initiate =
            [(1 --> 1) pI] ++ start_horizontal ++ start_vertical

        terminus =
            [(2 --> 1) pI
            ,(3 --> 1) pX
            ,((4+width_-1) --> 1) pZ]

        middle_horizontal =
            start_horizontal ++ end_horizontal
        middle_vertical =
            start_vertical ++ run_vertical ++ end_vertical

        top_vertical = start_vertical ++ run_vertical
        bottom_vertical = run_vertical ++ end_vertical

        top_left = makeOperatorSiteTensorFromSpecification 1 bandwidth $
            initiate
        top_middle = makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
            no_op ++ middle_horizontal ++ top_vertical
        top_right = makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
            no_op ++ end_horizontal ++ top_vertical
        top = makeRow top_left top_middle top_right

        middle_left = makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
            no_op ++ start_horizontal ++ middle_vertical
        middle_middle = makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
            no_op ++ middle_horizontal ++ middle_vertical
        middle_right = makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
            no_op ++ end_horizontal ++ middle_vertical
        middle = makeRow middle_left middle_middle middle_right

        bottom_left = makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
            no_op ++ start_horizontal ++ bottom_vertical
        bottom_middle = makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
            no_op ++ middle_horizontal ++ bottom_vertical
        bottom_right = makeOperatorSiteTensorFromSpecification bandwidth 1 $
            terminus
        bottom = makeRow bottom_left bottom_middle bottom_right

    in top ++ (concat . replicate (height-2)) middle ++ bottom
-- @-node:gcross.20091120111528.1234:Operator tensors
-- @+node:gcross.20091120111528.1237:analyzeTrialEnergies
data TrialAnalysis = TrialDidBetter | TrialDidWorse | TrialDidTheSame

analyzeTrialEnergies tolerance best_energy trial_energy
    | best_energy - trial_energy > tolerance = TrialDidBetter
    | trial_energy - best_energy > tolerance = TrialDidWorse
    | otherwise = TrialDidTheSame
-- @-node:gcross.20091120111528.1237:analyzeTrialEnergies
-- @+node:gcross.20091120111528.1236:main
main = do
    args <- getArgs

    let angle = read $ args !! 0
        width = read $ args !! 1
        height = read $ args !! 2
        operator_site_tensors = makeModelOperatorSiteTensors angle width height
        bandwidth_increment = 5
        initial_bandwidth = 2
        bandwidth_increase_energy_change_convergence_criterion = 1e-4
        multisweep_energy_change_convergence_criterion = 1e-4
        level_similarity_tolerance = 1e-3

    putStrLn $ "Angle = " ++ show angle
    putStrLn $ "Width = " ++ show width
    putStrLn $ "Height = " ++ show height
    putStrLn $ "Total = " ++ show (length operator_site_tensors)

    -- @    << Define callbacks >>
    -- @+node:gcross.20091205211300.1710:<< Define callbacks >>
    next_bandwidth_ref <- newIORef initial_bandwidth
    level_number_ref <- newIORef 1

    let getHeading = liftM (printf "LEVEL %i: ") (readIORef level_number_ref :: IO Int)
        callback_to_decide_whether_to_declare_victory_with_trial chain = do
            heading <- getHeading
            putStrLn $ heading ++ " energy = " ++ (show . chainEnergy $ chain)
            level_number <- readIORef level_number_ref
            let new_level_number = level_number + 1
            putStrLn $ printf "Now starting on level %i... (bandwidth=2 sweeps will not be displayed)" new_level_number
            writeIORef level_number_ref new_level_number
            alwaysDeclareVictory chain
        callback_to_increase_bandwidth chain = do
            next_bandwidth <- readIORef next_bandwidth_ref
            writeIORef next_bandwidth_ref (next_bandwidth+bandwidth_increment)
            increaseChainBandwidth 2 next_bandwidth chain
        callback_after_each_sweep victory_flag latest_chain = do
            heading <- getHeading
            next_bandwidth <- readIORef next_bandwidth_ref
            let current_bandwidth = next_bandwidth-bandwidth_increment
            unless (current_bandwidth <= 2) $
                putStrLn $ heading ++ (printf "bandwidth = %i, sweep energy = %f" current_bandwidth (chainEnergy latest_chain) )
    -- @-node:gcross.20091205211300.1710:<< Define callbacks >>
    -- @nl

    -- @    << Run simulation >>
    -- @+node:gcross.20091202133456.1303:<< Run simulation >>
    (energies,_,_) <- fmap unzip3 $
        solveForMultipleLevelsWithCallbacks
            callback_to_decide_whether_to_declare_victory_with_trial
            (newChainCreator
                (writeIORef next_bandwidth_ref (initial_bandwidth+bandwidth_increment))
                operator_site_tensors
                2 initial_bandwidth
            )
            callback_to_increase_bandwidth
            callback_after_each_sweep
            ignoreSiteCallback
            bandwidth_increase_energy_change_convergence_criterion
            multisweep_energy_change_convergence_criterion
            0
            1000
            2
            []
    -- @-node:gcross.20091202133456.1303:<< Run simulation >>
    -- @nl

    putStrLn ""
    putStrLn "The energy levels are:"
    forM_ energies $ \energy -> do
        putStr "\t"
        putStrLn . show $ energy

    TimeSpec time_in_seconds _ <- getTime ProcessCPUTime

    putStrLn $ "The elapsed CPU time for this run was " ++ show time_in_seconds ++ " seconds."

-- @-node:gcross.20091120111528.1236:main
-- @-others
-- @-node:gcross.20100128100529.1239:@thin simulate-compass.hs
-- @-leo
