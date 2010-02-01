-- @+leo-ver=4-thin
-- @+node:gcross.20100130190931.1265:@thin simulate-compass.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100130190931.1266:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20100130190931.1266:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100130190931.1267:<< Import needed modules >>
import Acme.Dont

import Control.Applicative.Infix
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
-- @-node:gcross.20100130190931.1267:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100130190931.1274:Operator tensors
makeModelOperatorSiteTensors :: Double -> Int -> Int -> [OperatorSiteTensor]
makeModelOperatorSiteTensors factor width height =
    let --angle = factor * pi/2
        --x_factor = cos angle
        --z_factor = sin angle
        z_factor = factor
        x_factor = 1-factor

        x_scale = (-x_factor) :+ 0
        z_scale = (-z_factor) :+ 0

        horizontal_obc = 3

        vertical_obc_start = 4
        vertical_obc_range = [vertical_obc_start..vertical_obc_start+width-1]

        bandwidth = maximum
            [horizontal_obc
            ,last vertical_obc_range
            ]

        passRange = concat . map pass

        pass index =
            [(index_ --> index_) pI]
          where
            index_ = fromIntegral index

        throw operator index =
            [(1 --> fromIntegral index) operator]
        catch operator index =
            [(fromIntegral index --> 2) operator]

        throwHorizontal = throw (x_scale *: pX)
        catchHorizontal = catch pX

        throw_horizontal_obc = throwHorizontal horizontal_obc
        catch_horizontal_obc = catchHorizontal horizontal_obc
        throw_or_catch_horizontal_obc = throw_horizontal_obc ++ catch_horizontal_obc

        throwVertical = throw (z_scale *: pZ)
        catchVertical = catch pZ

        throwVerticalOBC = throwVertical . (vertical_obc_start +)
        catchVerticalOBC = catchVertical . (vertical_obc_start +)
        throwOrCatchVerticalOBC = throwVerticalOBC <^(++)^> catchVerticalOBC

        passVerticalOBCRange = passRange . map (vertical_obc_start +)
        passThrownVerticalOBC column = passVerticalOBCRange [0..column-1]
        passCaughtVerticalOBC column = passVerticalOBCRange [column+1..width-1]
        passThrownAndCaughtVerticalOBC = passThrownVerticalOBC <^(++)^> passCaughtVerticalOBC

        top =
            [makeOperatorSiteTensorFromSpecification 1 bandwidth $
             pass 1
                ++ throw_horizontal_obc ++ throwVerticalOBC 0
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passThrownVerticalOBC column
                ++ throw_or_catch_horizontal_obc ++ throwVerticalOBC column
            | column <- [1..width-2]
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passThrownVerticalOBC (width-1)
                ++ catch_horizontal_obc ++ throwVerticalOBC (width-1)
            ]

        middle =
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passThrownAndCaughtVerticalOBC 0
                ++ throw_horizontal_obc ++ throwOrCatchVerticalOBC 0
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passThrownAndCaughtVerticalOBC column
                ++ throw_or_catch_horizontal_obc ++ throwOrCatchVerticalOBC column
            | column <- [1..width-2]
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passThrownAndCaughtVerticalOBC (width-1)
                ++ catch_horizontal_obc ++ throwOrCatchVerticalOBC (width-1)
            ]

        bottom =
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passCaughtVerticalOBC 0
                ++ throw_horizontal_obc ++ catchVerticalOBC 0
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passCaughtVerticalOBC column
                ++ throw_or_catch_horizontal_obc ++ catchVerticalOBC column
            | column <- [1..width-2]
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth 1 $
                [(2 --> 1) pI
                ,(3 --> 1) pX
                ,((fromIntegral . last) vertical_obc_range --> 1) pZ
                ]
            ]

    in top ++ (concat . replicate (height-2)) middle ++ bottom
-- @-node:gcross.20100130190931.1274:Operator tensors
-- @+node:gcross.20100130190931.1269:analyzeTrialEnergies
data TrialAnalysis = TrialDidBetter | TrialDidWorse | TrialDidTheSame

analyzeTrialEnergies tolerance best_energy trial_energy
    | best_energy - trial_energy > tolerance = TrialDidBetter
    | trial_energy - best_energy > tolerance = TrialDidWorse
    | otherwise = TrialDidTheSame
-- @-node:gcross.20100130190931.1269:analyzeTrialEnergies
-- @+node:gcross.20100130190931.1270:main
main = do
    args <- getArgs

    let angle = read $ args !! 0
        width = read $ args !! 1
        height = read $ args !! 2
        operator_site_tensors = makeModelOperatorSiteTensors angle width height
        initial_bandwidth = 2
        increment_factor = 1.5 :: Float
        bandwidth_increase_energy_change_convergence_criterion = 1e-5
        multisweep_energy_change_convergence_criterion = 1e-4

    putStrLn $ "Angle = " ++ show angle
    putStrLn $ "Width = " ++ show width
    putStrLn $ "Height = " ++ show height
    putStrLn $ "Total = " ++ show (length operator_site_tensors)

    -- @    << Define callbacks >>
    -- @+node:gcross.20100130190931.1271:<< Define callbacks >>
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
            writeIORef next_bandwidth_ref (floor $ fromIntegral next_bandwidth*increment_factor)
            increaseChainBandwidth 2 next_bandwidth chain
        callback_after_each_sweep victory_flag latest_chain = do
            heading <- getHeading
            next_bandwidth <- readIORef next_bandwidth_ref
            let current_bandwidth = (ceiling $ fromIntegral next_bandwidth/increment_factor :: Int) `div` 2
            unless (current_bandwidth <= 2) $
                putStrLn $ heading ++ (printf "bandwidth = %i, sweep energy = %f" current_bandwidth (chainEnergy latest_chain) )
    -- @-node:gcross.20100130190931.1271:<< Define callbacks >>
    -- @nl

    -- @    << Run simulation >>
    -- @+node:gcross.20100130190931.1272:<< Run simulation >>
    (energies,_,_) <- fmap unzip3 $
        solveForMultipleLevelsWithCallbacks
            callback_to_decide_whether_to_declare_victory_with_trial
            (newChainCreator
                (writeIORef next_bandwidth_ref ((floor $ fromIntegral initial_bandwidth*increment_factor)))
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
            3
            []
    -- @-node:gcross.20100130190931.1272:<< Run simulation >>
    -- @nl

    putStrLn ""
    putStrLn "The energy levels are:"
    forM_ energies $ \energy -> do
        putStr "\t"
        putStrLn . show $ energy

    TimeSpec time_in_seconds _ <- getTime ProcessCPUTime

    putStrLn $ "The elapsed CPU time for this run was " ++ show time_in_seconds ++ " seconds."

-- @-node:gcross.20100130190931.1270:main
-- @-others
-- @-node:gcross.20100130190931.1265:@thin simulate-compass.hs
-- @-leo
