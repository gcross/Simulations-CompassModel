-- @+leo-ver=4-thin
-- @+node:gcross.20100128100529.1739:@thin simulate-compass-pbc.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100128100529.1740:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20100128100529.1740:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100128100529.1741:<< Import needed modules >>
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
-- @-node:gcross.20100128100529.1741:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100128100529.1742:Operator tensors
makeModelOperatorSiteTensors :: Double -> Int -> Int -> [OperatorSiteTensor]
makeModelOperatorSiteTensors angle width height =
    let makeRow left middle right =
            [left] ++ replicate (width-2) middle ++ [right]

        vertical_obc_start = 5
        vertical_obc_range = [vertical_obc_start..vertical_obc_start+width-1]
        vertical_pbc_start = 5 + width
        vertical_pbc_range = [vertical_pbc_start..vertical_pbc_start+width-1]

        horizontal_obc = 3
        horizontal_pbc = 4

        bandwidth = maximum
            [horizontal_obc
            ,horizontal_pbc
            ,last vertical_obc_range
            ,last vertical_pbc_range
            ]

        propagateRange = concat . map propagate

        propagate index =
            [(index_ --> index_) pI]
          where
            index_ = fromIntegral index

        throwHorizontal index =
            [(1 --> fromIntegral index) (((-cos angle) :+ 0) *: pX)]
        catchHorizontal index =
            [(fromIntegral index --> 2) pX]

        throw_horizontal_obc = throwHorizontal horizontal_obc
        catch_horizontal_obc = catchHorizontal horizontal_obc
        throw_or_catch_horizontal_obc = throw_horizontal_obc ++ catch_horizontal_obc

        throw_horizontal_pbc = throwHorizontal horizontal_pbc
        catch_horizontal_pbc = catchHorizontal horizontal_pbc
        propagate_horizontal_pbc = propagate horizontal_pbc

        throwVertical index =
            [(1 --> fromIntegral index) (((-sin angle) :+ 0) *: pZ)]
        catchVertical index =
            [(fromIntegral index --> 2) pZ]

        throwVerticalOBC = throwVertical . (vertical_obc_start +)
        catchVerticalOBC = catchVertical . (vertical_obc_start +)
        throwOrCatchVerticalOBC = throwVertical <^(++)^> catchVertical

        propagate_vertical_obc = propagateRange vertical_obc_range

        throwVerticalPBC = throwVertical . (vertical_pbc_start +)
        catchVerticalPBC = catchVertical . (vertical_pbc_start +)

        propagate_vertical_pbc = propagateRange vertical_pbc_range

        top =
            [makeOperatorSiteTensorFromSpecification 1 bandwidth $
             propagate 1
                ++ throw_horizontal_obc ++ throw_horizontal_pbc
                ++ throwVerticalOBC 0 ++ throwVerticalPBC 0
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             propagateRange [1,2]
                ++ throw_or_catch_horizontal_obc ++ propagate_horizontal_pbc
                ++ throwVerticalOBC column ++ throwVerticalPBC column
                ++ propagate_vertical_obc ++ propagate_vertical_pbc
            | column <- [1..width-2]
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             propagateRange [1,2]
                ++ catch_horizontal_obc ++ catch_horizontal_pbc
                ++ throwVerticalOBC (width-1) ++ throwVerticalPBC (width-1)
                ++ propagate_vertical_obc ++ propagate_vertical_pbc
            ]

        middle =
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             propagateRange [1,2]
                ++ throw_horizontal_obc ++ throw_horizontal_pbc
                ++ throwOrCatchVerticalOBC 0
                ++ propagate_vertical_obc ++ propagate_vertical_pbc
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             propagateRange [1,2]
                ++ throw_or_catch_horizontal_obc ++ propagate_horizontal_pbc
                ++ throwOrCatchVerticalOBC column
                ++ propagate_vertical_obc ++ propagate_vertical_pbc
            | column <- [1..width-2]
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             propagateRange [1,2]
                ++ catch_horizontal_obc ++ catch_horizontal_pbc
                ++ throwOrCatchVerticalOBC (width-1)
                ++ propagate_vertical_obc ++ propagate_vertical_pbc
            ]

        bottom =
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             propagateRange [1,2]
                ++ throw_horizontal_obc ++ throw_horizontal_pbc
                ++ catchVerticalOBC 0 ++ catchVerticalPBC 0
                ++ propagate_vertical_obc ++ propagate_vertical_pbc
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             propagateRange [1,2]
                ++ throw_or_catch_horizontal_obc ++ propagate_horizontal_pbc
                ++ catchVerticalOBC column ++ catchVerticalPBC column
                ++ propagate_vertical_obc ++ propagate_vertical_pbc
            | column <- [1..width-2]
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth 1 $
                [(2 --> 1) pI
                ,(3 --> 1) pX
                ,(4 --> 1) pX
                ,((fromIntegral . last) vertical_obc_range --> 1) pZ
                ,((fromIntegral . last) vertical_pbc_range --> 1) pZ
                ]
            ]

    in top ++ (concat . replicate (height-2)) middle ++ bottom
-- @-node:gcross.20100128100529.1742:Operator tensors
-- @+node:gcross.20100128100529.1743:analyzeTrialEnergies
data TrialAnalysis = TrialDidBetter | TrialDidWorse | TrialDidTheSame

analyzeTrialEnergies tolerance best_energy trial_energy
    | best_energy - trial_energy > tolerance = TrialDidBetter
    | trial_energy - best_energy > tolerance = TrialDidWorse
    | otherwise = TrialDidTheSame
-- @-node:gcross.20100128100529.1743:analyzeTrialEnergies
-- @+node:gcross.20100128100529.1744:main
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
    -- @+node:gcross.20100128100529.1745:<< Define callbacks >>
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
    -- @-node:gcross.20100128100529.1745:<< Define callbacks >>
    -- @nl

    -- @    << Run simulation >>
    -- @+node:gcross.20100128100529.1746:<< Run simulation >>
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
    -- @-node:gcross.20100128100529.1746:<< Run simulation >>
    -- @nl

    putStrLn ""
    putStrLn "The energy levels are:"
    forM_ energies $ \energy -> do
        putStr "\t"
        putStrLn . show $ energy

    TimeSpec time_in_seconds _ <- getTime ProcessCPUTime

    putStrLn $ "The elapsed CPU time for this run was " ++ show time_in_seconds ++ " seconds."

-- @-node:gcross.20100128100529.1744:main
-- @-others
-- @-node:gcross.20100128100529.1739:@thin simulate-compass-pbc.hs
-- @-leo
