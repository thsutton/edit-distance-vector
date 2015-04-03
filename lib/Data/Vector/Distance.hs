{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

-- | Description: Calculate differences between vectors.
--
-- This module implements a variation on the Wagner-Fischer algorithm to find
-- the shortest sequences of operations which transformers one vector of values
-- into another.
module Data.Vector.Distance where

import           Control.Applicative
import           Control.Arrow       ((***))
import           Data.Function
import           Data.List           hiding (delete, insert)
import           Data.Maybe
import           Data.Monoid
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

-- | Operations invoked by the Wagner-Fischer algorithm.
data Params v o c = Params
    { equivalent     :: v -> v -> Bool
    -- ^ Are two values equivalent?
    , delete         :: Int -> v -> o
    -- ^ Delete the element at an index.
    , insert         :: Int -> v -> o
    -- ^ Insert an element at an index.
    , substitute     :: Int -> v -> v -> o
    -- ^ Substitute an element at an index.
    , cost           :: o -> c
    -- ^ Cost of a change.
    , positionOffset :: o -> Int
    -- ^ Positions to advance after a change. E.g. @0@ for a deletion.
    }

str :: Params Char (String, Int, Char) (Sum Int)
str = Params{..}
  where
    equivalent = (==)
    delete i c = ("delete", i, c)
    insert i c = ("insert", i, c)
    substitute i a a' = ("replace", i, a')
    cost _ = 1
    positionOffset ("delete", _, _) = 0
    positionOffset _ = 1

-- | Find the least-cost sequence of changes to transform one vector into
-- another.
leastChanges :: (Monoid c, Ord c) => Params v o c -> Vector v -> Vector v -> (c, [o])
leastChanges p ss tt = fmap (catMaybes . reverse) . V.last $ changes p ss tt

-- | Matrix used to find least cost sequence of changes.
type ChangeMatrix o c = Vector (c, [Maybe o])

-- | Calculate the complete matrix of edit scripts and costs between two
-- vectors.
--
-- This is a fairly direct implementation of Wagner-Fischer algorithm cast in
-- terms of 'Vector'.
changes
    :: (Monoid c, Ord c)
    => Params v o c
    -> Vector v -- ^ \"Source\" vector.
    -> Vector v -- ^ \"Destination" vector.
    -> ChangeMatrix o c
changes p@Params{..} src dst =
    let len_x = 1 + V.length dst
        len_y = 1 + V.length src
        len_n = len_x * len_y
        ix x y = (x * len_y) + y
        -- Get a cell from the 'ChangeMatrix'. It is an error to get a cell
        -- which hasn't been calculated yet!
        get :: ChangeMatrix o c -> Int -> Int -> (c, [Maybe o])
        get m x y = fromMaybe (error $ "Unable to get " <> show (x,y) <> " from change matrix") (m V.!? (ix x y))
        -- Calculate the position to be updated by the next edit in a script.
        position = sum . fmap (maybe 1 positionOffset)
        -- Given a partially complete 'ChangeMatrix', compute the next cell.
        ctr v = case V.length v `quotRem` len_y of
            -- Do nothing for "" ~> ""
            (        0,         0) -> (mempty, mempty)
            -- Delete everything in src for "..." ~> ""
            (        0, pred -> y) ->
                let o = delete 0 (src V.! y)
                    (pc, po) = get v 0 y
                in (cost o <> pc, Just o : po)
            -- Insert everything in dst for "" ~> "..."
            (pred -> x,         0) ->
                let o = insert x (fromMaybe (error "NAH") $ dst V.!? x)
                    (pc, po) = get v x 0
                in (cost o <> pc, Just o : po)
            -- Compare options between src and dst for "..." ~> "..."
            (pred -> x, pred -> y) ->
                let s = src V.! y
                    d = dst V.! x
                    tl   = get v (x)   (y)
                    top  = get v (x+1) (y)
                    left = get v (x)   (y+1)
                in if s `equivalent` d
                    then (Nothing:) <$> get v x y
                    else minimumBy (compare `on` fst)
                        -- Option 1: perform a deletion.
                        [ let c = delete (position . snd $ top) s
                          in (cost c <>) *** (Just c :) $ top
                        -- Option 2: perform an insertion.
                        , let c = insert (position . snd $ left) d
                          in (cost c <>) *** (Just c :) $ left
                        -- Option 3: perform a substitution.
                        , let c = substitute (position . snd $ tl) s d
                          in (cost c <>) *** (Just c :) $ tl
                        ]
    in V.constructN len_n ctr
