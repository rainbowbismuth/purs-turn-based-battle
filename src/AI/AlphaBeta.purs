{-
purs-turn-based-battle, a small browser game.
Copyright (C) 2016  Emily A. Bellows

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}


module AI.AlphaBeta where

import Prelude
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Ord (min, max)
import Data.Array as Array
import Simulation (Simulation)
import Combatant (Combatant)
import Player (Player(..))
import Command (Command(..), CommandType(..))
import Move (Move)
import Elm.Debug as Debug
import Data.Function (mkFn4, mkFn6, Fn4, runFn4, runFn6, Fn6)


scoreCombatant :: Combatant -> Number
scoreCombatant cmbt =
  let
    bonus =
      if Combatant.alive cmbt then
        50.0
      else
        0.0

    cScore =
      (max 0.0 cmbt.hitPoints) + bonus
  in
    if cmbt.player == AI then
      cScore
    else
      -cScore


score :: Simulation -> Number
score sim =
  foldr (\x y -> scoreCombatant x + y) 0.0 (Simulation.combatants sim)


targetsForMove :: Simulation -> Move -> Array Command
targetsForMove sim mv =
  let user = Simulation.activeCmbtMustExist sim
  in
    case Command.typeOfMove mv of
      SingleTargetType ->
        sim.combatants
          # Array.mapMaybe
              (\target ->
                if Combatant.shouldUseOn user mv target then
                  Just (SingleTarget mv target.id)
                else
                  Nothing
              )

      SelfTargetType ->
        [ SelfTarget mv ]


availableMoves :: Simulation -> Array Command
availableMoves sim =
  Combatant.moveArray (Simulation.activeCmbtMustExist sim)
    # Array.concatMap (targetsForMove sim)


inf :: Number
inf =
  256000.0


evaluatePosition :: Simulation -> Int -> Number
evaluatePosition sim depth =
  runFn4 alphabeta sim depth (-inf) inf


alphabeta :: Fn4 Simulation Int Number Number Number
alphabeta = mkFn4 $ \sim depth a b ->
  if depth == 0 || Simulation.gameOver sim then
    score sim
  else
    case Simulation.whosTurn sim of
      Just AI ->
        runFn6 alphabetaMaximizing (availableMoves sim) sim depth a b (-inf)

      Just User ->
        runFn6 alphabetaMinimizing (availableMoves sim) sim depth a b (inf)

      Nothing ->
        runFn4 alphabeta (Simulation.clockTick sim) depth a b


alphabetaMaximizing :: Fn6 (Array Command) Simulation Int Number Number Number Number
alphabetaMaximizing = mkFn6 $ \moves sim depth a b v ->
  case Array.uncons moves of
    Just {head = m, tail = ms} ->
      case Simulation.simulate m sim of
        Just nextSim ->
          let
            nextV =
              max v (runFn4 alphabeta nextSim (depth - 1) a b)

            nextA =
              max a nextV
          in
            if b < nextA then
              nextV
            else
              runFn6 alphabetaMaximizing ms sim depth nextA b nextV

        Nothing ->
          runFn6 alphabetaMaximizing ms sim depth a b v

    Nothing ->
      v


alphabetaMinimizing :: Fn6 (Array Command) Simulation Int Number Number Number Number
alphabetaMinimizing = mkFn6 $ \moves sim depth a b v ->
  case Array.uncons moves of
    Just {head = m, tail = ms} ->
      case Simulation.simulate m sim of
        Just nextSim ->
          let
            nextV =
              min v (runFn4 alphabeta nextSim (depth - 1) a b)

            nextB =
              min b nextV
          in
            if nextB < a then
              nextV
            else
              runFn6 alphabetaMinimizing ms sim depth a nextB nextV

        Nothing ->
          runFn6 alphabetaMinimizing ms sim depth a b v

    Nothing ->
      v


playAI :: Simulation -> Simulation
playAI sim =
  let
    depth = 10 - Array.length (Simulation.combatants sim)
    explore cmd =
      case Simulation.simulate cmd sim of
        Just nextSim ->
          let
            nextNextSim =
              Simulation.clockTickUntilTurn nextSim
          in
            Just { cmd : cmd, sim : nextNextSim, score : evaluatePosition nextNextSim depth }

        Nothing ->
          Nothing

    head =
      availableMoves sim
        # Array.mapMaybe explore
        # Array.sortBy (\x y -> -(x.score) `compare` -(y.score))
        # Array.head
  in
    case head of
      Just r ->
        r.sim

      Nothing ->
        Debug.crash "this shouldn't happen"
