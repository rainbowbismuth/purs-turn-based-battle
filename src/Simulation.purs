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


module Simulation where

import Prelude ((++), (<>), (&&), ($), (+), (-), show, (*), (>>>), (>), map, (#), (==), (/=), (||))
import Data.Foldable (foldr)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Int (toNumber)
import Elm.Basics (ceiling)
import Elm.Debug as Debug
import Player (Player(..))
import Combatant (Combatant)
import Command (Command(..))
import Move (Move(..))
import Id (Id(..), fromId)


type Simulation =
  { combatants :: Array Combatant
  , activeCombatant :: Maybe Id
  , combatLog :: Array String
  }


combatants :: Simulation -> Array Combatant
combatants sim =
  sim.combatants


lost :: Player -> Simulation -> Boolean
lost player sim =
  let
    reducer cmbt x =
      if cmbt.player == player then
        x && Combatant.dead cmbt
      else
        x
  in
    foldr reducer true sim.combatants


gameOver :: Simulation -> Boolean
gameOver sim =
  lost AI sim || lost User sim


party :: Player -> Simulation -> Array Combatant
party player sim =
  Array.filter (\cmbt -> cmbt.player == player) sim.combatants


enemies :: Player -> Simulation -> Array Combatant
enemies player sim =
  Array.filter (\cmbt -> cmbt.player /= player) sim.combatants


findActiveCmbt :: Simulation -> Maybe Combatant
findActiveCmbt sim =
  let
    recur i =
      case Array.index sim.combatants i of
        Just cmbt ->
          if Combatant.canHaveActiveTurn cmbt then
            Just cmbt
          else
            recur (i + 1)

        Nothing ->
          Nothing
  in
    recur 0


activeCmbt :: Simulation -> Maybe Combatant
activeCmbt sim =
  case sim.activeCombatant of
    Just id ->
      Just (combatantByIdMustExist id sim)

    Nothing ->
      Nothing


activeCmbtMustExist :: Simulation -> Combatant
activeCmbtMustExist sim =
  case activeCmbt sim of
    Just cmbt ->
      cmbt

    Nothing ->
      Debug.crash "this should not be possible"


doIHaveActiveTurn :: Id -> Simulation -> Boolean
doIHaveActiveTurn id sim =
  case activeCmbt sim of
    Just cmbt ->
      cmbt.id == id

    Nothing ->
      false


dropActiveTurn :: Simulation -> Simulation
dropActiveTurn sim =
  let
    cmbt =
      activeCmbtMustExist sim

    nextCmbt =
      Combatant.payTurnCT cmbt
  in
    clockTickUntilTurn $
      sim { combatants = ourUpdateAt (fromId cmbt.id) nextCmbt sim.combatants, activeCombatant = Nothing }


clockTickUntilTurn :: Simulation -> Simulation
clockTickUntilTurn initialSim =
  if gameOver initialSim then
    initialSim
  else
    let
      recur sim =
        case findActiveCmbt sim of
          Just cmbt ->
            let
              nextCmbt =
                Combatant.increaseAP cmbt

              nextCombatants =
                ourUpdateAt (fromId nextCmbt.id) nextCmbt sim.combatants
            in
              sim { activeCombatant = Just cmbt.id, combatants = nextCombatants }

          Nothing ->
            recur (clockTick sim)
    in
      case initialSim.activeCombatant of
        Just _ ->
          initialSim

        Nothing ->
          recur initialSim


whosTurn :: Simulation -> Maybe Player
whosTurn sim =
  activeCmbt sim
    # map (\cmbt -> cmbt.player)


clockTick :: Simulation -> Simulation
clockTick sim =
  sim { combatants = map Combatant.clockTick sim.combatants }


turnOrderArray :: Simulation -> Array Combatant
turnOrderArray initialSim =
  let
    recur i sim acc =
      if i > 0 then
        case activeCmbt sim of
          Just cmbt ->
            acc <> [cmbt]
              # recur (i - 1) (clockTickUntilTurn (dropActiveTurn sim))

          Nothing ->
            recur (i - 1) (clockTickUntilTurn sim) acc
      else
        acc
  in
    recur 12 initialSim []


combatantById :: Id -> Simulation -> Maybe Combatant
combatantById (Id id) sim =
  Array.index sim.combatants id


combatantByIdMustExist :: Id -> Simulation -> Combatant
combatantByIdMustExist id sim =
  case combatantById id sim of
    Just cmbt ->
      cmbt

    Nothing ->
      Debug.crash "combatant with this ID must exist!"


ourUpdateAt :: forall a. Int -> a -> Array a -> Array a
ourUpdateAt idx a as =
  case Array.updateAt idx a as of
    Just as' -> as'
    Nothing -> as


modifyById :: (Combatant -> Combatant) -> Id -> Simulation -> Simulation
modifyById f (Id id) sim =
  case Array.index sim.combatants id of
    Just cmbt ->
      sim { combatants = ourUpdateAt id (f cmbt) sim.combatants }

    Nothing ->
      sim


existsAndAlive :: Id -> Simulation -> Boolean
existsAndAlive id sim =
  case combatantById id sim of
    Just cmbt ->
      Combatant.alive cmbt

    Nothing ->
      false


targetReaction :: Combatant -> Move -> Combatant -> Tuple Combatant (Array String)
targetReaction user mv target =
  case mv of
    Attack ->
      let
        dmg =
          (ceiling >>> toNumber) (Combatant.strength user * Combatant.defense target * 20.0)

        msg =
          user.name ++ " deals " ++ (show dmg) ++ " damage to " ++ target.name ++ "."
      in
        Tuple (target { hitPoints = target.hitPoints - dmg }) [ msg ]

    Heal ->
      let
        msg =
          user.name ++ " heals " ++ target.name ++ " for 45 hitpoints."
      in
        Tuple (target { hitPoints = target.hitPoints + 45.0 }) [ msg ]

    _ ->
      Debug.crash "not a single target move"


selfReaction :: Combatant -> Move -> Tuple Combatant (Array String)
selfReaction user mv =
  case mv of
    Defend ->
      Tuple (Combatant.increaseAP (Combatant.toDefendState user)) [ user.name ++ " has started defending" ]

    _ ->
      Debug.crash "not a self targetting move"


simulate :: Command -> Simulation -> Maybe Simulation
simulate cmd initialSim =
  let
    tryPay sim mv cmbt =
      case Combatant.payAP (Move.cost mv) cmbt of
        Just nextCmbt ->
          Just $ Tuple nextCmbt (sim { combatants = ourUpdateAt (fromId nextCmbt.id) nextCmbt sim.combatants } )

        Nothing ->
          Nothing

    with sim cmbt =
      case cmd of
        SingleTarget mv tid ->
          case tryPay sim mv cmbt of
            Just (Tuple nextCmbt nextSim) ->
              if Combatant.moveAvailable mv nextCmbt && existsAndAlive tid nextSim then
                let
                  target =
                    combatantByIdMustExist tid nextSim
                  tup = targetReaction nextCmbt mv (combatantByIdMustExist tid nextSim)
                  newTarget = fst tup
                  logStuff = snd tup

                  nextNextSim =
                    nextSim {
                        combatants = ourUpdateAt (fromId tid) newTarget nextSim.combatants
                      , combatLog = logStuff ++ nextSim.combatLog
                    }
                in
                  Just (dropActiveTurn nextNextSim)
              else
                Nothing

            Nothing ->
              Nothing

        SelfTarget mv ->
          -- TODO: Ignoring because no self target move that costs AP
          if Combatant.moveAvailable mv cmbt then
            let
              tup = selfReaction cmbt mv
              newUser = fst tup
              logStuff = snd tup

              nextSim =
                sim {
                    combatants = ourUpdateAt (fromId cmbt.id) newUser sim.combatants
                  , combatLog = logStuff ++ sim.combatLog
                }
            in
              Just (dropActiveTurn nextSim)
          else
            Nothing
  in
    case activeCmbt initialSim of
      Just cmbt ->
        let
          sim =
            modifyById Combatant.toDefaultState cmbt.id initialSim

          nextCmbt =
            case activeCmbt sim of
              Just c ->
                c

              Nothing ->
                Debug.crash "should never happen"
        in
          with sim nextCmbt

      Nothing ->
        Nothing
