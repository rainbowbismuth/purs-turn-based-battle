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


module UserInterface where

import Prelude(const, ($), map, (#), (++), show, (>=), (-), (+), (*))
import Id (Id)
import Move (Move)
import Simulation (Simulation)
import Player (Player(..))
import Combatant (Combatant)
import Command (Command(..), CommandType(..))
import AI.AlphaBeta as AlphaBeta

import Pux.Html (Html, text, button, span, div)
import Pux.Html.Attributes (className, style)
import Pux.Html.Events (onClick)

import Data.Array as Array
import Elm.Basics(ceiling)
import Elm.String(repeat)
import Elm.Debug as Debug
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)


type Model =
  { sim :: Simulation
  , mov :: Maybe Move
  }


data Action
  = SelectMove Move
  | SelectTarget Id
  | CancelSelection


update :: Action -> Model -> Model
update action model =
  case action of
    SelectMove mv ->
      case Command.typeOfMove mv of
        SingleTargetType ->
          model { mov = Just mv }

        SelfTargetType ->
          case Simulation.simulate (SelfTarget mv) model.sim of
            Just sim ->
              model { sim = Simulation.clockTickUntilTurn (aiIfNecessary sim), mov = Nothing }

            Nothing ->
              Debug.crash "this should not be possible"

    SelectTarget id ->
      case model.mov of
        Just mov ->
          case Simulation.simulate (SingleTarget mov id) model.sim of
            Just sim ->
              model { sim = Simulation.clockTickUntilTurn (aiIfNecessary sim), mov = Nothing }

            Nothing ->
              Debug.crash "this should not be possible"

        Nothing ->
          Debug.crash "this should not be possible"

    CancelSelection ->
      model { mov = Nothing }


aiIfNecessary :: Simulation -> Simulation
aiIfNecessary sim =
  if Simulation.gameOver sim then
    sim
  else
    case Simulation.whosTurn (Simulation.clockTickUntilTurn sim) of
      Just AI ->
        aiIfNecessary (AlphaBeta.playAI sim)

      Just User ->
        sim

      Nothing ->
        Debug.crash "this should not be possible"


view :: Model -> Html Action
view   model =
  div
    [ className "game" ]
    [ div
        [ className "main" ]
        [ (div [ className "ai-party" ] [ viewParty   AI model ])
        , (div [ className "user-party" ] [ viewParty   User model ])
        , (viewCombatLog model.sim.combatLog)
        ]
    , viewCtBar model
    ]


viewCombatLog :: forall a. Array String -> Html a
viewCombatLog log =
  div
    [ className "combat-log" ]
    (log # Array.take 10 # indexedMap viewCombatLogLine)


viewCombatLogLine :: forall a. Int -> String -> Html a
viewCombatLogLine idx line =
  let
    op =
      show (1.0 - (toNumber idx * 8.0e-2))
  in
    div
      [ className "combat-log-line"
      , style { opacity : op }
      ]
      [ text line ]

indexedMap :: forall a b. (Int -> a -> b) -> Array a -> Array b
indexedMap f a =
  Array.zipWith f (Array.range 0 (Array.length a - 1)) a

viewCtBar :: forall a. Model -> Html a
viewCtBar model =
  let
    order =
      Simulation.turnOrderArray model.sim
  in
    div
      [ className "ct-bar" ]
      (Array.cons (text "Turn Order") (order # indexedMap viewCtBarUnit))


viewCtBarUnit :: forall a. Int -> Combatant -> Html a
viewCtBarUnit n cmbt =
  div
    [ className "ct-bar-unit" ]
    [ span [ className "ct-bar-unit-num" ] [ text (show (n + 1)) ]
    , text cmbt.name
    ]


viewParty :: Player -> Model -> Html Action
viewParty player model =
  div
    [ className "party" ]
    (Simulation.party player model.sim
      # map (viewCombatant   player model)
    )

viewCombatantStatusBar :: Player -> Model -> Combatant -> Html Action
viewCombatantStatusBar player model cmbt =
  div
      [ className "combatant-status-bar" ]
      [ (span [ className "combatant-name" ] [ text cmbt.name ])
      , (span [ className "combatant-class" ] [ text (show cmbt.class) ])
      , (div
          [ className "combatant-hp" ]
          [ span [ className "combatant-hp-label" ] [ text "HP" ]
          , text (show (ceiling cmbt.hitPoints))
          ]
        )
      , viewCombatantAP cmbt
      , (div
          [ className "combatant-ct" ]
          [ span [ className "combatant-ct-label" ] [ text "CT" ]
          , text (show cmbt.chargeTime)
          ]
        )
      ]

viewCombatantMoves :: Player -> Model -> Combatant -> Html Action
viewCombatantMoves player model cmbt =
  if Combatant.alive cmbt then
      case { a: Simulation.doIHaveActiveTurn cmbt.id model.sim, b: player, c: model.mov } of
        { a: true, b: User, c: Just mv } ->
          viewTargets player model

        { a: true, b: User, c: Nothing } ->
          viewMoves cmbt

        _ ->
          text ""
  else
    text ""

viewCombatant :: Player -> Model -> Combatant -> Html Action
viewCombatant player model cmbt =
  div
    [ if Combatant.alive cmbt then
        className "combatant combatant-alive"
      else
        className "combatant combatant-dead"
    ]
    [ viewCombatantStatusBar player model cmbt
    , viewCombatantMoves player model cmbt
    ]


viewCombatantAP :: forall a. Combatant -> Html a
viewCombatantAP cmbt =
  div
    [ className "combatant-ap" ]
    [ span [ className "combatant-ap-label" ] [ text "AP" ]
    , span [ className "combatant-ap-filled" ] [ text (repeat cmbt.actionPoints "•") ]
    , span [ className "combatant-ap-empty" ] [ text (repeat (5 - cmbt.actionPoints) "•") ]
    ]


viewMoves :: Combatant -> Html Action
viewMoves cmbt =
  div
    [ className "combatant-move-list" ]
    (Combatant.moveArray cmbt # map (viewMove   cmbt))


viewMove :: Combatant -> Move -> Html Action
viewMove unit mv =
  button
    (if unit.actionPoints >= Move.cost mv then
      [ className "combatant-move"
      , onClick   (const $ SelectMove mv)
      ]
     else
      [ className "combatant-move combatant-move-unusable" ]
    )
    [ text (show mv ++ " " ++ repeat (Move.cost mv) "•") ]


viewTargets :: Player -> Model -> Html Action
viewTargets player model =
  div
    [ className "combatant-target-list" ]
    [ div
        [ className "combatant-target-party" ]
        (Simulation.combatants model.sim
          # Array.filter (Combatant.foesOf player)
          # map (viewTarget  )
        )
    , div
        [ className "combatant-target-party" ]
        (Simulation.combatants model.sim
          # Array.filter (Combatant.friendsOf player)
          # map (viewTarget  )
        )
    , button
        [ className "combatant-target-cancel"
        , onClick   (const CancelSelection)
        ]
        [ text "Cancel" ]
    ]


viewTarget :: Combatant -> Html Action
viewTarget cmbt =
  let
    attributes =
      if Combatant.alive cmbt then
        [ className "combatant-target combatant-target-alive"
        , onClick   (const $ SelectTarget cmbt.id)
        ]
      else
        [ className "combatant-target combatant-target-dead" ]
  in
    button
      attributes
      [ text cmbt.name ]
