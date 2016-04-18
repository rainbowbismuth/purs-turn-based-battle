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

module Main(main) where

import Prelude (Unit, bind)
import Combatant(Combatant)
import Id(Id(..))
import Class(Class(..))
import Player(Player(..))
import Data.Maybe(Maybe(..))
import UserInterface
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Signal.Channel (CHANNEL)
import Pux (start, fromSimple, renderToDOM)

initialCombatants :: Array Combatant
initialCombatants =
  let
    warrior =
      Combatant.mkCombatant Warrior

    thief =
      Combatant.mkCombatant Thief

    cleric =
      Combatant.mkCombatant Cleric
  in
    [ warrior { player = User, id = Id 0, name = "Alpha" }
    , thief { player = User, id = Id 1, name = "Beta" }
    , cleric { player = User, id = Id 2, name = "Gamma" }
    , warrior { player = AI, id = Id 3, name = "Delta" }
    , thief { player = AI, id = Id 4, name = "Epsilon" }
    , cleric { player = AI, id = Id 5, name = "Zeta" }
    ]

initialModel :: UserInterface.Model
initialModel =
    { mov : Nothing
    , sim :
        Simulation.clockTickUntilTurn
          { combatLog : []
          , activeCombatant : Nothing
          , combatants : initialCombatants
          }
    }

main :: forall e. Eff (err :: EXCEPTION, channel :: CHANNEL | e) Unit
main = do
  app <- start
    { initialState: initialModel
    , update: fromSimple UserInterface.update
    , view: UserInterface.view
    , inputs: []
    }

  renderToDOM "#app" app.html
