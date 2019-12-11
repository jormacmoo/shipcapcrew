# shipcapcrew
R package for 'Ship, Captain, Crew' Dice Game

# Description of Game

## How to Play

### Rules

Ship, Captain, Crew is a game that requires five six-sided dice and usually at least two players. Each player has at most three rolls they can use per game, the initial roll and two optional rerolls. Using their three rolls, each player tries to obtain a ship, a captain, and a crew, in that specific order. A ship is represented by a 6, a captain is represented by 5, and a crew is represented by a 4. Once these three dice are obtained, the sum of the remaining two dice (their cargo) is the player's score. If a player is unable to assemble a ship, captain, and crew, then their score is zero. The player with the highest cargo score wins the game. 

### Example

On Charlie's first roll, they get: 6,4,3,3,2. They are able to keep the 6, however they are not able to keep the 4, since they did not roll a 5. Charlie now has a ship, four remaining dice, and two more rolls. On Jordan's first roll, they get: 5, 4, 3, 3, 2. The are unable to keep any of their dice, since they did not roll a 6. Jordan has nothing, five remaining dice, and two more rolls.

On Charlie's second roll, they get: 5, 3, 2, 1. They are able to keep the 5, since they already have a 6. Charlie now has a ship, a captain, three remaining dice, and one more roll. On Jordan's second roll, they get: 6, 6, 5, 3, 2. They are able to keep one 6 and the 5. Jordan now has a ship, a captain, three remaining dice, and one more roll.

On Charlie's third roll, they get: 6, 5, 2. They are unable to keep any of these dice, since they were not able to obtain a crew. Charlie now has a ship, a captain, three remaining dice, and no more rolls. Since Charlie was unable to assemble a crew, they have a score of 0. On Jordan's third roll, they get: 6, 4, 1. They are able to keep the 4, since they already have a 6 and a 5. This means that their remaining dice, added together is 7, represents their cargo score. 

Since Jordan's final score is 7 and Charlie's final score is 0, Jordan wins the game!

# Two Main Components of The Package

## Interactive Game Play



## Game play Simulations

