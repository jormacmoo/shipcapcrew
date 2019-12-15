#game_functions
#Includes:
#      start_game: user facing function to play interactive single player game
#      game_turn: function to simulate a single turn

#'start_game
#'
#'User-playable ship, captain, crew game
#'
#'@param None
#'@family Interactive Gameplay
#'@examples
#'start_game()
#'@export
start_game <- function(...) {
  prev_rolls <- 0
  prev_dice <- NULL
  # the first roll will always have five dice
  output <- game_turn(prev_rolls, 5, prev_dice)
  turn1 <- output[[1]]
  score <- output[[2]]
  prev_dice <- output[[3]]

  # give player option to reroll
  player_ans <- reroll_prompt(prev_rolls+1)

  # if player wants to reroll, go to second turn
  if (player_ans == TRUE) {
    output <- game_turn(1, turn1, prev_dice)
    turn2 <- output[[1]]
    score <- output[[2]]
    prev_dice <- output[[3]]
  } else if (player_ans == FALSE) {
    play_again(score)
  }

  # give player option to reroll
  player_ans <- reroll_prompt(prev_rolls+1)

  # if player wants to reroll, go to third turn
  if (player_ans == TRUE) {
    output <- game_turn(2, turn2, prev_dice)
    turn3 <- output[[1]]
    score <- output[[2]]
    prev_dice <- output[[3]]
  } else if (player_ans == FALSE) {
    play_again(score)
  }

  # does player want to play again?
  plag <- play_again(score)
  if (plag == TRUE) {
    start_game()
  } else {
    message("Thank you for playing!")
  }
}

#'game_turn
#'
#'Simulate single game turn
#'
#'@param prev_rolls Number of turns already completed
#'@param dice_to_roll Number of dice to be rolled this turn
#'@param prev_dice Any dice kept from previous turns
#'@return List containing number of dice that can be rolled next turn, current player score, and dice that are kept
#'@family Interactive Gameplay
#'@examples
#'game_turn(0, 5, NULL)
#'game_turn(1, 3, c(6, 5))
#'@export
game_turn <- function(prev_rolls, dice_to_roll, prev_dice) {
  roll <<- reroll(dice_to_roll)
  output <- check_roll(roll, prev_dice)
  good_dice <- output[[1]]
  leftover_dice <- output[[2]]
  score <- find_score(good_dice, leftover_dice)
  prev_rolls = prev_rolls + 1
  player_message(good_dice, score, prev_rolls)

  return(list(length(leftover_dice), score, good_dice))
}
