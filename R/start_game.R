# author: jormacmoo

start_game <- function(...) {
  prev_rolls <- 0
  # the first roll will always have five dice
  output <- game_turn(prev_rolls, 5)
  turn1 <- output[[1]]
  score <- output[[2]]

  # give player option to reroll
  player_ans <- reroll_prompt(prev_rolls+1)

  # if player wants to reroll, go to second turn
  if (player_ans == TRUE) {
    output <- game_turn(1, turn1)
    turn2 <- output[[1]]
    score <- output[[2]]
  } else if (player_ans == FALSE) {
    play_again(score)
  }

  # give player option to reroll
  player_ans <- reroll_prompt(prev_rolls+1)

  # if player wants to reroll, go to third turn
  if (player_ans == TRUE) {
    output <- game_turn(2, turn2)
    turn3 <- output[[1]]
    score <- output[[2]]
  } else if (player_ans == FALSE) {
    play_again(score)
  }

  # does player want to play again?
  plag <- play_again()
  if (TRUE) {
    start_game()
  } else {
    message("Thank you for playing!")
  }
}

game_turn <- function(prev_rolls, dice_to_roll) {
  if (prev_rolls == 0) {
    prev_dice <- NULL
  }
  roll <<- reroll(dice_to_roll)
  output <- check_roll(roll, prev_dice)
  good_dice <- output[[1]]
  leftover_dice <- output[[2]]
  score <- find_score(good_dice, leftover_dice)
  prev_rolls = prev_rolls + 1
  player_message(good_dice, score, prev_rolls+1)

  return(list(length(leftover_dice), score))
}
