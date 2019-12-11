# author: jormacmoo

start_game <- function(...) {
  prev_rolls = 0
  # the first roll will always have five dice
  list[turn1, score] <- game_turn(prev_rolls, 5)

  # give player option to reroll
  player_ans <- reroll_prompt(prev_rolls+1)

  # if player wants to reroll, go to second turn
  if (player_ans == TRUE) {
    list[turn2, score] <- game_turn(1, turn1)
  } else if (player_ans == FALSE) {
    play_again(score)
  }

  # give player option to reroll
  player_ans <- reroll_prompt(prev_rolls+1)

  # if player wants to reroll, go to third turn
  if (player_ans == TRUE) {
    list[turn3, score] <- game_turn(2, turn2)
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
  roll <- reroll(dice_to_roll)
  list[good_dice, leftover_dice] <- check_roll(roll)
  score <- find_score(good_dice, leftover_dice)
  prev_rolls = prev_rolls + 1
  player_message(good_dice, score, prev_rolls+1)

  return(list(length(leftover_dice), score))
}
