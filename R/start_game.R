# author: jormacmoo

start_game <- function(...) {
  prev_rolls = 0
  # the first roll will always have five dice
  turn1 <- game_turn(prev_rolls, 5)

  # give player option to reroll
  reroll_prompt(turn1)

  # if player wants to reroll, go to second turn
  turn2 <- game_turn(1, turn1)

  # give player option to reroll
  reroll_prompt(turn2)

  # if player wants to reroll, go to second turn
  turn3 <- game_turn(2, turn2)

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
  player_message(good_dice, score, prev)

  return(length(leftover_dice))
}
