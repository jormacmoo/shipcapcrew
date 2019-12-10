# author: jormacmoo

start_game <- function(...) {
  # the first roll will always have five dice
  roll1 <- reroll(5)
  list[good_dice, leftover_dice] <- check_roll(roll1)
  score <- find_score(good_dice, leftover_dice)
  num_rolls <- 1

  # messages


  # give player option to reroll
  reroll_prompt(length(leftover_dice))

}
