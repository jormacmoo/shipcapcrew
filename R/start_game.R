# author: jormacmoo

start_game <- function(...) {
  # the first roll will always have five dice
  roll1 <- reroll(5)
  list[good_dice, leftover_dice] <- check_roll(roll1)
}
