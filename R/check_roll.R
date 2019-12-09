# author: jormacmoo

check_roll <- function(rolled_dice) {

  # Check for valid dice rolls
  if (length(rolled_dice) > 5) {
    stop("Too many dice rolled. Please input five dice rolls.")
  } else if (length(rolled_dice) < 5) {
    stop("Not enough dice rolled. Please input five dice rolls.")
  } else if (length(rolled_dice == 5)) {
    roll_nums <- map(rolled_dice, check_number)
    if (FALSE %in% roll_nums) {
      stop("Invalid roll. Please input numbers between 1 and 6.")
    }
  }

  # If a 6 has been rolled
  if (6 %in% rolled_dice) {
    i <- match(6, rolled_dice)
    dice_left <- rolled_dice[-i]

    # If there is a 6, is there a 5?
    if (5 %in% dice_left) {
      j <- match(5, dice_left)
      dice_left <- dice_left[-j]

      # If there is a 6 and a 5, is there a 4?
      if (4 %in% dice_left) {
        k <- match(4, dice_left)
        dice_left <- dice_left[-k]
      }
    }
  }
}
