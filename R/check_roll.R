check_roll <- function(rolled_dice, prev_dice) {

  rolled_dice <- c(rolled_dice, prev_dice)

  check_number <- function(num) {
    if (num < 7) {
      return(TRUE)
    } else if (num >= 7) {
      return(FALSE)
    }
  }

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

  # Check for 6
  ship <- any(rolled_dice == 6)
  # Check for 5
  captain <- any(rolled_dice == 5)
  # Check for 4
  crew <- any(rolled_dice == 4)
  # Does player have all necessary pieces
  full <- all(ship, captain, crew)

  # Check for ship
  if (ship == TRUE) {
    i <- match(6, rolled_dice)
    dice_left <- rolled_dice[-i]
    good_dice <- c(6)

    # Check for captain
    if (captain == TRUE) {
      j <- match(5, dice_left)
      dice_left <- dice_left[-j]
      good_dice <- append(good_dice, 5)

      # Check for crew
      if (crew == TRUE) {
        k <- match(4, dice_left)
        dice_left <- dice_left[-k]
        good_dice <- append(good_dice, 4)

        if (full == TRUE && length(good_dice) != 3){
          stop("Stopping gameplay.")
        }
      }
    }
  } else if (ship == FALSE) {
    good_dice <- NULL
    dice_left <- rolled_dice
  }
  return(list(good_dice, dice_left))
}
