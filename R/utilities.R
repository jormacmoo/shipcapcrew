#Utility functions
#Includes:
#  reroll: Simulates a number of dice rolls between 1 and 5
#  find_score: Create score for dice roll after it is checked for necessary parts

reroll <- function(num_dice){
  if (is.numeric(num_dice) == FALSE && is.integer(num_dice) == FALSE) {
    stop("Bad input. Input must be a number between 1 and 5.")
  } else if (num_dice > 5 || num_dice < 1) {
    stop("Number out of range. Input must be a number between 1 and 5.")
  }
  roll <- sample(1:6, size = num_dice, replace = TRUE)
  if (num_dice == 1) {
    message(cat("You've rolled a ", roll))
  } else {
    message(cat("You've rolled ", roll))
  }
}

find_score <- function(needed, cargo) {
  perfect <- c(6, 5, 4)
  if (is_empty(needed)) {
    return(0)
  } else if (length(needed) < 3) {
    return(0)
  } else if (identical(needed, perfect)) {
    return(sum(cargo))
  }
}
