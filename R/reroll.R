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
