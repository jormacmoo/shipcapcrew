player_message <- function(components, score, num_rolls) {
  if (!(num_rolls %in% c(1, 2, 3))) {
    stop("Invalid number of rolls.")
  }

  if (num_rolls == 1) {
    if (identical(components, c(6, 5, 4))) {
      message(cat("Congratulations! You've acquired a ship, captain, and crew. ",
                  "Your current score is", score, ".",
                  "You have two rolls left to try for a higher score."))
      return(0)
    } else if (identical(components, c(6, 5))) {
      message("You've acquired a ship and a captain, but you're still searching
              for a crew. You've still got two rolls to find one, so get searchin'.")
    } else if (identical(components, c(6))) {
      message("You've found yourself a ship, but you've yet to find a captain
              or a crew. You've still got two rolls to find 'em, so get searchin'.")
    } else if (is_empty(components)) {
      message("Unfortunately, you don't have a ship yet. That's the first step, but
              you've still got two rolls to find one. Onwards!")
    }
  } else if (num_rolls == 2) {
    if (identical(components, c(6, 5, 4))) {
      message(cat("Congratulations! You've acquired a ship, captain, and crew.
                  Your current score is ", score, ". You have one more roll to get a better score."))
    }

  } else if (num_rolls == 3) {

  }
}
