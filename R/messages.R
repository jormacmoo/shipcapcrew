player_message <- function(components, score, num_rolls) {
  if (!(num_rolls %in% c(1, 2, 3))) {
    stop("Invalid number of rolls.")
  }

  if (num_rolls == 1) {
    if (identical(components, c(6, 5, 4))) {
      message(cat("Congratulations! You've acquired a ship, captain, and crew.",
                  "Your current score is", score,
                  "but you have two rolls left to try for a higher score."))
    } else if (identical(components, c(6, 5))) {
      message(cat("You've acquired a ship and a captain, but you're still searching for a crew.",
              "You've still got two rolls to find one, so get searchin'."))
    } else if (identical(components, c(6))) {
      message(cat("You've found yourself a ship, but you've yet to find a captain or a crew.",
              "You've still got two rolls to find 'em, so get searchin'."))
    } else if (is_empty(components)) {
      message(cat("Unfortunately, you don't have a ship yet.",
              "That's the first step, but you've still got two rolls to find one. Onwards!"))
    }
  } else if (num_rolls == 2) {
    if (identical(components, c(6, 5, 4))) {
      message(cat("Congratulations! You've acquired a ship, captain, and crew. Your current score is",
                  score, "but you have one more roll to get a better score."))
    } else if (identical(components, c(6, 5))) {
      message(cat("You've got a ship and captain, but that elusive crew slipped through your fingers.",
                  "You have one more roll to find it, so get lookin'."))
    } else if (identical(components, c(6))) {
      message(cat("You've got a ship, but you don't have a captain or a crew.",
                  "You only have one more roll to find them, so cross your fingers."))
    } else if (is_empty(components)) {
      message(cat("Still no ship, still no captain, still no crew.",
                  "Keep your head up because you have one more roll to find them.",
                  "Get crackin'."))
    }
  } else if (num_rolls == 3) {
    if (identical(components, c(6, 5, 4))) {
      message(cat("Congratulations, you have a ship, captain, and crew!",
                  "Your current score is", score, "and you don't have any more rolls."))
    } else if (identical(components, c(6, 5))) {
      message(cat("You have a ship and a captain, but you didn't find a crew in time.",
                  "Unfortunately, your score is 0."))
    } else if (identical(components, c(6))) {
      message(cat("You found a ship, but you didn't find a captain or a crew.",
                  "Unfortunately, you're ending this game with a score of 0."))
    }
  }
}
