#interactive_helpers
#Includes:
#    check_roll: separates rolled dice into those that can be kept and those
#                that can be rerolled
#    player_message: prints messages in response to player rolls
#    find_score: calculates current player score
#    reroll: randomly simulates dice rolls
#    reroll_prompt: asks player if they will to reroll and collects answer
#    play_again: at end of game, asks user if they will start a new game and collects answer

#'check_roll
#'
#'Helper function for interactive gameplay
#'Separate rolled dice into those that can be kept and those that can be rerolled
#'
#'@param rolled_dice - most recently rolled dice
#'@param prev_dice - any dice kept from previous turns
#'@return List containing a vector of the dice to be kept and a vector of dice to be rerolled
#'@import purrr
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
    roll_nums <- purrr::map(rolled_dice, check_number)
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

#'player_message
#'
#'Helper function for interactive gameplay
#'Print messages in response to player rolls
#'
#'@param components - dice to keep from roll
#'@param score - current player score
#'@param num_rolls - number of turns a player has played
#'@import purrr
#'
#'@family Interactive Gameplay
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

#'find_score
#'
#'Calculate current player score
#'
#'@param needed vector to check if score will be nonzero
#'@param cargo vector of numbers used to calculate score
#'@return number denoting player score
#'@family Interactive Gameplay
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

#'reroll
#'
#'Dice rolling function; Simulates a number of dice rolls between 1 and 5
#'
#'@param num_dice number of dice to be rolled
#'@return vector of dice rolls equal to num_dice
#'@family Interactive Gameplay
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
  return(roll)
}

#'reroll_prompt
#'
#'Ask user if they would like to roll again and collects player answer
#'
#'@param num_arg current turn number of player; must be between 1 and 3
#'@return TRUE if player will reroll, FALSE otherwise
#'@family Interactive Gameplay
reroll_prompt <- function(num_arg){
  # check if input pdata type is appropriate for the function
  if (is.numeric(num_arg) == FALSE & is.integer(num_arg) == FALSE) {
    stop("Bad input.")
  }

  # determining whether player has > 0 rolls left
  rolls_left <- (3 - num_arg)

  # if player has rolls left...
  if(rolls_left > 0)
    # ask if they would like to reroll
  {
    user_input <- readline(prompt = "Would you like to reroll? Y/N: ")
  }

  # if they do not have rolls left, send to play_again() function
  else{
    return(play_again())
  }

  # if user would like to reroll (enters Y), send to rolling_function()
  if(user_input == "Y"){
    return(TRUE)
  } else if(user_input == "N" & rolls_left > 0){
    # if user would not like to reroll (enters N)...
    certainty_ans <- readline(
      # # check if player is certain, given that they are known to have rolls left if they have
      # # reached this point in the function
      prompt = paste0("Are you sure you want to stop? You have ",
                      rolls_left, " rolls left! Y/N"))
  } else{print("You were supposed to enter Y or N..."); return(reroll_prompt(num_arg))}

  # if player is sure they would not like to roll again (enters Y), end game.
  if(certainty_ans == "Y"){
    message(cat("Bold move...I guess the game's over for you!"))
    return(FALSE)
  }

  # if player is not sure (enters N), send back to beginning of reroll_prompt()
  if (certainty_ans == "N") {
    reroll_prompt(num_arg)
  } else {
    # if player enters non-Y or non-N input, chastise and send back to beginning of reroll_prompt()
    print("Weird answer, dude. I can't read that. Enter Y or N.")
    return(reroll_prompt(num_arg))
  }
}

#'play_again
#'
#'Ask user if they would like to play another game
#'
#'@param score final player score of previous game
#'@return TRUE if player will start another game, FALSE otherwise
#'@family Interactive Gameplay
play_again <- function(score){
  message(paste0("Your final score is ", score, "."))
  play_again_ans <- readline(prompt = "No more rolls! Would you like to play again? Answer Y or N: ")

  if (play_again_ans == "Y") {
    return(TRUE)
  } else if (play_again_ans == "N") {
    return(FALSE)
  } else {
    message(cat("I can't understand that. Please enter Y or N."))
    return(play_again(score))
  }
}
