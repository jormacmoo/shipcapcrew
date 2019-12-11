# author: ed4ubenspeck

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
    return(rolling_function(num_arg))
  }

  # if user would not like to reroll (enters N)...
  if(user_input == "N" & rolls_left > 0){
    certainty_ans <- readline(
      # # check if player is certain, given that they are known to have rolls left if they have
      # # reached this point in the function
      prompt = paste0("Are you sure you want to stop? You have ",
                      rolls_left, " rolls left! Y/N"))}

  # if player enters non-Y or non-N input, chastise and send back to beginning of reroll_prompt()
  else{print("You were supposed to enter Y or N..."); return(reroll_prompt(num_arg))}

  # if player is sure they would not like to roll again (enters Y), end game.
  if(certainty_ans == "Y"){return("Bold move...I guess the game's over for you!")}

  # if player is not sure (enters N), send back to beginning of reroll_prompt()
  if (certainty_ans == "N") {
    reroll_prompt(num_arg)
  } else{
    # if player enters non-Y or non-N input, chastise and send back to beginning of reroll_prompt()
    print("Weird answer, dude. I can't read that. Enter Y or N.")
    return(reroll_prompt(num_arg))
  }
}
