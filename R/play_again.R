# author: ed4ubenspeck

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
