#simulation_functions
#includes:
#    start_simulation: initializes single-player game with dice, number of rolls, etc.
#    multiplayer_simulation: initializes multiplayer game
#    reset_simulation: removes gamecard from environment so new simulation can be run



#' Initializes gameplay for a single player simulation of Ship, Captain, Crew
#'
#' @param num_games the number of games to be simulated
#' @param strategy a character vector containing the player's strategy
#' @param multiplayer a logical vector with default value TRUE, indicating that the simulation is multiplayer
#'
#' @family simulation functions
#'
#' @examples
#' \dontrun{start_simulation(10, "greedy")}
#'
#' @export
start_simulation <- function(num_games, strategy = "default", multiplayer = FALSE){
  # start putting in error checks
  if(is.integer(num_games) == FALSE & is.numeric(num_games) == FALSE | length(num_games) > 1){
    stop("num_games should be an integer or numeric argument of length = 1.")
  }
  if(length(strategy) > 2){
    stop("strategy should only hold the strategy arguments for up to two players.")
  }
  # create vector to label the number game being played
  game_vector <- c(1:num_games)
  # if game is multiplayer and gamecard already exists, create p2's strategy vector,
  # append to gamecard, and call initial_gameplay for p2
  if(multiplayer == TRUE & exists("gamecard", where = .GlobalEnv)){
    p2_strategy_vector <- c(paste(strategy[2]))
    gamecard <- data.frame(gamecard, p2_strategy_vector)
    initial_gameplay(player_dice = c(), num_dice = 5, num_games, num_rolls = 0, strategy = strategy[2], game_ticker = 0, gamecard, scores_vector = c(), multiplayer)
  }
  # if game is multiplayer and gamecard does not exist, create p1's strategy vector,
  ## append to gamecard, and call initial_gameplay for p1
  if(multiplayer == TRUE & exists("gamecard", where = .GlobalEnv) == FALSE){
    p1_strategy_vector <- c(paste(strategy[1]))
    gamecard <- data.frame(p1_strategy_vector, game_vector)
    initial_gameplay(player_dice = c(), num_dice = 5, num_games, num_rolls = 0, strategy = strategy[1], game_ticker = 0, gamecard, scores_vector = c(), multiplayer)
  }
  # if game is single player, create gamecard and call initial_gameplay
  if(multiplayer == FALSE){
    strategy_vector <- c(paste(strategy))
    gamecard <- data.frame(strategy_vector, game_vector)
    initial_gameplay(player_dice = c(), num_dice = 5, num_games, num_rolls = 0, strategy, game_ticker = 0, gamecard, scores_vector = c(), multiplayer)

  }
}


#' multiplayer_simulation
#'
#' Initializes gameplay for a two player simulation of Ship, Captain, Crew
#'
#' @param num_games the number of games a user wishes to run within the simulation
#' @param strategy a character vector containing p1 and p2 strategies
#' @param multiplayer a logical vector with default value TRUE, indicating that the simulation is multiplayer
#'
#' @examples
#' \dontrun{multiplayer_simulation(4, c("greedy", "greedy"))
#' multiplayer_simulation(50, c("none", "greedy"))}
#'
#' @family simulation functions
#'
#' @export
multiplayer_simulation <- function(num_games, strategy, multiplayer = TRUE){
  # check that num_games is an integer or numeric vector of length = 1
  if(is.integer(num_games) == FALSE & is.numeric(num_games) == FALSE | length(num_games) > 1){
    stop("num_games should be an integer or numeric argument of length = 1.")
  }
  # check that strategy is a character vector of length = 1 or length = 2
  if(is.character(strategy) == FALSE | length(strategy) > 2){
    stop("strategy should be a character vector holding the strategy arguments for up to two players.")
  }
  # if gamecard does not exist in the GlobalEnv, create a strategy vector in GlobalEnv
  # call start_simulation to initialize gameplay
  if(exists("gamecard", where = .GlobalEnv) == FALSE){
    assign("strategy", strategy, .GlobalEnv)
    start_simulation(num_games, strategy = strategy[1], multiplayer = TRUE)
  }
  # if gamecard does exist in the GlobalEnv, define strategy argument as the second
  # object in the strategy vector, create p2's strategy vector,
  if(exists("gamecard", where = .GlobalEnv)){
    strategy <- get("strategy", .GlobalEnv)[2]
    p2_strategy_vector <- c(strategy)
    gamecard <- data.frame(gamecard, p2_strategy_vector)
    initial_gameplay(player_dice = c(), num_dice = 5, num_games, num_rolls = 0, strategy = "default", game_ticker = 0, gamecard, scores_vector = c(), multiplayer = FALSE)
  }
  if(dim(gamecard)[2] == 5){
    stop("Simulation complete!")
  }
}

#' reset_simulation
#'
#' Configures global environment to allow for repeated game simulations
#'
#' @family simulation functions
#'
#' @export
reset_simulation <- function(){
  if(exists("gamecard", where = .GlobalEnv)){
   confirm_input <- readline(prompt = "Resetting the simulation will remove the last simulation's dataframe
from the game environment. Be sure to assign gamecard to a new object if you'd
like to keep it! Would you like to reset? Y/N")
   if(confirm_input == "Y"){
     rm(gamecard, pos = .GlobalEnv)
     if(exists("strategy", where = .GlobalEnv)){
       rm(strategy, pos = .GlobalEnv)
     }
     cat("You're ready to run another simulation!")
   }
   if(confirm_input == "N"){
     cat("Ok, no-go on that reset!")
   }
  }else{
    cat("You're ready to go!")
  }
}

