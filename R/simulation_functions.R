#simulation_functions
#includes:
#    start_simulation: initializes single-player game with dice, number of rolls, etc.
#    multiplayer_simulation: initializes multiplayer game
#    initial_gameplay: checks for number of rolls, dice, etc. Calls other gameplay functions
#                      depending on game status
#    rolling_function: simulates dice roll with sampling based on number of dice left, determines
#                      whether dice values can be kept in player's hand
#    strategy_function: determines which, if any, strategy a player is using and calls the
#                       appropriate function
#    greedy_reroll: strategy function where player rerolls every time possible
#    final_scores_tab: determines value of player dice kept
#    add_scores: creates gamecard dataframe and appends latest player score
#    reset_game: removes gamecard from environment so new simulation can be run

# strategy function encompassing all strategies
# this function takes arguments from initial_gameplay and calls the correct strategy function
# based on the strategy argument for a particular player
strategy_function <- function(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer){
  # checking that arguments intended to be numeric or integer are not character or logical
  num_vars <- c(player_dice, num_dice, num_games, num_rolls, game_ticker)
  if(any(map_lgl(num_vars, is.character)) | any(map_lgl(num_vars, is.logical))){
    stop("player_dice, num_dice, num_games, num_rolls, game_ticker, and final_score arguments should all
         be numeric or integer inputs!")
  }
  # checking that arguments intended to be character or logical are such
  # checking that arguments intended to be of specific length are such
  if(is.character(strategy) == FALSE | length(strategy) > 2){
    stop("strategy argument should be a character vector of length = 1 or 2!")
  }
  if(is.logical(multiplayer) == FALSE | length(multiplayer) > 1){
    stop("multiplayer argument should be a logical vector of length = 1!")
  }
  # checking that argument intended to be a dataframe is as such
  if(is.data.frame(gamecard) == FALSE){
    stop("gamecard argument should be a data.frame!")
  }
  # if() statements determining the specific strategy roll function to be called
  # based on the strategy argument passed
  if(strategy == "greedy"){
    greedy_reroll(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }else{
    rolling_function(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }
  }

# a strategy function: always reroll when possible
greedy_reroll <- function(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer){
  num_vars <- c(player_dice, num_dice, num_games, num_rolls, game_ticker)
  # checking that arguments intended to be numeric or integer are not character or logical
  if(any(map_lgl(num_vars, is.character)) | any(map_lgl(num_vars, is.logical))){
    stop("player_dice, num_dice, num_games, num_rolls, game_ticker, and final_score arguments should all
         be numeric or integer inputs!")
  }
  # checking that arguments intended to be character or logical are such
  # checking that arguments intended to be of specific length are such
  if(is.character(strategy) == FALSE | length(strategy) > 2){
    stop("strategy argument should be a character vector of length = 1 or 2!")
  }
  if(is.logical(multiplayer) == FALSE | length(multiplayer) > 1){
    stop("multiplayer argument should be a logical vector of length = 1!")
  }
  # checking that argument intended to be a dataframe is as such
  if(is.data.frame(gamecard) == FALSE){
    stop("gamecard argument should be a data.frame!")
  }
  # recalculating the number of rolls
  num_rolls <- num_rolls + 1
  # simulating roll of player dice by taking a sample
  roll <- sample(1:6, size = num_dice, replace = TRUE)
  # if() statements defining player strategy: greedy
  # calls back to initial_gameplay to check value of num_rolls and game_ticker
  # in order to determine if gameplay should continue
  if(num_rolls < 3){
    player_dice <- c(6, 5, 4)
    initial_gameplay(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }else{
    player_dice <- c(6, 5, 4, roll)
    initial_gameplay(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }
  }

rolling_function <- function(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer){
  num_vars <- c(player_dice, num_dice, num_games, num_rolls, game_ticker)
  # checking that arguments intended to be numeric or integer are not character or logical
  if(any(map_lgl(num_vars, is.character)) | any(map_lgl(num_vars, is.logical))){
    stop("player_dice, num_dice, num_games, num_rolls, game_ticker, and final_score arguments should all
         be numeric or integer inputs!")
  }
  # checking that arguments intended to be character or logical are such
  # checking that arguments intended to be of specific length are such
  if(is.character(strategy) == FALSE | length(strategy) > 2){
    stop("strategy argument should be a character vector of length = 1 or 2!")
  }
  if(is.logical(multiplayer) == FALSE | length(multiplayer) > 1){
    stop("multiplayer argument should be a logical vector of length = 1!")
  }
  # checking that argument intended to be a dataframe is as such
  if(is.data.frame(gamecard) == FALSE){
    stop("gamecard argument should be a data.frame!")
  }
  # recalculating num_rolls
  num_rolls <- num_rolls + 1
  # simulating rolling player dice by taking a sample
  roll <- sample(1:6, size = num_dice, replace = TRUE)
  # if no ship, captain, or crew on first roll, player does not keep any dice
  # call back to initial_gameplay to check value of num_rolls and game_ticker
  if(6 %in% roll == FALSE & 5 %in% roll == FALSE & 4 %in% roll == FALSE & is.null(player_dice)){
    player_dice <- c()
    initial_gameplay(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }
  # if() statements concerning values of player dice rolls and what players may keep
  # in which circumstances
  if(6 %in% roll & 5 %in% roll & 4 %in% roll &
     6 %in% player_dice == FALSE & 5 %in% player_dice == FALSE & 4 %in% player_dice == FALSE & num_rolls < 3){
    player_dice <- c(6, 5, 4)
    initial_gameplay(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }
  if(6 %in% roll & 5 %in% roll &
     6 %in% player_dice == FALSE & 5 %in% player_dice == FALSE & num_rolls < 3){
    player_dice <- c(6, 5)
    initial_gameplay(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }
  if(6 %in% roll &
     6 %in% player_dice == FALSE & num_rolls < 3){
    player_dice <- c(6)
    initial_gameplay(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }
  if(5 %in% roll &
     6 %in% player_dice == TRUE & 5 %in% player_dice == FALSE & num_rolls < 3){
    player_dice <- c(player_dice, 5)
    initial_gameplay(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }
  if(4 %in% roll &
     6 %in% player_dice == TRUE & 5 %in% player_dice == TRUE & 4 %in% player_dice == FALSE & num_rolls < 3){
    player_dice <- c(player_dice, 4)
    initial_gameplay(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }
  if(6 %in% roll & 5 %in% roll & 4 %in% roll &
     6 %in% player_dice == FALSE & 5 %in% player_dice == FALSE & 4 %in% player_dice == FALSE & num_rolls == 3){
    player_dice <- c(roll)
    initial_gameplay(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }
  if(5 %in% roll & 4 %in% roll &
     6 %in% player_dice & 5 %in% player_dice == FALSE & 4 %in% player_dice == FALSE & num_rolls == 3){
    player_dice <- c(6, roll)
    initial_gameplay(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }
  if(4 %in% roll &
     6 %in% player_dice == TRUE & 5 %in% player_dice == TRUE & 4 %in% player_dice == FALSE & num_rolls == 3){
    player_dice <- c(6, 5, roll)
    initial_gameplay(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }
  if(6 %in% player_dice & 5 %in% player_dice & 4 %in% player_dice & num_rolls == 3){
    player_dice <- c(6, 5, 4, roll)
    initial_gameplay(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  } else{
    initial_gameplay(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }
  }


final_score_tab <- function(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer){
  num_vars <- c(player_dice, num_dice, num_games, num_rolls, game_ticker)
  # checking that arguments intended to be numeric or integer are not character or logical
  if(any(map_lgl(num_vars, is.character)) | any(map_lgl(num_vars, is.logical))){
    stop("player_dice, num_dice, num_games, num_rolls, game_ticker, and final_score arguments should all
         be numeric or integer inputs!")
  }
  # checking that arguments intended to be character or logical are such
  # checking that arguments intended to be of specific length are such
  if(is.character(strategy) == FALSE | length(strategy) > 2){
    stop("strategy argument should be a character vector of length = 1 or 2!")
  }
  if(is.logical(multiplayer) == FALSE | length(multiplayer) > 1){
    stop("multiplayer argument should be a logical vector of length = 1!")
  }
  # checking that argument intended to be a dataframe is as such
  if(is.data.frame(gamecard) == FALSE){
    stop("gamecard argument should be a data.frame!")
  }
  # increasing game_ticker to reflect the culmination of a game
  game_ticker <- game_ticker + 1
  # removing ship, captain, and crew from calculation of player score
  scc_positions <- c(match(6, player_dice), match(5, player_dice), match(4, player_dice))
  player_dice[scc_positions] <- 0
  final_score <- sum(player_dice)
  # calling add_scores to build vector of player's scores
  add_scores(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, final_score, multiplayer)
  }

add_scores <- function(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, final_score, multiplayer){
  num_vars <- c(player_dice, num_dice, num_games, num_rolls, game_ticker, final_score)
  # checking that arguments intended to be numeric or integer are not character or logical
  if(any(map_lgl(num_vars, is.character)) | any(map_lgl(num_vars, is.logical))){
    stop("player_dice, num_dice, num_games, num_rolls, game_ticker, and final_score arguments should all
         be numeric or integer inputs!")
  }
  # checking that arguments intended to be character or logical are such
  # checking that arguments intended to be of specific length are such
  if(is.character(strategy) == FALSE | length(strategy) > 2){
    stop("strategy argument should be a character vector of length = 1 or 2!")
  }
  if(is.logical(multiplayer) == FALSE | length(multiplayer) > 1){
    stop("multiplayer argument should be a logical vector of length = 1!")
  }
  # checking that argument intended to be a dataframe is as such
  if(is.data.frame(gamecard) == FALSE){
    stop("gamecard argument should be a data.frame!")
  }
  # appending final_score argument to scores_vector for a player
  scores_vector <- c(scores_vector, final_score)
  # if all requested games have been played and the game is single player,
  # create gamecard and assign it to the GlobalEnv, stop game.
  if(game_ticker == num_games & multiplayer == FALSE){
    gamecard <- data.frame(gamecard, scores_vector)
    if(dim(gamecard)[2] == 5){
      colnames(gamecard)[5] <- "p2_scores_vector"
      assign("gamecard", gamecard, envir = .GlobalEnv)
      stop("Simulation complete!")
    }
    assign("gamecard", gamecard, envir = .GlobalEnv)
    stop("Simulation complete!")
  }
  # if all requested games have been played and the game is multiplayer,
  # create gamecard, assign to GlobalEnv, and call multiplayer_simulation to play second player's game
  if(game_ticker == num_games & multiplayer == TRUE){
    gamecard <- data.frame(gamecard, scores_vector)
    assign("gamecard", gamecard, envir = .GlobalEnv)
    multiplayer_simulation(num_games, strategy, multiplayer)
  }
  # if the reuested number of games has not been played, reset num_rolls and
  # call initial_gameplay to begin a new game for this player
  if(game_ticker < num_games){
    num_rolls <- 0
    initial_gameplay(player_dice = c(), num_dice = 5, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }
  }

#' Initializes gameplay for a single player simulation of Ship, Captain, Crew
#'
#' @param num_games the number of games to be simulated
#' @param strategy a character vector containing the player's strategy
#'
#' @examples
#' start_simulation(10, "greedy")
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

initial_gameplay <- function(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer){
  num_vars <- c(player_dice, num_dice, num_games, num_rolls, game_ticker)
  # checking that arguments intended to be numeric or integer are not character or logical
  if(any(map_lgl(num_vars, is.character)) | any(map_lgl(num_vars, is.logical))){
    stop("player_dice, num_dice, num_games, num_rolls, game_ticker, and final_score arguments should all
         be numeric or integer inputs!")
  }
  # checking that arguments intended to be character or logical are such
  # checking that arguments intended to be of specific length are such
  if(is.character(strategy) == FALSE | length(strategy) > 2){
    stop("strategy argument should be a character vector of length = 1 or 2!")
  }
  if(is.logical(multiplayer) == FALSE | length(multiplayer) > 1){
    stop("multiplayer argument should be a logical vector of length = 1!")
  }
  # checking that argument intended to be a dataframe is as such
  if(is.data.frame(gamecard) == FALSE){
    stop("gamecard argument should be a data.frame!")
  }
  # check if num_rolls is in range
  if(abs(num_rolls) >= 3){
    # send to score tabulator function
    final_score_tab(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }
  # recalculate num_dice
  num_dice <- 5 - length(player_dice)
  # check for ship, captain, and crew in dice saved
  # if these dice are present, call strategy function
  if(6 %in% player_dice & 5 %in% player_dice & 4 %in% player_dice & strategy != "default"){
    strategy_function(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }else{
    # if ship, captain, and crew are not all present, call rolling_function
    rolling_function(player_dice, num_dice, num_games, num_rolls, strategy, game_ticker, gamecard, scores_vector, multiplayer)
  }
}

#' multiplayer_simulation
#'
#' Initializes gameplay for a two player simulation of Ship, Captain, Crew
#'
#' @param num_games the number of games a user wishes to run within the simulation
#' @param strategy a character vector containing p1 and p2 strategies
#'
#' @examples
#' multiplayer_simulation(4, c("greedy", "greedy"))
#' multiplayer_simulation(100, c("none", "greedy"))
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

#' reset_game
#'
#' Configures global environment to allow for repeated game simulations
#'
#' @param None
#'
#' @examples
#' reset_game()
#'
#' @family simulation functions
#'
#' @export
reset_game <- function(...){
  if(exists("gamecard", where = .GlobalEnv)){
   confirm_input <- readline(prompt = "Resetting the simulation will remove the last simulation's dataframe
from the global environment. Be sure to assign gamecard to a new object if you'd
like to keep it! Would you like to reset? Y/N")
   if(confirm_input == "Y"){
     rm(gamecard, pos = .GlobalEnv)
     cat("You're ready to run another simulation!")
   }
   if(confirm_input == "N"){
     cat("Ok, no-go on that reset!")
   }
  }else{
    cat("You're ready to go!")
  }
}

