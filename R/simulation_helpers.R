# simulation_helpers
# includes:
#    initial_gameplay: checks for number of rolls, dice, etc. Calls other gameplay functions
#                      depending on game status
#    rolling_function: simulates dice roll with sampling based on number of dice left, determines
#                      whether dice values can be kept in player's hand
#    strategy_function: determines which, if any, strategy a player is using and calls the
#                       appropriate function
#    greedy_reroll: strategy function where player rerolls every time possible
#    final_scores_tab: determines value of player dice kept
#    add_scores: creates gamecard dataframe and appends latest player score

globalVariables(c("gamecard", "strategy"))

#' initial_gameplay
#'
#' Checks status of game running and makes calls to appropriate other helper functions
#' depending on the number of rolls left, number of games completed, etc.
#'
#' @param player_dice a vector of the dice saved to a player's hand in a given game
#' @param num_dice the number of dice a player has left to roll
#' @param num_games the number of games a user wishes to run within the simulation
#' @param num_rolls the number of rolls a player has left to use
#' @param strategy a character vector containing p1 and p2 strategies
#' @param game_ticker the number of games already completed in the simulation
#' @param gamecard a dataframe storing game number and score information
#' @param scores_vector a vector of a player's scores
#' @param multiplayer a logical vector determining whether games are single or multiplayer
#'
#' @family helper functions
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


#' strategy_function
#'
#' Uses the `strategy` argument to determine which alternative rolling function to call when a ship,
#' captain, and crew are all present in a player's hand.
#'
#' @param player_dice a vector of the dice saved to a player's hand in a given game
#' @param num_dice the number of dice a player has left to roll
#' @param num_games the number of games a user wishes to run within the simulation
#' @param num_rolls the number of rolls a player has left to use
#' @param strategy a character vector containing p1 and p2 strategies
#' @param game_ticker the number of games already completed in the simulation
#' @param gamecard a dataframe storing game number and score information
#' @param scores_vector a vector of a player's scores
#' @param multiplayer a logical vector determining whether games are single or multiplayer
#'
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#'
#' @family helper functions
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

#' greedy_reroll
#'
#' An alternative strategy reroll function, greedy_reroll rolls as many times as permitted
#' without consideration for the value of any "cargo" in the player's hand.
#'
#' @param player_dice a vector of the dice saved to a player's hand in a given game
#' @param num_dice the number of dice a player has left to roll
#' @param num_games the number of games a user wishes to run within the simulation
#' @param num_rolls the number of rolls a player has left to use
#' @param strategy a character vector containing p1 and p2 strategies
#' @param game_ticker the number of games already completed in the simulation
#' @param gamecard a dataframe storing game number and score information
#' @param scores_vector a vector of a player's scores
#' @param multiplayer a logical vector determining whether games are single or multiplayer
#'
#' @family helper functions
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

#' rolling_function
#'
#' Simulates rolling of a player's remaining dice and calls initial_gameplay
#'
#' @param player_dice a vector of the dice saved to a player's hand in a given game
#' @param num_dice the number of dice a player has left to roll
#' @param num_games the number of games a user wishes to run within the simulation
#' @param num_rolls the number of rolls a player has left to use
#' @param strategy a character vector containing p1 and p2 strategies
#' @param game_ticker the number of games already completed in the simulation
#' @param gamecard a dataframe storing game number and score information
#' @param scores_vector a vector of a player's scores
#' @param multiplayer a logical vector determining whether games are single or multiplayer
#'
#' @family helper functions
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

#' final_score_tab
#'
#' Tabulates final score for individual game scenerios and calls add_scores
#'
#' @param player_dice a vector of the dice saved to a player's hand in a given game
#' @param num_dice the number of dice a player has left to roll
#' @param num_games the number of games a user wishes to run within the simulation
#' @param num_rolls the number of rolls a player has left to use
#' @param strategy a character vector containing p1 and p2 strategies
#' @param game_ticker the number of games already completed in the simulation
#' @param gamecard a dataframe storing game number and score information
#' @param scores_vector a vector of a player's scores
#' @param multiplayer a logical vector determining whether games are single or multiplayer
#'
#' @family helper functions
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

#' add_scores
#'
#' Checks if all requested games have been played, if the games are single or multiplayer, and assigns the final gamecard to the global environment
#'
#' @param player_dice a vector of the dice saved to a player's hand in a given game
#' @param num_dice the number of dice a player has left to roll
#' @param num_games the number of games a user wishes to run within the simulation
#' @param num_rolls the number of rolls a player has left to use
#' @param strategy a character vector containing p1 and p2 strategies
#' @param game_ticker the number of games already completed in the simulation
#' @param gamecard a dataframe storing game number and score information
#' @param scores_vector a vector of a player's scores
#' @param final_score a numeric vector of a player's most recent final score
#' @param multiplayer a logical vector determining whether games are single or multiplayer
#'
#' @importFrom utils globalVariables
#'
#' @family helper functions
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
      opt <- options(show.error.messages = FALSE)
      on.exit(options(opt))
      cat("Simulation complete!")
      stop()
    }
    assign("gamecard", gamecard, envir = .GlobalEnv)
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    cat("Simulation complete!")
    stop()
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
