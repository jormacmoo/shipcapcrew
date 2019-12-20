test_that("interactive works", {

  # this checks to see if the game_turn() returns a list,
  # when previous rolls = 0, number of dice = 5, and
  # previous dice from past rolls is NULL, because it doesn't apply since previous rolls = 0
  expect_type(game_turn(0, 5, NULL), "list")

  # similar to last check, this checks to see if  game_turn() returns a list,
  # this time though there was one previous roll, and there was only two dice remaining,
  # since a 6, 5, and 4 were rolled on the previous roll.
  expect_type(game_turn(1, 2, c(6, 5, 4)), "list")

  # checks if reset_simulation() code print output to the console
  expect_output(game_turn(2, 5, NULL))

  # meant to check that the length of the list game_turn returns is 5
  # wasn't able to get it figured out before 12:00am
  #expect_length(list(game_turn(2, 4 c(6))), 5)

})

## We found it difficult to engage with start_game() function, for reasons similarly to those mentioned in the test-interactive.R
