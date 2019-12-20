test_that("interactive works", {

  expect_type(game_turn(0, 5, NULL), "list")
  expect_type(game_turn(1, 2, c(6, 5, 4)), "list")
  expect_output(game_turn(2, 5, NULL))

})

test_that("simulation works", {

  expect_error(start_simulation(p), "object 'p' not found")
  expect_error(multiplayer_simulation("extra"), " integer or numeric argument of")
  expect_error(start_simulation("hello"), "num_games should be an integer or numeric argument of")
  expect_output(reset_simulation())

})

