test_that("simulation works", {

  expect_error(start_simulation(p), "object 'p' not found")
  expect_error(multiplayer_simulation("extra"), " integer or numeric argument of")
  expect_error(start_simulation("hello"), "num_games should be an integer or numeric argument of")
  expect_output(reset_simulation())

})
