test_that("interactive works", {

  #this reads
  expect_type(game_turn(0, 5, NULL), "list")
  expect_type(game_turn(1, 2, c(6, 5, 4)), "list")
  expect_output(game_turn(2, 5, NULL))

})

