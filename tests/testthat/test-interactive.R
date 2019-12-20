test_that("simulation works", {

  # checks for correct error message when user enters an object for start_simulation() function
  expect_error(start_simulation(p), "object 'p' not found")

  # checks for correct error message when user enters an string for multiplayer_simulation() function
  expect_error(multiplayer_simulation("extra"), " integer or numeric argument of")

  # checks for correct error message when user enters an string for start_simulation() function
  expect_error(start_simulation("hello"), "num_games should be an integer or numeric argument of")

  # checks if reset_simulation() code print output to the console
  expect_output(reset_simulation())

})

## As a group, we found it difficult to engage with test_that() for the simulation aspect of our package
## As discussed in the shipcapcrew-vignette, start_simulation() and multi_player_simulation() don't explicitly retun anything
## Because of this, we were unable to run very many checks that were not focused on error message
