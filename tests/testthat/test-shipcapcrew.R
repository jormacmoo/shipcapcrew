# start_simulation(5)
#
# test_gamecard <- gamecard
#
# test_that("simulation works", {
#   expect_equal(ncol(start_simulation(3)), 3)
#   expect_is(start_simulation(4), )
  #expect_error(start_simulation(p), "object 'p' not found")

# })

test_that("interactive works", {

  expect_type(class(start_game()), message)

})
