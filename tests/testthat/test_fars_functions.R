library(testthat)
test_that("fars_functions_tests", {

filename1 <- make_filename(2015)
readfile_results <- fars_read(filename1)
fakefile <- system.file("extdata", "x", package="farsdocs")
read_years_results <- fars_read_years(2015)
summarize_years_results <- fars_summarize_years(2015)
map_state_results <- fars_map_state(26, 2015)

expect_that(filename1, matches("accident_2015.csv.bz2"))
expect_that(readfile_results, is_a("data.frame"))
expect_that(fars_read(fakefile), throws_error())
expect_that(read_years_results, is_a("list"))
expect_that(summarize_years_results, is_a("data.frame"))
expect_that(fars_map_state(3, 2015), throws_error())
expect_that(map_state_results, is_a("NULL"))

})
