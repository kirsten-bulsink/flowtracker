# dummy data -------------------------------------------------------------------
df_start <- mtcars
# less rows (filter)
df_less_rows <- mtcars |> dplyr::filter(cyl != 6)
# more rows (bind)
df_more_rows <- dplyr::bind_rows(mtcars, mtcars)
# less cols (select)
df_less_cols <- dplyr::select(mtcars, -hp)
# more cols (enriched)
df_more_cols <- mutate(mtcars, new_var = "blablabla")

# checks for compare_dfs -------------------------------------------------------
test_that("compare_dfs returns a tibble", {
  report <- compare_dfs(df_start, df_less_rows)
  expect_equal(tibble::is_tibble(report),
               T)
})

test_that("compare_dfs gives error when data_a or data_b is not a dataframe", {
  expect_error(compare_dfs(c(1,2,3), df_start))
  expect_error(compare_dfs(df_start, c(1,2,3)))
})

test_that("compare_dfs gives error when expect values are not valid", {
  expect_error(compare_dfs(df_start, df_start, expect_n_rows = "eqqual"),
               regexp = "The value for expect_n_rows is not valid. Did you mispell?")
  expect_error(compare_dfs(df_start, df_start, expect_n_cols = "lesss"),
               regexp = "The value for expect_n_cols is not valid. Did you mispell?")
  expect_error(compare_dfs(df_start, df_start, expect_colnames = "notequal"),
               regexp = "The value for expect_colnames is not valid. Did you mispell?")
})

# checks for check_n_rows ------------------------------------------------------
test_that("check_n_rows returns 'less' as result if data_b has less rows than data_a", {
  result_n_rows <- check_n_rows(df_start, df_less_rows, expect = "equal")
  expect_equal(result_n_rows[[1]], "less")
  expect_true(stringr::str_starts(result_n_rows[[2]], pattern = "Unexpected!"))
})

test_that("check_n_rows returns 'equal' as result if data_b has same nr of rows as data_a", {
  result_n_rows <- check_n_rows(df_start, df_start, expect = "equal")
  expect_equal(result_n_rows[[1]], "equal")
  expect_true(stringr::str_starts(result_n_rows[[2]], pattern = "Succesful!"))
})

test_that("check_n_rows returns 'more' as result if data_b has more rows than data_a", {
  result_n_rows <- check_n_rows(df_start, df_more_rows, expect = "equal")
  expect_equal(result_n_rows[[1]], "more")
  expect_true(stringr::str_starts(result_n_rows[[2]], pattern = "Unexpected!"))
})

# checks for check_n_cols ------------------------------------------------------
test_that("check_n_cols returns 'less' as result if data_b has less cols than data_a", {
  result_n_cols <- check_n_cols(df_start, df_less_cols, expect = "equal")
  expect_equal(result_n_cols[[1]], "less")
  expect_true(stringr::str_starts(result_n_cols[[2]], pattern = "Unexpected!"))
})

test_that("check_n_cols returns 'equal' as result if data_b has same nr of cols as data_a", {
  result_n_cols <- check_n_cols(df_start, df_start, expect = "equal")
  expect_equal(result_n_cols[[1]], "equal")
  expect_true(stringr::str_starts(result_n_cols[[2]], pattern = "Succesful!"))
})

test_that("check_n_cols returns 'more' as result if data_b has more cols than data_a", {
  result_n_cols <- check_n_cols(df_start, df_more_cols, expect = "equal")
  expect_equal(result_n_cols[[1]], "more")
  expect_true(stringr::str_starts(result_n_cols[[2]], pattern = "Unexpected!"))
})

# checks for check_colnames ----------------------------------------------------
test_that("check_colnames returns 'equal' as result if colnames of data_b are identical to colnames of data_a", {
  result_colnames <- check_colnames(df_start, df_start, expect = "equal")
  expect_equal(result_colnames[[1]], "equal")
  expect_true(stringr::str_starts(result_colnames[[2]], pattern = "Succesful!"))
})

test_that("check_colnames returns 'not_equal' as result if colnames of data_b are not identical to colnames of data_a", {
  result_colnames <- check_colnames(df_start, df_less_cols, expect = "equal")
  expect_equal(result_colnames[[1]], "not_equal")
  expect_true(stringr::str_starts(result_colnames[[2]], pattern = "Unexpected!"))
})
