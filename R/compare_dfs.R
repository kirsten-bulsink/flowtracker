#' Compare properties of two dataframes
#'
#' Creates a comparison report based on the dimensional properties of the two
#' dataframes and evaluates this based on your your expectation. The default
#' expectation is that all properties are equal.
#'
#' @param data_a dataframe
#' @param data_b dataframe
#' @param expect_n_rows c("equal", "less", "more"). Defaults to equal.
#' @param expect_n_cols c("equal", "less", "more"). Defaults to equal.
#' @param expect_colnames c("equal", "not_equal"). Defaults to equal.
#'
#' @return tibble with result and evaluation of checks
#' @export
#'
#' @examples
#' compare_dfs(mtcars,
#'             mtcars %>% dplyr::filter(cyl != 6))
#'
#' compare_dfs(mtcars,
#'             dplyr::select(mtcars, -hp),
#'             expect_n_cols = "less",
#'             expect_colnames = "not_equal")
#'
compare_dfs <- function(data_a,
                        data_b,
                        expect_n_rows = "equal",
                        expect_n_cols = "equal",
                        expect_colnames = "equal"){

  # check if data_a and data_b are df's
  if(!is.data.frame(data_a)){
    stop("data_a is not a dataframe. Checks cannot be performed.")
  }
  if(!is.data.frame(data_b)){
    stop("data_b is not a dataframe. Checks cannot be performed.")
  }

  # check whether expect values are valid
  if(!expect_n_rows %in% c("equal", "less", "more")){
    stop("The value for expect_n_rows is not valid. Did you mispell?")
  }
  if(!expect_n_cols %in% c("equal", "less", "more")){
    stop("The value for expect_n_cols is not valid. Did you mispell?")
  }
  if(!expect_colnames %in% c("equal", "not_equal")){
    stop("The value for expect_colnames is not valid. Did you mispell?")
  }

  # perform checks
  result_n_rows <- check_n_rows(data_a, data_b, expect_n_rows)
  result_n_cols <- check_n_cols(data_a, data_b, expect_n_cols)
  result_colnames <- check_colnames(data_a, data_b, expect_colnames)

  # create report
  report <- tibble::tibble(check = c("n_rows", "n_cols", "colnames"),
                 expect = c(expect_n_rows, expect_n_cols, expect_colnames),
                 result = c(result_n_rows[[1]], result_n_cols[[1]], result_colnames[[1]]),
                 eval = c(result_n_rows[[2]], result_n_cols[[2]], result_colnames[[2]]))

  return(report)
}


#' Compare number of rows between two dataframes
#'
#' @param data_a dataframe
#' @param data_b dataframe
#' @param expect c("equal", "less", "more")
#'
#' @return list with result and evaluation
#'
check_n_rows <- function(data_a, data_b, expect){

  n_rows_a <- nrow(data_a)
  n_rows_b <- nrow(data_b)

  if(n_rows_a == n_rows_b){
    result = "equal"
  } else if(n_rows_a < n_rows_b) {
    result = "more"
  } else {
    result = "less"
  }

  if(expect == result){
    eval <- stringr::str_c("Succesful! Number of rows was ", n_rows_a, " in data_a and number of rows was ", n_rows_b, " in data_b.")
  } else {
    eval <- stringr::str_c("Unexpected! Number of rows was ", n_rows_a, " in data_a and number of rows was ", n_rows_b, " in data_b.")
  }

  return(list(result, eval))

}


#'  Compare number of cols between two dataframes
#'
#' @param data_a dataframe
#' @param data_b dataframe
#' @param expect c("equal", "less", "more")
#'
#' @return list with result and evaluation
#'
check_n_cols <- function(data_a, data_b, expect){

  n_cols_a <- ncol(data_a)
  n_cols_b <- ncol(data_b)

  if(n_cols_a == n_cols_b){
    result = "equal"
  } else if(n_cols_a < n_cols_b) {
    result = "more"
  } else {
    result = "less"
  }

  if(expect == result){
    eval <- stringr::str_c("Succesful! Number of cols was ", n_cols_a, " in data_a and number of cols was ", n_cols_b, " in data_b.")
  } else {
    eval <- stringr::str_c("Unexpected! Number of cols was ", n_cols_a, " in data_a and number of cols was ", n_cols_b, " in data_b.")
  }

  return(list(result, eval))
}

#' Compare colnames between two dataframes
#'
#' @param data_a dataframe
#' @param data_b dataframe
#' @param expect c("equal", "not_equal")
#'
#' @return list with result and evaluation
#'
check_colnames <- function(data_a, data_b, expect){

  colnames_a <- colnames(data_a)
  colnames_b <- colnames(data_b)

  if(identical(colnames_a, colnames_b)){
    result <- "equal"
  } else {
    result <- "not_equal"
  }

  dif_ab <- setdiff(colnames_a, colnames_b)
  dif_ab_string <- ifelse(length(dif_ab) <= 10, paste(dif_ab, collapse = " "), "too many for display")

  if(expect == result){
    eval <- stringr::str_c("Succesful! There were ", length(dif_ab), "/", length(colnames_a), " column names in data_a that were not found in data_b (",
                           dif_ab_string, ").")
  } else {
    eval =

    eval <- stringr::str_c("Unexpected! There were ", length(dif_ab), "/", length(colnames_a), " column names in data_a that were not found in data_b (",
                           dif_ab_string, ").")
  }

  return(list(result, eval))
}
