#' View Some Random Tweets
#'
#' This function lets you to view a random number of tweets in a nice format.
#'
#' @param df A tweet dataset with a column named text and another column named user_name
#'
#' @param n Number of tweets you want to see
#'
#' @return Table
#'
#' @examples
#' tweets <- Rtwitter::tw
#' view.randomly(tweets, n=5)
#'
#' @export
view.randomly <- function (df, n=3) {
  df %>%
    sample_n(n) %>%
    select(user_name, text) %>%
    kable() %>%
    kable_styling()}
