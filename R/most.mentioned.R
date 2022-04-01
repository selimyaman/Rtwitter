#' See 10 Most Mentioned Accounts in Tweets
#'
#' @param df A dataset containing a column named 'text'
#'
#' @return Table
#'
#' @examples
#' tweets <- Rtwitter::tw
#' most.mentioned(tweets)
#'
#' @export
most.mentioned <- function (df){
  df %>%
    unnest_tokens(mentions, text, "tweets", to_lower = FALSE) %>%
    filter(str_detect(mentions, "^@")) %>%
    count(mentions, sort = TRUE) %>%
    top_n(10) %>%
    return()
}
