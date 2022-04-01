#' See Most Used Emojis in Tweets
#'
#' @param df A dataset containing a column named 'text'
#'
#' @return Table
#'
#' @examples
#' tweets <- Rtwitter::tw
#' most.emo(tweets)
#'
#' @export
most.emo <- function(df) {
  df %>%
    mutate(emoji = ji_extract_all(text)) %>%
    unnest(cols = c(emoji)) %>%
    count(emoji, sort = TRUE) %>%
    top_n(10) %>%
    return()
}
