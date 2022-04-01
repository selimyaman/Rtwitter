#' See Most Frequently Used Languages in Tweets
#'
#' @param df A dataset containing a column named 'lang'
#'
#' @return Plot
#'
#' @examples
#' tweets <- Rtwitter::tw
#' tweet.lang(tweets)
#'
#' @export
tweet.lang <- function(df){
  df %>%
    count(lang, sort = TRUE) %>%
    mutate(lang = reorder(lang, n)) %>%
    na.omit() %>%
    top_n(10) %>%
    ggplot(aes(x = lang, y = n)) +
    geom_col() +
    coord_flip() +
    labs(
      y = "Location",
      title = "Which Languages are Tweets in?")+
    theme(axis.title.y = element_blank(),
    )}
