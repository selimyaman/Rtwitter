#' See Most Frequent User Locations
#'
#' @param df A dataset containing a column named text
#'
#' @return Graph
#'
#' @examples
#' tweets <- Rtwitter::tw
#' user.locations(tweets)
#'
#' @export

user.locations <- function (df) {
  df %>%
    count(user_location, sort = TRUE) %>%
    mutate(user_location = reorder(user_location, n)) %>%
    na.omit() %>%
    top_n(20) %>%
    ggplot(aes(x = user_location, y = n)) +
    geom_col() +
    coord_flip() +
    labs(x = "Count",
         y = "Location",
         title = "Where are our users from?") +
    theme(axis.title.y = element_blank(),
    )
}
