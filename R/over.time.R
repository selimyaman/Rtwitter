#' See the FREQUENCY OF TWEETS OVER TIME
#'
#' @param df A dataset containing a column named 'created_at'
#'
#' @param timeframe This could be months (default), years, or days.
#'
#' @return Ggplot object
#'
#' @examples
#' over.time(tweets, timeframe="months")
#'
#' @export
over.time <- function (df, timeframe="months") {
  df <- df %>%
    mutate(
      created_at2 = created_at%>%
        # Remove zeros.
        str_remove_all(pattern = '\\+0000') %>%
        # Parse date.
        parse_date_time(orders = '%y-%m-%d %H%M%S')
    )
  df <- df%>%
    mutate(Created_At_Round = created_at2%>% round(units = timeframe) %>% as.POSIXct())

  df %>%
    dplyr::count(Created_At_Round) %>%
    ggplot(mapping = aes(x = Created_At_Round, y = n)) +
    theme_light() +
    geom_line() +
    xlab(label = 'Date') +
    ylab(label = NULL) +
    ggtitle(label = paste("Number of Tweets per", timeframe))
  # NOTE : FOR THE EXAMPLE DATASET, IT DOESN'T WORK PERFECTLY BECAUSE ALL TWEETS ARE FROM A VERY SHORT TIME-FRAME.
}
