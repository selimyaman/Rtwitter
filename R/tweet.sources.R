#' See Most Frequently Used Sources for Tweets
#'
#' @param df A dataset containing a column named 'source'
#'
#' @return Plot
#'
#' @examples
#' tweets <- Rtwitter::tw
#' tweet.sources(tweets)
#'
#' @export
tweet.sources <- function(df){
  tw_app <- df %>%
    select(source) %>%
    group_by(source) %>%
    summarize(count=n())
  tw_app <- subset(tw_app, count > 11)
  data <- data.frame(
    category=tw_app$source,
    count=tw_app$count
  )
  data$fraction = data$count / sum(data$count)
  data$percentage = data$count / sum(data$count) * 100
  data$ymax = cumsum(data$fraction)
  data$ymin = c(0, head(data$ymax, n=-1))
  data <- data %>%
    mutate_if(is.numeric, round, digits=2)
  Source <- paste(data$category,":", data$percentage, "%")
  ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
    geom_rect() +
    coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
    xlim(c(2, 4)) +
    theme_void() +
    theme(legend.position = "right")+
    ggtitle("Tweet Sources")+
    theme(plot.title = element_text(hjust = 0.5))
}
