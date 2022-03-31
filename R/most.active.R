#' See Most Active Accounts that Post Most Tweets
#'
#' @param df A dataset containing a column named 'user_username'
#'
#' @return Wordcloud
#'
#' @examples
#' most.active(tweets)
#'
#' @export
most.active <- function (df,k=15) {
  par(bg = "black")
  t <- tw %>%
    select(user_username) %>%
    count(user_username, sort=TRUE) %>%
    top_n(k)
  wordcloud(t$user_username,
            t$n,
            scale=c(3.5,1.5),
            random.order=FALSE,
            colors = c("tomato", "wheat", "lightblue"),
            rot.per=0.25)
}
