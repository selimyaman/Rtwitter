#' See Most Liked Tweets
#'
#' @param df A dataset containing a column named 'sourcetweet_type', 'text', and 'like_count'
#'
#' @return Ggplot Graph
#'
#' @examples
#' most.liked(tweets)
#'
#' @export
most.liked <- function (df) {
  dt_tweets_organic = df[df$sourcetweet_type!="retweet",]
  dt_tweets_organic <- subset(dt_tweets_organic,
                              is.na(dt_tweets_organic$in_reply_to_user_id))
  dt_tweets_organic <- dt_tweets_organic %>% arrange(-like_count)
  tmp=dt_tweets_organic[1:10,]
  ggplot(tmp, aes(x=reorder(text, like_count), y=like_count))+
    geom_bar(stat="identity", fill =  "#377F97")+
    ggtitle("Top 10 Most Liked Tweets")+
    labs(x="Organic Tweets", y=("Favorite Count"))+
    theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1),
          plot.title = element_text(hjust = 0.5))+
    coord_flip()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 80))
}
