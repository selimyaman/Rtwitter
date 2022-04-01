#' Sentiment Analysis of Tweets
#'
#' @param df A dataset containing a column named 'text'
#'
#' @return Ggplot Graph
#'
#' @examples
#' tweets <- Rtwitter::tw
#' extract_sentiments(tweets)
#'
#' @export
extract_sentiments <- function(df){
  # Converting tweets to ASCII to trackle strange characters
  sentiments <- iconv(df$text, from="UTF-8", to="ASCII", sub="")
  # removing retweets, in case needed
  sentiments <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",sentiments)
  # removing mentions, in case needed
  sentiments <-gsub("@\\w+","",sentiments)
  ew_sentiment<-get_nrc_sentiment((sentiments))
  sentimentscores<-data.frame(colSums(ew_sentiment[,]))
  names(sentimentscores) <- "Score"
  sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
  rownames(sentimentscores) <- NULL
  ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
    geom_bar(aes(fill=sentiment),stat = "identity")+
    theme(legend.position="none")+
    xlab("Sentiments")+ylab("Scores")+
    ggtitle("Total sentiment based on scores")+
    theme_minimal()
}
