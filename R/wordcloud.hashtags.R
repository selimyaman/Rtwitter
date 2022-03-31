#' See Top Hashtags in a Wordcloud
#'
#' This function lets you to view the top 20 hashtags via a wordcloud
#'
#' @param df A dataset containing a column named text
#'
#' @return Wordcloud Graph
#'
#' @examples
#' wordcloud.hashtags(tweets)
#'
#' @export

wordcloud.hashtags <- function (df) {
  hashtag_pat <- "#[a-zA-Z0-9_-ー\\.]+"
  hashtag <- str_extract_all(df$text, hashtag_pat)
  hashtag_word <- unlist(hashtag)
  hashtag_word <- tolower(hashtag_word)
  hashtag_word <- gsub("[[:punct:]ー]", "", hashtag_word)
  hashtag_count <- table(hashtag_word)
  top_20_freqs <- sort(hashtag_count, decreasing = TRUE)[1:20]
  top_20_hashtags <- as.character(as.data.frame(top_20_freqs)[,1])
  wordcloud(top_20_hashtags, top_20_freqs,
            scale=c(3.5,1.5), random.order=FALSE,
            colors=brewer.pal(8, "Dark2"),
            random.color=FALSE,
            rot.per=.25,
            min.freq = 20)}
