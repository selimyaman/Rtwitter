#' See Most Frequently Used Words in a Wordcloud
#'
#' @param df A dataset containing a column named text
#'
#' @return Wordcloud Graph
#'
#' @examples
#' wordcloud.tweets(tweets)
#'
#' @export

wordcloud.tweets <- function (df) {
  tweets.txt <- str_replace_all(df$text,"[^[:graph:]]", " ")
  ## pre-processing text:
  clean.text = function(x)
  {
    # convert to lower case
    x = tolower(x)
    # remove rt
    x = gsub("rt", "", x)
    # remove at
    x = gsub("@\\w+", "", x)
    # remove punctuation
    x = gsub("[[:punct:]]", "", x)
    # remove numbers
    x = gsub("[[:digit:]]", "", x)
    # remove links http
    x = gsub("http\\w+", "", x)
    # remove tabs
    x = gsub("[ |\t]{2,}", "", x)
    # remove blank spaces at the beginning
    x = gsub("^ ", "", x)
    # remove blank spaces at the end
    x = gsub(" $", "", x)
    # some other cleaning text
    x = gsub('https://','',x)
    x = gsub('http://','',x)
    x = gsub('[^[:graph:]]', ' ',x)
    x = gsub('[[:punct:]]', '', x)
    x = gsub('[[:cntrl:]]', '', x)
    x = gsub('\\d+', '', x)
    x = str_replace_all(x,"[^[:graph:]]", " ")
    return(x)
  }
  cleanText <- clean.text(tweets.txt)
  # remove empty results (if any)
  idx <- which(cleanText == " ")
  cleanText <- cleanText[cleanText != " "]
  library(tm)
  text_corpus <- Corpus(VectorSource(cleanText))
  text_corpus <- tm_map(text_corpus, content_transformer(tolower))
  text_corpus <- tm_map(text_corpus, function(x)removeWords(x,stopwords("english")))
  tdm <- TermDocumentMatrix(text_corpus)
  tdm <- as.matrix(tdm)
  tdm <- sort(rowSums(tdm), decreasing = TRUE)
  tdm <- data.frame(word = names(tdm), freq = tdm)
  wordcloud(text_corpus, min.freq = 1, max.words = 50, scale = c(2.2,1),
            colors=brewer.pal(8, "Dark2"), random.color = T, random.order = F)

}
