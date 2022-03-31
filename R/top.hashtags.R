#' See Top Hashtags
#'
#' This function lets you to view the top hashtags.
#'
#' @param df A dataset containing a column named text
#'
#' @param n Number of hashtags you want to see
#'
#' @return Table
#'
#' @examples
#' top.hashtags(tweets, n=5)
#'
#' @export
top.hashtags <- function (df, k=20) {
  hashtag_pat <- "#[a-zA-Z0-9_-ー\\.]+"
  hashtag <- str_extract_all(df$text, hashtag_pat)
  hashtag_word <- unlist(hashtag)
  hashtag_word <- tolower(hashtag_word)
  hashtag_word <- gsub("[[:punct:]ー]", "", hashtag_word)
  # if you want to remove a spesific keyword, like 'ukraine':
  #hashtag_word <- hashtag_word[!str_detect(hashtag_word, "ukraine")]
  as.data.frame(hashtag_word) %>%
    count(hashtag_word, sort = TRUE) %>%
    top_n(k) %>%
    ggplot(aes(x = reorder(hashtag_word, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(x = NULL,
         y = "Hashtag",
         title = paste("Top",k,"Popular Hashtags"))}
