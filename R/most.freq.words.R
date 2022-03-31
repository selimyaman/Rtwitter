#' See Most Frequently Used Words in Tweets
#'
#' @param df A dataset containing a column named 'text'
#'
#' @return Ggplot Graph
#'
#' @examples
#' most.freq.words(tweets)
#'
#' @export

most.freq.words <- function (df) {df %>%
    select(text) %>%
    unnest_tokens(word, text) %>%
    filter(!(gsub("'", "", word) %in% gsub("'", "", stop_words$word)) & word != "") %>%
    count(word, sort = TRUE) %>%
    top_n(15) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    labs(y = "Count",
         x = "Unique words",
         title = "Most frequent words found in the tweets of Bill Gates",
         subtitle = "Stop words removed from the list")
}
