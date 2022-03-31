#' See How Words Assosciate with Each Other
#'
#' @param df A dataset containing a column named 'text'
#'
#' @param k Number of words that you want to include. Default is 10
#' @return Interactive Plot
#'
#' @examples
#' word.network(tweets)
#'
#' @export
word.network <- function (df,k=10) {
  bi.gram.words <- df %>%
    unnest_tokens(
      input = text,
      output = bigram,
      token = 'ngrams',
      n = 2) %>%
    filter(! is.na(bigram))
  bi.gram.words %>%
    select(bigram) %>%
    head(k)
  extra.stop.words <- c('https')
  stopwords.df <- tibble(
    word = c(stopwords(kind = 'en'),extra.stop.words))
  bi.gram.words %<>%
    separate(col = bigram, into = c('word1', 'word2'), sep = ' ') %>%
    filter(! word1 %in% stopwords.df$word) %>%
    filter(! word2 %in% stopwords.df$word) %>%
    filter(! is.na(word1)) %>%
    filter(! is.na(word2))
  bi.gram.count <- bi.gram.words %>%
    dplyr::count(word1, word2, sort = TRUE) %>%
    dplyr::rename(weight = n)
  bi.gram.count %>% head()
  threshold <- bi.gram.count$weight[10]
  network <-  bi.gram.count %>%
    filter(weight > threshold) %>%
    graph_from_data_frame(directed = FALSE)
  V(network)$degree <- strength(graph = network)
  E(network)$width <- E(network)$weight/max(E(network)$weight)
  network.D3 <- igraph_to_networkD3(g = network)
  network.D3$nodes  <- network.D3$nodes %>% mutate(Degree = (1E-2)*V(network)$degree)
  network.D3$nodes  <- network.D3$nodes %>% mutate(Group = 1)
  network.D3$links$Width <- 10*E(network)$width
  forceNetwork(
    Links = network.D3$links,
    Nodes = network.D3$nodes,
    Source = 'source',
    Target = 'target',
    NodeID = 'name',
    Group = 'Group',
    opacity = 0.9,
    Value = 'Width',
    Nodesize = 'Degree',
    linkWidth = JS("function(d) { return Math.sqrt(d.value); }"),
    fontSize = 12,
    zoom = TRUE,
    opacityNoHover = 1)
}
