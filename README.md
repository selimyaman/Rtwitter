
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rtwitter

Rtwitter lets you analyze and visualize Twitter data. You can easily do
a sentiment analysis, plot interactive word networks, or create table of
most used emojis along with 13 other functions for various visualization
purposes.

The package is still in its early versions - but eventually it will
transform into an Rshiny application where you can type a keyword and
see various analytics. Around those times, I plan to publish it in CRAN
as well.

## Installation

You can install the development version of Rtwitter from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("selimyaman/Rtwitter")
```

## Example Usage

``` r
library(Rtwitter)

# get the example dataset on Ukraine tweets
tweets <- Rtwitter::tw 

# visualize word associations
Rtwitter::word.network(tweets)
```

You can check out other functions included in the package by typing
`Rtwitter::` in your console.
