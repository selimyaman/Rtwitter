% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/most.mentioned.R
\name{most.mentioned}
\alias{most.mentioned}
\title{See 10 Most Mentioned Accounts in Tweets}
\usage{
most.mentioned(df)
}
\arguments{
\item{df}{A dataset containing a column named 'text'}
}
\value{
Table
}
\description{
See 10 Most Mentioned Accounts in Tweets
}
\examples{
tweets <- Rtwitter::tw
most.mentioned(tweets)

}
