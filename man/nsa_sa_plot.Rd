% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/level2_validation.R
\name{nsa_sa_plot}
\alias{nsa_sa_plot}
\title{Plot nsa and sa}
\usage{
nsa_sa_plot(nsa, sa, title = NULL)
}
\arguments{
\item{nsa}{ts object (usually unadjusted time series)}

\item{sa}{ts object (usually seasonally adjusted time series)}

\item{title}{Optional title}
}
\value{
ggplot of nsa and sa series
}
\description{
Plot nsa and sa
}
\examples{
data(data_to_check)
nsa_sa_plot(data_to_check$nsa,data_to_check$sa)
}
