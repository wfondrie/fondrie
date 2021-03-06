% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggcolor.R
\name{ggcolor}
\alias{ggcolor}
\title{Emulate ggplot2 colors}
\usage{
ggcolor(n)
}
\arguments{
\item{n}{Number of colors}
}
\value{
A vector of length n containing hexidecimal color codes.
}
\description{
Creates a vector of n colors spread evenly across hues, thereby emulating
the default color scheme of ggplot2.
}
\examples{
ggcolor(2)
}
\references{
http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
}

