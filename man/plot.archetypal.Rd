\name{plot.archetypal}
\alias{plot.archetypal}
\title{
Plot an object of the class archetypal.
}
\description{
It makes a plot of the archetypes creating after using \code{\link{archetypal}}
}
\usage{
\method{plot}{archetypal}(x, ...)
}
\arguments{
  \item{x}{
An object of the class archetypal
}
  \item{\dots}{
Other arguments (ignored)
}
}
\details{
If the data frame has column dimension at most 3, then a direct plot is available. Otherwise we use a "spike-spider" plot which is a combination of the common "spider" or "web" or "radar" plot with an additional "spike plot" that shows the frequency of each variable at the same line of the spider plot.
}
\examples{
{\donttest{
## Use the sample data "wd2"
data(wd2)
aa=archetypal(wd2, 3)
plot(aa)
}
}
}
