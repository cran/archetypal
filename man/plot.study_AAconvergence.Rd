\name{plot.study_AAconvergence}
\alias{plot.study_AAconvergence}
\title{
Plot an object of the class study_AAconvergence
}
\description{
It makes a plot of the results created after using \code{\link{study_AAconvergence}}
}
\usage{
\method{plot}{study_AAconvergence}(x, ...)
}
\arguments{
  \item{x}{
An object of the class study_AAconvergence
}
  \item{\dots}{
Other arguments (ignored)
}
}
\details{
A panel of 7 plots is being created, see \code{\link{study_AAconvergence}} for details.
}
\seealso{
\code{\link{study_AAconvergence}}
}
\examples{
{\donttest{
## Use the sample data "wd2"
data(wd2)
yy=study_AAconvergence(wd2, 3, plot = FALSE)
plot(yy)
}
}
}
