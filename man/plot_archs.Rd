\name{plot_archs}
\alias{plot_archs}
\title{
A function for plotting arechetypes 
}
\description{
A data frame or matrix of archetypes can be plotted
}
\usage{
plot_archs(archs, data = NULL, show_data = FALSE, ...)
}
\arguments{
  \item{archs}{
The matrix or data frame of archetypes where each row represents an archetype
}
  \item{data}{
Optional argument, if used data frame is known
}
  \item{show_data}{
if it set to TRUE, then the used data frame will be plotted at the same plot
}
  \item{\dots}{
Other arguments (ignored)
}
}
\details{
If the column dimension of the archetypes is less or ewqual to 3, then a normal plot is presented.\
Otherwise, a "spike-spider" plot is crerated, see \code{\link{plot.archetypal}} for details.
}
\seealso{
\code{\link{plot.archetypal}}
}
\examples{
BY=matrix(c(5.430744, 2.043404, 3.128485, 3.146242, 2.710978, 4.781843), nrow = 3, byrow = TRUE)
plot_archs(BY)
}
