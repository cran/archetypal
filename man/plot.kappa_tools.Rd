\name{plot.kappa_tools}
\alias{plot.kappa_tools}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot an object of the class kappa_tools
}
\description{
It makes a plot of the results created after using \code{\link{kappa_tools}}
}
\usage{
\method{plot}{kappa_tools}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of the class kappa_tools
}
  \item{\dots}{
Other arguments (ignored)
}
}
\details{
A panel of 2 plots is being created, see \code{\link{kappa_tools}} for details.
}
\seealso{
\code{\link{kappa_tools}}
}
\examples{
{\donttest{
### Use the sample data "wd2"
data(wd2)
ch=convhulln(as.matrix(wd2),'Fx')
chlist=as.list(ch)
chvertices = unique(do.call(c,chlist))
aa=archetypal(wd2, 3)
out=kappa_tools(aa ,  df = wd2, numBins = 100, chvertices, verbose = T )
plot(out)
}
}
}