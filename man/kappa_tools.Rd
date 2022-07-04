\name{kappa_tools}
\alias{kappa_tools}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute kappa tools for data dimensionality analysis
}
\description{
For a given data set and a given Archetypal Analysis (AA) solution, it finds a set of useful proxies
for the dimensionality.
}
\usage{
kappa_tools(aa, df = NULL, numBins = 100, chvertices = NULL, verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aa}{
An object of the class 'archetypal'
}
  \item{df}{
The data frame that was used for AA
}
  \item{numBins}{
The number of bins to be used for computing entropy
}
  \item{chvertices}{
The Convex Hull vertices, if they are given
}
  \item{verbose}{
Logical, set to TRUE if details must be printed
}
  \item{\dots}{
Other areguments, not used.
}
}
\details{
The ECDF for the Squared Errors (SE) is computed and then the relevant curve
is classified as 'convex' or 'concave' and its UIK & inflcetion point is found. Then the number of used
rows for cfreating archetypes is found. A procedure for creating BIC and andjusted BIC is used. 
Finally the pecentage of used points that lie on the exact Convex Hull is given.
}
\value{
A list with next arguments:
\item{ecdf}{The ECDF of SE}
\item{Convexity}{The convex or concave classification for ECDF curve}
\item{UIK}{The UIK points of ECDF curve by using [1]}
\item{INFLECTION}{The inflection points of ECDF curve by using [2]}
\item{NumberRowsUsed}{The number of rows used for creating archetypes}
\item{RowsUsed}{The exact rows used for creating archetypes}
\item{SSE}{The Sum of SE}
\item{BIC}{The computed BIC by using [3], [4]}
\item{adjBIC}{The computed adjusted BIC by using [3], [4]}
\item{CXHE}{The percentage of used points that lie on the exact Convex Hull}
%% ...
}
\references{
[1] Demetris T. Christopoulos, Introducing Unit Invariant Knee (UIK) As an Objective Choice for Elbow Point in Multivariate Data Analysis Techniques (March 1, 2016). Available at SSRN: https://ssrn.com/abstract=3043076 or http://dx.doi.org/10.2139/ssrn.3043076

[2] Demetris T. Christopoulos, On the efficient identification of an inflection point,International Journal of Mathematics and Scientific Computing,(ISSN: 2231-5330), vol. 6(1), 2016.

[3] Felix Abramovich, Yoav Benjamini, David L. Donoho, Iain M. Johnstone. "Adapting to unknown sparsity by
controlling the false discovery rate." The Annals of Statistics, 34(2) 584-653 April 2006.
https://doi.org/10.1214/009053606000000074

[4] Murari, Andrea, Emmanuele Peluso, Francesco Cianfrani, Pasquale Gaudio, and Michele Lungaroni. 2019. "On the Use of Entropy to Improve Model Selection Criteria" Entropy 21, no. 4: 394. https://doi.org/10.3390/e21040394
}
\author{
Demetris T. Christopoulos, David F. Midgley (creator of BIC and adjBIC procedures)
}
\examples{
{\donttest{
## Use the sample data "wd2"
data(wd2)
ch=convhulln(as.matrix(wd2),'Fx')
chlist=as.list(ch)
chvertices = unique(do.call(c,chlist))
aa=archetypal(wd2, 3)
out=kappa_tools(aa ,  wd2, chvertices, verbose = T )
out
}
}
}
