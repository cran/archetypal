\name{FurthestSum}
\alias{FurthestSum}
\title{Application of FurthestSum algorithm in order to find an initial solution for Archetypal Analysis}
\usage{
FurthestSum(Y, kappas, irows, exclude = NULL)
}
\arguments{
\item{Y}{The data frame with dimensions \eqn{n \times d}}

\item{kappas}{The number of archetypes}

\item{irows}{The initially used rows of data frame for starting algorithm}

\item{exclude}{The rows of data frame that we want to exclude from being checked}
}
\value{
The vector of rows that constitute the initial FurthestSum solution
}
\description{
The FurthestSum algorithm as was written by Morup and Hansen
in Matlab, see [1] and it is based on [2]. The algorithm has been converted in order to use commonly used 
data frames in R.
}
\examples{
data("wd3") #3D demo 
df = wd3
FurthestSum(df, kappas = 4, irows = sample(1:dim(df)[1],1))
}
\references{
[1] Source: https://mortenmorup.dk/?page_id=2 , last accessed 2024-03-09

[2] D.S. Hochbaum, D.B. Shmoys, A best possible heuristic for the k-center problem, \cr
   Math. Oper. Res. 10(2) (1985) 180-184. https://doi.org/10.1287/moor.10.2.180
}
\seealso{
\code{\link{find_furthestsum_points}}
}