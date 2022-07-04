\name{print.archetypal}
\alias{print.archetypal}
\title{
Print an object of the class archetypal.
}
\description{
It prints the output of \code{\link{archetypal}}
}
\usage{
\method{print}{archetypal}(x, ...)
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
Since Archetypal Analysis (AA) is essentially one more matrix decomposition of the form Y ~ ABY, it is reasonable to print:
\enumerate{
\item the \eqn{kappas \times d} matrix of archetypes found
\item the \eqn{n \times kappas} matrix A such that Y ~ ABY or Frobenius norm ||Y-ABY|| is minimum
\item the \eqn{kappas \times n} matrix B such that Y ~ ABY or Frobenius norm ||Y-ABY|| is minimum
}
}
\examples{
{\donttest{
## Use the sample data "wd2"
data(wd2)
aa=archetypal(wd2, 3)
print(aa)
}
}
}
