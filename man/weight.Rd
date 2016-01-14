\name{weight}
\alias{weight}
\docType{data}
\title{
Weight / Sex
}
\description{
The data contains the weight of 400 people (200 females and 200 males).
}
\usage{data(weight)}
\format{
  A data frame with 400 observations on the following 2 variables.
  \describe{
    \item{\code{sex}}{a factor with levels \code{Female} \code{Male}}
    \item{\code{weight}}{a numeric vector}
  }
}
\references{
http://www.sthda.com
}
\examples{
data(weight)
ggplot2.density(data=weight, xName='weight', groupName='sex',
    alpha=0.5, fillGroupDensity=TRUE )
}
\keyword{datasets}
