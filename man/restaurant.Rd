\name{restaurant}
\alias{restaurant}
\docType{data}
\title{
Restaurant bill for man and woman
}
\description{
Total bill at different time
}
\usage{data(restaurant)}
\format{
  A data frame with 4 observations on the following 3 variables.
  \describe{
    \item{\code{sex}}{a factor with levels \code{Female} \code{Male}}
    \item{\code{time}}{a factor with levels \code{Lunch} \code{Dinner}}
    \item{\code{total_bill}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
http://www.sthda.com
}
\examples{
data(restaurant)
ggplot2.barplot(data=restaurant, xName="sex", yName='total_bill', width=0.5)
}
\keyword{datasets}
