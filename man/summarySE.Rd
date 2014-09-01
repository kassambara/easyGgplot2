\name{summarySE}
\alias{summarySE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Helper function to summarize data
}
\description{
It gives the count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
}
\usage{
summarySE(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data.frame.
}
  \item{measurevar}{
The name of a column that contains the variable to be summariezed.
}
  \item{groupvars}{
A vector containing names of columns that contain grouping variables.
}
  \item{na.rm}{
A boolean that indicates whether to ignore NA.
}
  \item{conf.interval}{
The percent range of the confidence interval (default is 95%)
}
  \item{.drop}{
see ddply function from plyr package.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
http://www.sthda.com
}
\author{
Alboukadel Kassambara <alboukadel.kassambara@gmail.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#summarize len variable from ToothGrowth data
summarySE(ToothGrowth, measurevar="len")

#mean, sd, se and ci of len according to the dose
summarySE(ToothGrowth, measurevar="len", groupvars="dose")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
