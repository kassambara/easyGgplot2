\name{ggplot2.barplot}
\alias{ggplot2.barplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Easy barplot plot with R package ggplot2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ggplot2.barplot(data, xName = NULL, yName = NULL, groupName = NULL, groupColors = NULL, brewerPalette = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data.frame or a numeric vector. Columns are variables and rows are observations.
}
  \item{xName}{
The name of column containing x variable (i.e groups). Default value is NULL.
}
  \item{yName}{
The name of column containing y variable. If yName=NULL, data should be a numeric vector.
}
  \item{groupName}{
The name of column containing group variable. This variable is used to color plot according to the group.
}
  \item{groupColors}{
Color of groups. groupColors should have the same length as groups.
}
  \item{brewerPalette}{
This can be also used to indicate group colors. In this case the parameter groupColors should be NULL. e.g: brewerPalette="Paired".
}
  \item{\dots}{
Other arguments passed on to ggplot2.customize custom function or to geom_bar functions from ggplot2 package.
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
\code{\link{ggplot2.lineplot}, \link{ggplot2.histogram}}
}
\examples{
#data
df <- data.frame(time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
                 total_bill = c(14.89, 17.23))

#plot
ggplot2.barplot(data=df, xName="time", yName='total_bill',
                mainTitle="Plot of total bill\n per time of day",
                xtitle="Time of day", ytitle="Total bill")

#Or use this
plot<-ggplot2.barplot(data=df, xName="time", yName='total_bill')
plot<-ggplot2.customize(plot,  mainTitle="Plot of total bill\n per time of day",
                      xtitle="Time of day", ytitle="Total bill")
print(plot)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
