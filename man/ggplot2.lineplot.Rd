\name{ggplot2.lineplot}
\alias{ggplot2.lineplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Easy lineplot plot with R package ggplot2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ggplot2.lineplot(data, xName, yName, groupName = NULL, addPoint = FALSE, pointSize = 1.5, pointShape = 19, pointFill = NULL, pointColor = "black", arrow = NULL, xAxisType = c("categorical", "continuous"), groupColors = NULL, brewerPalette = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data.frame or a numeric vector. Columns are variables and rows are observations.
}
  \item{xName}{
The name of column containing x variable (i.e groups).
}
  \item{yName}{
The name of column containing y variable.
}
  \item{groupName}{
The name of column containing group variable. This variable is used to color plot according to the group.
}
  \item{addPoint}{
If TRUE, points are added to the plot. Default value is FALSE.
}
  \item{pointSize, pointShape, pointFill, pointColor}{
Modify point size, shape, fill and color.
}
  \item{arrow}{
 arrow is function from package grid. This function Describe arrows to add to a line. See ?grid::arrow for more details. Usage : arrow=arrow(). Default value is NULL.
}
  \item{xAxisType}{
Indicate the type of x-axis. Possible values =c("categorical", "continuous"). Default value is "categorical". When the variable on the x-axis is numeric, it is sometimes useful to treat it as continuous, and sometimes useful to treat it as categorical.
}
  \item{groupColors}{
Color of groups. groupColors should have the same length as groups.
}
  \item{brewerPalette}{
This can be also used to indicate group colors. In this case the parameter groupColors should be NULL. e.g: brewerPalette="Paired".
}
  \item{\dots}{
Other arguments passed on to ggplot2.customize custom function or to geom_line functions from ggplot2 package.
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
\code{\link{ggplot2.barplot}}
}
\examples{
#data
df <- data.frame(time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
                 total_bill = c(14.89, 17.23))

#plot
ggplot2.lineplot(data=df, xName="time", yName='total_bill',
                mainTitle="Plot of total bill\n per time of day",
                xtitle="Time of day", ytitle="Total bill")

#Or use this
plot<-ggplot2.lineplot(data=df, xName="time", yName='total_bill')
plot<-ggplot2.customize(plot,  mainTitle="Plot of total bill\n per time of day",
                      xtitle="Time of day", ytitle="Total bill")
print(plot)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
