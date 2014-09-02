\name{ggplot2.histogram}
\alias{ggplot2.histogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Easy histogram plot with R package ggplot2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ggplot2.histogram(data, xName = NULL, groupName = NULL, position = c("identity", "stack", "dodge"), addMeanLine = FALSE, meanLineColor = NULL, meanLineType = "dashed", meanLineSize = 1, addDensityCurve = FALSE, densityFill = "#FF6666", densityAlpha = 0.2, densityLineType = "solid", densityLineColor = "#2F2F2F", scale = c("frequency", "density"), groupColors = NULL, brewerPalette = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data.frame or a numeric vector. Columns are variables and rows are observations.
}
  \item{xName}{
The name of column containing x variable (i.e groups). Default value is NULL.
}
  \item{groupName}{
The name of column containing group variable. This variable is used to color plot according to the group.
}
  \item{position}{
Change the position adjustment to use for overlapping points on the layer. Possible values for the argument position is "identity", "stack", "dodge". Default value is identity.
}
  \item{addMeanLine}{
if TRUE, the mean line is added on the plot for each group. Default value is FALSE.
}
  \item{meanLineColor, meanLineType, meanLineSize}{
mean line color, type and size.
}
  \item{addDensityCurve}{
If true, add density curve. Default value is FALSE.
}
  \item{densityFill}{
The fill color of density plot. The value is considered only when groupName=NULL. If groupName is specified, density curves are colored according groupColors pr brewerPalette.
}
  \item{densityAlpha}{
Degre of transparency of overlaid colors for density curves. Default is 0.2 (20\%).
}
  \item{densityLineType, densityLineColor}{
Line type and color for density curve.
}
  \item{scale}{
Indicate whether y axis values are density or frequency. Default value is frequency.
}
\item{groupColors}{
Color of groups. groupColors should have the same length as groups.
}
  \item{brewerPalette}{
 This can be also used to indicate group colors. In this case the parameter groupColors should be NULL. e.g: brewerPalette="Paired".
}
  \item{\dots}{
Other arguments passed on to ggplot2.customize custom function or to geom_histogram and geom_density functions from ggplot2 package.
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
\code{\link{ggplot2.dotplot}, \link{ggplot2.violinplot}, \link{ggplot2.stripchart}, \link{ggplot2.boxplot}, \link{ggplot2.density}}
}
\examples{
#data
set.seed(1234)
df <- data.frame(grp = factor( rep(c("A","B"), each=200) ), 
                   var = c(rnorm(200),rnorm(200, mean=.8)))
#plot
plot<-ggplot2.histogram(data=df, xName='var',groupName='grp', groupColors=c('#999999','#E69F00'))
plot<-ggplot2.customize(plot, mainTitle="Plot of variable histogram \nper group",
                        xtitle="Variable", ytitle="Histogram")
print(plot)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
