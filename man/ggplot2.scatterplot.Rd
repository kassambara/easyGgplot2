\name{ggplot2.scatterplot}
\alias{ggplot2.scatterplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Easy scatterplot plot with R package ggplot2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ggplot2.scatterplot(data, xName, yName, groupName = NULL, addRegLine = FALSE, regLineColor = "blue", regLineSize = 0.5, smoothingMethod = c("lm", "glm", "gam", "loess", "rlm"), addConfidenceInterval = FALSE, confidenceLevel = 0.95, confidenceIntervalFill = "#C7C7C7", setColorByGroupName = TRUE, setShapeByGroupName = FALSE, groupColors = NULL, brewerPalette = NULL, ...)
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
  \item{addRegLine}{
If TRUE, regression line is added. Default value is FALSE.
}
  \item{regLineColor}{
Color of regression line. Default value is blue.
}
  \item{regLineSize}{
Weight of regression line. Default value is 0.5.
}
  \item{smoothingMethod}{
Smoothing method (function) to use, eg. lm, glm, gam, loess, rlm. For datasets with n < 1000 default is loess. For datasets with 1000 or more observations defaults to gam. lm for linear smooths, glm for generalised linear smooths, loess for local smooths, gam fits a generalized additive model.
}
  \item{addConfidenceInterval}{
Display confidence interval around smooth? (FALSE by default).
}
  \item{confidenceLevel}{
Level controling confidence region. Default is 95%
}
  \item{confidenceIntervalFill}{
Fill color of confidence intervall
}
  \item{setColorByGroupName}{
If TRUE, points are colored according the groups. Default value is TRUE.
}
  \item{setShapeByGroupName}{
If TRUE, point shapes are different according to the group. Default value is FALSE.
}
  \item{groupColors}{
Color of groups. groupColors should have the same length as groups.
}
  \item{brewerPalette}{
 This can be also used to indicate group colors. In this case the parameter groupColors should be NULL. e.g: brewerPalette="Paired".
}
  \item{\dots}{
Other parameters passed on to ggplot2.customize custom function or to geom_smooth and to geom_point functions from ggplot2 package.
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
\code{\link{ggplot2.dotplot}, \link{ggplot2.violinplot}, \link{ggplot2.stripchart}, \link{ggplot2.boxplot}, \link{ggplot2.histogram}, \link{ggplot2.lineplot}, \link{ggplot2.barplot}}
}
\examples{
df <- mtcars
ggplot2.scatterplot(data=df, xName='wt',yName='mpg', size=3,
                mainTitle="Plot of miles per gallon \n according to the weight",
                xtitle="Weight (lb/1000)", ytitle="Miles/(US) gallon")

#Or use this
plot<-ggplot2.scatterplot(data=df, xName='wt',yName='mpg', size=3)
plot<-ggplot2.customize(plot, mainTitle="Plot of miles per gallon \n according to the weight",
                        xtitle="Weight (lb/1000)", ytitle="Miles/(US) gallon")
print(plot)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
