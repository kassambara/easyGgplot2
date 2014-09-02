\name{ggplot2.stripchart}
\alias{ggplot2.stripchart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Easy stripchart plot with R package ggplot2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ggplot2.stripchart(data, xName = NULL, yName = NULL, groupName = NULL, position = position_jitter(0.2), addMean = FALSE, meanPointShape = 5, meanPointSize = 4, meanPointColor = "black", meanPointFill = "blue", addBoxplot = FALSE, boxplotFill = "white", boxplotColor = "black", boxplotLineWeight = 0.5, groupColors = NULL, brewerPalette = NULL, ...)
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
} \item{position}{
The position adjustment to use for overlappling points. Default value is position_jitter(0.2).
}
  \item{addMean}{
if TRUE, the mean point is added on the plot for each group. Default value is FALSE.
}
  \item{meanPointShape}{
The shape of mean point.
}
  \item{meanPointSize}{
The size of mean point
}
  \item{meanPointColor}{
 Border color of the mean point. Default value is "black".
}
  \item{meanPointFill}{
Fill color of mean point. This parameter is used only when meanPointShape=21 to 25. Default value is "blue"
}
  \item{addBoxplot}{
If TRUE, boxplot is added on the dotplot. Default value is FALSE.
}
  \item{boxplotFill}{
 Fill color of the boxplot. Default value is white.
}
  \item{boxplotColor}{
Boxplot line color. Default value is black.
}
  \item{boxplotLineWeight}{
Boxplot line weight. Default value is 0.5.
}
  \item{groupColors}{
 Color of groups. groupColors should have the same length as groups.
}
  \item{brewerPalette}{
This can be also used to indicate group colors. In this case the parameter groupColors should be NULL. e.g: brewerPalette="Paired".
}
  \item{\dots}{
 Other arguments passed on to ggplot2.customize custom function or to geom_dotplot functions from ggplot2 package.
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
\code{\link{ggplot2.boxplot}, \link{ggplot2.violinplot}, \link{ggplot2.dotplot}, \link{ggplot2.density}, \link{ggplot2.histogram}, \link{ggplot2.customize}}
}
\examples{
df <- ToothGrowth
ggplot2.stripchart(data=df, xName='dose',yName='len',
                mainTitle="Plot of length according\n to the dose",
                xtitle="Dose (mg)", ytitle="Length")

#Or use this
plot<-ggplot2.stripchart(data=df, xName='dose',yName='len')
plot<-ggplot2.customize(plot, mainTitle="Plot of length according\n to the dose",
                        xtitle="Dose (mg)", ytitle="Length")
print(plot)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
