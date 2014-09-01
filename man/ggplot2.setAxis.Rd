\name{ggplot2.setAxis}
\alias{ggplot2.setAxis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Customize x and y axis of a plot generated with ggplot2 package
}
\description{
A helper function used by ggplot2.customize to customize x and y axis. 
}
\usage{
ggplot2.setAxis(plot, xShowTitle = TRUE, yShowTitle = TRUE, xtitle = NULL, ytitle = NULL, xtitleFont = c(14, "bold", "black"), ytitleFont = c(14, "bold", "black"), xlim = NULL, ylim = NULL, xScale = c("none", "log2", "log10"), yScale = c("none", "log2", "log10"), xShowTickLabel = TRUE, yShowTickLabel = TRUE, xTickLabelFont = c(12, "bold", "black"), yTickLabelFont = c(12, "bold", "black"), xtickLabelRotation = 0, ytickLabelRotation = 0, hideAxisTicks = FALSE, axisLine = c(0.5, "solid", "#E5E5E5"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot}{
A plot object generated with ggplot2 or easyGgplot2 packages (boxplot, densityplot, histogram, dotplot, ....)
} \item{xShowTitle, yShowTitle}{if TRUE, x axis and y axis titles will be shown. \strong{Default values are TRUE.}}
  \item{xtitle, ytitle}{x and y axis labels.}
  \item{xtitleFont, ytitleFont}{A vector of length 3 indicating respectively the size, the style and the color of x and y axis titles. \strong{Default values are c(14,"bold", "black").}}
  \item{xlim, ylim}{Limit for the x and y axis. \strong{Default values are NULL}.}
  \item{xScale, yScale}{x and y axis scales. Possible values are c("none", "log2", "log10"). Example: yScale="log2". \strong{Default values are "none".}}
  \item{xShowTickLabel, yShowTickLabel}{if TRUE, x and y axis tick mark labels will be shown. \strong{Default values are TRUE.}}
  \item{xTickLabelFont, yTickLabelFont}{A vector of length 3 indicating respectively the size, the style and the color of x and y axis tick label fonts. \strong{Default values are c(12, "bold", "black").}}
  \item{xtickLabelRotation, ytickLabelRotation}{Rotation angle of x and y axis tick labels. \strong{Default values are 0.}}
  \item{hideAxisTicks}{if TRUE, x and y axis ticks are removed. \strong{Default value is FALSE}.}
  \item{axisLine}{A vector of length 3 indicating respectively the size, the line type and the color of axis lines.\strong{ Default value is c(0.5, "solid", "#E5E5E5").}}
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
\code{\link{ggplot2.customize}}
}
\examples{
#Boxplot using easyGgplot2 package
p<-ggplot2.boxplot(data=ToothGrowth, xName='dose',yName='len')
ggplot2.setAxis(p, xtitle="Dose (mg)", ytitle="length", 
                axisLine=c(0.5, "solid", "black"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
