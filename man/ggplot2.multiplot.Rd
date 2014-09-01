\name{ggplot2.multiplot}
\alias{ggplot2.multiplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Put multiple graphs on the same page.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ggplot2.multiplot(..., plotlist = NULL, cols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots, plotlist}{
List of ggplot2 objects separated by a comma. (e.g: plot1, plot2, plot3)
}
  \item{cols}{
Number of columns in layout
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
#data.frame
df <- ToothGrowth
#Custom boxplot with centered dot plot
plot1<-ggplot2.boxplot(data=df, xName='dose',yName='len', groupName='dose',
                addDot=TRUE, dotSize=1)

#Custom dotplot with centered dot plot
plot2<-ggplot2.dotplot(data=df, xName='dose',yName='len', groupName='dose')

#Custom stripchart with centered dot plot
plot3<-ggplot2.stripchart(data=df, xName='dose',yName='len', groupName='dose')

#Notched boxplot
plot4<-ggplot2.boxplot(data=df, xName='dose',yName='len',
                notch=TRUE)

#Multple graphs on the same page
ggplot2.multiplot(plot1,plot2,plot3,plot4, cols=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
