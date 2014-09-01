\name{ggplot2.jitter}
\alias{ggplot2.jitter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot easily a stripchart plot with R package ggplot2
}
\description{
This function is an alias of ggplot2.stripchart. See ggplot2.stripchart for more documentation on the arguments to use.
}
\usage{
ggplot2.jitter(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Other arguments passed on to ggplot2.customize custom function or to geom_jitter and to geom_boxplot functions from ggplot2 package
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
code{\link{ggplot2.stripchart}}
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
