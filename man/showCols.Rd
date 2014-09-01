\name{showCols}
\alias{showCols}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a plot of color names which R knows about.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
showCols(cl = colors(), bg = "grey", cex = 0.75, rot = 30)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cl}{
A vector of colors to plot. Default value is the list of color names returned by R color() function. 
}
  \item{bg}{
Background color of the plot
}
  \item{cex}{
The size of the text.
}
  \item{rot}{
The rotation angle of the text
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
\code{\link{generateRLineTypes}, \link{generateRPointShapes}}
}
\examples{
col=colors() #list of color names
showCols(col[1:100]) #plot the first 100 colors
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
