\name{ggplot2.plotMeans}
\alias{ggplot2.plotMeans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean plot 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ggplot2.plotMeans(data = "", xName, yName, groupName = NULL, plotType = c("lineplot", "barplot"), error.bars = c("se", "sd", "conf.int", "none"), level = 0.95, errorBarWidth = 0.2, errorBarColor = NULL, printSummary = F, pairedSample = FALSE, idSubject = NULL, errorBarPosition = position_dodge(0.9), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{xName}{
%%     ~~Describe \code{xName} here~~
}
  \item{yName}{
%%     ~~Describe \code{yName} here~~
}
  \item{groupName}{
%%     ~~Describe \code{groupName} here~~
}
  \item{plotType}{
%%     ~~Describe \code{plotType} here~~
}
  \item{error.bars}{
%%     ~~Describe \code{error.bars} here~~
}
  \item{level}{
%%     ~~Describe \code{level} here~~
}
  \item{errorBarWidth}{
%%     ~~Describe \code{errorBarWidth} here~~
}
  \item{errorBarColor}{
%%     ~~Describe \code{errorBarColor} here~~
}
  \item{printSummary}{
%%     ~~Describe \code{printSummary} here~~
}
  \item{pairedSample}{
%%     ~~Describe \code{pairedSample} here~~
}
  \item{idSubject}{
%%     ~~Describe \code{idSubject} here~~
}
  \item{errorBarPosition}{
%%     ~~Describe \code{errorBarPosition} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data = "", xName, yName, groupName = NULL, plotType = c("lineplot", 
    "barplot"), error.bars = c("se", "sd", "conf.int", "none"), 
    level = 0.95, errorBarWidth = 0.2, errorBarColor = NULL, 
    printSummary = F, pairedSample = FALSE, idSubject = NULL, 
    errorBarPosition = position_dodge(0.9), ...) 
{
    rquery.loadPackages("ggplot2")
    data = data.frame(data)
    data[, xName] = factor(data[, xName])
    if (pairedSample) {
        if (is.null(groupName)) 
            df <- summarySEwithin(data, measurevar = yName, withinvars = c(xName), 
                idvar = idSubject, conf.interval = level)
        else df <- summarySEwithin(data, measurevar = yName, 
            idvar = idSubject, withinvars = c(groupName, xName), 
            conf.interval = level)
    }
    else {
        if (is.null(groupName)) 
            df <- summarySE(data, measurevar = yName, groupvars = c(xName), 
                conf.interval = level)
        else df <- summarySE(data, measurevar = yName, groupvars = c(xName, 
            groupName), conf.interval = level)
    }
    ymin = NULL
    ymax = NULL
    if (error.bars[1] == "sd") {
        ymin = df[, yName] - df[, "sd"]
        ymax = df[, yName] + df[, "sd"]
    }
    else if (error.bars[1] == "se") {
        ymin = df[, yName] - df[, "se"]
        ymax = df[, yName] + df[, "se"]
    }
    else if (error.bars[1] == "conf.int") {
        ymin = df[, yName] - df[, "ci"]
        ymax = df[, yName] + df[, "ci"]
    }
    if (error.bars[1] == "none") 
        data = df
    else data = cbind(df, ymin = ymin, ymax = ymax)
    if (plotType[1] == "lineplot") {
        p <- ggplot2.lineplot(data = data, xName = xName, yName = yName, 
            groupName = groupName, ...)
        if (error.bars[1] != "none") {
            if (is.null(errorBarColor)) 
                p <- p + geom_errorbar(aes_string(ymin = "ymin", 
                  ymax = "ymax"), width = errorBarWidth, ...)
            else p <- p + geom_errorbar(aes_string(ymin = "ymin", 
                ymax = "ymax"), width = errorBarWidth, colour = errorBarColor, 
                ...)
        }
    }
    else if (plotType[1] == "barplot") {
        p <- ggplot2.barplot(data = data, xName = xName, yName = yName, 
            groupName = groupName, position = "dodge", ...)
        if (error.bars[1] != "none") {
            if (is.null(errorBarColor)) 
                p <- p + geom_errorbar(aes_string(ymin = "ymin", 
                  ymax = "ymax"), width = errorBarWidth, position = errorBarPosition, 
                  ...)
            else p <- p + geom_errorbar(aes_string(ymin = "ymin", 
                ymax = "ymax"), width = errorBarWidth, colour = errorBarColor, 
                position = errorBarPosition, ...)
        }
    }
    if (printSummary) {
        cat("\n\n----------------------------------------------\nSummary of Data\n----------------------------------------------\n")
        print(data, digits = 1)
        cat("\n----------------------------------------------\n\n")
    }
    p
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
