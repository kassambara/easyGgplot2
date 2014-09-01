\name{easyGgplot2-package}
\alias{easyGgplot2-package}
\alias{easyGgplot2}
\docType{package}
\title{Perform and customize easily a plot with ggplot2}
\description{An easy-to-use way to make plot with R package ggplot2}
\details{
\tabular{ll}{
Package: \tab easyGgplot2\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-08-31\cr
License: \tab GPL-2\cr
}
}
\author{
Alboukadel Kassambara <alboukadel.kassambara@gmail.com>

Maintainer: Alboukadel Kassambara <alboukadel.kassambara@gmail.com>
}
\references{
http://www.sthda.com
}

\keyword{ package }
\examples{
#Boxplot
ggplot2.boxplot(data=ToothGrowth, xName='dose',yName='len',
                mainTitle="Plot of length according\n to the dose",
                xtitle="Dose (mg)", ytitle="Length")
                
#Dotplot               
ggplot2.dotplot(data=ToothGrowth, xName='dose',yName='len',
                mainTitle="Plot of length according\n to the dose",
                xtitle="Dose (mg)", ytitle="Length")

#Scatterplot
ggplot2.scatterplot(data=mtcars, xName='wt',yName='mpg', size=3,
                mainTitle="Plot of miles per gallon \n according to the weight",
                xtitle="Weight (lb/1000)", ytitle="Miles/(US) gallon")
}
