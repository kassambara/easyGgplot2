#' Customize x and y axis of a plot generated with ggplot2 package
#' 
#' @param plot A plot object generated with ggplot2 or easyGgplot2 packages
#' (boxplot, densityplot, histogram, dotplot, ....)
#' @param xShowTitle,yShowTitle if TRUE, x axis and y axis titles will be
#' shown. \strong{Default values are TRUE.}
#' @param xtitle,ytitle x and y axis labels.
#' @param xtitleFont,ytitleFont A vector of length 3 indicating respectively
#' the size, the style and the color of x and y axis titles. \strong{Default
#' values are c(14,"bold", "black").}
#' @param xlim,ylim Limit for the x and y axis. \strong{Default values are
#' NULL}.
#' @param xScale,yScale x and y axis scales. Possible values are c("none",
#' "log2", "log10"). Example: yScale="log2". \strong{Default values are
#' "none".}
#' @param xShowTickLabel,yShowTickLabel if TRUE, x and y axis tick mark labels
#' will be shown. \strong{Default values are TRUE.}
#' @param xTickLabelFont,yTickLabelFont A vector of length 3 indicating
#' respectively the size, the style and the color of x and y axis tick label
#' fonts. \strong{Default values are c(12, "bold", "black").}
#' @param xtickLabelRotation,ytickLabelRotation Rotation angle of x and y axis
#' tick labels. \strong{Default values are 0.}
#' @param hideAxisTicks if TRUE, x and y axis ticks are removed.
#' \strong{Default value is FALSE}.
#' @param axisLine A vector of length 3 indicating respectively the size, the
#' line type and the color of axis lines.\strong{ Default value is c(0.5,
#' "solid", "#E5E5E5").}
#' @return a ggplot
#' @author Alboukadel Kassambara <alboukadel.kassambara@@gmail.com>
#' @seealso \code{\link{ggplot2.customize}}
#' @references http://www.sthda.com
#' @examples
#' 
#' #Boxplot using easyGgplot2 package
#' p<-ggplot2.boxplot(data=ToothGrowth, xName='dose',yName='len')
#' ggplot2.setAxis(p, xtitle="Dose (mg)", ytitle="length", 
#'                 axisLine=c(0.5, "solid", "black"))
#' 
#' @export ggplot2.setAxis
ggplot2.setAxis<-function(plot,
                          #x and y axis titles and scales
                          xShowTitle=TRUE, yShowTitle=TRUE, 
                          xtitle=NULL, ytitle=NULL,
                          xtitleFont=c(14,"bold", "black"), ytitleFont=c(14,"bold", "black"),
                          xlim=NULL, ylim=NULL,
                          xScale=c("none", "log2", "log10"), yScale=c("none", "log2", "log10"),
                          #x and y axis tick mark labels
                          xShowTickLabel=TRUE, yShowTickLabel=TRUE,
                          xTickLabelFont=c(12, "bold", "black"), yTickLabelFont=c(12, "bold", "black"),
                          xtickLabelRotation=0, ytickLabelRotation=0,
                          hideAxisTicks=FALSE, axisLine=c(0.5, "solid", "#E5E5E5"))
{
  
  #axis title
  if(!is.null(xtitle)) plot<-plot+xlab(xtitle)
  if(!is.null(ytitle)) plot<-plot+ylab(ytitle)
  
  if(!xShowTitle)plot<-plot+theme(axis.title.x=element_blank())
  else plot<-plot+theme(axis.title.x=element_text(size=as.numeric(xtitleFont[1]),
                                                  face=xtitleFont[2], colour=xtitleFont[3], hjust=0.4))
  
  if(!yShowTitle)plot<-plot+theme(axis.title.y=element_blank())
  else plot<-plot+theme(axis.title.y=element_text(size=as.numeric(ytitleFont[1]),
                                                  face=ytitleFont[2], colour=ytitleFont[3], vjust=0.4, angle=90))
  
  
  #xlim, ylim: limit for the x and y axis
  if(!is.null(ylim)) plot<-plot+ylim(ylim[1], ylim[2])
  if(!is.null(xlim)) plot<-plot+xlim(xlim[1], xlim[2])
  
  #log scale
  if(xScale[1]=="log2") plot<-plot+scale_x_continuous(trans='log2')
  else if(xScale[1]=="log10") plot<-plot+scale_x_log10()
  
  if(yScale[1]=="log2") plot<-plot+scale_y_continuous(trans='log2')
  else if(yScale[1]=="log10") plot<-plot+scale_y_log10()
  
  #x and y axis tick mark labels
  if(!xShowTickLabel) plot<-plot+theme(axis.text.x=element_blank())
  else plot<-plot+theme(axis.text.x=element_text(size=as.numeric(xTickLabelFont[1]),
                                                 face=xTickLabelFont[2], colour=xTickLabelFont[3], angle=xtickLabelRotation))
  if(!yShowTickLabel) plot<-plot+theme(axis.text.y=element_blank())
  else plot<-plot+theme(axis.text.y=element_text(size=as.numeric(yTickLabelFont[1]),
                                                 face=yTickLabelFont[2], colour=yTickLabelFont[3], angle=ytickLabelRotation))
  if(hideAxisTicks) plot<-plot+theme(axis.ticks=element_blank())
  
  #Axis line
  plot<-plot+theme(axis.line=element_line(size=as.numeric(axisLine[1]),
                                          linetype=axisLine[2], colour=axisLine[3]))
  
  plot
}
