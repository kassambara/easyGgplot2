#' @include utilities.R ggplot2.customize.R
NULL
#' Easy line plot plot with R package ggplot2
#' @description Draw a line plot using ggplot2
#' @param data data.frame or a numeric vector. Columns are variables and rows
#' are observations.
#' @param xName The name of column containing x variable (i.e groups).
#' @param yName The name of column containing y variable.
#' @param groupName The name of column containing group variable. This variable
#' is used to color plot according to the group.
#' @param addPoint If TRUE, points are added to the plot. Default value is
#' FALSE.
#' @param pointSize,pointShape,pointFill,pointColor Modify point size, shape,
#' fill and color.
#' @param arrow arrow is function from package grid. This function Describe
#' arrows to add to a line. See ?grid::arrow for more details. Usage :
#' arrow=arrow(). Default value is NULL.
#' @param xAxisType Indicate the type of x-axis. Possible values
#' =c("categorical", "continuous"). Default value is "categorical". When the
#' variable on the x-axis is numeric, it is sometimes useful to treat it as
#' continuous, and sometimes useful to treat it as categorical.
#' @param groupColors Color of groups. groupColors should have the same length
#' as groups.
#' @param brewerPalette This can be also used to indicate group colors. In this
#' case the parameter groupColors should be NULL. e.g: brewerPalette="Paired".
#' @param ... Other arguments passed on to ggplot2.customize custom function
#' or to geom_line functions from ggplot2 package.
#' @return a ggplot
#' @author Alboukadel Kassambara <alboukadel.kassambara@@gmail.com>
#' @seealso \code{\link{ggplot2.barplot}}
#' @references http://www.sthda.com
#' @examples
#' 
#' #data
#' df <- data.frame(time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
#'                  total_bill = c(14.89, 17.23))
#' 
#' #plot
#' ggplot2.lineplot(data=df, xName="time", yName='total_bill',
#'                 mainTitle="Plot of total bill\n per time of day",
#'                 xtitle="Time of day", ytitle="Total bill")
#' 
#' #Or use this
#' plot<-ggplot2.lineplot(data=df, xName="time", yName='total_bill')
#' plot<-ggplot2.customize(plot,  mainTitle="Plot of total bill\n per time of day",
#'                       xtitle="Time of day", ytitle="Total bill")
#' print(plot)
#' 
#' @export ggplot2.lineplot
ggplot2.lineplot<-function(data, xName, yName,  groupName=NULL,
                           addPoint=FALSE, pointSize=1.5, pointShape=19, pointFill=NULL, pointColor="black",
                           arrow=NULL, xAxisType=c("categorical", "continuous"),                     
                           groupColors=NULL, brewerPalette=NULL, 
                            ...)
{
  spms <- .standard_params(...)
  
  data=data.frame(data)
  if(xAxisType[1]=="categorical") data[,xName]=factor(data[,xName])
  else if(xAxisType[1]=="continuous") data[,xName] = as.numeric(data[,xName])
  #basic graphs
  #+++++++++++++++++++++++
  if(is.null(groupName)){
    p<-ggplot(data=data, aes_string(x=xName, y=yName, group=1))
    p<-p+geom_line(arrow=arrow, linetype = spms$linetype, 
                   color = spms$color, size = spms$size)
    if(is.null(pointFill)) pointFill="white"
    if(addPoint) p<-p+geom_point(colour=pointColor, size=pointSize,shape=pointShape, fill=pointFill) 
  }
  # graphs with groups: Set color/shape by another variable
  #+++++++++++++++++++++++
  else{    
    data[,groupName]=factor(data[,groupName])#transform groupName to factor
    p<-ggplot(data=data, aes_string(x=xName, y=yName,group=groupName, color=groupName,linetype=groupName,
                                    shape=groupName, fill=groupName )) 
    p<-p+geom_line(arrow=arrow) 
    if(addPoint){
      if(is.null(pointFill)) p<-p+geom_point(size=pointSize)
      else p<-p+geom_point(size=pointSize, fill=pointFill)
    } 
  } 
  #group colors
  if(!is.null(groupColors)){
    p<-p+scale_fill_manual(values=groupColors)
    p<-p+scale_colour_manual(values=groupColors)
  } 
  else if(!is.null(brewerPalette)){
    p<-p+scale_fill_brewer(palette=brewerPalette)
    p<-p+scale_colour_brewer(palette=brewerPalette, guide="none")
  }  
  #ggplot2.customize : titles, colors, background, legend, ....
  p<-ggplot2.customize(p,...) 
  p
}
