#' @include utilities.R ggplot2.customize.R
NULL
#' Easy scatterplot plot using ggplot2
#' 
#' @description 
#' Draw easily a scatter plot using ggplot2 package
#' @param data data.frame or a numeric vector. Columns are variables and rows
#' are observations.
#' @param xName The name of column containing x variable (i.e groups).
#' @param yName The name of column containing y variable.
#' @param groupName The name of column containing group variable. This variable
#' is used to color plot according to the group.
#' @param addRegLine If TRUE, regression line is added. Default value is FALSE.
#' @param regLineColor Color of regression line. Default value is blue.
#' @param regLineSize Weight of regression line. Default value is 0.5.
#' @param smoothingMethod Smoothing method (function) to use, eg. lm, glm, gam,
#' loess, rlm. For datasets with n < 1000 default is loess. For datasets with
#' 1000 or more observations defaults to gam. lm for linear smooths, glm for
#' generalised linear smooths, loess for local smooths, gam fits a generalized
#' additive model.
#' @param addConfidenceInterval Display confidence interval around smooth?
#' (FALSE by default).
#' @param confidenceLevel Level controling confidence region. Default is 95\%
#' @param confidenceIntervalFill Fill color of confidence intervall
#' @param setColorByGroupName If TRUE, points are colored according the groups.
#' Default value is TRUE.
#' @param setShapeByGroupName If TRUE, point shapes are different according to
#' the group. Default value is FALSE.
#' @param groupColors Color of groups. groupColors should have the same length
#' as groups.
#' @param brewerPalette This can be also used to indicate group colors. In this
#' case the parameter groupColors should be NULL. e.g: brewerPalette="Paired".
#' @param ... Other parameters passed on to ggplot2.customize custom function
#' or to geom_smooth and to geom_point functions from ggplot2 package.
#' @return a ggplot
#' @author Alboukadel Kassambara <alboukadel.kassambara@@gmail.com>
#' @seealso \code{\link{ggplot2.dotplot}, \link{ggplot2.violinplot},
#' \link{ggplot2.stripchart}, \link{ggplot2.boxplot}, \link{ggplot2.histogram},
#' \link{ggplot2.lineplot}, \link{ggplot2.barplot}}
#' @references http://www.sthda.com
#' @examples
#' df <- mtcars
#' ggplot2.scatterplot(data=df, xName='wt',yName='mpg', size=3,
#'                 mainTitle="Plot of miles per gallon \n according to the weight",
#'                 xtitle="Weight (lb/1000)", ytitle="Miles/(US) gallon")
#' 
#' #Or use this
#' plot<-ggplot2.scatterplot(data=df, xName='wt',yName='mpg', size=3)
#' plot<-ggplot2.customize(plot, mainTitle="Plot of miles per gallon \n according to the weight",
#'                         xtitle="Weight (lb/1000)", ytitle="Miles/(US) gallon")
#' print(plot)
#' 
#' @export ggplot2.scatterplot
ggplot2.scatterplot<-function(data, xName, yName, groupName=NULL,
                              addRegLine=FALSE,regLineColor="blue",regLineSize=0.5,
                              smoothingMethod=c("lm", "glm", "gam", "loess", "rlm"),
                              addConfidenceInterval=FALSE, confidenceLevel= 0.95,confidenceIntervalFill="#C7C7C7",
                              setColorByGroupName=TRUE, setShapeByGroupName=FALSE,
                              groupColors=NULL, brewerPalette=NULL,...)
  
{
  spms <- .standard_params(...)
  
  p<-ggplot(data=data, aes_string(x=xName, y=yName))
  
  #Set color/shape by another variable
  #++++++++++
  if(!is.null(groupName)){
    data[,groupName]=factor(data[,groupName])#transform groupName to factor
    #set shape and color by groups
    if(setColorByGroupName & setShapeByGroupName)
      p<-ggplot(data=data, aes_string(x=xName, y=yName, color=groupName, shape=groupName))	
    #set only color by group
    else if(setColorByGroupName==TRUE & setShapeByGroupName==FALSE)
      p<-ggplot(data=data, aes_string(x=xName, y=yName, color=groupName))	
    #set only shape by group
    else if(setColorByGroupName==FALSE & setShapeByGroupName==TRUE)
      p<-ggplot(data=data, aes_string(x=xName, y=yName, shape=groupName))  							
  }  
  #plot
  if(is.null(groupName))   p<-p+geom_point(shape = spms$shape, color = spms$color, fill = spms$fill, size = spms$size)
  else p <- p+geom_point()
  #Add regression line
  if(addRegLine){
    if(is.null(groupName))
      p<-p+geom_smooth(method=smoothingMethod[1], se=addConfidenceInterval, level=confidenceLevel,
                       color=regLineColor, size=regLineSize,fill=confidenceIntervalFill) 
    else p<-p+geom_smooth(method=smoothingMethod[1], se=addConfidenceInterval, level=confidenceLevel,
                          size=regLineSize,fill=confidenceIntervalFill)       
  }#end addRegressionLine
  else if (addConfidenceInterval) p<-p+geom_smooth(se=addConfidenceInterval, level=confidenceLevel,
                                                   fill=confidenceIntervalFill) 
  
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
