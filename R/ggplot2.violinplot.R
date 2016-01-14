#' @include utilities.R ggplot2.customize.R
NULL
#' Easy violinplot plot with R package ggplot2
#' 
#' 
#' @param data data.frame or a numeric vector. Columns are variables and rows
#' are observations.
#' @param xName The name of column containing x variable (i.e groups). Default
#' value is NULL.
#' @param yName The name of column containing y variable. If yName=NULL, data
#' should be a numeric vector.
#' @param groupName The name of column containing group variable. This variable
#' is used to color plot according to the group.
#' @param addMean if TRUE, the mean point is added on the plot for each group.
#' Default value is FALSE.
#' @param meanPointShape The shape of mean point.
#' @param meanPointSize The size of mean point
#' @param meanPointColor Border color of the mean point. Default value is
#' "black".
#' @param meanPointFill Fill color of mean point. This parameter is used only
#' when meanPointShape=21 to 25. Default value is "blue"
#' @param addDot If TRUE, dotplot is added on the boxplot. Default value is
#' FALSE.
#' @param dotSize The size of dots.
#' @param dotPosition Possible values are "center" and "jitter". Default value
#' is "center".
#' @param jitter Degree of jitter in x direction. Default value is 0.2.
#' @param groupColors Color of groups. groupColors should have the same length
#' as groups.
#' @param brewerPalette This can be also used to indicate group colors. In this
#' case the parameter groupColors should be NULL. e.g: brewerPalette="Paired".
#' @param \dots Other arguments passed on to ggplot2.customize custom function
#' or to geom_dotplot and to geom_violin functions from ggplot2 package.
#' @return a ggplot
#' @author Alboukadel Kassambara <alboukadel.kassambara@@gmail.com>
#' @seealso \code{\link{ggplot2.dotplot}, \link{ggplot2.boxplot},
#' \link{ggplot2.stripchart}, \link{ggplot2.density}, \link{ggplot2.histogram}}
#' @references http://www.sthda.com
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' df <- ToothGrowth
#' ggplot2.violinplot(data=df, xName='dose',yName='len',
#'                 mainTitle="Plot of length according\n to the dose",
#'                 xtitle="Dose (mg)", ytitle="Length")
#' 
#' #Or use this
#' plot<-ggplot2.violinplot(data=df, xName='dose',yName='len')
#' plot<-ggplot2.customize(plot, mainTitle="Plot of length according\n to the dose",
#'                         xtitle="Dose (mg)", ytitle="Length")
#' print(plot)
#' 
#' @export ggplot2.violinplot
ggplot2.violinplot<-function(data, xName=NULL, yName=NULL, groupName=NULL,
                             addMean=F, meanPointShape=5, meanPointSize=4, meanPointColor="black", meanPointFill="blue",
                             addDot=FALSE, dotSize=1, dotPosition=c("center", "jitter"), jitter=0.2,
                             groupColors=NULL, brewerPalette=NULL,...)
{
  
  params <- list(...)
  trim <- ifelse(!is.null(params$trim), params$trim, TRUE)
  position <- ifelse(!is.null(params$position), params$position, "dodge")
  
  #if yName is missing or null, data should be a numeric vector
  if(is.null(yName) & !is.numeric(data))
    stop("yName is missing or NULL. In this case data should be a numeric vector")
  #data is a numeric vector
  else if(is.numeric(data)){
    data=cbind(y=data, x=rep(1, length(data)))
    xName="x"
    yName="y"
  }
  #xName is missing or  NULL => single boxplot corresponding to the deistribution of the variable
  #bind group column to data
  if(is.null(xName)){
    data=cbind(data, x=rep(1, nrow(data)))
    xName="x"
  }
  
  #data
  data=data.frame(data)
  data[,xName]=factor(data[,xName])
  #plot
  if(is.null(groupName))p<-ggplot(data=data, aes_string(x=xName, y=yName))
  else{
    p<-ggplot(data=data, aes_string(x=xName, y=yName, fill=groupName))
    data[,groupName]=factor(data[,groupName])#transform groupName to factor
  }
  p<-p+geom_violin(trim = trim, position = position)
  #add Mean point
  if(addMean) p<-p+stat_summary(fun.y=mean, geom='point', shape=meanPointShape, 
                                size=meanPointSize, colour=meanPointColor, fill=meanPointFill)
  #add dot
  if(addDot){
    pms <- .dotplot_params(...)
    if(dotPosition[1]=="center") p<-p+geom_dotplot(binaxis='y', stackdir='center', dotsize=dotSize,
                                                   width = pms$width, stackratio = pms$stackratio)
    else p<-p+geom_jitter(position=position_jitter(jitter), cex=dotSize, shape=16)
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
