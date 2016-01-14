#' @include utilities.R ggplot2.customize.R
NULL
#' Easy barplot plot with R package ggplot2
#' 
#' @param data data.frame or a numeric vector. Columns are variables and rows
#' are observations.
#' @param xName The name of column containing x variable (i.e groups). Default
#' value is NULL.
#' @param yName The name of column containing y variable. If yName=NULL, data
#' should be a numeric vector.
#' @param groupName The name of column containing group variable. This variable
#' is used to color plot according to the group.
#' @param groupColors Color of groups. groupColors should have the same length
#' as groups.
#' @param brewerPalette This can be also used to indicate group colors. In this
#' case the parameter groupColors should be NULL. e.g: brewerPalette="Paired".
#' @param \dots Other arguments passed on to ggplot2.customize custom function
#' or to geom_bar functions from ggplot2 package.
#' @return a ggplot
#' @author Alboukadel Kassambara <alboukadel.kassambara@@gmail.com>
#' @seealso \code{\link{ggplot2.lineplot}, \link{ggplot2.histogram}}
#' @references http://www.sthda.com
#' @examples
#' 
#' #data
#' df <- data.frame(time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
#'                  total_bill = c(14.89, 17.23))
#' 
#' #plot
#' ggplot2.barplot(data=df, xName="time", yName='total_bill',
#'                 mainTitle="Plot of total bill\n per time of day",
#'                 xtitle="Time of day", ytitle="Total bill")
#' 
#' #Or use this
#' plot<-ggplot2.barplot(data=df, xName="time", yName='total_bill')
#' plot<-ggplot2.customize(plot,  mainTitle="Plot of total bill\n per time of day",
#'                       xtitle="Time of day", ytitle="Total bill")
#' print(plot)
#' 
#' @export ggplot2.barplot
ggplot2.barplot<-function(data, xName=NULL, yName=NULL,groupName=NULL, 
                          groupColors=NULL, brewerPalette=NULL,...)
{
  
  pms <- .barplot_params(...)
  
  #stat: the statistical transformation to use on the data for this layer. Default value is "identity"
  #To get a bar graph of counts, don't map a variable to y, and use stat="bin"
  stat="identity"
  
  if(is.null(xName) & is.null(yName)){
    if(!is.vector(data)) stop("yName is missing or NULL. In this case data should be a numeric or character vector")
    data=cbind(value=data, index=1:length(data))
    xName="index"
    if(is.numeric(data)) yName="value"
    else if(is.character(data)){
      yName=NULL #barplot of the count
      xName="value"
    }
    
  } 
  #x variable is null; create a new data
  #Barplot of y using the index as xName
  if(is.null(xName) & !is.null(yName)){
    y=as.numeric(unlist(data[,yName]))
    data=as.data.frame(cbind(y, index=1:length(y)))
    colnames(data)<-c(yName, "index")
    xName="index"
  }
  #barplot of the count
  else if(!is.null(xName) & is.null(yName)){
    #stat: the statistical transformation to use on the data for this layer. Default value is "identity"
    #To get a bar graph of counts, don't map a variable to y, and use stat="bin"
    stat="count"
  }
  #data
  data=data.frame(data)
  data[,xName]=factor(data[,xName])
  if(is.null(groupName)){
    if(!is.null(yName)) p<-ggplot(data=data, aes_string(x=xName, y=yName))
    else p<-ggplot(data=data, aes_string(x=xName))#barplot of the count
  }
  else {
    data[,groupName]=factor(data[,groupName])#transform groupName to factor
    
    if(!is.null(yName)) p<-ggplot(data=data, aes_string(x=xName, y=yName, fill=groupName))
    else p<-ggplot(data=data, aes_string(x=xName, fill=groupName))
  }
  p<- p+geom_bar(stat=stat, position = "stack", width = pms$width,
                 binwidth = pms$binwidth,
                 na.rm = pms$na.rm, show.legend = pms$show.legend,
                 inherit.aes = pms$inherit.aes)
  
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
