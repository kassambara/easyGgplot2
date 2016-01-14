#' @include utilities.R ggplot2.customize.R
NULL
#' Easy histogram plot with R package ggplot2
#' 
#' @param data data.frame or a numeric vector. Columns are variables and rows
#' are observations.
#' @param xName The name of column containing x variable (i.e groups). Default
#' value is NULL.
#' @param groupName The name of column containing group variable. This variable
#' is used to color plot according to the group.
#' @param position Change the position adjustment to use for overlapping points
#' on the layer. Possible values for the argument position is "identity",
#' "stack", "dodge". Default value is identity.
#' @param addMeanLine if TRUE, the mean line is added on the plot for each
#' group. Default value is FALSE.
#' @param meanLineColor,meanLineType,meanLineSize mean line color, type and
#' size.
#' @param addDensityCurve If true, add density curve. Default value is FALSE.
#' @param densityFill The fill color of density plot. The value is considered
#' only when groupName=NULL. If groupName is specified, density curves are
#' colored according groupColors pr brewerPalette.
#' @param densityAlpha Degre of transparency of overlaid colors for density
#' curves. Default is 0.2 (20\%).
#' @param densityLineType,densityLineColor Line type and color for density
#' curve.
#' @param scale Indicate whether y axis values are density or frequency.
#' Default value is frequency.
#' @param groupColors Color of groups. groupColors should have the same length
#' as groups.
#' @param brewerPalette This can be also used to indicate group colors. In this
#' case the parameter groupColors should be NULL. e.g: brewerPalette="Paired".
#' @param linetype,fill,color linetype, outline and fill colors
#' @param \dots Other arguments passed on to ggplot2.customize custom function
#' or to geom_histogram and geom_density functions from ggplot2 package.
#' @return a ggplot
#' @author Alboukadel Kassambara <alboukadel.kassambara@@gmail.com>
#' @seealso \code{\link{ggplot2.dotplot}, \link{ggplot2.violinplot},
#' \link{ggplot2.stripchart}, \link{ggplot2.boxplot}, \link{ggplot2.density}}
#' @references http://www.sthda.com
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' #data
#' set.seed(1234)
#' df <- data.frame(grp = factor( rep(c("A","B"), each=200) ), 
#'                    var = c(rnorm(200),rnorm(200, mean=.8)))
#' #plot
#' plot<-ggplot2.histogram(data=df, xName='var',groupName='grp', groupColors=c('#999999','#E69F00'))
#' plot<-ggplot2.customize(plot, mainTitle="Plot of variable histogram \nper group",
#'                         xtitle="Variable", ytitle="Histogram")
#' print(plot)
#' 
#' @export ggplot2.histogram
ggplot2.histogram<-function(data, xName=NULL, groupName=NULL, position=c("identity", "stack", "dodge"),
                            addMeanLine=FALSE, meanLineColor=NULL, meanLineType="dashed", meanLineSize=1,
                            addDensityCurve=FALSE, densityFill="#FF6666", densityAlpha=0.2, densityLineType="solid", densityLineColor="#2F2F2F",
                            scale=c("frequency", "density"),groupColors=NULL, brewerPalette=NULL,
                            fill = "black", color = "black", linetype = "solid", ...)
{
  
  pms <- .hist_params(...)
  alpha <- ifelse(is.null(groupName), 1, 0.5)
  
  #if xName is missing or null, data should be a numeric vector
  if(is.null(xName) & !is.numeric(data))
    stop("xName is missing or NULL. In this case data should be a numeric vector")
  #data is a numeric vector
  else if(is.numeric(data)){
    data=cbind(x=data, grp=rep(1, length(data)))
    xName="x"
  }
  
  #data
  data=data.frame(data)
  if(is.null(groupName))p<-ggplot(data=data, aes_string(x=xName))
  else {
    data[,groupName]=factor(data[,groupName])#transform groupName to factor
    p<-ggplot(data=data, aes_string(x=xName, fill=groupName, colour=groupName))
  }
  #add density curve
  if(addDensityCurve) {
    if(!is.null(groupName)) {
      p<-ggplot(data=data, aes_string(x=xName, fill=groupName, colour=groupName))
      p<-p+ geom_histogram(aes_string(y="..density.."),position=position[1], 
                           binwidth = pms$binwidth, bins = pms$bins, alpha = alpha) #hist with density	
      p<-p+geom_density(alpha=densityAlpha, linetype=densityLineType)
    }
    else{
      p<-p+ geom_histogram(aes_string(y="..density.."),position=position[1], 
                           binwidth = pms$binwidth, bins = pms$bins,
                           color = color, fill = fill, linetype = linetype) #hist with density	
      p<-p+geom_density(fill=densityFill, alpha=densityAlpha, 
                        linetype=densityLineType,colour=densityLineColor
                        )
    }
    
  }	
  else {
    if(scale[1]=="density") p<-p+ geom_histogram(aes_string(y="..density.."),
                                                 position=position[1],  binwidth = pms$binwidth,
                                                 bins = pms$bins) #hist with density
    else p<-p+geom_histogram(aes_string(y="..count.."), position=position[1], 
                             binwidth = pms$binwidth, bins = pms$bins, alpha = alpha
                             )#histogram with count
  }
  #add Mean line
  if(addMeanLine){		
    #cas d'une seule variable
    if(is.null(groupName)){
      if(is.null(meanLineColor)) meanLineColor='red'
      m=mean(data[,xName], na.rm=T)
      p<-p+geom_vline(aes_string(xintercept=m),
                      color=meanLineColor, linetype=meanLineType, size=meanLineSize)						
    }#end of if
    #cas de plusieurs group
    else {
      df<-data.frame(grp=factor(data[,groupName]), x=data[,xName])
      df.m <- stats::aggregate(df[, "x"], by = list(grp = df[, "grp"]), mean)
      # df.m= plyr::ddply(df, .(grp), summarise, x.mean=mean(x))
      names(df.m)<-c(groupName,'x.mean')	
      
      if(is.null(meanLineColor)) p<-p+geom_vline(data=df.m, 
                                                 aes_string(xintercept='x.mean', colour=groupName),
                                                 linetype=meanLineType, size=meanLineSize)
      
      else p<-p+geom_vline(data=df.m, aes_string(xintercept='x.mean'),
                           linetype=meanLineType, color=meanLineColor,size=meanLineSize)													
    }#end of else
    
  }#end of addMeanLine
  
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
