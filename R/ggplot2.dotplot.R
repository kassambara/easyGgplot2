#' @include utilities.R ggplot2.customize.R
NULL
#' Easy dotplot plot with R package ggplot2
#'
#' @param data data.frame or a numeric vector. Columns are variables and rows
#' are observations.
#' @param xName The name of column containing x variable (i.e groups). Default
#' value is NULL.
#' @param yName The name of column containing y variable. If yName=NULL, data
#' should be a numeric vector.
#' @param groupName The name of column containing group variable. This variable
#' is used to color plot according to the group.
#' @param position The position adjustment to use for overlappling points.
#' Default value is position_dodge(0.8).
#' @param addMean if TRUE, the mean point is added on the plot for each group.
#' Default value is FALSE.
#' @param meanPointShape The shape of mean point.
#' @param meanPointSize The size of mean point
#' @param meanPointColor Border color of the mean point. Default value is
#' "black".
#' @param meanPointFill Fill color of mean point. This parameter is used only
#' when meanPointShape=21 to 25. Default value is "blue"
#' @param addBoxplot If TRUE, boxplot is added on the dotplot. Default value is
#' FALSE.
#' @param boxplotFill Fill color of the boxplot. Default value is white.
#' @param boxplotColor Boxplot line color. Default value is black.
#' @param boxplotLineWeight Boxplot line weight. Default value is 0.5.
#' @param groupColors Color of groups. groupColors should have the same length
#' as groups.
#' @param brewerPalette This can be also used to indicate group colors. In this
#' case the parameter groupColors should be NULL. e.g: brewerPalette="Paired".
#' @param \dots Other arguments passed on to ggplot2.customize custom function
#' or to geom_dotplot functions from ggplot2 package.
#' @return a ggplot
#' @author Alboukadel Kassambara <alboukadel.kassambara@@gmail.com>
#' @seealso \code{\link{ggplot2.boxplot}, \link{ggplot2.violinplot},
#' \link{ggplot2.stripchart}, \link{ggplot2.density}, \link{ggplot2.histogram},
#' \link{ggplot2.customize}}
#' @references http://www.sthda.com
#' @examples
#'
#' df <- ToothGrowth
#' ggplot2.dotplot(data=df, xName='dose',yName='len',
#'                 mainTitle="Plot of length according\n to the dose",
#'                 xtitle="Dose (mg)", ytitle="Length")
#'
#' #Or use this
#' plot<-ggplot2.dotplot(data=df, xName='dose',yName='len')
#' plot<-ggplot2.customize(plot, mainTitle="Plot of length according\n to the dose",
#'                         xtitle="Dose (mg)", ytitle="Length")
#' print(plot)
#'
#' @export ggplot2.dotplot
ggplot2.dotplot <-
  function(data, xName = NULL, yName = NULL, groupName = NULL,
           position = position_dodge(0.8),
           addMean = FALSE, meanPointShape = 5, meanPointSize =
             4,
           meanPointColor = "black", meanPointFill = "blue",
           addBoxplot = FALSE, boxplotFill = "white", boxplotColor =
             "black",	boxplotLineWeight = 0.5,
           groupColors = NULL, brewerPalette = NULL,...)
  {
    pms <- .dotplot_params(...)
    
    #if yName is missing or null, data should be a numeric vector
    if (is.null(yName) & !is.numeric(data))
      stop("yName is missing or NULL. In this case data should be a numeric vector")
    #data is a numeric vector
    else if (is.numeric(data)) {
      data = cbind(y = data, x = rep(1, length(data)))
      xName = "x"
      yName = "y"
    }
    #xName is missing or  NULL => single boxplot corresponding to the deistribution of the variable
    #bind group column to data
    if (is.null(xName)) {
      data = cbind(data, x = rep(1, nrow(data)))
      xName = "x"
    }
    
    #data
    data = data.frame(data)
    data[,xName] = factor(data[,xName])
    if (is.null(groupName))
      p <- ggplot(data = data, aes_string(x = xName, y = yName))
    else {
      data[,groupName] = factor(data[,groupName])#transform groupName to factor
      p <-
        ggplot(data = data, aes_string(x = xName, y = yName, fill = groupName))
    }
    
    #add boxplot
    if (addBoxplot) {
      if (is.null(boxplotFill))
        p <-
          p + geom_boxplot(
            colour = boxplotColor,  position = position_dodge(0.8),
            size = boxplotLineWeight,  outlier.shape = NA
          )
      else
        p <- p + geom_boxplot(
          fill = boxplotFill, colour = boxplotColor,
          position = position_dodge(0.8),size = boxplotLineWeight,
          outlier.shape = NA
        )
    }
    #dotplot
    p <-
      p + geom_dotplot(
        binaxis = "y", stackdir = "center", position = position,
        width = pms$width, stackratio = pms$stackratio
      )
    #add Mean point
    if (addMean)
      p <- p + stat_summary(
        fun.y = mean, geom = 'point', shape = meanPointShape,
        size = meanPointSize, colour = meanPointColor, fill =
          meanPointFill
      )
    #group colors
    if (!is.null(groupColors)) {
      p <- p + scale_fill_manual(values = groupColors)
      p <- p + scale_colour_manual(values = groupColors)
    }
    else if (!is.null(brewerPalette)) {
      p <- p + scale_fill_brewer(palette = brewerPalette)
      p <- p + scale_colour_brewer(palette = brewerPalette, guide = "none")
    }
    
    #ggplot2.customize : titles, colors, background, legend, ....
    p <- ggplot2.customize(p,...)
    p
  }
