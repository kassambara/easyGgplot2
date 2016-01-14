#' @include utilities.R ggplot2.customize.R
NULL
#' Easy density plot with R package ggplot2
#'
#' @param data data.frame or a numeric vector. Columns are variables and rows
#' are observations.
#' @param xName The name of column containing x variable (i.e groups). Default
#' value is NULL.
#' @param groupName The name of column containing group variable. This variable
#' is used to color plot according to the group.
#' @param addMeanLine if TRUE, the mean line is added on the plot for each
#' group. Default value is FALSE.
#' @param meanLineColor,meanLineType,meanLineSize mean line color, type and
#' size.
#' @param densityFill Fill color of density plot. This is only considered when
#' groupName=NULL.
#' @param fillGroupDensity If TRUE, density curve of each group is filled.
#' Default value is FALSE.
#' @param colorGroupDensityLine If TRUE, density curve line are colored.
#' Default value is FALSE.
#' @param groupColors Color of groups. groupColors should have the same length
#' as groups.
#' @param brewerPalette This can be also used to indicate group colors. In this
#' case the parameter groupColors should be NULL. e.g: brewerPalette="Paired".
#' @param \dots Other arguments passed on to ggplot2.customize custom function
#' or to geom_density functions from ggplot2 package.
#' @return a ggplot
#' @author Alboukadel Kassambara <alboukadel.kassambara@@gmail.com>
#' @seealso \code{\link{ggplot2.dotplot}, \link{ggplot2.violinplot},
#' \link{ggplot2.stripchart}, \link{ggplot2.boxplot}, \link{ggplot2.histogram}}
#' @references http://www.sthda.com
#' @keywords ~kwd1 ~kwd2
#' @examples
#'
#' #data
#' set.seed(1234)
#' df <- data.frame(grp = factor( rep(c("A","B"), each=200) ),
#'                    var = c(rnorm(200),rnorm(200, mean=.8)))
#'
#' #plot
#' ggplot2.density(data=df, xName='var',groupName='grp', groupColors=c('#999999','#E69F00'),
#'                 mainTitle="Plot of variable histogram \nper group",
#'                 xtitle="Variable", ytitle="Density")
#'
#' #Or use this
#' plot<-ggplot2.density(data=df, xName='var',groupName='grp', groupColors=c('#999999','#E69F00'))
#' plot<-ggplot2.customize(plot, mainTitle="Plot of variable histogram \nper group",
#'                         xtitle="Variable", ytitle="Density")
#' print(plot)
#'
#' @export ggplot2.density
ggplot2.density <- function(data, xName = NULL, groupName = NULL,
                            addMeanLine = FALSE, meanLineColor = NULL, meanLineType =
                              "dashed", meanLineSize = 1,
                            densityFill = NULL,fillGroupDensity = FALSE, colorGroupDensityLine =
                              FALSE,
                            groupColors = NULL, brewerPalette = NULL,  ...)
{
  spms <- .standard_params(...)
  #if xName is missing or null, data should be a numeric vector
  if (is.null(xName) & !is.numeric(data))
    stop("xName is missing or NULL. In this case data should be a numeric vector")
  #data is a numeric vector
  else if (is.numeric(data)) {
    data = cbind(x = data, grp = rep(1, length(data)))
    xName = "x"
  }
  
  #data
  data = data.frame(data)
  if (is.null(groupName)) {
    p <- ggplot(data = data, aes_string(x = xName))
    if (!is.null(densityFill) &&
        densityFill != '')
      p <-
        p + geom_density(fill = densityFill, linetype = spms$linetype, colour = spms$color)
    else
      p <- p + geom_density( linetype = spms$linetype, colour = spms$color)
  }
  else {
    #transform groupName to factor
    data[,groupName] = factor(data[,groupName])
    #remplissage de la courbe
    if (fillGroupDensity == TRUE) {
      #coloration des ligne
      if (colorGroupDensityLine)
        p <- ggplot(data = data, aes_string(
          x = xName, fill = groupName, colour = groupName
        ))
      else
        p <- ggplot(data = data, aes_string(x = xName, fill = groupName))
    }
    else
      p <- ggplot(data = data, aes_string(x = xName, colour = groupName))
    p <- p + geom_density(alpha = 0.5)
  }
  #add Mean line
  if (addMeanLine) {
    #cas d'une seule variable
    if (is.null(groupName)) {
      if (is.null(meanLineColor))
        meanLineColor = 'red'
      m = mean(data[,xName], na.rm = T)
      p <- p + geom_vline(
        aes_string(xintercept = m),
        color = meanLineColor, linetype = meanLineType,
        size = meanLineSize
      )
    }#end of if
    #cas de plusieurs group
    else {
      df <- data.frame(grp = factor(data[,groupName]), x = data[,xName])
      df.m <- stats::aggregate(df[, "x"], by = list(grp = df[, "grp"]), mean)
      # df.m = plyr::ddply(df, .(grp), summarise, x.mean = mean(x))
      names(df.m) <- c(groupName,'x.mean')
      if (is.null(meanLineColor))
        p <-
        p + geom_vline(
          data = df.m, aes_string(xintercept = 'x.mean', colour = groupName),
          linetype = meanLineType, size =
            meanLineSize
        )
      else
        p <-
        p + geom_vline(
          data = df.m, aes_string(xintercept = 'x.mean', colour = groupName),
          linetype = meanLineType, color = meanLineColor,size =
            meanLineSize
        )
    }#end of else
  }#end of addMeanLine
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
