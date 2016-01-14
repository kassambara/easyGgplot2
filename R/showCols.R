#' Generate a plot of color names which R knows about.
#' @description Generate R color names.
#' @param cl A vector of colors to plot. Default value is the list of color
#' names returned by R color() function.
#' @param bg Background color of the plot
#' @param cex The size of the text.
#' @param rot The rotation angle of the text
#' @return a ggplot
#' @author Alboukadel Kassambara <alboukadel.kassambara@@gmail.com>
#' @seealso \code{\link{generateRLineTypes}, \link{generateRPointShapes}}
#' @references http://www.sthda.com
#' @examples
#' 
#' col=colors() #list of color names
#' showCols(col[1:100]) #plot the first 100 colors
#' 
#' @export showCols
showCols <- function(cl=colors(), bg = "grey", cex = 0.75, rot = 30) {
  m <- ceiling(sqrt(n <-length(cl)))
  length(cl) <- m*m; cm <- matrix(cl, m)
  ##
  grid::grid.newpage()
  vp <- grid::viewport(w = .92, h = .92)
  grid::grid.rect(gp=grid::gpar(fill=bg))
  grid::grid.text(cm, x = col(cm)/m, y = rev(row(cm))/m, rot = rot,
            vp=vp, gp=grid::gpar(cex = cex, col = cm))
}
