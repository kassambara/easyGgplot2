#' Put multiple graphs on the same page.
#' @description Put multiple graphs on the same page.
#' @param plotlist a list of plots
#' @param \dots List of ggplot2 objects separated by a comma. (e.g: plot1,
#' plot2, plot3)
#' @param cols Number of columns in layout
#' @return a ggplot
#' @author Alboukadel Kassambara <alboukadel.kassambara@@gmail.com>
#' @references http://www.sthda.com
#' @examples
#' 
#' #data.frame
#' df <- ToothGrowth
#' #Custom boxplot with centered dot plot
#' plot1<-ggplot2.boxplot(data=df, xName='dose',yName='len', groupName='dose',
#'                 addDot=TRUE, dotSize=1)
#' 
#' #Custom dotplot with centered dot plot
#' plot2<-ggplot2.dotplot(data=df, xName='dose',yName='len', groupName='dose')
#' 
#' #Custom stripchart with centered dot plot
#' plot3<-ggplot2.stripchart(data=df, xName='dose',yName='len', groupName='dose')
#' 
#' #Notched boxplot
#' plot4<-ggplot2.boxplot(data=df, xName='dose',yName='len',
#'                 notch=TRUE)
#' 
#' #Multple graphs on the same page
#' ggplot2.multiplot(plot1,plot2,plot3,plot4, cols=2)
#' 
#' @export ggplot2.multiplot
ggplot2.multiplot <- function(..., plotlist=NULL, cols=2) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    grid::viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}
