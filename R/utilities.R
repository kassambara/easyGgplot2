#' @import ggplot2
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Official argument from ggplot2
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# bar plot arguments
.barplot_params <- function(...){
  x <- list(...)
  res <- list()
  res$width  <- x$width
  res$binwidth  <- x$binwidth
  res$na.rm  <- ifelse(!is.null(x$na.rm), x$na.rm, FALSE)
  res$show.legend <- ifelse(!is.null(x$show.legend), x$show.legend, NA)
  res$inherit.aes <- ifelse(!is.null(x$inherit.aes), x$inherit.aes, TRUE)
  return(res)
}

# box plot arguments
.boxplot_params <- function(...){
  x <- list(...)
  res <- list()
  res$outlier.colour  <- x$outlier.colour
  res$outlier.shape  <- ifelse(!is.null(x$outlier.shape), x$outlier.shape, 19)
  res$outlier.size  <- ifelse(!is.null(x$outlier.size), x$outlier.size, 1.5)
  res$outlier.stroke  <- ifelse(!is.null(x$outlier.stroke), x$outlier.stroke, 0.5)
  res$notch  <- ifelse(!is.null(x$notch), x$notch, FALSE)
  res$notchwidth  <- ifelse(!is.null(x$notchwidth), x$notchwidth, 0.5)
  res$varwidth  <- ifelse(!is.null(x$varwidth), x$varwidth, FALSE)
  res$na.rm  <- ifelse(!is.null(x$na.rm), x$na.rm, FALSE)
  res$show.legend <- ifelse(!is.null(x$show.legend), x$show.legend, NA)
  res$inherit.aes <- ifelse(!is.null(x$inherit.aes), x$inherit.aes, TRUE)
  return(res)
}

.dotplot_params <- function(...){
  x <- list(...)
  res <- list()
  res$stackratio  <- ifelse(!is.null(x$stackratio ), x$stackratio, 1)
  res$width <- ifelse(!is.null(x$width), x$width, 0.9)
  return(res)
}

.hist_params <- function(...){
  x <- list(...)
  res <- list()
  res$binwidth <- x$binwidth
  res$bins <- x$bins
  return(res)
}

.standard_params <- function(...){
  x <- list(...)
  res <- list()
  res$color <- ifelse(!is.null(x$color), x$color, "black")
  res$color <- ifelse(!is.null(x$colour), x$colour, res$color)
  res$linetype <- ifelse(!is.null(x$linetype), x$linetype, "solid")
  res$size <- ifelse(!is.null(x$size), x$size, 1)
  res$fill <- ifelse(!is.null(x$fill), x$fill, "black")
  res$shape <- ifelse(!is.null(x$shape), x$shape, 19)
  res
}

