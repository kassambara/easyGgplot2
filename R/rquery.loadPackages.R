#' Installation and loading of packages
#' 
#' @description
#' Missing packages are automatically installed and loaded.
#' 
#' @param pkgs The packages to load.
#' @author Alboukadel Kassambara <alboukadel.kassambara@@gmail.com>
#' @references http://www.sthda.com
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' rquery.loadPackages(c("ggplot2", "plyr"))
#' 
#' @export rquery.loadPackages
rquery.loadPackages <- function(pkgs) {
  # install packages not already loaded:
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    cat('==================================================\n')
    cat('some required packages shown below are missed:\n')
    cat(pkgs_miss)
    cat('\nWait few minutes for installation. Internet connexion are required.\n')
    cat('==================================================\n')
    
    source("http://bioconductor.org/biocLite.R")
    all_repos=c('http://cran.r-project.org', biocinstallRepos())
    install.packages(pkgs_miss, repos=all_repos)
  }
  
  # load packages not already loaded:
  attached <- search()
  attached_pkgs <- attached[grepl("package", attached)]
  need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
  
  if (length(need_to_attach) > 0) {
    for (i in 1:length(need_to_attach)) 
      suppressPackageStartupMessages(require(need_to_attach[i], 
                                             character.only = TRUE, quietly=T, warn.conflicts=F))
  }
  
}
