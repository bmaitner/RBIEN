#' BIEN: Tools for accessing the BIEN database.
#'
#' @description The Botanical Information and Ecology Network(BIEN) R package provides access to the BIEN database as well as useful tools for working with the BIEN data.
#' 
#' @section Getting started:
#' Type vignette("BIEN") to view the vignette, which contains useful information on the BIEN package.
#' 
#' @references Maitner BS, Boyle B, Casler N, et al. The bien r package: A tool to access the Botanical Information and Ecology Network (BIEN) Database. Methods Ecol Evol. 2018;9:373–379. https://doi.org/10.1111/2041-210X.12861
#' 
#' @docType package
#' @name BIEN
#' @aliases BIEN-package
NULL
###################


.onAttach <- function(libname,pkgname) {
  packageStartupMessage('Type vignette("BIEN") or vignette("BIEN_tutorial") to get started')
  
  
  suppressWarnings(x<-try(readLines("http://raw.github.com/bmaitner/RBIEN/master/BIEN/NOTES",warn = F),silent = T))
  if(class(x)=="character"){
    if(length(x)!=0){
      packageStartupMessage(x,appendLF = T)  
      
    }  
    
  }
  
  
}


#######################
