#' BIEN: Tools for accessing the BIEN database.
#'
#' @description The Botanical Information and Ecology Network(BIEN) R package provides access to the BIEN database as well as useful tools for working with the BIEN data.
#' 
#' @section Getting started:
#' Type vignette("BIEN") to view the vignette, which contains useful information on the BIEN package.
#' 
#' @references Enquist, B.J., Sandel, B., Boyle, B., Donoghue II, J.C., Regetz, J., Svenning, J.C., McGill, B.J., Peet, R.K., Jorgensen, P.M., Condit, R., Thiers, B., Schildhauer, M., Smith, S.A., Hinchliff, C.E., Wiser, S.K., Violle, C., Simova, I., Spencer, N., Dolins, S., Morueta-Holme, N., Marcuse-Kubitza, A., Kraft, N.J.B., Ott, J.E., Andelman, S., ter Steege, H., Phillips, O., Sloat, L.L., Narro, M.L., Casler, N., Guaderama, D.,  Merow, C., Maitner, B.S. (in prep) A general signature of taxonomic and phylogenetic diversity across the Land Plants of the New World.
#' 
#' @docType package
#' @name BIEN
#' @aliases BIEN-package
NULL
###################


.onAttach <- function(libname,pkgname) {
  packageStartupMessage('Type vignette("BIEN") to get started')
  
  
  suppressWarnings(x<-try(readLines("http://raw.github.com/bmaitner/RBIEN/master/BIEN/NOTES",warn = F),silent = T))
  if(class(x)=="character"){
    if(length(x)!=0){
      packageStartupMessage(x,appendLF = T)  
      
    }  
    
  }
  
  
}


#######################
