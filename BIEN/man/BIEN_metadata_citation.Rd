% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BIEN.R
\name{BIEN_metadata_citation}
\alias{BIEN_metadata_citation}
\title{Generate citations for data extracted from BIEN.}
\usage{
BIEN_metadata_citation(
  dataframe = NULL,
  trait.dataframe = NULL,
  trait.mean.dataframe = NULL,
  bibtex_file = NULL,
  acknowledgement_file = NULL,
  ...
)
}
\arguments{
\item{dataframe}{A data.frame of occurrence data downloaded from the BIEN R package.}

\item{trait.dataframe}{A data.frame of trait data downloaded from the BIEN R package.}

\item{trait.mean.dataframe}{A data.frame of species mean trait data from the function BIEN_trait_mean.}

\item{bibtex_file}{Output file for writing bibtex citations.}

\item{acknowledgement_file}{Output file for writing acknowledgements.}

\item{...}{Additional arguments passed to internal functions.}
}
\value{
A list object containing information needed for data attribution.  Full information for herbaria is available at http://sweetgum.nybg.org/science/ih/
}
\description{
BIEN_metadata_citation guides a user through the proper documentation for data downloaded from the BIEN database.
}
\examples{
\dontrun{
BIEN_metadata_citation()#If you are referencing the phylogeny or range maps.
Xanthium_data<-BIEN_occurrence_species("Xanthium strumarium")
citations<-BIEN_metadata_citation(dataframe=Xanthium_data)#If you are referencing occurrence data}
}
\seealso{
Other metadata functions: 
\code{\link{BIEN_metadata_database_version}()},
\code{\link{BIEN_metadata_list_political_names}()},
\code{\link{BIEN_metadata_match_data}()},
\code{\link{BIEN_plot_metadata}()},
\code{\link{BIEN_ranges_list}()}
}
\concept{metadata functions}
