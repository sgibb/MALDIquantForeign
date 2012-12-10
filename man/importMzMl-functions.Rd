\name{importMzMl}
\alias{importMzMl}
\title{Import mzML files}
\usage{
  importMzMl(path, verbose = FALSE)
}
\arguments{
  \item{path}{\code{character}, path to directory or file
  which should be read in.}

  \item{verbose}{\code{logical}, verbose output?}
}
\value{
  a \code{list} of
  \code{\link[MALDIquant]{MassSpectrum-class}} objects.
}
\description{
  This function imports files in mzML file format into
  \code{\link[MALDIquant]{MassSpectrum-class}} objects.
}
\examples{
library("MALDIquant")
library("MALDIquantForeign")

## get example directory
exampleDirectory <- system.file(file.path("tests", "data"),
                                package="MALDIquantForeign")

## import
s <- importMzMl(exampleDirectory)
}
\author{
  Sebastian Gibb
}
\references{
  \url{http://strimmerlab.org/software/maldiquant/}, \cr
  Definition of \code{mzML} format:
  \url{http://www.psidev.info/mzml_1_0_0\%20}
}
\seealso{
  \code{\link[MALDIquant]{MassSpectrum-class}},
  \code{\link[readMzXmlData]{readMzXmlFile}}
}

