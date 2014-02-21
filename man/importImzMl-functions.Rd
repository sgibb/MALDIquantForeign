\name{importImzMl}
\alias{importImzMl}
\title{Import imzML files}
\usage{
importImzMl(path, ...)
}
\arguments{
  \item{path}{\code{character}, path to directory or file
  which should be read in.}

  \item{\ldots}{arguments to be passed to
  \code{\link[MALDIquantForeign]{import}}.}
}
\value{
a \code{list} of
\code{\link[MALDIquant]{MassSpectrum-class}} or
\code{\link[MALDIquant]{MassPeaks-class}} objects
(depending on the \code{centroided} argument).
}
\description{
This function imports files in imzML file format into
\code{\link[MALDIquant]{MassSpectrum-class}} or
\code{\link[MALDIquant]{MassPeaks-class}} objects.
}
\examples{
library("MALDIquant")
library("MALDIquantForeign")

## get example directory
exampleDirectory <- system.file(file.path("tests", "data"),
                                package="MALDIquantForeign")

## import
s <- importImzMl(exampleDirectory)
}
\author{
Sebastian Gibb
}
\references{
\url{http://strimmerlab.org/software/maldiquant/}, \cr
Definition of \code{imzML} format:
\url{http://www.imzml.org/}
}
\seealso{
\code{\link[MALDIquant]{MassSpectrum-class}},
\code{\link[MALDIquant]{MassPeaks-class}}
}

