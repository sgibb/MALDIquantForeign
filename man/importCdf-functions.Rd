\name{importCdf}
\alias{importCdf}
\title{Import CDF files}
\usage{
importCdf(path, ...)
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
This function imports files in NetCDF file format into
\code{\link[MALDIquant]{MassSpectrum-class}} or
\code{\link[MALDIquant]{MassPeaks-class}} objects.\cr
Please note that the \emph{RNetCDF} is needed.
}
\examples{
library("MALDIquant")
library("MALDIquantForeign")

## get example directory
exampleDirectory <- system.file(file.path("tests", "data"),
                                package="MALDIquantForeign")

## import
s <- importCdf(exampleDirectory)
}
\author{
Sebastian Gibb
}
\references{
\url{http://strimmerlab.org/software/maldiquant/}
}
\seealso{
\code{\link[MALDIquant]{MassSpectrum-class}},
\code{\link[MALDIquant]{MassPeaks-class}}
}

