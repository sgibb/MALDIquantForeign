\name{importMzXml}
\alias{importMzXml}
\title{Import mzXML files}
\usage{
  importMzXml(path, ...)
}
\arguments{
  \item{path}{\code{character}, path to directory or file
  which should be read in.}

  \item{\ldots}{arguments to be passed to
  \code{\link[readMzXmlData]{readMzXmlFile}}.}
}
\value{
  a \code{list} of
  \code{\link[MALDIquant]{MassSpectrum-class}} objects.
}
\description{
  This function imports files in mzXML file format into
  \code{\link[MALDIquant]{MassSpectrum-class}} objects.
}
\examples{
library("MALDIquant")
library("MALDIquantForeign")

## get example directory
exampleDirectory <- system.file("tests/data", package="MALDIquantForeign")

## import
s <- importMzXml(exampleDirectory)
}
\author{
  Sebastian Gibb
}
\references{
  \url{http://strimmerlab.org/software/maldiquant/}, \cr
  Definition of \code{mzXML} format:
  \url{http://tools.proteomecenter.org/mzXMLschema.php}
}
\seealso{
  \code{\link[MALDIquant]{MassSpectrum-class}},
  \code{\link[readMzXmlData]{readMzXmlFile}}
}

