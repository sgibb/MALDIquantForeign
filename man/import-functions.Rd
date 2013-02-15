\name{import}
\alias{import}
\title{Import files}
\usage{
  import(path, type = "auto", pattern, verbose = FALSE,
    ...)
}
\arguments{
  \item{path}{\code{character}, path to directory or file
  which should be read in.}

  \item{type}{\code{character}, file format. If \code{type}
  is set to \dQuote{auto} the file extension (if
  \code{path} is a directory the most represented file
  extension) is used.}

  \item{pattern}{\code{character}, a regular expression to
  find files in a directory (see details).}

  \item{verbose}{\code{logical}, verbose output?}

  \item{\ldots}{arguments to be passed to specific import
  functions.}
}
\value{
  a \code{list} of
  \code{\link[MALDIquant]{MassSpectrum-class}} objects.
}
\description{
  This function provides a general interface to import
  different file formats into
  \code{\link[MALDIquant]{MassSpectrum-class}} objects.
}
\details{
  Specific import functions: \tabular{ll}{ txt \tab
  \code{\link[MALDIquantForeign]{importTxt}} \cr tab \tab
  \code{\link[MALDIquantForeign]{importTab}} \cr csv \tab
  \code{\link[MALDIquantForeign]{importCsv}} \cr fid \tab
  \code{\link[MALDIquantForeign]{importBrukerFlex}} \cr
  ciphergen \tab
  \code{\link[MALDIquantForeign]{importCiphergenXml}} \cr
  mzXML \tab \code{\link[MALDIquantForeign]{importMzXml}}
  \cr mzML \tab \code{\link[MALDIquantForeign]{importMzMl}}
  \cr }

  \code{pattern}: Sometimes unusual file extensions are
  used (e.g. \code{"*.xml"} for mzXML files). In this case
  a specific \code{pattern} could be defined to import
  files with an unusual file extension (e.g.
  \code{pattern="^.*\\.xml$"} to read all \code{*.xml}
  files in a directory; see \code{\link[base]{regexp}} for
  details).
}
\examples{
library("MALDIquant")
library("MALDIquantForeign")

## get example directory
exampleDirectory <- system.file(file.path("tests", "data"),
                                package="MALDIquantForeign")

## import mzXML files
s <- import(exampleDirectory, type="mzXML")

## import tab delimited file with different file extension (default: *.tab)
s <- import(exampleDirectory, type="tab", pattern="^.*\\\\.txt")

## import single mzML file
s <- import(file.path(exampleDirectory, "tiny1.mzML1.1.mzML"))
}
\author{
  Sebastian Gibb
}
\references{
  \url{http://strimmerlab.org/software/maldiquant/}
}
\seealso{
  \code{\link[MALDIquant]{MassSpectrum-class}}
}

