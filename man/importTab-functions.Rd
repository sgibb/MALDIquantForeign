\name{importTxt}
\alias{importCsv}
\alias{importCsv,AbstractMassObject-method}
\alias{importCsv,list-method}
\alias{importTab}
\alias{importTab,AbstractMassObject-method}
\alias{importTab,list-method}
\alias{importTxt}
\alias{importTxt,AbstractMassObject-method}
\alias{importTxt,list-method}
\title{Import text files.}
\usage{
  importTxt(path, ...)

  importTab(path, ...)

  importCsv(path, ...)
}
\arguments{
  \item{path}{\code{character}, path to directory or file
  which should be read in.}

  \item{\ldots}{arguments to be passed to
  \code{\link[utils]{read.table}}.}
}
\description{
  This function imports different text file formats into
  \code{\link[MALDIquant]{MassSpectrum-class}} objects.
}
\details{
  \code{importTab}, \code{importTxt} and \code{importCsv}
  use \code{\link[utils]{read.table}} with different
  defaults.
}
\examples{
library("MALDIquant")
library("MALDIquantForeign")

## get example directory
exampleDirectory <- system.file("tests/data", package="MALDIquantForeign")

## import txt files
s <- importTxt(exampleDirectory)

## import csv files
s <- importCsv(exampleDirectory)
}
\author{
  Sebastian Gibb
}
\references{
  \url{http://strimmerlab.org/software/maldiquant/}
}
\seealso{
  \code{\link[MALDIquant]{MassSpectrum-class}},
  \code{\link[utils]{read.table}}
}

