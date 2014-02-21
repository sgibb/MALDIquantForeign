\name{import}
\alias{import}
\title{Import files}
\usage{
import(path, type = "auto", pattern, excludePattern = NULL,
  removeEmptySpectra = TRUE, centroided = NA, massRange = c(0, Inf),
  minIntensity = 0, verbose = TRUE, ...)
}
\arguments{
  \item{path}{\code{character}, path to directory or file
  which should be read in.}

  \item{type}{\code{character}, file format. If \code{type}
  is set to \dQuote{auto} MALDIquant tries to detect the
  correct file type automatically. It often depends on the
  file extension (if \code{path} is a directory the most
  represented file extension is used; \code{pattern}
  argument is ignored).}

  \item{pattern}{\code{character}, a regular expression to
  find files in a directory (see details).}

  \item{excludePattern}{\code{character}, a regular
  expression to exclude files in a directory (see
  details).}

  \item{removeEmptySpectra}{\code{logical}, should empty
  spectra excluded?}

  \item{centroided}{\code{logical}, if
  \code{centroided=TRUE} a list of
  \code{\link[MALDIquant]{MassPeaks-class}} objects is
  returned otherwise a list of
  \code{\link[MALDIquant]{MassSpectrum-class}} objects. If
  \code{centroided=NA} \code{import} tries to guess the
  correct value.}

  \item{massRange}{\code{double}, limits of mass import
  (left/minimal mass, right/maximal mass).}

  \item{minIntensity}{\code{double}, minimal intensity to
  import.}

  \item{verbose}{\code{logical}, verbose output?}

  \item{\ldots}{arguments to be passed to specific import
  functions.}
}
\value{
a \code{list} of
\code{\link[MALDIquant]{MassSpectrum-class}} or
\code{\link[MALDIquant]{MassPeaks-class}} objects
(depending on the \code{centroided} argument).
}
\description{
This function provides a general interface to import
different file formats into
\code{\link[MALDIquant]{MassSpectrum-class}} or
\code{\link[MALDIquant]{MassPeaks-class}} objects.
}
\details{
Specific import functions: \tabular{ll}{ txt \tab
\code{\link[MALDIquantForeign]{importTxt}} \cr tab \tab
\code{\link[MALDIquantForeign]{importTab}} \cr csv \tab
\code{\link[MALDIquantForeign]{importCsv}} \cr fid \tab
\code{\link[MALDIquantForeign]{importBrukerFlex}} \cr
ciphergen \tab
\code{\link[MALDIquantForeign]{importCiphergenXml}} \cr
mzXML \tab \code{\link[MALDIquantForeign]{importMzXml}} \cr
mzML \tab \code{\link[MALDIquantForeign]{importMzMl}} \cr
imzML \tab \code{\link[MALDIquantForeign]{importImzMl}} \cr
analyze \tab \code{\link[MALDIquantForeign]{importAnalyze}}
\cr cdf \tab \code{\link[MALDIquantForeign]{importCdf}} \cr
}

\code{path}: In addition to the above mentioned file types
the following (compressed) archives are supported, too:
zip, tar, tar.gz, tar.bz2, tar.xz. The archives are
uncompressed in a temporary directory. Afterwards the
\code{\link[MALDIquantForeign]{import}} function is called
(with \code{type="auto"}).

\code{pattern}: Sometimes unusual file extensions are used
(e.g. \code{"*.xml"} for mzXML files). In this case a
specific \code{pattern} could be defined to import files
with an unusual file extension (e.g.
\code{pattern="^.*\\.xml$"} to read all \code{*.xml} files
in a directory; see \code{\link[base]{regexp}} for
details).

\code{excludePattern}: Sometimes some files should be
excluded. E.g. to ignore additional aquired Bruker LIFT
spectra (MALDI-TOF/TOF; which are not support, yet) you
could use
\code{excludePattern="([[:digit:]\\.]+)LIFT[\\\\/]1SRef"}.
}
\examples{
library("MALDIquant")
library("MALDIquantForeign")

## get example directory
exampleDirectory <- system.file("exampledata", package="MALDIquantForeign")

## import mzXML files
s <- import(exampleDirectory, type="mzXML")

## import tab delimited file with different file extension (default: *.tab)
s <- import(exampleDirectory, type="tab", pattern="^.*\\\\.txt")

## import single mzML file
s <- import(file.path(exampleDirectory, "tiny1.mzML1.1.mzML"))

## import gzipped csv file
s <- import(file.path(exampleDirectory, "compressed", "csv1.csv.gz"))
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

