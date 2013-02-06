\name{export,AbstractMassObject-method}
\alias{export}
\alias{export,AbstractMassObject-method}
\alias{export,list-method}
\title{Export files}
\usage{
  \S4method{export}{AbstractMassObject}(x, file,
    type="auto", force=FALSE, \ldots)

  \S4method{export}{list}(x, path, type, force=FALSE,
    \ldots)
}
\arguments{
  \item{x}{a
  \code{\link[MALDIquant]{AbstractMassObject-class}} object
  or a \code{list} of
  \code{\link[MALDIquant]{AbstractMassObject-class}}
  objects.}

  \item{file}{\code{character}, file name.}

  \item{path}{\code{character}, path to directory in which
  the \code{list} of
  \code{\link[MALDIquant]{AbstractMassObject-class}} would
  be exported.}

  \item{type}{\code{character}, file format. If \code{type}
  is set to \dQuote{auto} the file extension is used.}

  \item{force}{\code{logical}, If \code{TRUE} the
  \code{file} would be overwritten or \code{path} would be
  created.}

  \item{\ldots}{arguments to be passed to specific export
  functions.}
}
\description{
  This function provides a general interface to export
  \code{\link[MALDIquant]{AbstractMassObject-class}}
  objects (e.g.
  \code{\link[MALDIquant]{MassSpectrum-class}},
  \code{\link[MALDIquant]{MassPeaks-class}}) into different
  file formats.
}
\details{
  Specific export functions: \tabular{ll}{ tab \tab
  \code{\link[MALDIquantForeign]{exportTab}} \cr csv \tab
  \code{\link[MALDIquantForeign]{exportCsv}} \cr msd \tab
  \code{\link[MALDIquantForeign]{exportMsd}} \cr }
}
\examples{
\dontrun{
library("MALDIquant")
library("MALDIquantForeign")

s <- list(createMassSpectrum(mass=1:5, intensity=1:5),
          createMassSpectrum(mass=1:5, intensity=1:5))

## export a single spectrum
export(s[[1]], file="spectrum.csv")
## identical to exportCsv(s[[1]], file="spectrum.csv")

## export a list of spectra
export(s, path="spectra", type="csv")
## identical to exportCsv(s, path="spectra")
}
}
\author{
  Sebastian Gibb
}
\references{
  \url{http://strimmerlab.org/software/maldiquant/}
}
\seealso{
  \code{\link[MALDIquant]{MassPeaks-class}},
  \code{\link[MALDIquant]{MassSpectrum-class}}
}

