\name{export}
\alias{export}
\alias{export,AbstractMassObject-methods}
\alias{export,list-methods}
\title{Export}
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

  \item{\ldots}{arguments to be passed to other export
  functions (see
  \code{\link[MALDIquantForeign]{exportTab,AbstractMassObject-methods}},
  \code{\link[MALDIquantForeign]{exportCsv,AbstractMassObject-methods}},
  \code{\link[MALDIquantForeign]{exportMsd,AbstractMassObject-methods}}).}
}
\description{
  This function provides a general interface to export
  \code{\link[MALDIquant]{AbstractMassObject-class}} (e.g.
  \code{\link[MALDIquant]{MassSpectrum-class}},
  \code{\link[MALDIquant]{MassPeaks-class}}) into different
  file formats.
}
\examples{
\dontrun{
library("MALDIquant")
library("MALDIquantForeign")

s <- createMassSpectrum(mass=1:5, intensity=1:5)

export(s, file="~/spectrum.csv")
## identical to exportCsv(s, file="~/spectrum.csv")
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

