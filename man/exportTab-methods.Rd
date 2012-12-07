\name{exportTab}
\alias{exportCsv}
\alias{exportCsv,AbstractMassObject-method}
\alias{exportCsv,list-method}
\alias{exportTab}
\alias{exportTab,AbstractMassObject-method}
\alias{exportTab,list-method}
\title{Export to text files.}
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

  \item{force}{\code{logical}, If \code{TRUE} the
  \code{file} would be overwritten or \code{path} would be
  created.}

  \item{\ldots}{arguments to be passed to
  \code{\link[utils]{write.table}}.}
}
\description{
  This function exports
  \code{\link[MALDIquant]{AbstractMassObject-class}}
  objects (e.g.
  \code{\link[MALDIquant]{MassSpectrum-class}},
  \code{\link[MALDIquant]{MassPeaks-class}}) into different
  text file formats.
}
\details{
  \code{exportTab} and \code{exportCsv} use
  \code{\link[utils]{write.table}} with different defaults
  (\code{sep="\t"} in \code{exportTab} and \code{sep=",")}
  in \code{exportCsv}).
}
\examples{
\dontrun{
library("MALDIquant")
library("MALDIquantForeign")

s <- list(createMassSpectrum(mass=1:5, intensity=1:5),
          createMassSpectrum(mass=1:5, intensity=1:5))

## export a single spectrum
exportTab(s[[1]], file="~/spectrum.tab")

## export a list of spectra
exportCsv(s, path="~/spectra", sep=";")
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
  \code{\link[MALDIquant]{MassSpectrum-class}},
  \code{\link[utils]{write.table}}
}

