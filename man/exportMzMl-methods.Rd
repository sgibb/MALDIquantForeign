\name{exportMzMl,MassSpectrum-method}
\alias{exportMzMl}
\alias{exportMzMl,list-method}
\alias{exportMzMl,MassSpectrum-method}
\title{Export to mzML files}
\usage{
  \S4method{exportMzMl}{MassSpectrum}(x, file, force=FALSE,
    \ldots)

  \S4method{exportMzMl}{list}(x, path, force=FALSE, \ldots)
}
\arguments{
  \item{x}{a \code{\link[MALDIquant]{MassSpectrum-class}}
  object or a \code{list} of
  \code{\link[MALDIquant]{MassSpectrum-class}} objects.}

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
  \code{\link[MALDIquant]{MassPeaks-class}}) into mzML
  files.
}
\examples{
\dontrun{
library("MALDIquant")
library("MALDIquantForeign")

s <- list(createMassSpectrum(mass=1:5, intensity=1:5),
          createMassSpectrum(mass=1:5, intensity=1:5))

## export a single spectrum
exportMzMl(s[[1]], file="spectrum.mzML")

## export a list of spectra with corresponding peaks
exportMzMl(s, path="spectra.mzMl")
}
}
\author{
  Sebastian Gibb
}
\references{
  \url{http://strimmerlab.org/software/maldiquant/}, \cr
  HUPO Proteomics Standards Inititative mzML 1.1.0
  Specification: \url{http://www.psidev.info/mzml_1_0_0}
}
\seealso{
  \code{\link[MALDIquant]{MassPeaks-class}},
  \code{\link[MALDIquant]{MassSpectrum-class}}
}

