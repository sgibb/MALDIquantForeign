\name{.createMassObject}
\alias{.createMassObject}
\title{Create a MassSpectrum or a MassPeaks object.}
\usage{
.createMassObject(data, metaData = list(), centroided = NA,
  massRange = c(0, Inf), minIntensity = 0, verbose = FALSE)
}
\arguments{
  \item{data}{\code{list}, spectrum data with elements
  mass, intensity, snr}

  \item{metaData}{\code{list}, metaData}

  \item{centroided}{\code{logical}, centroided (if TRUE =>
  MassPeaks, if FALSE => MassSpectrum, if NA => try to
  guess)}

  \item{massRange}{\code{double}, length == 2, trim
  spectrum to \code{massRange}.}

  \item{minIntensity}{\code{double}, minimal intensity}

  \item{verbose}{\code{logical}, verbose output?}
}
\value{
Returns a MassSpectrum or a MassPeaks object.
}
\description{
This function creates a MassSpectrum or MassPeaks object
(depending on the centroided argument).
}
\author{
Sebastian Gibb \email{mail@sebastiangibb.de}
}
\keyword{internal}

