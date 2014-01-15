\name{.createMassObject}
\alias{.createMassObject}
\title{Create a MassSpectrum or a MassPeaks object.}
\usage{
.createMassObject(data, metaData, centroided = NA)
}
\arguments{
  \item{data}{\code{list}, spectrum data with elements
  mass, intensity, snr}

  \item{metaData}{\code{list}, metaData}

  \item{centroided}{\code{logical}, centroided (if TRUE =>
  MassPeaks, if FALSE => MassSpectrum, if NA => try to
  guess)}
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

