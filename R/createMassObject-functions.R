## Copyright 2014 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This file is part of MALDIquantForeign for R and related languages.
##
## MALDIquantForeign is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## MALDIquantForeign is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with MALDIquantForeign. If not, see <http://www.gnu.org/licenses/>

#' Create a MassSpectrum or a MassPeaks object.
#'
#' This function creates a MassSpectrum or MassPeaks object (depending on the
#' centroided argument).
#'
#' @param data \code{list}, spectrum data with elements mass, intensity, snr
#' @param metaData \code{list}, metaData
#' @param centroided \code{logical}, centroided
#'  (if TRUE => MassPeaks, if FALSE => MassSpectrum, if NA => try to guess)
#' @param massRange \code{double}, length == 2, trim spectrum to
#'  \code{massRange}.
#' @param minIntensity \code{double}, minimal intensity
#'
#' @return Returns a MassSpectrum or a MassPeaks object.
#'
#' @author Sebastian Gibb \email{mail@@sebastiangibb.de}
#' @rdname createMassObject
#' @keywords internal
#'
.createMassObject <- function(data, metaData=list(),
                              centroided=NA,
                              massRange=c(0, Inf),
                              minIntensity=0) {
  if (is.na(centroided)) {
    if (!is.null(metaData$centroided)) {
      centroided <- as.logical(as.numeric(metaData$centroided))
    } else if (!is.null(metaData$dataProcessing$centroided)) {
      centroided <- as.logical(as.numeric(metaData$dataProcessing$centroided))
    } else {
      centroided <- .isCentroided(data$mass)
    }
  }

  if (centroided) {
    if (is.null(data$snr)) {
      m <- createMassPeaks(mass=data$mass, intensity=data$intensity,
                           metaData=metaData)
    } else {
      m <- createMassPeaks(mass=data$mass, intensity=data$intensity,
                           snr=data$snr, metaData=metaData)
    }
  } else {
    m <- createMassSpectrum(mass=data$mass, intensity=data$intensity,
                            metaData=metaData)
  }

  ## trim AbstractMass object
  m <- trim(m, massRange)
  m <- m[which(m@intensity >= minIntensity)]

  return(m)
}

## use MALDIquant's irregularScore to guess centroided/profile
.isCentroided <- function(mass, threshold=1e-3) {
  return(MALDIquant:::.irregularScore(mass) > threshold)
}

