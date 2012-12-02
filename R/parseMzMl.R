## Copyright 2012 Sebastian Gibb
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

#' Parse mzML files.
#'
#' This function parses mzML files.
#'
#' @param file \code{character}, path to mzML file
#' @param verbose \code{logical}, verbose output?
#'
#' @return Returns a list with metadata and spectra.
#'
#' @author Sebastian Gibb \email{mail@@sebastiangibb.de}
#' @rdname parseMzMl
#' @keywords internal
#'
.parseMzMl <- function(file, verbose=FALSE, ...) {
  return(XML::xmlEventParse(file=file,
                            handlers=.mzMlHandlers(fileName=file,
                                                   verbose=verbose),
                            addContext=FALSE, useTagName=TRUE, useDotNames=TRUE,
                            ...)$getData())
}

#' Parse mzML files.
#'
#' This function is defines handlers for XML SAX parser. Internal use only.
#'
#' TODO: fetch obo map file and use it
#'
#' @param fileName \code{character}, path to mzML file
#' @param verbose \code{logical}, verbose output?
#'
#' @return function closure
#'
#' @author Sebastian Gibb \email{mail@@sebastiangibb.de}
#' @references 
#' Definition of \code{mzML} format:
#' \url{http://www.psidev.info/mzml_1_0_0}
#' @rdname mzMlHandlers
#' @keywords internal
#'
.mzMlHandlers <- function(fileName, verbose=FALSE) {
  ## define local variables

  ## handle different mzXML versions
  mzMlVersion <- 0 

  ## save last opened tag (needed for .text()-processing)
  openTag <- ""

  ## store current scan values
  nSpectra <- 0
  currentSpectrumId <- 0
  precision <- 0
  compressionType <- "none"
  currentArray <- "mass"
  currentArrayContent <- character()
  fileCheckSum <- character()

  ## supported?
  supported <- TRUE
 
  ## build final list
  xml <- list()
  xml$metaData <- list()
  xml$spectra <- list()

  ## handlers for specific tags
  ## mzML
  mzML <- function(name, attrs) {
    ## fetch version information
    mzMlVersion <<- readMzXmlData:::.grepDouble(attrs["xsi:schemaLocation"])

    if (verbose) {
      message("Found mzML document (version: ", mzMlVersion, ").")
    }
  }

  ## mzML/run/spectrumList
  spectrumList <- function(name, attrs) {
    ## fetch number of spectra
    nSpectra <<- readMzXmlData:::.attributeToDouble(attrs, "count", 
                                                    required=TRUE)

    if (verbose) {
      message("Found ", nSpectra, " ",
              ifelse(nSpectra == 1, "spectrum", "spectra"), ".")
    }
  }

  ## mzML/run/spectrumList/spectrum
  spectrum <- function(name, attrs) {
    currentSpectrumId <<- currentSpectrumId + 1

    supported <<- TRUE

    xml$spectra[[currentSpectrumId]] <<- list()
    xml$spectra[[currentSpectrumId]]$metaData <<- list()

    xml$spectra[[currentSpectrumId]]$metaData[["id"]] <<-
      readMzXmlData:::.attributeToString(attrs, "id", required=TRUE)

    xml$spectra[[currentSpectrumId]]$metaData[["numberInFile"]] <<-
      currentSpectrumId

    if (verbose) {
      message("Processing spectrum ", currentSpectrumId, "/", nSpectra,
              " (id: ", attrs["id"], ") ...")
    }
  }

  chromatogram <- function(name, attrs) {
    warning("<chromatogram> tag is not supported!")
    supported <<- FALSE
  }

  ## *cvParam
  cvParam <- function(name, attrs) {
    ## mzML/run/spectrumList/spectrum - children
    ## ms level
    if (.isAttrSet(attrs, "MS:1000129", "negative scan")) {
      if (currentSpectrumId == 0) {
        xml$metaData[["polarity"]] <<- "negative"
      } else {
        xml$spectra[[currentSpectrumId]]$metaData[["polarity"]] <<- "negative"
      }
      return()
    } else if (.isAttrSet(attrs, "MS:1000130", "positive scan")) {
      if (currentSpectrumId == 0) {
        xml$metaData[["polarity"]] <<- "positive"
      } else {
        xml$spectra[[currentSpectrumId]]$metaData[["polarity"]] <<- "positive"
      }
      return()
    }

    if (.isAttrSet(attrs, "MS:1000511",  "ms level")) {
      xml$spectra[[currentSpectrumId]]$metaData[["msLevel"]] <<-
        readMzXmlData:::.attributeToDouble(attrs, "value", required=TRUE)
      return()
    }
    
    if (.isAttrSet(attrs, "MS:1000127", "centroid spectrum")) {
      warning("centroid data are not supported!")
    }

    ## mzML/run/spectrumList/spectrum/binaryDataArrayList/binaryData - children
    ## precision
    if (.isAttrSet(attrs, "MS:1000521", "32-bit float")) {
      precision <<- 32
      return()
    } else if (.isAttrSet(attrs, "MS:1000523", "64-bit float")) {
      precision <<- 64
      return()
    }

    ## compression
    if (.isAttrSet(attrs, "MS:1000576", "no compression")) {
      compressionType <<- "none"
      return()
    } else if (.isAttrSet(attrs, "MS:1000574", "zlib compression")) {
      compressionType <<- "gzip"
      return()
    }

    ## data arrays
    if (.isAttrSet(attrs, "MS:1000514", "m/z array")) {
      currentArray <<- "mass"
      return()
    } else if (.isAttrSet(attrs, "MS:1000515", "intensity array")) {
      currentArray <<- "intensity"
      return()
    }
  }

  ## default functions to catch tags without a handler
  .startElement <- function(name, attrs) {
    openTag <<- name
  }
  
  .endElement <- function(name, attrs) {
    if (name == "binary" && supported) {
      .decodeArray()
    } else if (name == "fileChecksum") {
      .calculateFileChecksum()
    }

    if (openTag == name) {
      openTag <<- ""
    }
  }

  .text <- function(x) {
    if (openTag == "binary" && supported) {
      currentArrayContent <<- paste(currentArrayContent, x, sep="")
    } else if (openTag == "fileChecksum") {
      fileCheckSum <<- paste(fileCheckSum, x, sep="")
    }
  }

  ## helper functions
  .isAttrSet <- function(attrs, id, name) {
    return(attrs["accession"] == id || attrs["name"] == name)
  }

  .decodeArray <- function() {
    if (nchar(currentArrayContent) <= 0) {
      return()
    }

    ## read base64 encoded array content (endian must be "little")
    content <- readMzXmlData:::.base64decode(x=currentArrayContent,
                                             endian="little",
                                             size=round(precision/8),
                                             compressionType=compressionType)

    ## clear array content
    currentArrayContent <<- character()

    xml$spectra[[currentSpectrumId]][[currentArray]] <<- content
  }

  .calculateFileChecksum <- function() {
    n <- nchar(fileCheckSum)

    if (n <= 0) {
      return()
    }

    ## sha1 sum for this file (from the beginning of the file up to (and
    ## including) the opening tag <fileChecksum>
    if (verbose) {
      message("Look for '<fileChecksum>' position ...")
    }

    ## 14 == nchar("<fileChecksum>")
    checkSumPos <- readMzXmlData:::.revfregexpr("<fileChecksum>", fileName) + 14

    if (verbose) {
      cat("Calculating sha1-sum for ", sQuote(fileName), ": ", sep="")
    }
    
    sha1Calc <- digest::digest(fileName, algo="sha1", file=TRUE,
                               length=checkSumPos-1)
    if (verbose) {
      cat(sha1Calc, "\n")
    }

    if (fileCheckSum != sha1Calc) {
      warning("Stored and calculated Sha-1 sums do not match ",
               "(stored ", sQuote(fileCheckSum), " calculated ",
                sQuote(sha1Calc), ")!")
    }
  }

  ## return statement (please call getData())
  return(list(getData=function() {return(xml)},
              mzML=mzML,
              spectrumList=spectrumList,
              spectrum=spectrum,
              chromatogram=chromatogram,
              cvParam=cvParam,
              .startElement=.startElement,
              .endElement=.endElement,
              .text=.text))
}
