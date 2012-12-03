### Copyright 2012 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## It is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## See <http://www.gnu.org/licenses/>

.writeMsdDocument <- function(x, file, peaks, encoding="utf-8") {
  ## stop if file isn't writeable
  if (file.exists(file) && file.access(file, 2) != 0) {
    stop("No permissions to write into ", sQuote(file), "!")
  }

  ## header
  doc <- XML::xmlOutputDOM("mSD", attrs=c(version="2.2"))

  doc <- .writeMsdDescription(x, xmlDoc=doc, file=file)
  doc <- .writeMsdSpectrum(x, xmlDoc=doc)

  if (!missing(peaks)) {
    doc <- .writeMsdPeakList(peaks, xmlDoc=doc)
  }

  invisible(XML::saveXML(doc$value(), file=file, encoding=encoding))
}

.writeMsdDescription <- function(x, xmlDoc, file) {
  xmlDoc$addTag("description", close=FALSE)
    xmlDoc$addTag("title", .sanitize(file))
    xmlDoc$addTag("date", attrs=c(value=.sanitize(date())))
    xmlDoc$addTag("operator", attrs=c(value=.sanitize(x@metaData$owner)))
    xmlDoc$addTag("contact", attrs=c(value=.sanitize(x@metaData$owner)))
    xmlDoc$addTag("institution", 
                  attrs=c(value=.sanitize(x@metaData$institution)))
    xmlDoc$addTag("instrument", attrs=c(value=.sanitize(x@metaData$instrument)))
    xmlDoc$addTag("notes", .sanitize(paste(x@metaData$comments, collapse="\n")))
  xmlDoc$closeTag() # description

  return(xmlDoc)
}

.writeMsdSpectrum <- function(x, xmlDoc) {
  polarity <- paste(x@metaData$ionizationMode, x@metaData$polarity)

  if (length(polarity)) {
    polarity <- ifelse(grepl(pattern="+|positive", x=polarity), "1", "-1")
  } else {
    polarity <- ""
  }

  xmlDoc$addTag("spectrum", attrs=c(points=length(x),
                                    msLevel=ifelse(is.null(x@metaData$msLevel),
                                                   1, x@metaData$msLevel),
                                    polarity=polarity), close=FALSE)
    xmlDoc <- .writeMsdArray(x@mass, name="mzArray", xmlDoc=xmlDoc)
    xmlDoc <- .writeMsdArray(x@intensity, name="intArray", xmlDoc=xmlDoc)
  xmlDoc$closeTag() # spectrum

  return(xmlDoc)
}

.writeMsdArray <- function(x, name, xmlDoc) {
  xmlDoc$addTag(name, .base64encode(x, size=8, compressionType="gzip"),
                attrs=c(precision="64", compression="zlib",
                        endian=.Platform$endian))
  return(xmlDoc)
}

.writeMsdPeakList <- function(x, xmlDoc) {
  xmlDoc$addTag("peaklist", close=FALSE)

  for (i in seq(along=x@mass)) {
    xmlDoc$addTag("peak", attrs=c(mz=x@mass[i], intensity=x@intensity[i],
                                  baseline=0, sn=x@snr[i]))
  }
  xmlDoc$closeTag() # peaklist

  return(xmlDoc)
}

