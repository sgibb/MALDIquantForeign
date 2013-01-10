## Copyright 2012-2013 Sebastian Gibb
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

  ## file handler
  f <- file(file, open="wt", encoding=encoding)

  ## header
  .writeXmlHeader(file=f, encoding=encoding)

  .writeXmlTag("mSD", attrs=c(version=2.2), close=FALSE, file=f)

  .writeMsdDescription(x, file=f)
  .writeMsdSpectrum(x, file=f)

  if (!missing(peaks)) {
    .writeMsdPeakList(peaks, file=f)
  }

  .writeCloseXmlTag("mSD", file=f)

  invisible(close(f))
}

.writeMsdDescription <- function(x, file) {
  .writeXmlTag("description", intend=1, close=FALSE, file=file)
    .writeXmlTag("title", text=.sanitize(file), intend=2, file=file)
    .writeXmlTag("date", attrs=c(value=.sanitize(date())),
                 intend=2, file=file)
    .writeXmlTag("operator", attrs=c(value=.sanitize(x@metaData$owner)), 
                 intend=2, file=file)
    .writeXmlTag("contact", attrs=c(value=.sanitize(x@metaData$owner)),
                 intend=2, file=file)
    .writeXmlTag("institution", 
                 attrs=c(value=.sanitize(x@metaData$institution)),
                 intend=2, file=file)
    .writeXmlTag("instrument", attrs=c(value=.sanitize(x@metaData$instrument)),
                 intend=2, file=file)
    .writeXmlTag("notes", .sanitize(paste(x@metaData$comments, collapse="\n")),
                 intend=2, file=file)
  .writeCloseXmlTag("description", intend=1, file=file)
}

.writeMsdSpectrum <- function(x, file) {
  polarity <- paste(x@metaData$ionizationMode, x@metaData$polarity)

  if (length(polarity)) {
    polarity <- ifelse(grepl(pattern="+|positive", x=polarity), "1", "-1")
  } else {
    polarity <- ""
  }

  .writeXmlTag("spectrum", attrs=c(points=length(x),
                                    msLevel=ifelse(is.null(x@metaData$msLevel),
                                                   1, x@metaData$msLevel),
                                    polarity=polarity), close=FALSE,
               intend=1, file=file)
    .writeMsdArray(x@mass, name="mzArray", file=file)
    .writeMsdArray(x@intensity, name="intArray", file=file)
  .writeCloseXmlTag("spectrum", intend=1, file=file)
}

.writeMsdArray <- function(x, name, file) {
  .writeXmlTag(name, text=.base64encode(x, size=8, compressionType="gzip"),
               attrs=c(precision="64", compression="zlib",
                       endian=.Platform$endian), intend=2, file=file)
}

.writeMsdPeakList <- function(x, file) {
  .writeXmlTag("peaklist", close=FALSE, intend=1, file=file)

  cat(paste("  <peak mz=\"", x@mass, "\" intensity=\"", x@intensity, 
            "\" baseline=\"0\" sn=\"", x@snr,
      "\"/>\n", sep=""), file=file, sep="", append=TRUE)

  .writeCloseXmlTag("peaklist", intend=1, file=file)
}

