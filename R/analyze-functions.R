## Copyright 2013-2014 Sebastian Gibb
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

## Analyze 7.5 header file
.readAnalyzeHdr <- function(filename, verbose=FALSE) {

  if (!file.exists(filename)) {
    stop(sQuote(filename), " isn't readable.")
  }

  size <- file.info(filename)$size

  ## Analyze 7.5 header must be of size 348; ABSciex files are a little bit
  ## larger (384)
  if (size != 348 && size != 384) {
    stop(sQuote(filename), " is no ANALYZE header file.")
  }

  if (verbose) {
    message("Extracting header information from ", sQuote(filename), " ...")
  }

  f <- file(filename, open="rb")

  ## first 4 bytes have to be 348 in little endian mode
  ## (384 for ABSciex)
  endian <- ifelse(readBin(f, "integer", n=1, size=4, endian="little") %in%
                   c(348, 384), "little", "big")

  ## skip unused entries
  seek(f, where=38)
  regular <- readChar(f, nchars=1, useBytes=TRUE)

  if (regular != "r") {
    stop("Wrong file format. Images have to be of equal size (",
         sQuote("regular"), " must be ", sQuote("r"), ")")
  }

  ## skip unused entries
  seek(f, where=40)
  ## 2 == number of intensity, 3 == ncol (x), 4 == nrow (y)
  ## use as.double() here to avoid integer overflows in calculations like nx*ny
  ## later
  ## Thanks to Ken Frankel <kafrankel@gmail.com> for reporting this problem.
  dimensions <- as.double(readBin(f, "integer", n=8, size=2, endian=endian))

  ## We use the size of the t2m file. See .readAnalyzeIntensity for details.
  #ni <- dimensions[2]
  nx <- dimensions[3]
  ny <- dimensions[4]

  ## skip unused entries
  seek(f, where=70)
  datatype <- readBin(f, "integer", n=1, size=2, endian=endian)
  bitpix <- readBin(f, "integer", n=1, size=2, endian=endian)

  if (datatype %in% c(2, 4, 8)) {
    what <- "integer"
  } else if (datatype %in% c(16, 32, 64)) {
    what <- "double"
  } else {
    what <- "raw"
  }

  signed <- datatype == 2
  size <- bitpix/8

  ## skip unused entries
  seek(f, where=76)
  pixdim <- readBin(f, "double", n=8, size=4, endian=endian)
  ## pixelwidth in mm
  xd <- pixdim[2]
  yd <- pixdim[3]

  close(f)

  return(list(nx=nx, ny=ny, xd=xd, yd=yd,
              endian=endian, what=what, signed=signed, size=size))
}

## Analyze 7.5 img file
.readAnalyzeIntensity <- function(filename, header, ni, verbose=FALSE) {
  if (!file.exists(filename)) {
    stop(sQuote(filename), " isn't readable.")
  }

  if (verbose) {
    message("Reading intensity values from ", sQuote(filename), " ...")
  }

  f <- file(filename, open="rb")
  ## header$ni should contain the number of intensity values
  ## because the format specification uses int16 for header$ni it is limited to
  ## 32767 intensity values.
  ## We use the size of the t2m file divided by 4 to find the correct number to
  ## circumvent this size limit.
  ## Thanks to Ken Frankel <kafrankel@gmail.com> for reporting this problem.
  i <- readBin(f, what=header$what, n=ni*header$nx*header$ny,
               size=header$size, signed=header$signed, endian=header$endian)
  dim(i) <- c(ni, header$nx, header$ny)
  close(f)

  return(i)
}

## Analyze 7.5 t2m file
.readAnalyzeMass <- function(filename, header, verbose=FALSE) {
  if (!file.exists(filename)) {
    stop(sQuote(filename), " isn't readable.")
  }

  if (verbose) {
    message("Reading mass values from ", sQuote(filename), " ...")
  }

  n <- file.info(filename)$size/4
  f <- file(filename, open="rb")
  m <- readBin(f, what="double", n=n, size=4, signed=TRUE, endian=header$endian)
  close(f)

  return(m)
}

