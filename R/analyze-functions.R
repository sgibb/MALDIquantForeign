## Copyright 2013 Sebastian Gibb
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
  endian <- ifelse(readBin(f, integer(), n=1, size=4, endian="little") %in%
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
  ## 2 == number of intesity, 3 == ncol (x), 4 == nrow (y)
  dimensions <- readBin(f, integer(), n=8, size=2, endian=endian)
  ni <- dimensions[2]
  nx <- dimensions[3]
  ny <- dimensions[4]

  ## skip unused entries
  seek(f, where=70)
  datatype <- readBin(f, integer(), n=1, size=2, endian=endian)
  bitpix <- readBin(f, integer(), n=1, size=2, endian=endian)

  if (datatype %in% c(2, 4, 8)) {
    what <- integer()
  } else if (datatype %in% c(16, 32, 64)) {
    what <- double()
  } else {
    what <- raw()
  }

  signed <- datatype == 2
  size <- bitpix/8

  ## skip unused entries
  seek(f, where=76)
  pixdim <- readBin(f, double(), n=8, size=4, endian=endian)
  ## pixelwidth in mm
  xd <- pixdim[2]
  yd <- pixdim[3]

  close(f)

  return(list(ni=ni, nx=nx, ny=ny, xd=xd, yd=yd,
              endian=endian, what=what, signed=signed, size=size))
}

## Analyze 7.5 img file
.readAnalyzeIntensity <- function(filename, header, verbose=FALSE) {
  if (!file.exists(filename)) {
    stop(sQuote(filename), " isn't readable.")
  }

  if (verbose) {
    message("Reading intensity values from ", sQuote(filename), " ...")
  }

  f <- file(filename, open="rb")
  i <- readBin(f, what=header$what, n=header$ni*header$nx*header$ny,
               size=header$size, signed=header$signed, endian=header$endian)
  dim(i) <- c(header$ni, header$nx, header$ny)
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

  f <- file(filename, open="rb")
  m <- readBin(f, what=double(), n=header$ni, size=4, signed=TRUE,
               endian=header$endian)
  close(f)

  return(m)
}

