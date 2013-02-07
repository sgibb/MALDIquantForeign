## Copyright 2013 Sebastian Gibb
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

.writeMzMlDocument <- function(x, file, id,  encoding="utf-8") {
  ## stop if file isn't writeable
  if (file.exists(file) && file.access(file, 2) != 0) {
    stop("No permissions to write into ", sQuote(file), "!")
  }

  ## file handle
  f <- file(file, open="wt", encoding=encoding)

  .writeXmlHeader(file=f, encoding=encoding)

  .writeXmlTag("mzML", attrs=c(xmlns="http://psi.hupo.org/ms/mzml",
    "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance",
    "xsi:schemaLocation"=paste("http://psi.hupo.org/ms/mzml",
      "http://psidev.info/files/ms/mzML/xsd/mzML1.1.0.xsd"),
    id=.sanitize(ifelse(missing(id) || is.null(id),
                        deparse(substitute(x)), id)),
    version="1.1.0"), close=FALSE, file=f)

  .writeMzMlCvList(file=f)
  .writeMzMlFileDescription(x, file=f)
  .writeMzMlSoftwareList(x, file=f)
  .writeMzMlInstrumentConfigurationList(x, file=f)
  .writeMzMlDataProcessingList(x, file=f)
  .writeMzMlRun(x, file=f)

  .writeCloseXmlTag("mzML", file=f)

  invisible(close(f))
}

.writeMzMlCvList <- function(file) {
  items <- list(
    ms=list(id="MS",
      fullName="Proteomics Standards Initiative Mass Spectrometry Ontology",
      version="3.44.0",
      URI="http://psidev.cvs.sourceforge.net/*checkout*/psidev/psi/psi-ms/mzML/controlledVocabulary/psi-ms.obo"),
    uo=list(id="UO",
      fullName="Unit Ontology",
      version="12:10:2012",
      URI="http://obo.cvs.sourceforge.net/*checkout*/obo/obo/ontology/phenotype/unit.obo"))

  .writeXmlTag("cvList", attrs=c(count=2), intend=1, close=FALSE, file=file)
  for (i in seq(along=items)) {
    .writeXmlTag("cv", attrs=items[[i]], intend=2, file=file)
  }
  .writeCloseXmlTag("cvList", intend=1, file=file)
}

.writeMzMlFileDescription <- function(x, file) {
  .writeXmlTag("fileDescription", intend=1, close=FALSE, file=file)
    .writeXmlTag("fileContent", intend=2, close=FALSE, file=file)
      .writeXmlTag("cvParam", intend=3,
                   attrs=c(cvRef="MS", accession="MS:1000579",
                           name="MS1 spectrum"), file=file)
      .writeXmlTag("userParam", intend=3,
                   attrs=c(name="MALDIquantForeign",
                           value="MALDIquant object(s) exported to mzML"),
                   file=file)
    .writeCloseXmlTag("fileContent", intend=2, file=file)
    .writeMzMlSourceFileList(x, file=file)
  .writeCloseXmlTag("fileDescription", intend=1, file=file)
}

.writeMzMlSourceFileList <- function(x, file) {
  files <- unique(unlist(lapply(x, function(s)metaData(s)$file)))

  if (length(files)) {
    dname <- dirname(files)
    bname <- basename(files)
    ext <- tolower(.fileExtension(bname))

    .writeXmlTag("sourceFileList", attrs=c(count=length(files)), intend=2,
                 close=FALSE, file=file)
    for (i in seq(along=files)) {
      .writeXmlTag("sourceFile", intend=3,
                   attrs=c(id=paste("SF", i, sep=""), location=dname[i],
                           name=bname[i]), close=FALSE, file=file)

      if (ext[i] == "fid") {
        .writeXmlTag("cvParam", intend=4,
                     attrs=c(cvRef="MS", accession="MS:1000825",
                             name="Bruker FID file"), file=file)
        .writeXmlTag("cvParam", intend=4,
                     attrs=c(cvRef="MS", accession="MS:1000773",
                             name="Bruker FID nativeID format"), file=file)
      } else if (ext[i] == "mzxml") {
        .writeXmlTag("cvParam", intend=4,
                     attrs=c(cvRef="MS", accession="MS:1000566",
                             name="ISB mzXML file"), file=file)
      } else if (ext[i] == "mzml") {
        .writeXmlTag("cvParam", intend=4,
                     attrs=c(cvRef="MS", accession="MS:1000584",
                             name="mzML file"), file=file)
      }

      if (file.exists(files[i])) {
        .writeXmlTag("cvParam", intend=4,
                     attrs=c(cvRef="MS", accession="MS:1000569", name="SHA-1",
                             value=digest::digest(files[i], algo="sha1",
                                                  file=TRUE)), file=file)
      }
      .writeCloseXmlTag("sourceFile", intend=3, file=file)
    }
    .writeCloseXmlTag("sourceFileList", intend=2, file=file)
  }
}

.writeMzMlSoftwareList <- function(x, file) {
  .writeXmlTag("softwareList", attrs=c(count=1), intend=1, close=FALSE,
               file=file)
    .writeXmlTag("software", intend=2, attrs=c(id="MALDIquantForeign",
                  version=as.character(packageVersion("MALDIquantForeign"))),
                 file=file)
  .writeCloseXmlTag("softwareList", intend=1, file=file)
}

.writeMzMlInstrumentConfigurationList <- function(x, file) {
  .writeXmlTag("instrumentConfigurationList", attrs=c(count=1), intend=1,
               close=FALSE, file=file)
    .writeXmlTag("instrumentConfiguration", attrs=c(id="IC0"), intend=2,
                 file=file)
  .writeCloseXmlTag("instrumentConfigurationList", intend=1, file=file)
}

.writeMzMlDataProcessingList <- function(x, file) {
  .writeXmlTag("dataProcessingList", attrs=c(count=1), intend=1, close=FALSE,
               file=file)
    .writeXmlTag("dataProcessing", attrs=c(id="export"), intend=2, close=FALSE,
                 file=file)
      .writeXmlTag("processingMethod", intend=3,
                   attrs=c(order=1, softwareRef="MALDIquantForeign"),
                   close=FALSE, file=file)
        .writeXmlTag("userParam", intend=4,
                     attrs=c(name="MALDIquant object(s) exported to mzML",
                             value=""), file=file)
      .writeCloseXmlTag("processingMethod", intend=3, file=file)
    .writeCloseXmlTag("dataProcessing", intend=2, file=file)
  .writeCloseXmlTag("dataProcessingList", intend=1, file=file)
}

.writeMzMlRun <- function(x, file) {
  .writeXmlTag("run", attrs=c(id="run0",
                              defaultInstrumentConfigurationRef="IC0"),
                intend=1, close=FALSE, file=file)
  .writeMzMlSpectrumList(x, file=file)
  .writeCloseXmlTag("run", intend=1, file=file)
}

.writeMzMlSpectrumList <- function(x, file) {
  .writeXmlTag("spectrumList",
               attrs=c(count=length(x), defaultDataProcessingRef="export"),
               intend=2, close=FALSE, file=file)

  for (i in seq(along=x)) {
    .writeXmlTag("spectrum", intend=3,
                 attrs=c(index=i-1, id=paste("scan=", i-1, sep=""),
                         defaultArrayLength=length(x[[i]]),
                         spotID=metaData(x[[i]])$fullName), close=FALSE,
                 file=file)

      msLevel <- ifelse(is.null(metaData(x[[i]])$msLevel), 1,
                        metaData(x[[i]])$msLevel)
      .writeXmlTag("cvParam", intend=4, attrs=c(cvRef="MS",
                     accession="MS:1000511", name="ms level", value=msLevel),
                   file=file)
      .writeXmlTag("cvParam", intend=4, attrs=c(cvRef="MS",
                     accession="MS:1000294", name="mass spectrum"), file=file)
      .writeXmlTag("cvParam", intend=4, attrs=c(cvRef="MS",
                     accession="MS:1000528", name="lowest observed m/z",
                     value=min(mass(x[[i]])), unitCvRef="MS",
                     unitAccession="MS:1000040", unitName="m/z"), file=file)
      .writeXmlTag("cvParam", intend=4, attrs=c(cvRef="MS",
                     accession="MS:1000527", name="highest observed m/z",
                     value=max(mass(x[[i]])), unitCvRef="MS",
                     unitAccession="MS:1000040", unitName="m/z"), file=file)
      .writeXmlTag("cvParam", intend=4, attrs=c(cvRef="MS",
                     accession="MS:1000285", name="total ion current",
                     value=totalIonCurrent(x[[i]])), file=file)

    if (MALDIquant::isMassSpectrum(x[[i]])) {
      .writeXmlTag("cvParam", intend=4, attrs=c(cvRef="MS",
                     accession="MS:1000128", name="profile spectrum"),
                   file=file)
    } else {
      .writeXmlTag("cvParam", intend=4, attrs=c(cvRef="MS",
                     accession="MS:1000127", name="centroid spectrum"),
                   file=file)
    }

    .writeMzMlBinaryDataArrayList(x[[i]], file=file)

    .writeCloseXmlTag("spectrum", intend=3, file=file)
  }
  .writeCloseXmlTag("spectrumList", intend=2, file=file)
}

.writeMzMlBinaryDataArrayList <- function(x, file) {
  count <- ifelse(MALDIquant:::isMassSpectrum(x), 2, 3)
  .writeXmlTag("binaryDataArrayList", attrs=c(count=count), intend=4,
               close=FALSE, file=file)
  .writeMzMlBinaryData(mass(x), file=file, c(cvRef="MS", accession="MS:1000514",
                        name="m/z array", unitCvRef="MS",
                        unitAccession="MS:1000040", unitName="m/z"))

  .writeMzMlBinaryData(intensity(x), file=file, c(cvRef="MS",
                        accession="MS:1000515", name="intensity array",
                        unitCvRef="MS", unitAccession="MS:1000131",
                        unitName="number of counts"))

  if (MALDIquant::isMassPeaks(x)) {
    .writeMzMlBinaryData(snr(x), file=file, c(cvRef="MS",
                          accession="MS:1000517", name="signal to noise array"))
  }
  .writeCloseXmlTag("binaryDataArrayList", intend=4, file=file)
}

.writeMzMlBinaryData <- function(x, file, additionalAttrs) {
  binaryData <- .base64encode(x, size=8, endian="little",
                              compressionType="gzip")

  .writeXmlTag("binaryDataArray", attrs=c(encodedLength=nchar(binaryData)),
                intend=5, close=FALSE, file=file)
    .writeXmlTag("cvParam", intend=6, file=file,
                 attrs=c(cvRef="MS", accession="MS:1000574",
                         name="zlib compression"))
    .writeXmlTag("cvParam", intend=6, file=file,
                 attrs=c(cvRef="MS", accession="MS:1000523",
                         name="64-bit float"))
    if (!missing(additionalAttrs)) {
      .writeXmlTag("cvParam", attrs=additionalAttrs, intend=6, file=file)
    }
    .writeXmlTag("binary", text=binaryData, intend=6, file=file)
  .writeCloseXmlTag("binaryDataArray", intend=5, file=file)
}

