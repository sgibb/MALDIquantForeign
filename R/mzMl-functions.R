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

.writeMzMlDocument <- function(x, file, encoding="utf-8") {
  ## stop if file isn't writeable
  if (file.exists(file) && file.access(file, 2) != 0) {
    stop("No permissions to write into ", sQuote(file), "!")
  }

  ## header
  doc <- XML::xmlOutputDOM("mzML", attrs=c(
    xmlns="http://psi.hupo.org/ms/mzml",
    "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance",
    "xsi:schemaLocation"=paste("http://psi.hupo.org/ms/mzml",
                         "http://psidev.info/files/ms/mzML/xsd/mzML1.1.0.xsd"),
    id=names(x),
    version="1.1.0"
    ))

  .writeCvList(doc)
  .writeFileDescription(x, doc)
  .writeSoftwareList(x, doc)
  .writeInstrumentConfigurationList(x, doc)
  .writeDataProcessingList(x, doc)
  .writeRun(x, doc)

  invisible(XML::saveXML(doc$value(), file=file, encoding=encoding))
}

.writeCvList <- function(xmlDoc) {
  items <- list(
    ms=list(id="MS", 
      fullName="Proteomics Standards Initiative Mass Spectrometry Ontology",
      version="3.44.0",
      URI="http://psidev.cvs.sourceforge.net/*checkout*/psidev/psi/psi-ms/mzML/controlledVocabulary/psi-ms.obo"),
    uo=list(id="UO",
      fullName="Unit Ontology",
      version="12:10:2012",
      URI="http://obo.cvs.sourceforge.net/*checkout*/obo/obo/ontology/phenotype/unit.obo"))

  xmlDoc$addTag("cvList", attrs=c(count=2), close=FALSE)
  for (i in seq(along=items)) {
    xmlDoc$addTag("cv", attrs=items[[i]])
  }
  xmlDoc$closeTag() # cvList
}

.writeFileDescription <- function(x, xmlDoc) {
  xmlDoc$addTag("fileDescription", close=FALSE)
    xmlDoc$addTag("fileContent", close=FALSE)
      xmlDoc$addTag("cvParam", attrs=c(cvRef="MS",
                      accession="MS:1000579", name="MS1 spectrum"))
      xmlDoc$addTag("userParam", attrs=c(name="MALDIquantForeign",
                      value="MALDIquant object(s) exported to mzML"))
    xmlDoc$closeTag() # fileContent
    .writeSourceFileList(x, xmlDoc)
  xmlDoc$closeTag() # fileDescription
}

.writeSourceFileList <- function(x, xmlDoc) {
  files <- unique(vapply(x, function(s)s@metaData$file, character(1)))
  dname <- dirname(files)
  bname <- basename(files)
  ext <- tolower(.fileExtension(bname))
  
  if (length(files)) {
    xmlDoc$addTag("sourceFileList", attrs=c(count=length(files)), close=FALSE)
    for (i in seq(along=files)) {
      xmlDoc$addTag("sourceFile", attrs=c(
        id=paste("SF", i, sep=""),
        location=dname[i],
        name=bname[i]), close=FALSE)

      if (ext[i] == "fid") {
        xmlDoc$addTag("cvParam", attrs=c(cvRef="MS",
                      accession="MS:1000825", name="Bruker FID file"))
        xmlDoc$addTag("cvParam", attrs=c(cvRef="MS",
                      accession="MS:1000773", name="Bruker FID nativeID format"))
      } else if (ext[i] == "mzxml") {
        xmlDoc$addTag("cvParam", attrs=c(cvRef="MS",
                      accession="MS:1000566", name="ISB mzXML file"))
      } else if (ext[i] == "mzml") {
        xmlDoc$addTag("cvParam", attrs=c(cvRef="MS",
                      accession="MS:1000584", name="mzML file"))
      } 

      if (file.exists(files[i])) {
        xmlDoc$addTag("cvParam", attrs=c(cvRef="MS", accession="MS:1000569",
                        name="SHA-1", 
                        value=digest::digest(files[i], algo="sha1", file=TRUE)))
      }
      xmlDoc$closeTag() # sourceFile
    }
    xmlDoc$closeTag() # sourceFileList
  }
}

.writeSoftwareList <- function(x, xmlDoc) {
  xmlDoc$addTag("softwareList", attrs=c(count=1), close=FALSE)
    xmlDoc$addTag("software", attrs=c(id="MALDIquantForeign",
                    version=as.character(packageVersion("MALDIquantForeign"))))
  xmlDoc$closeTag() # softwareList 
}

.writeInstrumentConfigurationList <- function(x, xmlDoc) {
  xmlDoc$addTag("instrumentConfigurationList", attrs=c(count=1), close=FALSE)
    xmlDoc$addTag("instrumentConfiguration", attrs=c(id="IC1"))
  xmlDoc$closeTag() # instrumentConfigurationList
}

.writeDataProcessingList <- function(x, xmlDoc) {
  xmlDoc$addTag("dataProcessingList", attrs=c(count=1), close=FALSE)
    xmlDoc$addTag("dataProcessing", attrs=c(id="export"), close=FALSE)
      xmlDoc$addTag("processingMethod", attrs=c(order=1,
                      softwareRef="MALDIquantForeign"), close=FALSE)
        xmlDoc$addTag("userParam", attrs=c(
                        name="MALDIquant object(s) exported to mzXML",
                        value=""))
      xmlDoc$closeTag() # processingMethod
    xmlDoc$closeTag() # dataProcessing
  xmlDoc$closeTag() # dataProcessingList
}

.writeRun <- function(x, xmlDoc) {
  xmlDoc$addTag("run", attrs=c(id="run",
                               defaultInstrumentConfigurationRef="IC1"),
                close=FALSE)
  .writeSpectrumList(x, xmlDoc)
  xmlDoc$closeTag() # run
}

.writeSpectrumList <- function(x, xmlDoc) {
  xmlDoc$addTag("spectrumList", attrs=c(count=length(x),
                  defaultDataProcessingRef="export"), close=FALSE)
  for (i in seq(along=x)) {
    xmlDoc$addTag("spectrum", attrs=c(index=i-1,
                    id=paste("scan=", i-1, sep=""),
                    defaultArrayLength=length(x[[i]]),
                    spotID=x[[i]]@metaData$fullName),  close=FALSE)

      msLevel <- ifelse(is.null(x[[1]]@metaData$msLevel), 1,
                        x[[1]]@metaData$msLevel)
      xmlDoc$addTag("cvParam", attrs=c(cvRef="MS", accession="MS:1000511",
                                       name="ms level", value=msLevel))
      xmlDoc$addTag("cvParam", attrs=c(cvRef="MS", accession="MS:1000294",
                                       name="mass spectrum"))

    if (MALDIquant::isMassSpectrum(x[[i]])) {
      xmlDoc$addTag("cvParam", attrs=c(cvRef="MS", accession="MS:1000128",
                                       name="profile spectrum"))
    } else {
      xmlDoc$addTag("cvParam", attrs=c(cvRef="MS", accession="MS:1000127",
                                       name="centroid spectrum"))
    }

    .writeBinaryDataArrayList(x[[i]], xmlDoc)

    xmlDoc$closeTag() # spectrum
  }
  xmlDoc$closeTag() # spectrumList
}

.writeBinaryDataArrayList <- function(x, xmlDoc) {
  count <- ifelse(MALDIquant:::isMassSpectrum(x), 2, 3)
  xmlDoc$addTag("binaryDataArrayList", attrs=c(count=count), close=FALSE)
  .writeBinaryData(x@mass, xmlDoc, c(cvRef="MS", accession="MS:1000514",
                    name="m/z array", unitCvRef="MS",
                    unitAccession="MS:1000040", unitName="m/z"))

  .writeBinaryData(x@intensity, xmlDoc, c(cvRef="MS", accession="MS:1000515",
                    name="intensity array", unitCvRef="MS",
                    unitAccession="MS:1000131", unitName="number of counts"))

  if (MALDIquant::isMassPeaks(x)) {
    .writeBinaryData(x@snr, xmlDoc, c(cvRef="MS", accession="MS:1000517",
                      name="signal to noise array"))
  }
  xmlDoc$closeTag() # binaryDataArrayList
}

.writeBinaryData <- function(x, xmlDoc, additionalAttrs) {
  binaryData <- .base64encode(x, size=8, endian="little",
                              compressionType="gzip")

  xmlDoc$addTag("binaryDataArray", attrs=c(encodedLength=nchar(binaryData)),
                close=FALSE)
    xmlDoc$addTag("cvParam", attrs=c(cvRef="MS", accession="MS:1000574",
                                     name="zlib compression"))
    xmlDoc$addTag("cvParam", attrs=c(cvRef="MS", accession="MS:1000523",
                                     name="64-bit float"))
    if (!missing(additionalAttrs)) {
      xmlDoc$addTag("cvParam", attrs=additionalAttrs)
    }
    xmlDoc$addTag("binary", binaryData)
  xmlDoc$closeTag() # binaryDataArray
}

