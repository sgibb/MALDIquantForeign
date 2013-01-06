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

  .writeCvList(x, doc)
  .writeFileDescription(x, doc)
  .writeSoftwareList(x, doc)
  .writeInstrumentConfigurationList(x, doc)
  #.writeDataProcessingList(x, xmlDoc)
  #.writeRun(x, xmlDoc)

  invisible(XML::saveXML(doc$value(), file=file, encoding=encoding))
}

.writeCvList <- function(x, xmlDoc) {
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
  xmlDoc$closeTag() # fileDescription
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

