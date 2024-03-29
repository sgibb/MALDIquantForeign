context("exportImzMl")

tmp <- tempdir()

imzML <- c(
"<?xml version=\"1.0\" encoding=\"utf-8\"?>",
"<mzML xmlns=\"http://psi.hupo.org/ms/mzml\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://psi.hupo.org/ms/mzml http://psidev.info/files/ms/mzML/xsd/mzML1.1.0.xsd\" id=\"tmp\" version=\"1.1.0\">",
" <cvList count=\"2\">",
"  <cv id=\"MS\" fullName=\"Proteomics Standards Initiative Mass Spectrometry Ontology\" version=\"3.44.0\" URI=\"http://psidev.cvs.sourceforge.net/*checkout*/psidev/psi/psi-ms/mzML/controlledVocabulary/psi-ms.obo\"/>",
"  <cv id=\"UO\" fullName=\"Unit Ontology\" version=\"12:10:2012\" URI=\"http://obo.cvs.sourceforge.net/*checkout*/obo/obo/ontology/phenotype/unit.obo\"/>",
"  <cv id=\"IMS\" fullName=\"Imaging MS Ontology\" version=\"0.9.1\" URI=\"http://www.maldi-msi.org/download/imzml/imagingMS.obo\"/>",
" </cvList>",
" <fileDescription>",
"  <fileContent>",
"   <cvParam cvRef=\"MS\" accession=\"MS:1000579\" name=\"MS1 spectrum\"/>",
"   <cvParam cvRef=\"IMS\" accession=\"IMS:1000080\" name=\"universally unique identifier\" value=\"{12345678-90ab-4cde-af12-34567890abcd}\"/>",
"   <cvParam cvRef=\"IMS\" accession=\"IMS:1000091\" name=\"ibd SHA-1\" value=\"ddf1105f5622d26dea30ae378b592aa9ab33f3a9\"/>",
"   <cvParam cvRef=\"IMS\" accession=\"IMS:1000031\" name=\"processed\"/>",
"   <userParam name=\"MALDIquantForeign\" value=\"MALDIquant object(s) exported to mzML\"/>",
"  </fileContent>",
"  <sourceFileList count=\"1\">",
"   <sourceFile id=\"SF1\" location=\"TESTS\" name=\"fid\">",
"    <cvParam cvRef=\"MS\" accession=\"MS:1000825\" name=\"Bruker FID file\"/>",
"    <cvParam cvRef=\"MS\" accession=\"MS:1000773\" name=\"Bruker FID nativeID format\"/>",
"   </sourceFile>",
"  </sourceFileList>",
" </fileDescription>",
" <softwareList count=\"1\">",
paste0("  <software id=\"MALDIquantForeign\" version=\"",
       packageVersion("MALDIquantForeign"), "\"/>"),
" </softwareList>",
" <referenceableParamGroupList count=\"2\">",
"  <referenceableParamGroup id=\"mzArray\">",
"   <cvParam cvRef=\"MS\" accession=\"MS:1000514\" name=\"m/z array\" unitCvRef=\"MS\" unitAccession=\"MS:1000040\" unitName=\"m/z\"/>",
"   <cvParam cvRef=\"MS\" accession=\"MS:1000576\" name=\"no compression\" value=\"\"/>",
"   <cvParam cvRef=\"MS\" accession=\"MS:1000523\" name=\"64-bit float\" value=\"\"/>",
"   <cvParam cvRef=\"IMS\" accession=\"IMS:1000101\" name=\"external data\" value=\"true\"/>",
"  </referenceableParamGroup>",
"  <referenceableParamGroup id=\"intensityArray\">",
"   <cvParam cvRef=\"MS\" accession=\"MS:1000515\" name=\"intensity array\" unitCvRef=\"MS\" unitAccession=\"MS:1000131\" unitName=\"number of counts\"/>",
"   <cvParam cvRef=\"MS\" accession=\"MS:1000576\" name=\"no compression\" value=\"\"/>",
"   <cvParam cvRef=\"MS\" accession=\"MS:1000523\" name=\"64-bit float\" value=\"\"/>",
"   <cvParam cvRef=\"IMS\" accession=\"IMS:1000101\" name=\"external data\" value=\"true\"/>",
"  </referenceableParamGroup>",
" </referenceableParamGroupList>",
" <scanSettingsList count=\"1\">",
"  <scanSettings id=\"scansetting1\">",
"   <cvParam cvRef=\"IMS\" accession=\"IMS:1000042\" name=\"max count of pixel x\" value=\"1\"/>",
"   <cvParam cvRef=\"IMS\" accession=\"IMS:1000043\" name=\"max count of pixel y\" value=\"1\"/>",
"   <cvParam cvRef=\"IMS\" accession=\"IMS:1000044\" name=\"max dimension x\" value=\"100\" unitCvRef=\"UO\" unitAccession=\"UO:0000017\" unitName=\"micrometer\"/>",
"   <cvParam cvRef=\"IMS\" accession=\"IMS:1000045\" name=\"max dimension y\" value=\"100\" unitCvRef=\"UO\" unitAccession=\"UO:0000017\" unitName=\"micrometer\"/>",
"   <cvParam cvRef=\"IMS\" accession=\"IMS:1000046\" name=\"pixel size x\" value=\"100\" unitCvRef=\"UO\" unitAccession=\"UO:0000017\" unitName=\"micrometer\"/>",
"   <cvParam cvRef=\"IMS\" accession=\"IMS:1000047\" name=\"pixel size y\" value=\"100\" unitCvRef=\"UO\" unitAccession=\"UO:0000017\" unitName=\"micrometer\"/>",
"  </scanSettings>",
" </scanSettingsList>",
" <instrumentConfigurationList count=\"1\">",
"  <instrumentConfiguration id=\"IC0\"/>",
" </instrumentConfigurationList>",
" <dataProcessingList count=\"1\">",
"  <dataProcessing id=\"export\">",
"   <processingMethod order=\"1\" softwareRef=\"MALDIquantForeign\">",
"    <userParam name=\"MALDIquant object(s) exported to mzML\" value=\"\"/>",
"   </processingMethod>",
"  </dataProcessing>",
" </dataProcessingList>",
" <run id=\"run0\" defaultInstrumentConfigurationRef=\"IC0\">",
"  <spectrumList count=\"1\" defaultDataProcessingRef=\"export\">",
"   <spectrum index=\"0\" id=\"scan=0\" defaultArrayLength=\"5\">",
"    <cvParam cvRef=\"MS\" accession=\"MS:1000511\" name=\"ms level\" value=\"1\"/>",
"    <cvParam cvRef=\"MS\" accession=\"MS:1000294\" name=\"mass spectrum\"/>",
"    <cvParam cvRef=\"MS\" accession=\"MS:1000528\" name=\"lowest observed m/z\" value=\"1\" unitCvRef=\"MS\" unitAccession=\"MS:1000040\" unitName=\"m/z\"/>",
"    <cvParam cvRef=\"MS\" accession=\"MS:1000527\" name=\"highest observed m/z\" value=\"5\" unitCvRef=\"MS\" unitAccession=\"MS:1000040\" unitName=\"m/z\"/>",
"    <cvParam cvRef=\"MS\" accession=\"MS:1000128\" name=\"profile spectrum\"/>",
"    <cvParam cvRef=\"MS\" accession=\"MS:1000285\" name=\"total ion current\" value=\"32\"/>",
"    <scanList count=\"1\">",
"     <scan>",
"      <cvParam cvRef=\"IMS\" accession=\"IMS:1000050\" name=\"position x\" value=\"1\"/>",
"      <cvParam cvRef=\"IMS\" accession=\"IMS:1000051\" name=\"position y\" value=\"1\"/>",
"     </scan>",
"    </scanList>",
"    <binaryDataArrayList count=\"2\">",
"     <binaryDataArray encodedLength=\"0\">",
"      <referenceableParamGroupRef ref=\"mzArray\"/>",
"      <cvParam cvRef=\"IMS\" accession=\"IMS:1000102\" name=\"external offset\" value=\"16\"/>",
"      <cvParam cvRef=\"IMS\" accession=\"IMS:1000103\" name=\"external array length\" value=\"5\"/>",
"      <cvParam cvRef=\"IMS\" accession=\"IMS:1000104\" name=\"external encoded length\" value=\"40\"/>",
"      <binary/>",
"     </binaryDataArray>",
"     <binaryDataArray encodedLength=\"0\">",
"      <referenceableParamGroupRef ref=\"intensityArray\"/>",
"      <cvParam cvRef=\"IMS\" accession=\"IMS:1000102\" name=\"external offset\" value=\"56\"/>",
"      <cvParam cvRef=\"IMS\" accession=\"IMS:1000103\" name=\"external array length\" value=\"5\"/>",
"      <cvParam cvRef=\"IMS\" accession=\"IMS:1000104\" name=\"external encoded length\" value=\"40\"/>",
"      <binary/>",
"     </binaryDataArray>",
"    </binaryDataArrayList>",
"   </spectrum>",
"  </spectrumList>",
" </run>",
"</mzML>")

test_that(".exportImzMl", {
  m <- createMassSpectrum(mass=1:5, intensity=6:10,
                          metaData=list(name="TEST", file="TESTS/fid"))

  expect_error(MALDIquantForeign:::.exportImzMl(m,
                                                file=file.path(tmp, "m.imzML")),
               "The spectra contain no imaging information.")
  MALDIquantForeign:::.exportImzMl(m, file=file.path(tmp, "m.imzML"), coordinates=cbind(1, 1), uuid="12345678-90ab-4cde-af12-34567890abcd")
  expect_equal(readLines(file.path(tmp, "m.imzML")),
               sub(pattern="id=\"tmp\"", replacement="id=\"m\"", x=imzML))
})

test_that("exportImzMl,MassSpectrum", {
  m <- createMassSpectrum(mass=1:5, intensity=6:10,
                          metaData=list(name="TEST", file="TESTS/fid"))

  expect_error(MALDIquantForeign::exportImzMl(m,
                                              file=file.path(tmp, "ms.imzML")),
               "The spectra contain no imaging information.")
  MALDIquantForeign::exportImzMl(m, file=file.path(tmp, "ms.imzML"), coordinates=cbind(1, 1), uuid="12345678-90ab-4cde-af12-34567890abcd")
  expect_equal(readLines(file.path(tmp, "ms.imzML")),
               sub(pattern="id=\"tmp\"", replacement="id=\"ms\"", x=imzML))
})

test_that("exportImzMl,list", {
  expect_error(MALDIquantForeign:::.exportImzMl(list(1:3, 4:6),
                                                file="tmp.imzML"),
               "MassSpectrum.*supported")

  m <- createMassSpectrum(mass=1:5, intensity=6:10,
                          metaData=list(name="TEST", file="TESTS/fid",
                                        imaging=list(pos=c(1, 1),
                                                     pixelSize=c(100, 100),
                                                     size=c(1, 1),
                                                     dim=c(100, 100))))
  spectra <- list(m, m)
  MALDIquantForeign::exportImzMl(spectra, path=tmp, force=TRUE,
                                 uuid="12345678-90ab-4cde-af12-34567890abcd")
  expect_equal(readLines(file.path(tmp, "TESTS_1.imzML")),
               sub(pattern="id=\"tmp\"", replacement="id=\"TESTS_1\"", x=imzML))
  expect_equal(readLines(file.path(tmp, "TESTS_2.imzML")),
               sub(pattern="id=\"tmp\"", replacement="id=\"TESTS_2\"", x=imzML))
})
