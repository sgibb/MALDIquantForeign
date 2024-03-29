context("exportMzMl")

m <- createMassSpectrum(mass=1:5, intensity=6:10,
                        metaData=list(name="TEST", file="TESTS/fid"))
bm <- .base64encode(1:5, size=8, endian="little", compressionType="gzip")
bi <- .base64encode(6:10, size=8, endian="little", compressionType="gzip")

tmp <- tempdir()

mzML <- c(
"<?xml version=\"1.0\" encoding=\"utf-8\"?>",
"<mzML xmlns=\"http://psi.hupo.org/ms/mzml\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://psi.hupo.org/ms/mzml http://psidev.info/files/ms/mzML/xsd/mzML1.1.0.xsd\" id=\"tmp\" version=\"1.1.0\">",
" <cvList count=\"2\">",
"  <cv id=\"MS\" fullName=\"Proteomics Standards Initiative Mass Spectrometry Ontology\" version=\"3.44.0\" URI=\"http://psidev.cvs.sourceforge.net/*checkout*/psidev/psi/psi-ms/mzML/controlledVocabulary/psi-ms.obo\"/>",
"  <cv id=\"UO\" fullName=\"Unit Ontology\" version=\"12:10:2012\" URI=\"http://obo.cvs.sourceforge.net/*checkout*/obo/obo/ontology/phenotype/unit.obo\"/>",
" </cvList>",
" <fileDescription>",
"  <fileContent>",
"   <cvParam cvRef=\"MS\" accession=\"MS:1000579\" name=\"MS1 spectrum\"/>",
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
"    <binaryDataArrayList count=\"2\">",
paste0("     <binaryDataArray encodedLength=\"", nchar(bm), "\">"),
"      <cvParam cvRef=\"MS\" accession=\"MS:1000574\" name=\"zlib compression\"/>",
"      <cvParam cvRef=\"MS\" accession=\"MS:1000523\" name=\"64-bit float\"/>",
"      <cvParam cvRef=\"MS\" accession=\"MS:1000514\" name=\"m/z array\" unitCvRef=\"MS\" unitAccession=\"MS:1000040\" unitName=\"m/z\"/>",
paste0("      <binary>", bm, "</binary>"),
"     </binaryDataArray>",
paste0("     <binaryDataArray encodedLength=\"", nchar(bi), "\">"),
"      <cvParam cvRef=\"MS\" accession=\"MS:1000574\" name=\"zlib compression\"/>",
"      <cvParam cvRef=\"MS\" accession=\"MS:1000523\" name=\"64-bit float\"/>",
"      <cvParam cvRef=\"MS\" accession=\"MS:1000515\" name=\"intensity array\" unitCvRef=\"MS\" unitAccession=\"MS:1000131\" unitName=\"number of counts\"/>",
paste0("      <binary>", bi, "</binary>"),
"     </binaryDataArray>",
"    </binaryDataArrayList>",
"   </spectrum>",
"  </spectrumList>",
" </run>",
"</mzML>")

test_that(".exportMzMl", {
  MALDIquantForeign:::.exportMzMl(m, file=file.path(tmp, "m.mzML"))
  expect_equal(readLines(file.path(tmp, "m.mzML")),
               sub(pattern="id=\"tmp\"", replacement="id=\"m\"", x=mzML))
})

test_that("exportMzMl,MassSpectrum", {
  MALDIquantForeign:::exportMzMl(m, file=file.path(tmp, "ms.mzML"))
  expect_equal(readLines(file.path(tmp, "ms.mzML")),
               sub(pattern="id=\"tmp\"", replacement="id=\"ms\"", x=mzML))
})

test_that("exportMzMl,list", {
  spectra <- list(m, m)
  MALDIquantForeign::exportMzMl(spectra, path=tmp, force=TRUE)
  expect_equal(readLines(file.path(tmp, "TESTS_1.mzML")),
               sub(pattern="id=\"tmp\"", replacement="id=\"TESTS_1\"", x=mzML))
  expect_equal(readLines(file.path(tmp, "TESTS_2.mzML")),
               sub(pattern="id=\"tmp\"", replacement="id=\"TESTS_2\"", x=mzML))
})
