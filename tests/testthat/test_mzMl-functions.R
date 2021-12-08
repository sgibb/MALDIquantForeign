context("mzMl")

test_that(".writeImzMlScanList", {
  m <- createMassSpectrum(mass=1:5, intensity=6:10,
                          metaData=list(imaging=list(pos=c(x=1e8, y=1e8))))
  f <- file.path(tempdir(), "imzlscanlist")

  r <- c(
"    <scanList count=\"1\">",
"     <scan>",
"      <cvParam cvRef=\"IMS\" accession=\"IMS:1000050\" name=\"position x\" value=\"100000000\"/>",
"      <cvParam cvRef=\"IMS\" accession=\"IMS:1000051\" name=\"position y\" value=\"100000000\"/>",
"     </scan>",
"    </scanList>"
)

  expect_error(.writeImzMlScanList(1:5, file=f), "MassSpectrum.*supported")
  MALDIquantForeign:::.writeImzMlScanList(m, file=f)
  expect_equal(readLines(f), r)
})
