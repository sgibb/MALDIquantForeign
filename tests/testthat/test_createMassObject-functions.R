context("createMassObject")

test_that(".createMassObject", {
  expect_true(isMassSpectrum(
    MALDIquantForeign:::.createMassObject(
      list(mass=1:5, intensity=1:5), list())))
  expect_true(isMassSpectrum(
    MALDIquantForeign:::.createMassObject(
      list(mass=1:5, intensity=1:5), list(), centroided=FALSE)))
  expect_warning(MALDIquantForeign:::.createMassObject(
      list(mass=1:5, intensity=1:5), list(dataProcessing=list(centroided=0)),
      centroided=TRUE),
      paste0("According to the metadata information the imported data are ",
             "not centroided."))
  expect_warning(MALDIquantForeign:::.createMassObject(
      list(mass=1:5, intensity=1:5), list(centroided="1")),
      paste0("According to the metadata information the imported data are ",
             "centroided."))
  expect_true(suppressWarnings(isMassPeaks(
    MALDIquantForeign:::.createMassObject(
      list(mass=1:5, intensity=1:5), list(), centroided=TRUE))))
  expect_true(suppressWarnings(isMassPeaks(
    MALDIquantForeign:::.createMassObject(
      list(mass=1:5, intensity=1:5),
      list(dataProcessing=list(centroided="1")), centroided=TRUE))))
  expect_true(isMassSpectrum(
    MALDIquantForeign:::.createMassObject(
      list(mass=c(1, 5, 7), intensity=1:3), list())))
  expect_equal(mass(MALDIquantForeign:::.createMassObject(
      list(mass=1:10, intensity=1:10), massRange=c(4, 8))), 4:8)
  expect_equal(intensity(MALDIquantForeign:::.createMassObject(
      list(mass=1:10, intensity=1:10), minIntensity=5)), 5:10)
})
