context("createMassObject")

test_that(".createMassObject", {
  expect_true(isMassSpectrum(
    MALDIquantForeign:::.createMassObject(
      list(mass=1:5, intensity=1:5), list())))
  expect_true(isMassSpectrum(
    MALDIquantForeign:::.createMassObject(
      list(mass=1:5, intensity=1:5), list(), centroided=FALSE)))
  expect_true(isMassPeaks(
    MALDIquantForeign:::.createMassObject(
      list(mass=1:5, intensity=1:5), list(), centroided=TRUE)))
  expect_true(isMassPeaks(
    MALDIquantForeign:::.createMassObject(
      list(mass=1:5, intensity=1:5),
      list(dataProcessing=list(centroided="1")))))
  expect_true(isMassPeaks(
    MALDIquantForeign:::.createMassObject(
      list(mass=1:5, intensity=1:5), list(centroided="1"))))
  expect_true(isMassPeaks(
    MALDIquantForeign:::.createMassObject(
      list(mass=c(1, 5, 7), intensity=1:3), list())))
})

