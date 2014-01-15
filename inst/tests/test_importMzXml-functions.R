context("importMzXml")

test_that("importMzXml", {
  expect_error(MALDIquantForeign:::.importMzXml("tmp.tmp"))

  path <- file.path("data", "tiny1.mzXML3.0.mzXML")
  s <- MALDIquantForeign:::.importMzXml(path)

  expect_equal(s, import(path))
  expect_equal(s, importMzXml(path))
  expect_equal(s, import(path, type="mzXML"))

  expect_true(isMassSpectrum(s[[1]]))
  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "tiny1.mzXML3.0.mzXML")
})

test_that("importMzXml compressed", {
  expect_error(MALDIquantForeign:::.importMzXml("tmp.tmp"))

  path <- file.path("data", "tiny1-compressed.mzXML3.0.mzXML")
  s <- MALDIquantForeign:::.importMzXml(path)

  expect_equal(s, import(path))
  expect_equal(s, importMzXml(path))
  expect_equal(s, import(path, type="mzXML"))

  expect_true(isMassSpectrum(s[[1]]))
  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file),
               "tiny1-compressed.mzXML3.0.mzXML")
})

test_that("importMzXml centroided", {
  expect_error(MALDIquantForeign:::.importMzXml("tmp.tmp"))

  path <- file.path("data", "tiny1-centroided.mzXML3.0.mzXML")
  p <- MALDIquantForeign:::.importMzXml(path)

  expect_equal(p, import(path))
  expect_equal(p, importMzXml(path))
  expect_equal(p, import(path, type="mzXML"))

  expect_true(isMassPeaks(p[[1]]))
  expect_equal(mass(p[[1]]), 1:5)
  expect_equal(intensity(p[[1]]), 6:10)
  expect_equal(basename(metaData(p[[1]])$file),
               "tiny1-centroided.mzXML3.0.mzXML")
})

