context("importCiphergenXml")

test_that("importCiphergenXml", {
  expect_error(MALDIquantForeign:::.importCiphergenXml("tmp.tmp"))

  path <- file.path("data", "ciphergen", "tiny.xml")
  s <- MALDIquantForeign:::.importCiphergenXml(path)

  expect_equal(s, import(path))
  expect_equal(s, importCiphergenXml(path))
  expect_equal(s, import(path, type="ciph"))

  expect_equal(trunc(mass(s[[1]])), rep(26, 5))
  expect_false(all(mass(s[[1]])[1] == mass(s[[1]])))
  expect_equal(intensity(s[[1]]), 1:5)
  expect_equal(basename(metaData(s[[1]])$file), "tiny.xml")
  expect_equal(metaData(s[[1]])$name, "tiny example")
})
