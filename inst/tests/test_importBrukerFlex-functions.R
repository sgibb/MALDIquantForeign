context("importBrukerFlex")

test_that("importBrukerFlex", {
  expect_error(MALDIquantForeign:::.importBrukerFlex("tmp.tmp"))

  path <- file.path("data", "brukerflex", "0_A1", "1", "1SLin", "fid")
  s <- MALDIquantForeign:::.importBrukerFlex(path)

  expect_equal(s, import(path))
  expect_equal(s, importBrukerFlex(path))
  expect_equal(s, import(path, type="fid"))

  expect_equal(trunc(mass(s[[1]])), 226:230)
  expect_equal(intensity(s[[1]]), 1:5)
  expect_equal(basename(metaData(s[[1]])$file), "fid")
  expect_equal(metaData(s[[1]])$laserShots, 100)
  expect_equal(metaData(s[[1]])$comments, paste("TESTSAMPLE", 1:4, sep=""))
})

