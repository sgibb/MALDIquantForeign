context("ibd")

s <- list(createMassSpectrum(mass=1:5, intensity=1:5),
          createMassSpectrum(mass=1:5, intensity=6:10))

test_that(".writeIbd", {
  #uuid <- "3858f622-30ac-4c91-9f30-0c664312c63f"
  #file <- tempfile()
  #MALDIquantForeign:::.writeIbd(filename=file, uuid=uuid)
})

test_that(".ibdOffsets", {
  processed <- matrix(c(cumsum(c(16L, rep.int(40L, 3L))),
                        rep.int(5L, 4L), rep.int(40L, 4L)), nrow=4L)
  continuous<- matrix(c(16L, 56L, 16L, 96L, rep.int(5L, 4L),
                      rep.int(40L, 4L)), nrow=4L)
  dimnames(processed) <- dimnames(continuous) <-
    list(c("mass.1", "intensity.1", "mass.2", "intensity.2"),
         c("offset", "length", "encodedLength"))

  expect_identical(MALDIquantForeign:::.ibdOffsets(s, processed=TRUE),
                   processed)
  expect_identical(MALDIquantForeign:::.ibdOffsets(s, processed=FALSE),
                   continuous)
})

