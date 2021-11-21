
library(here)
source(here::here("ReadSequences.R"))
source(here::here("NorwalkHelpers.R"))

testthat::test_that("Regex for reading accession numbers works", {
  testthat::expect_equal("NC1.1", ReduceAccession("NC1.1.6"))
  testthat::expect_equal("OK2", ReduceAccession("OK2.3"))
  testthat::expect_equal("AH6", ReduceAccession("AH6.0 | what1.1"))
  testthat::expect_equal("HM7", ReduceAccession("HM7.2 | 12.1.2.1....."))
  testthat::expect_equal("YES", ReduceAccession("YES"))
  testthat::expect_equal("Bad|5yntax5|Here", ReduceAccession("Bad|5yntax5|Here.0"))
  testthat::expect_equal("", ReduceAccession(""))
})

testthat::test_that("Maximum accession value is correct", {
  testthat::expect_equal("A", NewestAccessions("A"))
  testthat::expect_equal("A.1.2", NewestAccessions(c("A.1.1", "A.1.2")))
  testthat::expect_equal(c("A.1", "B.1"), NewestAccessions(c("A.1", "B.1")))
  testthat::expect_equal("", NewestAccessions(""))
  
  # TODO: Implement full dupe check or x[1]
  #testthat::expect_equal("A|B", NewestAccessions(c("A|B", "A|B")))
  
  # TODO: Perhaps implement favorite metadata?
  #testthat::expect_equal("A.2|A.3", NewestAccessions(c("A.2|A.3", "A.2|A.4")))
  #testthat::expect_equal("C.2|C.3", NewestAccessions(c("C.2|C.4", "C.2|C.3")))
  
  acc = c("A.1.2", "A.2.1", "A.3.3.3.3", "A.1.1", "A.3.3.3.4", "A.5.3 | A.5.1", "A.5.2| A.5")
  intent = c("A.2.1", "A.1.2", "A.3.3.3.4", "A.5.3 | A.5.1")
  actual = NewestAccessions(acc)
  
  testthat::expect_equal(intent, actual)
})
  