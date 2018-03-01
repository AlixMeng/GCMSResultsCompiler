#Testing Helper Functions
context("Testing Helper Functions")

test_that("Elements data are loaded", {
  expect_true(all(c("C", "H", "Re") %in% elements))
  expect_equal(length(elements), 119)
})

test_that("Heteroatoms Validation works", {
  expect_equal(check_heteroatoms(NULL), elements)

  expect_message(check_heteroatoms(c("N","O")), "Adding C, H to heteroatoms list")
  expect_message(check_heteroatoms(c("H","N","O")), "Adding C to heteroatoms list")
  expect_message(check_heteroatoms(c("C","N","O")), "Adding H to heteroatoms list")

  expect_equal(check_heteroatoms(c("N","O")), c("C", "H", "N", "O"))
  expect_equal(check_heteroatoms(c("H", "N","O")), c("C", "H", "N", "O"))
  expect_equal(check_heteroatoms(c("C", "N","O")), c("H", "C", "N", "O"))

  expect_equal(check_heteroatoms(list("C", "H", "N", "O")), c("C", "H", "N", "O"))

  expect_error(check_heteroatoms(c(1,2,3)), "Heteroatoms must be a character only vector")
  expect_error(check_heteroatoms(c(1,"C","H")), "Heteroatoms must be a character only vector")

  expect_message(check_heteroatoms(c("BOB", "C", "H")), "Removed 1 non-atoms from heteroatom list: BOB")
  expect_equal(check_heteroatoms(c("BOB", "C", "H")), c("C", "H"))

  expect_equal(check_heteroatoms(c("ALICE", "BOB")), c("C", "H"))
})

test_that("Element Counter Works", {
  expect_equal(element_count("C12H24", "C"), 12)
  expect_equal(element_count("C12H24", "H"), 24)
  expect_equal(element_count("C12H24", "O"), 0)
  expect_equal(element_count("C12H24", "Ca"), 0)
  expect_equal(element_count("Ca12H24", "C"), 0)
  expect_equal(element_count("C12H24", 12), 0)
  expect_equal(element_count("Ca12CH24", "C"), 1)
  expect_equal(element_count("Ca12CH24", "Ca"), 12)
})

test_that("Key Values Work", {
  kvdf<-data.frame(key = letters[1:4], value=LETTERS[1:4], stringsAsFactors = FALSE)
  expect_equal(extract_key_value(kvdf, 'a'), 'A')
  expect_equal(extract_key_value(kvdf, 'c'), 'C')
  expect_error(extract_key_value(kvdf, 'z'), "z not found in source file headers.")
})
