#Testing Results Compiler Functions
context("Testing Results Compiler Functions")

test_that("Compile Reports Does proper checks", {
  examplefile <- system.file("extdata", "testreport.xlsx", package = "GCMSResultsCompiler")
  examplepath <- dirname(examplefile)

  expect_error(compile_reports(".", write_pdf = FALSE, write_csv = FALSE), "Must generate either PDF or CSV files")
  expect_error(compile_reports("."), "No .xlsx files found in source_file_dir: .")
  expect_message(compile_reports(examplepath, "./reports"), "Creating results folder ./reports")

  teardown()

})
