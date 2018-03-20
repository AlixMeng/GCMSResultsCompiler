#Testing Results Compiler Functions
context("Testing Results Compiler Functions")

test_that("Compile Reports Does proper checks", {
  expect_error(compile_reports(".", write_pdf = FALSE, write_csv = FALSE), "Must generate either PDF or CSV files")
  expect_error(compile_reports("."), "No .xls\\(x\\) files found in source_file_dir: .")
})

test_that("Compile reports makes reports", {
  examplefile <- system.file("extdata", "testreport.xls", package = "GCMSResultsCompiler", mustWork = TRUE)
  examplepath <- dirname(examplefile)

  compile_reports(examplepath, "./reports", verbose=2)

  expect_true(file.exists("./reports/alkanes_by_c.csv"))
  expect_true(file.exists("./reports/alkanes_by_ch.csv"))
  expect_true(file.exists("./reports/alkanes_by_all.csv"))
  expect_true(file.exists("./reports/alkanes_breakdown.pdf"))

  teardown({
    unlink("./reports", recursive = TRUE)
  })
})
