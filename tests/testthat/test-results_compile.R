#Testing Results Compiler Functions
context("Testing Results Compiler Functions")

test_that("Compile Reports Does proper checks", {
  examplefile <- system.file("extdata", "testreport.xls", package = "GCMSResultsCompiler", mustWork = TRUE)
  examplepath <- dirname(examplefile)

  expect_error(compile_reports(".", write_pdf = FALSE, write_csv = FALSE), "Must generate either PDF or CSV files")
  expect_error(compile_reports("."), "No .xls\\(x\\) files found in source_file_dir: .")
  expect_message(compile_reports(examplepath, "./reports"), "Creating results folder ./reports")

  expect_true(file.exists("./reports/alkanes_by_c.csv"))
  expect_true(file.exists("./reports/alkanes_by_ch.csv"))
  expect_true(file.exists("./reports/alkanes_by_all.csv"))
  expect_true(file.exists("./reports/alkanes_breakdown.pdf"))

  teardown({
    unlink("./reports", recursive = TRUE)
  })

})
