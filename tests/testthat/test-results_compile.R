#Testing Results Compiler Functions
context("Testing Results Compiler Functions")

rm.file<-function(file){
  if(file.exists(file)){
    file.remove(file)
  }
}

rm.dir<-function(direct){
  if(dir.exists(direct)){
    if(length(list.files(direct))==0){
      file.remove(direct)
    } else {
      for (f in list.files(direct)){
        rm.file(f)
      }
      rm.dir(direct)
    }
  }
}


test_that("Compile Reports Does proper checks", {
  examplefile <- system.file("extdata", "testreport.xlsx", package = "GCMSResultsCompiler")
  examplepath <- dirname(examplefile)

  expect_error(compile_reports(".", write_pdf = FALSE, write_csv = FALSE), "Must generate either PDF or CSV files")
  expect_error(compile_reports("."), "No .xlsx files found in source_file_dir: .")
  expect_message(compile_reports(examplepath, "./reports"), "Creating results folder ./reports")

  expect_true(file.exists("./reports/by_c.csv"))
  expect_true(file.exists("./reports/by_ch.csv"))
  expect_true(file.exists("./reports/by_all.csv"))
  expect_true(file.exists("./reports/100-01_breakdown.pdf"))

  teardown({
    rm.dir("./reports")
  })

})
