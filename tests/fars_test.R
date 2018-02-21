fars_test<-function(){
  require(testthat)

#test for the make_filename function
expect_that(make_filename(1983), equals("accident_1983.csv.bz2"))



print("All tests for farstools have passed")
}
