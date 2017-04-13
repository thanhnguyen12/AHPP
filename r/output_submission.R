output_submission <- function(model, data) {
  # sale price prediction on test data
  saleprice <- predict(gbm, newdata = data)
  result <- data.frame(id = data$id, saleprice = exp(saleprice))
  
  # Name the output file with date stamp
  filename <- paste("submission_", Sys.Date(), ".csv", sep = "", collapse = "")
  write.csv(result, file = filename, row.names = F)
}