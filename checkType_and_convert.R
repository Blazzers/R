#Many alforithms require the numeric conversion...but the datatype can modified in line 10

checkType_and_convert <- function(dataset) {
  print("--The dimentions,the stored class type --")
  print(dim(dataset))
  print(class(dataset))
  print('--Convert variables to numeric--')
  for (i in 1:ncol(dataset)) {
    if (class(dataset[, i]) != 'numeric') {
      print(class(as.numeric(as.character(dataset[, i]))))
      # labels = as.numeric(as.character(balanced_train[,'target']))
    }
    else{
      status = 1
    }
  }
  if (status == 1)
    print('--Every attribute is already in <numeric> type stored class.--')
  return(dataset)
}