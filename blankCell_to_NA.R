blankCell_to_NA <- function (dataset) {
  dataset[dataset == ""] = NA
  return(dataset)
}