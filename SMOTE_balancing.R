# Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
SMOTE_balancing <- function(dataset) {
  factor(dataset$target)
  # nrow(train_data_subset)
  # builds the contigency table to count the frequency distribution
  print('---Labels frequency ---')
  table(dataset$target)
  # SMOTE algorithm requires dataframe input at trainning data and factor for target data.
  dataset$target = as.factor(dataset$target)
  # majority_class = nrow(dataset$target==0)
  # minority_class = nrow(dataset$target==1)
  perc_oversample = signif((72343 / 3949) * 100, digits = 4)
  balanced_dataset <- SMOTE(
    target ~ .,
    dataset,
    perc.over = perc_oversample,
    k = 10,
    #number of nearest neightbors used
    perc.under = 108
  )
  print('---Dimensions of dataset after SMOTE---')
  dim(balanced_dataset)
  print('---Frequency of new labels---')
  table(balanced_dataset[, 'target'])
  
  # Figures of data
  par(mfrow = c(1, 2))
  plot(dataset[, 1],
       dataset[, 2],
       pch = 19 + as.integer(dataset[, 3]),
       main = "Original Data")
  plot(
    balanced_dataset[, 1],
    balanced_dataset[, 2],
    pch = 19 + as.integer(balanced_dataset[, 3]),
    main = "SMOTE'd Data"
  )
}