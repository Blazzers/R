algorithm_xgboost <- function(dataset) {
  sparse_matrix <-
    sparse.model.matrix(target ~ ., data = dataset)[, -1]   #remove the target col from the sparse matrix
  
  parameters = list(
    eta = 0.1,
    # 0<eta<1 robust to overfit(but slower)<..<less robust to overfit but faster
    gamma = 0.1,
    max.depth = 15,
    subsample = 0.5,
    num_parallel_tree = 10,
    lambda = 0.1,
    objective = 'binary:logistic',
    eval_metric = 'logloss' #more roboust on data skeweness 
  )
  
  boostTree <<- xgboost(
    params = parameters,
    data = data,
    label = dataset[, -1],
    nrounds = 1000,
    #watchlist = evaluation data
    # early_stopping_rounds = 5
    missing = NA,
    weight = NULL
  )
  return(boostTree)
}
