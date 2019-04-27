  # library(read.table)
  loadData <- function(path_csv) {
    
    files = list.files(path = path_csv, pattern = '*.csv', all.files = TRUE)
    cat('The csv files: ', files)
    print('set path')
    setwd(path_csv)
    # <<- : global 
    train <<-data.frame(read.csv(file = files[2],
                                 header = TRUE,
                                 sep = ",",
                                 encoding = "UTF-8"
    ))
    
    test <<- data.frame(read.csv(file = files[1],
                                 header = TRUE,
                                 sep = ",",
                                 encoding = "UTF-8"
    ))
}

