
# A module which receives the dataset with date in format yyyy-mm-dd or yyyy/mm/dd and automaticaly
#..separates the date columns and attach to the original dataset without
# the previous date coloumns.

DateProc_module <- module({
  DateProcessing <- function(dataset) {
    column_name <- function(dataset, index_date_col) {
      return(colnames(dataset[index_date_col]))
    }
    
    separate_cols <- function(dataset, col_date_index) {
      #Step 2
      col_names <- column_name(dataset, col_date_index)
      for (i in 1:length(col_names)) {
        dataset <- separate(dataset,
                            col = col_names[i],
                            into =
                              c(
                                paste0('year_', col_names[i]),
                                paste0('month_', col_names[i]),
                                paste0('day_', col_names[i])
                              ))
      }
      return(dataset)
    }
    
    # Search for dates in format: yyyy-mm-dd in T(n)=O(nxm).We will optimize it with binary search later)
    find_date_cols <- function(dataset) {
      cat(
        '1. Automatic search in T=O(NxM) in a dataframe with columns of date format
        < yyyy-mm-dd or yyyy-mm-d or yyyy-m-dd or yyyy-m-d >
        2. Automatic separation, new named columns and attachment of new columns'
      )
      status_date_regexp <- 0
      dates_col_positions <-
        c() # yyyy-mm-dd or yyyy-mm-d or yyyy-m-dd or yyyy-m-d
      for (k in 1:ncol(dataset)) {
        for (l in 1:nrow(dataset)) {
          if (grepl(
            "[0-9]{2}-[0-9]{2}-[0-9]{2}|
            [0-9]{2}-[0-9]{2}-[0-9]{1}|
            [0-9]{2}-[0-9]{1}-[0-9]{2}|
            [0-9]{2}-[0-9]{1}-[0-9]{1}",
            dataset[l, k]
          ) == TRUE)
          {
            status_date_regexp <-
              1 # identify the existence or no of date format
          }
        }
        if (status_date_regexp == 1) {
          dates_col_positions <-
            append(dates_col_positions, k)  # identify which are the column dates
          
          print(paste0("Data column position at:", k))
          status_found <- 1
        } else{
          status_date_regexp <- 0
        }
        status_date_regexp <-
          0 #reset the status to recheck for the next coloumns and do not hold the  memory of a previous status
      }
      
      if (status_found == 1) {
        #found date format yyyy-mm-dd
        print(
          'Found a date in the given format
          Automatic separation and attachment begins for the columns:'
        )
        dates_separated = separate_cols(dataset, dates_col_positions) #separation begin
        return(dates_separated)
      } else{
        print("Did not found any date in given format")
      }
  }
    #Step 1
    dates_separated <- find_date_cols(dataset)
    return(dates_separated)
  }# end of DateProcessing
})
