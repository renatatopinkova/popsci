# Packages and custom functions -------------------------------------------

library(tidyverse)

source("99_custom_functions.R") #loads a script with custom functions

# Data sets names ---------------------------------------------------------

data_names <- list.files("data-unmerged", pattern = ".csv")
data_names <- str_extract(data_names, "[^\\.]+")


# Merging partial data sets -----------------------------------------------

merged_list <- merge_parted(data_names, folder = "data-unmerged") #merges partial data sets


# Export data -------------------------------------------------------------

lapply(seq_along(merged_list), function(i) write.csv2(merged_list[[i]], 
                                                file = paste0("data-processed/", names(merged_list[i]), ".warc.gz-network_data.csv"),
                                                row.names = FALSE)) # writes the merged data sets as .csv files into "data-processed" folder.
                                                                    # The function(i) is called anonymous function, it's basically
                                                                    # a function created just for the loop and then discarded
                                                                    # (doesn't even get a name, unlike other custom functions).
                                                                    # What this does is: "For each element "i" of the list merged_list,
                                                                    # write "i" as .csv file with file path
                                                                    # "data-processed/(name of the inth element).warc.gz-network_data.csv"


