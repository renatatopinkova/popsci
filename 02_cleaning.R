# Packages and custom functions -------------------------------------------

library(tidyverse)

source("99_custom_functions.R")

# Import data -------------------------------------------------------------


data_names <- list.files("data-processed", pattern = ".csv")
data_names <- str_extract(data_names, "[^\\.]+")

data_paths <- list.files("data-processed", pattern = ".csv", full.names = TRUE)

data_list <- lapply(data_paths, read.csv2)
names(data_list) <- data_names


# Clean website names -----------------------------------------------------

data_list <- lapply(data_list, clean_edgelist)

data_list <- lapply(data_list, remove_loops)

# Export data -------------------------------------------------------------

lapply(seq_along(data_list), function(i) write.csv2(data_list[[i]], 
                                                      file = paste0("data-cleaned/", names(data_list[i]), ".warc.gz-network_data.csv"),
                                                      row.names = FALSE)) #writes the merged datasets as .csv files into "data-cleaned" folder
