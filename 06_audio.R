library(tidyverse)
library(tidytext)
library(stringr)

# Load data ---------------------------------------------------------------


data_names <- list.files("../data/webpage-processed", pattern = ".csv")
data_names <- str_extract(data_names, "[^\\.]+")

data_paths <- list.files("../data/webpage-processed", pattern = ".csv", full.names = TRUE)

data_list <- lapply(data_paths, read.csv2, encoding="UTF-8")
names(data_list) <- str_replace_all(data_names, "_", ".")


# Pre-processing ----------------------------------------------------------


# Check: Are all 'adress' links unique?
for (name in names(data_list)) {
  print(paste(name, "is unique? ", length(unique(data_list[[name]]$adress)) == length(data_list[[name]]$adress)))
}

# -> Yes.


# However, some websites use backlinks
# -> leads to multiple duplicates
# requires manually looking into the files to find those that use backlinks

# how many of the links in the files is about the default web (also sanity check for data import)
for (name in names(data_list)) {
sum <- round(sum(str_detect(data_list[[name]]$web, name))/nrow(data_list[[name]]),2)
rows <- sum(str_detect(data_list[[name]]$web, name))
print(paste(name, "contains", sum, "information from the original web"))
print(paste(name, "has", rows, "rows"))
}


## keep only information from the default webs

data_subset <- list()

for (name in names(data_list)) {
data_subset[[name]] <- data_list[[name]][str_detect(data_list[[name]]$web, name), ]
}

## additional cleaning - amu catches also hamu, damu etc. - use lookbehind
data_subset$amu.cz <- data_subset$amu.cz[str_detect(data_subset$amu.cz$web, "(?<![a-z])amu.cz"), ]


## count n of occurences of each tag
summarise_frames <- function (name) {
  sum <- summarise(data_subset[[name]], iframe_sum = sum(iframe),
            ampiframeN_sum = sum(ampiframeN),
            embed_sum = sum(embed),
            object_sum = sum(object),
            video_sum = sum(video),
            audio_sum = sum(audio),
            total_media = sum(ampiframeN_sum, iframe_sum, embed_sum, object_sum, video_sum, audio_sum))
  name <- name
  resp <- cbind(name, sum)
  return(resp)
  }


# initialize data frame
summary_stats <- data.frame()
# bind summary stats from each list to a data frame
for (name in names(data_subset)) {
  summary_stats <- rbind(summary_stats, summarise_frames(name))
}

# get dataframe with n of webs (n of rows) from each web
rows_n <- data.frame(name = names(data_subset), n = sapply(data_subset, nrow))

# join with n of webs 
summary_stats <- summary_stats %>% full_join(rows_n, by="name")

summary_stats <- summary_stats %>%
  mutate(iframe_prop = round(iframe_sum/n,3),
            ampiframeN_prop = round(ampiframeN_sum/n,3),
            embed_prop = round(embed_sum/n,3),
            object_prop = round(object_sum/n,3),
            video_prop = round(video_sum/n,3),
            audio_prop = round(audio_sum/n,3),
            total_prop = round(total_media/n,3))

write.csv(summary_stats, "media_summary_stats.csv")
