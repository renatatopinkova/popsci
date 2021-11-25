
# Load libraries ----------------------------------------------------------

library(dplyr)
library(stringr)
library(udpipe)
library(stringi)
library(tidytext)
library(topicmodels)


# increase memory limit, otherwise the loops keeps randomly failing
memory.limit(25000)


# Loading data ------------------------------------------------------------

data_names <- list.files("../data/context-processed", pattern = ".csv")
data_names <- str_extract(data_names, "[^\\.]+")

data_paths <- list.files("../data/context-processed", pattern = ".csv", full.names = TRUE)

data_list <- lapply(data_paths, read.csv2, encoding="UTF-8")
names(data_list) <- data_names


# Pre-processing ----------------------------------------------------------

# remove all numbers and punctuation & convert to lowercase
for (i in 1:length(data_list)) {
  data_list[[i]] <- distinct(data_list[[i]], context, keyword, .keep_all = T)
  data_list[[i]]$context <- str_trim(data_list[[i]]$context)
  data_list[[i]]$context <- str_replace_all(data_list[[i]]$context, pattern="[0-9]+|[[:punct:]]|\\(.*\\)", "")
  data_list[[i]]$context <- tolower(data_list[[i]]$context)
}

## Remove everything that is not from the default sites

### Replace - in names for . to match website names 
names(data_list) <- str_replace_all(names(data_list), "_", ".")

# Check: How much data we originally had
cdf<-data.frame()
for (i in 1:length(data_list)) {
  cdf[i,1] <- (names(data_list[i]))
  cdf[i,2] <- (nrow(data_list[[i]]))
}



# Keep only data that matches pattern (i.e., data from the default websites)
for (name in names(data_list)) {
  # pasting the pattern
  # ? lookbehind: no character allowed before the website name (prohibits matchin "hamu.cz" while searching for "amu.cz")
  pattern <- paste("(?<![a-z])", name, sep="")
  data_list[[name]] <- data_list[[name]] %>% 
    filter(str_detect(web, pattern)) %>%
    # remove duplicities
    group_by(web, keyword) %>% 
    distinct(context) %>%
    ungroup()
}



# Check: Add data about new dataframe, calculate losses
for (i in 1:length(data_list)) {
  cdf[i,3] <- (nrow(data_list[[i]]))
  cdf[i,4] <- cdf[i,3]/cdf[i,2]
}


# Export
# saveRDS(data_list, "context_data_list_clean")
# Load
# data_list <- readRDS("context_data_list")

# Lemmatization -----------------------------------------------------------


# Load library
# library(udpipe)

# Load Czech language
udmodel_czech <- udpipe_load_model(file = "czech-pdt-ud-2.5-191206.udpipe")


# ! DO NOT RUN AGAIN, takes ~ 8h: 
# Load "lemma_list" RDS file instead
# Lemmatize each list (=website)
lemma_list <- list()
for (name in names(data_list)) {
  if (nrow(data_list[[name]] > 0)) {
  print(paste("working on", name))
  print(Sys.time())
  lemma_list[[name]] <- udpipe_annotate(udmodel_czech, x = data_list[[name]]$context, tagger = "default", parser = "none")
  } else {
    print(paste("Length of", names(data_list[i]), "is zero", sep=" "))
  }
}


# Save
# saveRDS(lemma_list, "lemma_list")
# Load
# lemma_list <- readRDS("lemma_list")


# Convert udpipe objects to data.frames
## Try lapply first, if fails, use for loop (to see where it fails to resume from there):
df <- lapply(lemma_list, as.data.frame)
# for (name in names(lemma_list)) {
#  print(paste("Now working on", name, sep = " "))
#  df[[name]] <- as.data.frame(lemma_list[[name]])
# }

# saveRDS(df, "df_lemmatized")



# convert each list to corpus
for (name in names(lemma_list)) {
  df[[name]] <- as.data.frame(table(df[[name]]$lemma))
  # add column that denotes from which site it is
  df[[name]]$document <- name
}

# converting lists to one big data frame
df_full <- data.frame()
for (name in names(lemma_list)) {
  df_full <- rbind(df_full, df[[name]])
}

#saveRDS(df_full, "df_full_pre-processed")
# df_full <- readRDS("df_full_pre-processed")


# Further pre-processing --------------------------------------------------


# Load stop words
stop_words_cz <- read.delim("stop_words_czech_R.txt", encoding="UTF-8", header = F, quote="\"", comment.char="")
stop_words_cz <- stop_words_cz$V1

# bind to a tibble, so it contains both word and lexicon, and can be used instead of a en file
my_stop_words <- tibble(word = stop_words_cz, lexicon="custom")

# convert to lowecase
df_full$Var1 <- tolower(df_full$Var1)
# remove whitespace
df_full$Var1 <- str_trim(df_full$Var1, side="both")

# pattern = only words that are made of letters, no other chars
# this version only works when text is without accents -> text has to be converted first!
pattern <- "^(:?[a-z]*)$"
# str_view(df_full, pattern, match = T)


df_processed <-
  df_full %>%
  # filter out stop words
  filter(!(Var1 %in% my_stop_words$word)) %>%
  # filter out english stop words too
  filter(!Var1 %in% stop_words$word) %>%
  # remove accents
  mutate(Var1 = stri_trans_general(Var1, "Latin-ASCII")) %>%
  # only words of length 3+
  filter(str_length(Var1) > 2) %>%
  # remove words that are longer than 25 chars (mistakes, leftover hyperlinks)
  filter(str_length(Var1) < 25) %>%
  # only words that contain only letters (no digits, special chars)
  filter(str_detect(Var1, pattern)) 



# Topic models ------------------------------------------------------------

### This model is featured in the paper

# cast to document-frame matrix
popsci_dtm <- df_processed %>%
  cast_dtm(document, Var1, Freq)  

# topic model w/ 10 topics
ten_lda <- LDA(popsci_dtm, k = 10, control = list(seed = 1234))
# saveRDS(ten_lda, "ten_lda_all")  

ten_topics <- tidy(ten_lda, matrix = "beta")

ten_top_terms <- ten_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% 
  ungroup() %>%
  arrange(topic, -beta)


ten_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# show which documents belong to which topics
ten_gamma <- tidy(ten_lda, matrix = "gamma")

ten_gamma <- popsci_gamma %>%
    group_by(document) %>%
    filter(gamma == max(gamma))

ten_gamma <- ten_gamma %>% inner_join(def_web, by=c("document" = "web"))

ggsave("../grafy/ten_lda_final.png")



# Topic models: Alternative solutions  ------------------------------------

## Five topics --------------------------------------------------------------

# topic model w/ 5 topics
five_lda <- LDA(popsci_dtm, k = 5, control = list(seed = 1234))

five_topics <- tidy(five_lda, matrix = "beta")

five_top_terms <- five_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% 
  ungroup() %>%
  arrange(topic, -beta)


five_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


# show which documents belong to which topics
five_gamma <- tidy(five_lda, matrix = "gamma")

five_gamma <- five_gamma %>%
  group_by(document) %>%
  filter(gamma == max(gamma))

five_gamma <- five_gamma %>% inner_join(def_web, by=c("document" = "web"))


## Thirty topics -----------------------------------------------------------


# model w/ 30 topics
thirty_lda <- LDA(popsci_dtm, k = 30, control = list(seed = 1234))
# saveRDS(thirty_lda, "thirty_lda_full")


thirty_topics <- tidy(thirty_lda, matrix = "beta")

thirty_top_terms <- thirty_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% 
  ungroup() %>%
  arrange(topic, -beta)


thirty_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


## A solution where each text is a document --------------------------------


df <- readRDS("df_lemmatized")

### NEW PARTS

df2 <- list()
for (name in names(df)) {
  df2[[name]] <- select(df[[name]], doc_id, lemma)
  df2[[name]]$document <- name
}

df_full <- data.frame()
for (name in names(df2)) {
  df_full <- rbind(df_full, df2[[name]])
}


df_full$doc_id <- substring(df_full$doc_id, 4)
df_full$doc <- paste0(df_full$document, "_", df_full$doc_id)

df_full$lemma <- tolower(df_full$lemma)
# remove whitespace
df_full$lemma <- str_trim(df_full$lemma, side="both")

# pattern = only words that are made of letters, no other chars
# this version only works when text is without accents -> text has to be converted first!
pattern <- "^(:?[a-z]*)$"

df_clean <- df_full %>%
  # filter out stop words
  filter(!(lemma %in% my_stop_words$word)) %>%
  # remove accents
  mutate(word = stri_trans_general(lemma, "Latin-ASCII")) %>%
  # only words of length 3+
  filter(str_length(word) > 2) %>%
  # remove words that are longer than 25 chars (mistakes, leftover hyperlinks)
  filter(str_length(word) < 25) %>%
  # only words that contain only letters (no digits, special chars)
  filter(str_detect(word, pattern)) %>%
  select(document, doc, word)


df_processed <- df_clean %>%
  group_by(doc) %>%
  count(word) %>%
  ungroup()

popsci_dtm <- df_processed %>%
  cast_dtm(doc, word, n) 

#saveRDS(popsci_dtm, "popsci_dtm")
# popsci_dtm <- readRDS("popsci_dtm)

# topic model w/ 10 topics
ten_lda <- LDA(popsci_dtm, k = 10, control = list(seed = 1234))


ten_topics <- tidy(ten_lda, matrix = "beta")

ten_top_terms <- ten_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)


ten_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
## same results as the solution that does not account for each text


## this crashes the system - cannot be done on this pc!
thirty_lda <- LDA(popsci_dtm, k = 30, control = list(seed = 1234))




