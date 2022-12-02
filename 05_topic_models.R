
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(udpipe)
library(stringi)
library(tidytext)
library(stm)
library(quanteda)
library(ggrepel)
library(ggthemes)

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
data_list <- readRDS("context_data_list_clean")


# Shortening context around words -----------------------------------------



shorten_texts <- function(df) {
  df %>% 
  # trim the keywords to version without regex chars 
  # (otherwise only matches if keyword in the beginning of string, etc.)
  mutate(keyword2 = str_extract(keyword, "[a-zA-Zěščřžýáíéůúďňť]+"),
         # remove additional whitespace
         context = str_squish(context)) %>%
  # match 0-10 words around the keyword, allowing for extra chars around word
    # i.e., "fyzik" would match "biofyzikální"
    # [^\\s] anything that isn't a whitespace (workaround for special chars in CZ)
    # + is safe, pre-processing before removed digits and other special chars.
  mutate(text_short = str_extract(context, regex(paste0("(?:[^\\s]+\\s){0,10}[a-zA-Zěščřžýáíéůúďňť]*", 
                                                        keyword2,"[a-zA-Zěščřžýáíéůúďňť]*(?:\\s[^\\s]+){0,10}"), ignore_case = TRUE)))
}


# get shorter text versions for all 
data_list_short <- list()
for (i in 1:length(data_list)) {
  data_list_short[[i]] <- shorten_texts(data_list[[i]])
  print(paste("finished", names(data_list[i]) ))
}

# stick back names
names(data_list_short) <- names(data_list)

# saveRDS(data_list_short, "data_list_short")

# Lemmatization -----------------------------------------------------------


# Load Czech language
udmodel_czech <- udpipe_load_model(file = "czech-pdt-ud-2.5-191206.udpipe")


# Lemmatize each list (=website)
lemma_list_short <- list()

for (name in names(data_list_short)) {
  if (nrow(data_list_short[[name]] > 0)) {
  print(paste("working on", name))
  print(Sys.time())
  lemma_list_short[[name]] <- udpipe_annotate(udmodel_czech, 
                                        x = data_list_short[[name]]$text_short, 
                                        tagger = "default", parser = "none")
  } else {
    print(paste("Length of", names(data_list_short[i]), "is zero", sep=" "))
  }
}



# lemma_list <- readRDS("lemma_list_short")
lemma_list <- lemma_list_short

# Convert udpipe objects to data.frames
df <- lapply(lemma_list, as.data.frame)


# saveRDS(df, "df_lemmatized_short")
# df <- readRDS('df_lemmatized_short')


# converting lists into df 
df2 <- list()
# reduce the lists
for (name in names(df)) {
  df2[[name]] <- select(df[[name]], doc_id, lemma)
  df2[[name]]$document <- name
}

# bind them into one big df
df_full <- data.frame()
for (name in names(df2)) {
  df_full <- rbind(df_full, df2[[name]])
}





# Further pre-processing --------------------------------------------------



# Load stop words
stop_words_cz <- read.delim("stop_words_czech_R.txt", encoding="UTF-8", header = F, quote="\"", comment.char="")

# bind to a tibbl + rowbind the english stopwords as well
my_stop_words <- tibble(word = stop_words_cz$V1, lexicon="custom") %>% 
  bind_rows(stop_words)



df_processed <-
  df_full %>%
  # paste doc_id and document together, to get 1 text = 1 document
  mutate(doc_id = substring(doc_id, 4),
         doc = paste0(document, "_", doc_id)) %>% 
  # filter out stop words
  filter(!(lemma %in% my_stop_words$word)) %>%
  # remove accents
  mutate(lemma = stri_trans_general(lemma, "Latin-ASCII")) %>%
  # only words of length 3+
  filter(str_length(lemma) > 2) %>%
  # remove words that are longer than 25 chars (mistakes, leftover hyperlinks)
  filter(str_length(lemma) < 25) %>%
  # only words that contain only letters (no digits, special chars)
  # this can only be done after removing accents!! (accents = special chars)
  filter(str_detect(lemma, "^(:?[a-z]*)$"))  %>% 
  # get counts per word per document
  group_by(doc, lemma) %>% 
  mutate(freq = n()) %>% 
  distinct(lemma, .keep_all = TRUE) %>% 
  ungroup()



# saveRDS(df_full, "df_full_pre-processed_short")
# df_full <- readRDS("df_full_pre-processed_short")

# Corpus ------------------------------------------------------------

### This model is featured in the paper

# cast to document-feature matrix
popsci_dtm <- df_processed %>%
  cast_dfm(doc, lemma, freq)  


# trimming the corpus (quanteda)
popsci_dtm_trim <- dfm_trim(popsci_dtm, 
                          max_docfreq = 0.5,
                          docfreq_type = "prop") %>% 
                  dfm_trim(min_docfreq = 500,
                          docfreq_type = "count")               

# lookup dimensions
dim(popsci_dtm_trim)

topfeatures(popsci_dtm_trim, n = 30, scheme = "docfreq")


# convert dfm to stm object (via stm package)
dfm_stm <- convert(popsci_dtm_trim, to = "stm")



# Topic models ------------------------------------------------------------

## TODO: Semantic cohorence
# collocation : limits

# calculate models k 10 to 50 (by 10s)
many_models <- data_frame(K = seq(10, 50, by = 10)) %>%
  mutate(topic_model = map(K, ~stm(documents = dfm_stm$documents, ## TODO: lookup furrr
                                          vocab = dfm_stm$vocab, 
                                          K = .,
                                          verbose = TRUE)))


# calculate exclusivity and semantic coherence for each model
k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, dfm_stm$documents, M = 10))

# get means of semantic coherence per model
k_result %>% 
  transmute(K,
            sem_coherence_mean = map_dbl(semantic_coherence, mean),
            excl_mean = map_dbl(exclusivity, mean),
            sem_coherence_median = map_dbl(semantic_coherence, median),
            excl_median = map_dbl(exclusivity, median)) %>% 
  ggplot(aes(sem_coherence_mean, excl_mean, color = as.factor(K))) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Sémantická koherence (průměr)",
       y = "Exkluzivita (průměr)",
       title = "Porovnání sémantické koherence a exkluzivity modelů") +
  theme_minimal() +
  geom_text_repel(aes(label = K), size = 3) +
  theme(legend.position = "none",
        plot.title = element_text(size = 10),
              axis.title=element_text(size=9),
              axis.text = element_text(size = 8))



topic_model_20 <- k_result %>% 
  filter(K == 20) %>% 
  pull(topic_model) %>% 
  .[[1]]


td_beta <- tidy(topic_model_20)

td_gamma <- tidy(topic_model_20, matrix = "gamma",
                 document_names = popsci_dtm_trim@Dimnames$docs)


top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest(terms)

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Téma ", topic),
         topic = reorder(topic, gamma))

plot_topics <- gamma_terms %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 2) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.13),
                     labels = scales::percent_format()) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 10),
        axis.text=element_text(size=7)) +
  labs(x = NULL, y = expression(gamma),
       title = "Prevalence témat v korpusu")



td_gamma <- td_gamma %>% 
  mutate(doc_orig = str_remove(document, "_\\d+"))

gamma_ord <- td_gamma %>% 
  group_by(document) %>% 
  arrange(desc(gamma)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank == 1) %>% 
  group_by(doc_orig) %>% 
  count(topic)

gamma_ord_short <- gamma_ord %>% 
  slice_max(n, n=3)
  
  