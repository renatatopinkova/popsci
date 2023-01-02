library(dplyr)
library(tidytext)
library(stringr)
library(stringi)
library(udpipe)
library(ggplot2)

# Load data ---------------------------------------------------------------


data_names <- list.files("../data/context-processed", pattern = ".csv")
data_names <- str_extract(data_names, "[^\\.]+")

data_paths <- list.files("../data/context-processed", pattern = ".csv", full.names = TRUE)

data_list <- lapply(data_paths, read.csv2, encoding="UTF-8")
names(data_list) <- str_replace_all(data_names, "_", ".")
# edit mistake from data import
names(data_list)[51] <- "jh-inst.cas.cz"

# Pre-processing ----------------------------------------------------------

# remove duplicites & all numbers & punctuation & convert to lowercase
for (i in 1:length(data_list)) {
  data_list[[i]]$context <- str_trim(data_list[[i]]$context)
  data_list[[i]] <- distinct(data_list[[i]], context, .keep_all = T)
  data_list[[i]]$context <- str_replace_all(data_list[[i]]$context, pattern="[0-9]+|[[:punct:]]|\\(.*\\)", "")
  data_list[[i]]$context <- tolower(data_list[[i]]$context)
  data_list[[i]] <- distinct(data_list[[i]], context, .keep_all = T)
}


# Select keywords to examine ----------------------------------------------

## POPULARIZACE
popularizace <- list()
for (i in 1:length(data_list)) {
  # look in text to find popularizace -> somehow, this is not a keyword?
  print(paste("looking into", names(data_list[i])))
  popularizace[[i]] <- data_list[[i]] %>% 
    filter(str_detect(keyword, "populari")) 
}

names(popularizace) <- names(data_list)
# saveRDS(popularizace, "popularizace_full_text")
popularizace <- readRDS("popularizace_full_text")

## VEDA
veda <- list()
for (i in 1:length(data_list)) {
  print(paste("looking into", names(data_list[i])))
  veda[[i]] <- data_list[[i]] %>% 
    filter(keyword == "^v?d.*") 
}

names(veda) <- names(data_list)
# saveRDS(veda, "veda_full_text")
veda <- readRDS("veda_full_text")

lapply(popularizace, nrow) %>% bind_rows() %>% 
  tidyr::pivot_longer(., cols = everything()) %>% 
  summarise(sum = sum(value))


for (i in 1:length(veda)) {
  veda[[i]] <- distinct(veda[[i]], context, .keep_all = T)
}

# veda_dejin <- data.frame()
# for (name in names(veda)) {
#   veda_dejin <- rbind(veda_dejin, filter(veda[[name]], str_detect(veda[[name]]$context, "dějin")))
# }

# nrow(veda_dejin)
# nrow(distinct(veda_dejin))



# Lemmatization -----------------------------------------------------------


# Load library
library(udpipe)

# Load Czech language
udmodel_czech <- udpipe_load_model(file = "czech-pdt-ud-2.5-191206.udpipe")

pattern <- "^(:?[a-z]*)$"

### POPULARIZACE
# Lemmatize each list (=website)
lemma_list_popularizace <- list()
for (name in names(data_list)) {
  if (nrow(popularizace[[name]] > 0)) {
    print(paste("working on", name))
    print(Sys.time())
    lemma_list_popularizace[[name]] <- udpipe_annotate(udmodel_czech, x = popularizace[[name]]$context, tagger = "default", parser = "none")
  } else {
    print(paste("Length of", names(data_list[i]), "is zero", sep=" "))
  }
}

# convert udpipe object to dataframe
df_popularizace <- lapply(lemma_list_popularizace, as.data.frame)

# convert each list to corpus
for (name in names(lemma_list_popularizace)) {
  df_popularizace[[name]] <- as.data.frame(table(df_popularizace[[name]]$lemma))
  # add column that denotes from which site it is
  df_popularizace[[name]]$document <- name
}

# converting lists to one big data frame
df_full_popularizace <- data.frame()
for (name in names(lemma_list_popularizace)) {
  df_full_popularizace <- rbind(df_full_popularizace, df_popularizace[[name]])
}

# saveRDS(df_full_popularizace, "df_full_popularizace")


### VEDA
lemma_list_veda <- list()
for (name in names(veda)) {
  if (nrow(veda[[name]] > 0)) {
    print(paste("working on", name))
    print(Sys.time())
    lemma_list_veda[[name]] <- udpipe_annotate(udmodel_czech, x = veda[[name]]$context, tagger = "default", parser = "none")
  } else {
    print(paste("Length of", names(veda[name]), "is zero", sep=" "))
  }
}


# convert udpipe object to dataframe
df_veda <- lapply(lemma_list_veda, as.data.frame)

# convert each list to corpus
for (name in names(lemma_list_veda)) {
  df_veda[[name]] <- as.data.frame(table(df_veda[[name]]$lemma))
  # add column that denotes from which site it is
  df_veda[[name]]$document <- name
}

# converting lists to one big data frame
df_full_veda <- data.frame()
for (name in names(lemma_list_veda)) {
  df_full_veda <- rbind(df_full_veda, df_veda[[name]])
}

saveRDS(df_full_veda, "df_full_veda")


# Load file with web type for later ---------------------------------------


# file with coded type 1 = hum/soc, 2 - nat/tech
def_web <- read.csv2("../default_weby_typ.csv", header=T)

# filter out names of webs by types
soc <- filter(def_web, typ==1) %>% select(web)
nat <- filter(def_web, typ==2) %>% select(web)

# Load stopwords ----------------------------------------------------------


# Load stopwords from a file
stop_words_cz <- read.delim("stop_words_czech_R.txt", encoding="UTF-8", header = F, quote="\"", comment.char="")
stop_words_cz <- stop_words_cz$V1

# bind to a tibble, so it contains both word and lexicon, and can be used instead of a en file
my_stop_words <- tibble(word = stop_words_cz, lexicon="custom")



# V?DA ------------------------------------------------------------
# this should be done simpler, will simplify later (DRY)


# graph
df_veda_clean <- df_full_veda %>%
  # filter out stop words
  filter(!(Var1 %in% my_stop_words$word)) %>%
  # filter out stop words
  filter(!(Var1 %in% stop_words)) %>%
  # remove accents
  mutate(Var1 = stri_trans_general(Var1, "Latin-ASCII")) %>%
  # only words of length 3+
  filter(str_length(Var1) > 2) %>%
  # remove words that are longer than 25 chars (mistakes, leftover hyperlinks)
  filter(str_length(Var1) < 25) %>%
  # only words that contain only letters (no digits, special chars)
  filter(str_detect(Var1, pattern)) %>%
  # remove everything related to keyword
  filter(!(str_detect(Var1, "^ved.*")))
  
df_veda_clean <- df_veda_clean %>% 
  inner_join(def_web, by = c("document"="web")) %>% 
  filter(typ <= 2) %>% 
  # group so words
  group_by(Var1, typ) %>%
  # count each word across documents
  mutate(n = sum(Freq)) %>%
  # keep each word just once
  distinct(Var1, typ, .keep_all = TRUE) %>%
  # convert frequency to proportion
  group_by(typ) %>% 
  mutate(prop = n/sum(n)*100) 



diff_veda <- df_veda_clean %>% 
  select(Var1, typ, prop) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "typ", values_from = "prop", names_prefix = "typ_") %>% 
  replace_na(list(typ_1 = 0,
                  typ_2 = 0)) %>% 
  mutate(diff = typ_1 - typ_2) 

diff_veda %>% 
  arrange(desc(abs(diff))) %>% 
  print(n =50)

diffs_clean_veda <- diff_veda %>% 
  mutate(bump_1 = ifelse(typ_1 < typ_2,
                         typ_1 - .25,
                         typ_1 + .25),
         bump_2 = ifelse(typ_1 < typ_2,
                         typ_2 + .25,
                         typ_2 - .25)) %>% 
  pivot_longer(cols = -c(Var1, diff), names_to = c(".value", "web"), names_sep = "_") %>% 
  mutate(prop = round(typ, 2),
         diff = round(diff, 2))

diffs_clean_veda %>% 
  slice_max(abs(diff), n = 28) %>% 
  plot_dumbbell() 


### VEDA in SOCIAL/HUM SCIENCES
veda_soc <- df_veda_clean %>%
  # group so words
  group_by(Var1) %>%
  # count each word across documents
  mutate(n = sum(Freq)) %>%
  # ungroup
  ungroup() %>%
  # keep each word just once
  distinct(Var1, .keep_all = TRUE) %>%
  # convert frequency to proportion
  mutate(prop = n/sum(n)) %>%
  # get top 50 words
  top_n(20, prop) %>%
  # sort
  arrange(Var1, desc(prop)) 
  
# reorder the position of Var1 by the relative frequency (rel) and plot
veda_soc %>%  
  ggplot(aes(reorder(Var1, prop), weight=prop)) + labs(x="Slovo", y="Proporce korpusu", title = "Společenské vědy") + coord_flip() + geom_bar() + theme_bw()

ggsave("../grafy/veda_socw_50.png")


### VEDA in NATURAL SCIENCES
veda_nat <- df_veda_clean %>%
  filter(document %in% nat$web) %>%
  # group so words
  group_by(Var1) %>%
  # count each word across documents
  mutate(n = sum(Freq)) %>%
  # ungroup
  ungroup() %>%
  # keep each word just once
  distinct(Var1, .keep_all = TRUE) %>%
  # convert frequency to proportion
  mutate(prop = n/sum(n)) %>%
  # get top 50 words
  top_n(50, prop) %>%
  # sort
  arrange(Var1, desc(prop))

veda_nat %>%
  # reorder the position of Var1 by the relative frequency (rel) and plot
  ggplot(aes(reorder(Var1, prop), weight=prop)) + labs(x="Slovo", y="Proporce korpusu", title = "P??rodn? v?dy") + coord_flip() + geom_bar() + theme_bw()

ggsave("../grafy/veda_natw_50.png")



# Popularizace by type ---------------------------------------------------------


# graph
df_popularizace_clean <- df_full_popularizace %>%
  # filter out stop words
  filter(!(Var1 %in% my_stop_words$word)) %>%
  #filter out stop words EN
  filter(!(Var1 %in% stop_words$word)) %>%
  # remove accents
  mutate(Var1 = stri_trans_general(Var1, "Latin-ASCII")) %>%
  # only words of length 3+
  filter(str_length(Var1) > 2) %>%
  # remove words that are longer than 25 chars (mistakes, leftover hyperlinks)
  filter(str_length(Var1) < 25) %>%
  # only words that contain only letters (no digits, special chars)
  filter(str_detect(Var1, pattern)) %>%
  # remove everything related to keyword
  filter(!(str_detect(Var1, "^populari.*"))) 
  
  
df_popularizace_clean %>%  # group so words
  group_by(Var1) %>%
  # count each word across documents
  mutate(n = sum(Freq)) %>%
  # ungroup
  ungroup() %>%
  # keep each word just once
  distinct(Var1, .keep_all = TRUE) %>%
  # convert frequency to proportion
  mutate(prop = n/sum(n)) %>%
  # get top 50 words
  top_n(20, prop) %>%
  # sort
  arrange(Var1, desc(prop)) %>%
  # reorder the position of Var1 by the relative frequency (rel) and plot
  ggplot(aes(reorder(Var1, prop), weight=prop)) + labs(x="Slovo", y="Proporce korpusu") + coord_flip() + geom_bar() + theme_bw()




### POPULARIZACE in SOCIAL/HUM SCIENCES
df_popularizace_clean <-  df_popularizace_clean %>% 
  inner_join(def_web, by = c("document"="web")) %>% 
  filter(typ <= 2) %>% 
  # group so words
  group_by(Var1, typ) %>%
  # count each word across documents
  mutate(n = sum(Freq)) %>%
  # keep each word just once
  distinct(Var1, typ, .keep_all = TRUE) %>%
  # convert frequency to proportion
  group_by(typ) %>% 
  mutate(prop = n/sum(n)*100) 


diff_pop <- df_popularizace_clean %>% 
  select(Var1, typ, prop) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "typ", values_from = "prop", names_prefix = "typ_") %>% 
  replace_na(list(typ_1 = 0,
                  typ_2 = 0)) %>% 
  mutate(diff = typ_1 - typ_2) 

diff_pop %>% 
  arrange(desc(abs(diff))) %>% 
  print(n =50)

diffs_clean <- diff_pop %>% 
  mutate(bump_1 = ifelse(typ_1 < typ_2,
                           typ_1 - .25,
                           typ_1 + .25),
         bump_2 = ifelse(typ_1 < typ_2,
                           typ_2 + .25,
                           typ_2 - .25)) %>% 
  pivot_longer(cols = -c(Var1, diff), names_to = c(".value", "web"), names_sep = "_") %>% 
  mutate(prop = round(typ, 2),
         diff = round(diff, 2))




plot_dumbbell <- function(df, color2 = "#15607a") {
  df %>% 
    ggplot(aes(x = prop, y = reorder(Var1, abs(-diff)), color = web)) +
    geom_line(color = "#E6E6E6", size = 1.75) +
    geom_point(size = 2) +
    labs(x = "Proporce korpusu", y = "Slovo") +
    geom_text(aes(label = glue("{prop}%"), x = bump), size = 3) +
    scale_color_manual(name = NULL, 
                       breaks = c("1", "2"),
                       values = c("#727272", color2),
                       labels = c("Přírodovědné", "Společenskovědní"))+
    scale_x_continuous(limits = c(-1,4),
                       breaks = seq(-1,4, by = 1),
                       labels = glue("{seq(-1,4, 1)}%")) +
    theme_minimal() +
    theme(legend.position =  c(1, 0.01),
          legend.justification = c("right", "bottom"),
          legend.background = element_rect(fill="white", linetype="blank"))
  
}


diffs_clean %>% 
  slice_max(abs(diff), n = 30) %>% 
  plot_dumbbell() 

## BW version
diffs_clean %>% 
  slice_max(abs(diff), n = 30) %>% 
  plot_dumbbell(color2 = "black") 
