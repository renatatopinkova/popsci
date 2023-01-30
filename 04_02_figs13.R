library(readr)
library(tidyverse)

# Read in data ------------------------------------------------------------


df_veda_clean <- readRDS("df_veda_clean")
df_popularizace_clean <- readRDS("df_popularizace_clean")

# word translations
transl <- read_delim("../data/helpers/fig_1-4.csv")


# Functions for plotting figs. 1 & 3 --------------------------------------------------
  
bar_chart_prep <- function(df){
  df %>% 
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
    arrange(desc(prop))
}


bar_chart <- function(df, lang = c("cz", "en")) {
  
  if (lang == "en") {
    x = "Word"
    y = "Corpus proportion"
    word = df$en
  } else {
    x = "Slovo"
    y = "Proporce korpusu"
    word = df$Var1
  }
  
  ggplot(df, aes(reorder(word, prop), weight = prop)) + 
    labs(x = x, y = y) + 
    coord_flip() + 
    geom_bar() + 
    theme_bw()
}



# Plot it  ----------------------------------------------------------------


## FIGURE 1
df_veda_clean %>%
  bar_chart_prep() %>% 
  bind_cols(., en = transl$fig_1) %>% 
  # reorder the position of Var1 by the relative frequency (rel) and plot
  bar_chart(lang = "en")



## FIGURE 3 
df_popularizace_clean %>%
  bar_chart_prep() %>% 
  bind_cols(., en = transl$fig_3) %>% 
  # reorder the position of Var1 by the relative frequency (rel) and plot
  bar_chart(lang = "en")

