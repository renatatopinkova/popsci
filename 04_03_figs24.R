library(tidyverse)
library(tidyr)
library(readr)
library(glue)

# Read in data ------------------------------------------------------------

df_veda_clean <- readRDS("df_veda_clean")
df_popularizace_clean <- readRDS("df_popularizace_clean")

# word translations
transl <- read_delim("../data/helpers/fig_1-4.csv")

# web types
def_web <- read.csv2("../default_weby_typ.csv", header=T)


# Data preparation --------------------------------------------------------



data_prep <- function(df) {
  df %>%
    inner_join(def_web, by = c("document" = "web")) %>%
    filter(typ <= 2) %>%
    # group so words
    group_by(Var1, typ) %>%
    # count each word across documents
    mutate(n = sum(Freq)) %>%
    # keep each word just once
    distinct(Var1, typ, .keep_all = TRUE) %>%
    # convert frequency to proportion
    group_by(typ) %>%
    mutate(prop = n / sum(n) * 100) %>%
    select(Var1, typ, prop) %>%
    ungroup() %>%
    pivot_wider(names_from = "typ", values_from = "prop", names_prefix = "typ_") %>%
    replace_na(list(
      typ_1 = 0,
      typ_2 = 0
    )) %>%
    mutate(diff = typ_1 - typ_2) %>%
    mutate(
      bump_1 = ifelse(typ_1 < typ_2,
        typ_1 - .25,
        typ_1 + .25
      ),
      bump_2 = ifelse(typ_1 < typ_2,
        typ_2 + .25,
        typ_2 - .25
      )
    ) %>%
    pivot_longer(cols = -c(Var1, diff), names_to = c(".value", "web"), names_sep = "_") %>%
    mutate(
      prop = round(typ, 2),
      diff = round(diff, 2)
    )
}



# Plotting function -------------------------------------------------------



plot_dumbbell <- function(df, lang = c("cz", "en"), color2 = "#15607a") {
  
  if (lang == "en") {
    x = "Word"
    y = "Corpus proportion"
    word = df$en
    nat = "Natural"
    soc = "Social"
  } else {
    x = "Slovo"
    y = "Proporce korpusu"
    word = df$Var1
    nat = "Přírodovědné"
    soc = "Společenskovědní"
  }
  
  
  df %>%
    ggplot(aes(x = prop, y = reorder(word, abs(-diff)), color = web)) +
    geom_line(color = "#E6E6E6", size = 1.75) +
    geom_point(size = 2) +
    labs(x = x, y = y) +
    geom_text(aes(label = glue("{prop}%"), x = bump), size = 3) +
    scale_color_manual(
      name = NULL,
      breaks = c("1", "2"),
      values = c("#727272", color2),
      labels = c(nat, soc)
    ) +
    scale_x_continuous(
      limits = c(-1, 4),
      breaks = seq(-1, 4, by = 1),
      labels = glue("{seq(-1,4, 1)}%")
    ) +
    theme_minimal() +
    theme(
      legend.position = c(1, 0.01),
      legend.justification = c("right", "bottom"),
      legend.background = element_rect(fill = "white", linetype = "blank")
    )
}


# Plotting ----------------------------------------------------------------

## FIGURE 2 
df_veda_clean %>% 
  data_prep() %>% 
  slice_max(abs(diff), n = 28) %>% 
  left_join(select(transl, contains("fig_2")), by = c("Var1" = "fig_2_cs")) %>% 
  rename(en = fig_2) %>% 
  plot_dumbbell(lang = "en", color2 = "black") # remove color2 for color graph


## FIGURE 4
df_popularizace_clean %>% 
  data_prep() %>% 
  slice_max(abs(diff), n = 32) %>% 
  left_join(select(transl, contains("fig_4")), by = c("Var1" = "fig_4_cs")) %>% 
  rename(en = fig_4) %>% 
  plot_dumbbell(lang = "en", color2 = "black") # remove color2 for color graph



