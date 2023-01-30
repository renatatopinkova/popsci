library(tidyverse)
library(tidyr)
library(glue)



# Coded datasets ----------------------------------------------------------


codedS <- read.csv("socw.csv", sep=";") %>% 
  filter(!(is.na(type)))

codedN <- read.csv("natw.csv", sep=";") %>% 
  filter(!(is.na(type)))


# Filter out only coded parts ---------------------------------------------


codedS_top <- codedS %>% 
  count(type) %>% 
  # % of each category in top 250
  mutate(prop = n/249) %>% 
  # convert type to factor & label it -> makes better plots
  mutate(type = as.factor(type))

codedN_top <- codedN %>% 
  count(type) %>% 
  mutate(prop = n/256) %>% 
  mutate(type = as.factor(type))



# Join datasets for soc & nat ---------------------------------------------


coded_full <- codedN_top %>% 
  full_join(codedS_top, by = "type", suffix = c("_nat", "_soc")) %>% 
  mutate(prop_nat = round(prop_nat * 100, 0),
         prop_soc = round(prop_soc * 100, 0),
         bump_nat = ifelse(prop_nat < prop_soc,
                           prop_nat - 2,
                           prop_nat + 2),
         bump_soc = ifelse(prop_nat < prop_soc,
                           prop_soc + 2,
                           prop_soc - 2)) %>% 
  select(-c(n_nat, n_soc)) %>% 
  pivot_longer(cols = -type, names_to = c(".value", "web"), names_sep = "_") 




# Plotting function -------------------------------------------------------


# barbell plot
plot_barbell <- function(df, lang = c("cz", "en"), color2 = "#15607a") {
    if (lang == "en") {
      x = "Percent"
      y = "Type of website"
      nat = "Natural"
      soc = "Social"
      levels(df$type) <- c("social media", "news", "publishing", "government", "popularization","science institutions", "grant agencies", "other")
    } else {
      x = "Procenta"
      y = "Typ webu"
      nat = "Přírodovědné"
      soc = "Společenskovědní"
      levels(df$type) <- c("sociální média", "zpravodajství", "nakladatelství", "vládní weby", "popularizační akce/weby","vědecké inst.", "grantové ag", "ostatní")
    }
    
    df %>% 
      ggplot(aes(x = prop, y = fct_reorder(type, desc(type)), color = web)) +
      geom_line(color = "#E6E6E6", size = 1.75) +
      geom_point(size = 2) +
      labs(x = "Percent", y = "Type of website") +
      geom_text(aes(label = glue("{prop}%"), x = bump), size = 3) +
      scale_color_manual(name = NULL, 
                         breaks = c("nat", "soc"),
                         values = c("#727272", color2),
                         labels = c("Natural", "Social"))+
      scale_x_continuous(limits = c(0,30),
                         breaks = seq(0,30, by = 5),
                         labels = glue("{seq(0,30, 5)}%")) +
      theme_minimal() +
      theme(legend.position =  c(1, 0.025),
            legend.justification = c("right", "bottom"),
            legend.background = element_rect(fill="white", linetype="blank"))
    
  }


## FIGURE 8
plot_barbell(coded_full, lan = "en", color2 = "black")

