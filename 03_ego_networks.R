# Load packages -------------------------------------------

library(dplyr)
library(stringr)
library(igraph)
library(forcats)
library(ggplot2)
library(tidyr)
library(glue)
library(patchwork)
library(readr)

# Data import -------------------------------------------------------------

data_names <- list.files("../data/network-processed", pattern = ".csv")
data_names <- str_extract(data_names, "[^\\.]+")

#in website names, changes "_" into "." to match the names in the data
data_names <- str_replace_all(data_names, pattern = "_", replacement = "\\.") 

# fixing an import mistake
data_names[50] <- "jh-inst.cas.cz"

# get data paths
data_paths <- list.files("../data/network-processed", pattern = ".csv", full.names = TRUE)

## load all data
data_list <- lapply(data_paths, read.csv2)

# name lists
names(data_list) <- data_names


# Cleaning ----------------------------------------------------------------

# remove prefixes
for (i in 1:length(data_list)) {
  data_list[[i]]$from <- str_replace(data_list[[i]]$from, pattern  = "^.+//www\\.|^.+//", replacement = "")
  data_list[[i]]$to <- str_replace(data_list[[i]]$to, pattern  = "^.+//www\\.|^.+//", replacement = "")
}


# remove everything after first slash
for (i in 1:length(data_list)) {
  data_list[[i]]$to <- sub("\\/.*", "", data_list[[i]]$to)
  data_list[[i]]$from <- sub("\\/.*", "", data_list[[i]]$from)
}

# remove loops
for (i in 1:length(data_list)) {
  data_list[[i]] <- filter(data_list[[i]], !(from == to))
}


# Creating full networks --------------------------------------------------------

net_list <- lapply(data_list, graph_from_data_frame, directed=T)

# saveRDS(net_list, "net_list_whole")
net_list <- readRDS("net_list_whole")

# Extracting egos ---------------------------------------------------------

#create empty list
ego_list <- list()

# Get nodes (vertices) in distance = 1 from ego
for (i in 1:length(net_list)) {
 ego_list[[i]] <- ego(net_list[[i]],
      order = 1,
      nodes = data_names[i],
      mode = "all",
      mindist = 0)
  summary(ego_list[[i]])
}

# sticking back names
names(ego_list) <- data_names


## Create ego networks
ego_net <- list() # initiate empty list

for (i in 1:length(net_list)) {
  ego_net[[i]] <- induced_subgraph(net_list[[i]], unlist(ego_list[[i]]))
}

# sticking back names
names(ego_net) <- data_names

# saveRDS(ego_net, "ego_net")
ego_net <- readRDS("ego_net")


# Summaries ---------------------------------------------------------------

# print summaries
for (i in 1:length(net_list)) {
  print(data_names[i])
  summary(ego_net[[i]])
}


## Create dataframe with summaries
summary_stats <- data.frame()
for (i in 1:length(net_list)) {
  df <- data.frame(name = names(ego_net[i]), 
                   nodes = gorder(ego_net[[i]]), 
                   edges = gsize(ego_net[[i]]),
                   avg_degree = round(mean(degree(ego_net[[i]])), 0)
  )
  summary_stats <- rbind(summary_stats, df)
}


# file with coded type 1 = hum/soc, 2 - nat/tech
def_web <- read_delim("./default_weby_typ.csv",
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

# link with web types 
summary_stats <- summary_stats %>% 
  inner_join(def_web, by = c("name" = "web")) %>% 
  select(name, nodes, edges, avg_degree, typ)

summary_stats %>% 
  filter(typ == 1 | typ == 2) %>% 
  mutate(typ = case_when(
    typ == 1 ~ "Sociálněvědní",
    typ == 2 ~ "Přírodovědné"
  )) %>% 
  group_by(typ) %>% 
  summarise(n = n(),
            avg_nodes = mean(nodes),
            avg_edges = mean(edges),
            avg_degree = mean(avg_degree))


# find smallest & greatest nets
summary_stats %>% filter(nodes == min(nodes))
summary_stats %>% filter(nodes == max(nodes))

# find smallest & greatest nets BY TYPE

## N of edges
summary_stats %>% 
  group_by(typ) %>% 
  filter(edges == min(edges))

summary_stats %>% 
  group_by(typ) %>% 
  filter(edges == max(edges)) 


## N of nodes
summary_stats %>% 
  group_by(typ) %>%
  filter(nodes == min(nodes))

summary_stats %>% 
  group_by(typ) %>%
  filter(edges == max(edges))


# means 
summary_stats %>% summarise(mean_nodes = mean(nodes), 
                            mean_edges = mean(edges))
# means BY TYPE
summary_stats %>% 
  group_by(typ) %>%
    summarise(mean_nodes = mean(nodes), 
              mean_edges = mean(edges))



# Get all unique webs -----------------------------------------------------
uni <- data.frame()

for (i in 1:length(ego_list)) {
  uni <- rbind(uni, as_data_frame(ego_net[[i]], what="vertices")) # gets unique nodes for each net and binds them to one data frame
}

count(unique(uni))
## > 13 000 unique webs, impossible to code type


# Find nodes that are in most nets
all <- uni %>% 
  count(name, sort = T) %>%
  mutate(prop = round(n/99, 2))




# Selecting nature and hum/soc  -------------------------------------------


# filter out names of webs by types
soc <- filter(def_web, typ==1) %>% select(web)
nat <- filter(def_web, typ==2) %>% select(web)

# subset nets from ego net list
soc_web <- ego_net[unlist(soc)]
nat_web <- ego_net[unlist(nat)]


## Subset vertices from hum/soc nets

### Soc nets
soc_uni <- data.frame()
for (i in 1:length(soc_web)) {
  soc_uni <- rbind(soc_uni, as_data_frame(soc_web[[i]], what="vertices"))
}

str(unique(soc_uni)) # 3381 unique vertices

### Nat webs
nat_uni <- data.frame()
for (i in 1:length(nat_web)) {
  nat_uni <- rbind(nat_uni, as_data_frame(nat_web[[i]], what="vertices"))
}

str(unique(nat_uni)) # 6086 unique vertices


# Comparing hum/soc and nat/tech ------------------------------------------

socw <- soc_uni %>% 
  count(name, sort = T) %>%
  mutate(prop = round(n/27, 2))


natw <- nat_uni %>% 
  count(name, sort = T) %>%
  mutate(prop = round(n/47, 2))

soc_nat <- socw %>% 
  full_join(natw, by="name", suffix=c("_soc","_nat")) %>%
  tidyr::replace_na(list(n_soc=0,
                  n_nat= 0,
                  prop_soc = 0,
                  prop_nat = 0)) %>% 
  filter(!str_detect(name, "(google)|(doi.org)"))


# Get websites that are most under/over-represented in soc/nat
diff_minus <- soc_nat %>% 
  mutate(diff = prop_soc - prop_nat) %>% 
  arrange(diff) %>% 
  head(15)

diff_plus <- soc_nat %>% 
  mutate(diff = prop_soc - prop_nat) %>% 
  arrange(desc(diff)) %>% 
  head(15)

diffs <- bind_rows(diff_minus, diff_plus)


diffs_clean <- diffs %>% 
  select(-c(n_soc, n_nat)) %>% 
  mutate(prop_nat = prop_nat * 100,
         prop_soc = prop_soc * 100,
         bump_nat = ifelse(prop_nat < prop_soc,
                           prop_nat - 2.5,
                           prop_nat + 2.5),
         bump_soc = ifelse(prop_nat < prop_soc,
                           prop_soc + 2.5,
                           prop_soc - 2.5)) %>% 
  pivot_longer(cols = -c(name, diff), names_to = c(".value", "web"), names_sep = "_") 


plot_dumbbell <- function(df) {
df %>% 
  ggplot(aes(x = prop, y = reorder(name, abs(-diff)), color = web)) +
  geom_line(color = "#E6E6E6", size = 1.75) +
  geom_point(size = 2) +
  labs(x = "Percent", y = "Website") +
  geom_text(aes(label = glue("{prop}%"), x = bump), size = 3) +
  scale_color_manual(name = NULL, 
                     breaks = c("nat", "soc"),
                     values = c("#727272", "#15607a"),
                     labels = c("Natural", "Social"))+
  scale_x_continuous(limits = c(-5,90),
                     breaks = seq(-5,90, by = 10),
                     labels = glue("{seq(-5,90, 10)}%")) +
  theme_minimal() +
  theme(legend.position =  c(1, 0.01),
        legend.justification = c("right", "bottom"),
        legend.background = element_rect(fill="white", linetype="blank"))

}


nadrepre_nat <- diffs_clean %>% 
  filter(diff < 0) %>% 
  plot_dumbbell() + ggtitle("Odkazy nadreprezentované na webech přírodovědných institucí")
  
nadrepre_soc <- diffs_clean %>% 
  filter(diff > 0) %>% 
  plot_dumbbell() + ggtitle("Odkazy nadreprezentované na webech společenskovědních institucí") +
  theme(plot.title = element_text(hjust = -1.4))




plot_dumbbell_combined <- function(df, color1 = "#727272", color2 ="#15607a") {
  df %>% 
    ggplot(aes(x = prop, y = reorder(name, abs(-diff)), color = web)) +
    geom_line(color = "#E6E6E6", size = 1.75) +
    geom_point(size = 2) +
    labs(x = "Percent", y = "Website") +
    geom_text(aes(label = glue("{prop}%"), x = bump), size = 3) +
    scale_color_manual(name = NULL, 
                       breaks = c("nat", "soc"),
                       values = c(color1, color2),
                       labels = c("Natural", "Social"))+
    theme_minimal() 

  
}



nadrepre_nat2 <- diffs_clean %>% 
  filter(diff < 0) %>% 
  plot_dumbbell_combined() + 
  labs(y = "", x = "Percent")


nadrepre_soc2 <- diffs_clean %>% 
  filter(diff > 0) %>% 
  plot_dumbbell_combined() 

# patch them together
(nadrepre_soc2 | nadrepre_nat2) + plot_layout(widths = c(1, 2), guides = "collect") & theme(legend.position = 'top')


# B&W ---------------------------------------------------------------------

nadrepre_nat_bw <- diffs_clean %>% 
  filter(diff < 0) %>% 
  plot_dumbbell_combined(color2 = "black") + 
  labs(y = "", x = "Percent")


nadrepre_soc_bw <- diffs_clean %>% 
  filter(diff > 0) %>% 
  plot_dumbbell_combined(color2 = "black") 

# patch them together
(nadrepre_soc_bw | nadrepre_nat_bw) + plot_layout(widths = c(1, 2), guides = "collect") & theme(legend.position = 'top')

