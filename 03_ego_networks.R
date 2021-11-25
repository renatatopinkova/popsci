# Load packages -------------------------------------------

library(dplyr)
library(stringr)
library(igraph)


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
# net_list <- readRDS("net_list_whole")

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
# loadRDS("ego_net)


# Summaries ---------------------------------------------------------------

# print summaries
for (i in 1:length(net_list)) {
  print(data_names[i])
  summary(ego_net[[i]])
}


## Create dataframe with summaries
summary_stats <- data.frame()
for (i in 1:length(net_list)) {
  df <- data.frame(name = names(ego_net[i]), nodes = gorder(ego_net[[i]]), edges = gsize(ego_net[[i]]))
  summary_stats <- rbind(summary_stats, df)
}


# file with coded type 1 = hum/soc, 2 - nat/tech
def_web <- read.csv2("../default_weby_typ.csv", header=T)

# link with web types 
summary_stats <- summary_stats %>% 
  inner_join(def_web, by = c("name" = "web")) %>% 
  select(name, nodes, edges, typ)


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
  replace_na(list(n_soc=0,
                  n_nat= 0,
                  prop_soc = 0,
                  prop_nat = 0))



# Add coded dataset -------------------------------------------------------

# coded into 7 groups + 99
# load coded datasets
codedS <- read.csv("socw.csv", sep=";")
codedN <- read.csv("natw.csv", sep=";")

# remove NAs
codedS <- codedS %>% filter(!(is.na(type)))
codedN <- codedN %>% filter(!(is.na(type)))

# % of each category in top 250
codedS_top <- codedS %>% 
  count(type) %>% mutate(prop = n/249)

codedN_top <- codedN %>% 
  count(type) %>% mutate(prop = n/256)

# Export: do not run, just for reproducibility, shows how the file in project was made
# write.csv2(codedS_top, "soc_top250_catg.csv")
# write.csv2(codedN_top, "nat_top250_catg.csv")

# convert type to factor & label it -> makes better plots
codedS_top$type <- as.factor(codedS_top$type)
codedN_top$type <- as.factor(codedN_top$type)

levels(codedS_top$type) <- c("sociální média", "zpravodajství", "nakladatelství", "vládní weby", "popularizační akce/weby","vědecké inst.", "grantové ag", "ostatní")
levels(codedN_top$type) <- c("sociální média", "zpravodajství", "nakladatelství", "vládní weby", "popularizační akce/weby","vědecké inst.", "grantové ag", "ostatní")



# graphs 
ggplot(codedS_top, aes(factor(type), prop, fill=type)) + geom_col() + theme(legend.position = "none")
ggplot(codedN_top, aes(factor(type), prop, fill=type)) + geom_col() + theme(legend.position = "none")


