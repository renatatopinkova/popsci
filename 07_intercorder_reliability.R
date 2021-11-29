### Inter-coder reliability
## Types of websites coded by PR & RT


# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# Load data ---------------------------------------------------------------

# Social science / humanities
socw_RT <- read.csv2("socw.csv")
socw_PR <- read.csv2("socw_PR.csv")

# Natural / tech science
natw_RT <- read.csv2("natw.csv")
natw_PR <- read.csv2("natw_PR.csv")



# Pre-processing ----------------------------------------------------------

# join the two frames
soc_combined <- inner_join(socw_RT, socw_PR, by=c("name", "prop", "n"), suffix=c("_RT", "_PR"))
nat_combined <- inner_join(natw_RT, natw_PR, by=c("name", "prop", "n"), suffix=c("_RT", "_PR"))

# get the top 250
soc_combined <- head(soc_combined, 250)
nat_combined <- head(nat_combined, 250)

# remove rows in missings (PR did not code all 250 - why?) -> 
# THIS SHOULD BE DISCUSSED - would make most sense to code proportion shared to avoid artificial cuts
soc_combined <- soc_combined %>% drop_na(type_PR, type_RT) # 242 rows w/o missings in type
nat_combined <- nat_combined %>% drop_na(type_PR, type_RT) # 239 rows w/o missings in type



# Reliability check -------------------------------------------------------


# % of agreement
sum(soc_combined$type_RT == soc_combined$type_PR)/nrow(soc_combined) # 77 %
sum(nat_combined$type_RT == nat_combined$type_PR)/nrow(nat_combined) # 71,5 %

# get rows where we disagreed
soc_d <- soc_combined %>% filter(!(type_RT == type_PR))
nat_d <- nat_combined %>% filter(!(type_RT == type_PR))

# export disagreement to csv
write.csv2(soc_d, "soc_disagreement.csv")
write.csv2(nat_d, "nat_disagreement.csv")

# --> this was resolved among the coders

# Loading resolved file ---------------------------------------------------

nat_resolved <- read.csv2("nat_disagreement_resolved.csv")
soc_resolved <- read.csv2("soc_disagreement_resolved.csv")



# Merge original coding with resolved disagreements -----------------------

soc_c <- soc_combined %>% 
  full_join(soc_resolved, by="name") %>%
  mutate(agreed_c = ifelse(is.na(agreed), type_RT, agreed))


nat_c <- nat_combined %>% 
  full_join(nat_resolved, by="name") %>%
  mutate(agreed_c = ifelse(is.na(agreed), type_RT, agreed))


# get proportions 
soc_types <- soc_c %>% 
  count(agreed_c) %>% mutate(prop = (n/242)*100)

nat_types <- nat_c %>% 
  count(agreed_c) %>% mutate(prop = (n/239)*100)

# convert to factors
soc_types$agreed_c <- as.factor(soc_types$agreed_c)
nat_types$agreed_c <- as.factor(nat_types$agreed_c)

# label factors
levels(soc_types$agreed_c) <- c("sociální média", "zpravodajství", "nakladatelství", "vládní weby", "popularizační","vědecké inst.", "grantové ag", "ostatní")
levels(nat_types$agreed_c) <- c("sociální média", "zpravodajství", "nakladatelství", "vládní weby", "popularizační","vědecké inst.", "grantové ag", "ostatní")



# Graphs ------------------------------------------------------------------


ggplot(soc_types, aes(factor(agreed_c), prop, fill=agreed_c)) + 
  geom_col() + theme(legend.position = "none", 
                     axis.text.x = element_text(vjust = -2), 
                     axis.title.x = element_text(vjust = -4, margin = margin(t = 5, b = 15))) +
  scale_fill_grey() + 
  labs(x = "Typ webu", y="Procenta", title = "Společenské weby") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,30)) + 
  geom_text(aes(label = round(prop)), vjust = -0.3)

ggsave("../grafy/soc_top_250.png")


ggplot(nat_types, aes(factor(agreed_c), prop, fill=agreed_c)) + 
  geom_col() + theme(legend.position = "none", 
                     axis.text.x = element_text(vjust = -2), 
                     axis.title.x = element_text(vjust = -4, margin = margin(t = 5, b = 15))) +
  scale_fill_grey() + 
  labs(x = "Typ webu", y="Procenta", title = "Přírodovědné weby") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,30)) + 
  geom_text(aes(label = round(prop)), vjust = -0.3)

ggsave("../grafy/nat_top_250.png")
