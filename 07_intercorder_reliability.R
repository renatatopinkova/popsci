### Inter-coder reliability
## Types of websites coded by PR & RT


# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyr)

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



