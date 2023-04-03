### A script to visualize how bee species are distributed among the 7 families. 
library(tidyverse)

bee_names <- read.delim("/home/jt-miller/Pollen-Project/Pollen-DB/bee-names-WorldWide/DiscoverLifeNames - For Bee Library.tsv", quote = "")

names_for_nomer <- bee_names %>% 
  select(acceptedStr)



write.table(names_for_nomer, file = "/home/jt-miller/Pollen-Project/Pollen-DB/bee-names-WorldWide/accepted-names-for-nomer", 
            sep = "\t", row.names=FALSE, quote=FALSE)


bee_names_w_fams <- read.delim("/home/jt-miller/Pollen-Project/Pollen-DB/bee-names-WorldWide/nomer-output.txt", quote = "")

# Keep only columns of interest:
fams <- bee_names_w_fams %>% 
  select(acceptedStr.1, X.3) %>% 
  rename(species = acceptedStr.1, family = X.3) %>% 
  distinct(species, .keep_all = TRUE)


fams_w_genus <- fams %>%
  mutate(genus = word(species, 1))
  
fams_counts <- fams_w_genus %>% 
group_by(family) %>% 
  count() %>% 
  filter(!family == "") 

genus_counts <- fams_w_genus %>% 
  group_by(genus, family) %>% 
  summarize(count = n())

# Tree mapping 
library(treemap)

# Plot
# Plot
treemap(genus_counts,
        
        # data
        index=c("family", "genus"),
        fontcolor.labels = c("black", "white"),
        vSize="count",
        type="index",
        
        # Main
        title="Global Family Representation",
        palette="Dark2",
  
        
        
)
