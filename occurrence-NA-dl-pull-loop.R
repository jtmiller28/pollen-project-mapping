# A script for pulling occurrence data by catalogue designations + some diagnostics  

### Goals:
# 1. Organize the discoverLife's online catalogues table so that we can grab the accepted name and all names associated with it (Synoynms & Homonyms)
# 2. Subset the discoverLife's names by those that only exist within NA
# 3. Create a loop that takes the accepted name and all of its associates and conducts a pull through the package gatoRs 
# 4. Grab simple diagnositcs during this step, such as number of records obtained + retained per pull/cleaning step
# 5. Using known IUCN Redlist + State Endangered lists: Create two fields to denote protection status and bind to the data outputs
# 6. Using known Endemic status according to DiscoverLife, create a new field called northAmericaEndemic
# 7. Convert data into spatial -> subset into the NA ecoregions -> Test for whether a particular threshold of these records falls within a ecoregion -> Infer Endemism status + Other occurrence distribution stats, bind these new fields to the output data. 
# 8. Specialization Status dependent on Region
# 9. Return overall data ready for spatial analysis (aka duplicates & locality cleaned)
# 10. Return specimen data summary, this can include duplicates as we dont mind this for trying to track down usable specimens, NOTE: This isn't necessary in bees as there really shouldn't be duplicates in the first place...

# Load in necessary libraries
library(gatoRs) # New Package by Natalie & Shelly
library(ggplot2)
library(tidyr)
library(scales)
library(data.table) # Very fast, Ideal but can have issues with sf objects dependent on the operation
library(stringr) # Deals with some nonsense in regular expressions 
library(sf) # spatial features
library(tidyverse) # Hopefully phase this out eventually due to computation time, works well with sf objects though. 

# Presteps: Load in other relevant datatables for determining endemic, specialist, IUCN, and endangered statuses. 
#na_endemic_list <- fread("")
iucn_list <- fread("/home/jt-miller/Pollen-Project/pollen-project-mapping/processed-data/iucn-processed-bee-names.txt")
state_redlists <- fread("/home/jt-miller/Pollen-Project/pollen-project-mapping/processed-data/endangered_lists/CA-Special-Species-list-cleaned.csv", header = TRUE)
specialist_table <- fread("/home/jt-miller/Pollen-Project/pollen-project-mapping/raw-data/fowler_hostplants.csv")
iucn_list <- iucn_list[, accepted_name := resolvedName] # Adjust naming convention for later conditional join
state_relists <- state_redlists[, accepted_name := resolvedName]
western_specialist_table <- specialist_table[region == "west"] # For now our pull will focus on these since the regions of interest are Western North America


western_specialist_table <- western_specialist_table[, accepted_name := scientificName]
# 1. Organize the discoverLife's online catalogues table so that we can grab the accepted name and all names associated with it (Synoynms & Homonyms)
discoverLife <- fread("/home/jt-miller/Pollen-Project/pollen-project-mapping/raw-data/discoverlife-February-18-2022.csv") # Load in the most current version of discoverLife

dl_accepted <- discoverLife[relationName == "HAS_ACCEPTED_NAME"] # Create an accepted name table
other_mapping_cases <- c("SYNONYM_OF", "HOMONYM_OF") # Define the other mapping cases 
dl_synonyms <- discoverLife[relationName %in% other_mapping_cases] # Create a synonym list table 
non_unique_accepted_names <- dl_accepted[, .N, by = "resolvedName"][N>1] # Quality check, Identify non unique names 
non_unique_accepted_v<- non_unique_accepted_names$resolvedName # Assure that Mapping is always consistent 
dl_accepted_dups <- dl_accepted[resolvedName %in% non_unique_accepted_v] # Grab these duplicates 
dl_accepted_dups[, .(isSameResolvedName = length(unique(resolvedName)) == 1), by = providedName] # Checks to see if all instances of providedName maps to the same ResolvedName
# All map correctly. 

non_unique_synonym_names <- dl_synonyms[, .N, by = c("providedName")][N>1] # Quality check on Synonym table to see where mapping goes 
non_unique_synonym_v <- non_unique_synonym_names$providedName # Create vector of non-unique synoynm mappings
dl_syn_dups <- dl_synonyms[providedName %in% non_unique_synonym_v] # Subset the synonym df for these 
synm_eval  <- dl_syn_dups[, .(isSameResolvedName = length(unique(resolvedName)) == 1), by = providedName] # Checks to see if all instances of providedName maps to the same ResolvedName
synm_eval_fails <- synm_eval[isSameResolvedName == FALSE] # These are names that we don't currently have the toolset to deal with...Therefore we will flag the final output occurrence datasets that have these names with a field called isMultResMaps = TRUE | FALSE?
multResMapping_v <- synm_eval_fails$providedName # These are the names to check in final datasets for. 

# Create a simplified version of each respective dt, then attach synonyms
output_dt <- discoverLife[, .(providedName = providedName), by = resolvedName] # Organize provided names by resolvedNames. 

accepted_name_v <- unique(output_dt$resolvedName)
name_list <- list() # Create a list to store the processed names in 


for(i in 1:length(accepted_name_v)){
  p <- output_dt[resolvedName == accepted_name_v[[i]]] # Create a for-loop, goes through by the Accepted Name vector and grabs the AcceptedName & its synoynms storing them as a compounded vector into a list 
  if (length(unique(p$providedName)) == length(unique(p$resolvedName))) { # IF the provided name is the same as the resolved name
    
    a <- print(unique(p$resolvedName)) # Then just retain the unique entries of the resolved name
    name_list[[i]] <- c(a) # Compile this into the vector
  }
  else{ # Else we will take the providedName as variable x, and the unique resolvedName as variable z
  x <- print(p$providedName)
  z <- print(unique(p$resolvedName))
  name_list[[i]] <- unique(c(z,x)) # And compile them into the list, noting only unique values of each. 
  }
}

# 2. Subset this the accepted_names_v for only Resolved Names that exist in North America (still in progress due to intersection issue)
discoverLife_NA_bee_names <- read.delim("/home/jt-miller/Pollen-Project/pollen-project-mapping/raw-data/discoverLife-NA-bees.txt", header = FALSE)
discoverLife_NA_bee_names_v <- discoverLife_NA_bee_names$V1

vector1 <- accepted_name_v
vector1 <- intersect(accepted_name_v, discoverLife_NA_bee_names_v)
vector2 <- discoverLife_NA_bee_names_v
non_intersecting_vector <- setdiff(union(vector1, vector2), intersect(vector1, vector2)) # These are names not recorded within that zenodo pub, but are on discoverlife. Presumably because they've been added since Feb 2022. 

write.table(non_intersecting_vector, "/home/jt-miller/Pollen-Project/pollen-project-mapping/raw-data/discoverlife-nonIntersecting-V-for-res", quote = FALSE, row.names = FALSE)
nonInt_res <- read.delim("/home/jt-miller/Pollen-Project/pollen-project-mapping/raw-data/nonIntersection-resolution.txt")



# 3. Create a for-loop that builds an occurrence file based upon gators download for every species. 
gators_cleaning <- data.frame() # Create a storage for looking at how much data is removed per cleaning step

# To change later, for testing specific names for workflow improv
# Target string to search for
target_string <- c("Atoposmia abjecta", "Calliopsis fracta")

name_list2 <- unique(name_list)
test_list <- name_list[sapply(name_list, function(x) any(grepl("Atoposmia abjecta|Calliopsis fracta", x)))]
test_list <- unique(test_list)

accepted_name_v <- accepted_name_v[str_detect(accepted_name_v, "Atoposmia abjecta|Calliopsis fracta")] # For now check with a known output species 

accepted_name_v_filestyle <- gsub(" ", "-", accepted_name_v) # filestyle of that name vector so we dont have spaces in the filenames

for(i in 1:length(accepted_name_v)){
  
  
  gators.output <-  gatoRs::gators_download(synonyms.list  = test_list[[i]], # Conduct gatoRs pulls
                                            write.file = FALSE, # No file for now
                                            gbif.match = "fuzzy", 
                                            idigbio.filter = TRUE, 
                                            limit = 1e+05) # Limits may pose an issue, check aggregator metrics to assure iDigBio doesnt hit 100,000 records...
  
  taxa_clean_holder <- taxa_clean(df = gators.output,  
                                  synonyms.list = test_list[[i]], 
                                  taxa.filter = "fuzzy", 
                                  accepted.name =  accepted_name_v[i]) # Accepted name will be the name first subsetted from the list (the object)
  
  locality_clean_holder <- basic_locality_clean(df = taxa_clean_holder,
                                                latitude = "latitude",
                                                longitude = "longitude",
                                                remove.zero = TRUE,
                                                precision = TRUE,
                                                digits = 3,
                                                remove.skewed = FALSE)
  
  duplicate_clean_holder <- remove_duplicates(df = locality_clean_holder,
                                              #event.date = "eventDate",
                                              latitude = "latitude",
                                              longitude = "longitude",
                                              #aggregator = "aggregator",
                                              #id = "ID",
                                              remove.NA.occ.id = TRUE, # We're going to remove all these, as at large scale this isn't trackable to deal with
                                              remove.NA.date = TRUE,
                                              remove.unparseable = TRUE)
  
  
  #spatial_analysis_dataset <- duplicate_clean_holder # Done after the duplicate remove step, this data is ready for spatial analysis
  specimen_analysis_dataset <- locality_clean_holder # After locality cleaning, this is informative if we just want to know how many specimens we can get for experimental work
  

  
# 4. Create an output table noting some diagnostics: Here we're adding how many records are removed per step by AcceptedName species pull. 
  gators_cleaning[i, 1] <- accepted_name_v[[i]] # Store the AcceptedName as the one we identify the species by
  gators_cleaning[i, 2] <- nrow(taxa_clean_holder) # Store the number of specimens after taxa cleaning
  gators_cleaning[i, 3] <- nrow(locality_clean_holder) # Store the number of specimens after locality cleaning 
  gators_cleaning[i, 4] <- nrow(duplicate_clean_holder) # Store the number of specimens after duplicate cleaning 
  gators_cleaning[i, 5] <- nrow(subset(gators.output, aggregator == "iDigBio")) # How many records for aggregator iDigBio
  gators_cleaning[i, 6] <- nrow(subset(gators.output, aggregator == "GBIF")) # How many records for aggregator GBIF
  colnames(gators_cleaning)[1] = "acceptedName" # Rename these cols for clarity
  colnames(gators_cleaning)[2] = "pulledRecords"
  colnames(gators_cleaning)[3] = "localityClean"
  colnames(gators_cleaning)[4] = "distinctRecordsClean"
  colnames(gators_cleaning)[5] = "iDigBioTotalRecordsPull"
  colnames(gators_cleaning)[6] = "GBIFTotalRecordsPull"

# 5. Append IUCN & State Redlist status to the data
specimen_analysis_dataset <- data.table(specimen_analysis_dataset) # Reformat as a datatable
specimen_analysis_dataset <- specimen_analysis_dataset[, IUCN_status := iucn_list[specimen_analysis_dataset, on = "accepted_name", redlistCategory ]]
specimen_analysis_dataset <- specimen_analysis_dataset[, State_status := state_redlists[specimen_analysis_dataset, on = "accepted_name", GlobalRank]]

# 8. Add Specialization Status: Note: while this would be largely dependent on location in the US (west, middle, east) our ecoregions of focus are Western NA, therefore we will just match whether the bee is a specialist in the West. IF we wanted to look at other regions we'd need to complicate this a bit more...
specimen_analysis_dataset <- specimen_analysis_dataset[, host_plant := western_specialist_table[specimen_analysis_dataset, on = "accepted_name", host_plant]]
specimen_analysis_dataset$western_specialist_status <- ifelse(specimen_analysis_dataset$host_plant != "", TRUE, FALSE)
# 6 & 7. Assess Endemism Status using Known North American Endemics and the spatial location of known specimens. 
na_ecoregions_3 <- sf::read_sf("/home/jt-miller/Pollen-Project/pollen-project-mapping/raw-data/Ecoregions/na-level3-ecoregions/NA_CEC_Eco_Level3.shp") # Read in the ecoregion shapefiles for NA 

sf_bee <- st_as_sf(as.data.frame(specimen_analysis_dataset), coords = c('longitude', 'latitude'), crs = 4326, remove = FALSE) # Make our bee data spatial

sf_bee <- st_transform(sf_bee, st_crs(na_ecoregions_3)) # Transform the projection of the specimen data to that of the ecoregions. 

region_name_v <- unique(na_ecoregions_3$NA_L3NAME) # Take the unique names of the ecoregions 
# Create a list to store our dataframes in for each region, use a for-loop to iterate subsetting
region_df_list <- list() # Create a holding list for the ecoregions 

for(j in 1:length(region_name_v)){ 
  region_df_list[[j]] <- subset(na_ecoregions_3, NA_L3NAME == region_name_v[j]) # Construct a for-loop that subsets the shapefiles to those names 
}

# Partition the records out by ecoregion assigning a ecoregion field. , Also Calculate whether the bee is endemic to a particular region with a 90% threshold
bees_regioned <- list() # Construct a holding list

for(k in 1:length(region_df_list)){ # for the ecoregion list
  if(nrow(sf_bee[region_df_list[[k]],]) > 0){ # IF there is more than 0 observations in the ecoregion
    bees_regioned_holder <- sf_bee[region_df_list[[k]],] # Subset the occurrences by the polygon boundaries of the ecoregion
    region_name <- unique(region_df_list[[k]]$NA_L3NAME) # Grab the name of the lvl 3 ecoregion 
    
    bees_regioned_holder$ecoregion <- paste0(region_name) # Paste the name of the level 3 ecoregion in a new field called ecoregion
 
    bees_regioned[[k]] <- bees_regioned_holder # Store this subsetted occurrences by region in the holding list 
    }
    else{ # Else do nothing...
    }
  }

bees_regioned_df <- do.call(rbind, bees_regioned) # Bind these regioned bees together

bees_regioned_df$isNAEndemic <- bees_regioned_df$accepted_name %in% discoverLife_NA_bee_names_v # Check whether the bee is a NA endemic 

bees_regioned_df <- bees_regioned_df %>% 
  mutate(regional_weighting = 1) %>% # Add some weighting, assuming that each occurrence is a single observation
  group_by(ecoregion) %>% # GROUP BY ecoregion 
  mutate(regional_props = sum(regional_weighting)/nrow(bees_regioned_df) ) %>%  # Calculate the proportion of bees in the region as compared to all regions
  mutate(isEcoregionEndemic = ifelse(isNAEndemic == FALSE, FALSE, # Create a new field, first: If not an NA endemic, then FALSE no matter what...
           case_when( # Apply conditions for what is an ecoregion ENDEMIC 
    regional_props >= 0.90 ~ TRUE, # If 90% or more of the occurrences are within a particular ecoregion, classify as endemic 
    regional_props < 0.90 ~ FALSE # If anything less than 90%, NOT an endemic 
  ))) %>% 
  mutate(occur_perc_in_ecoregion = round(regional_props * 100,2)) %>%  # Add percentage of occupancy based upon known specimen occurrences for interests sake 
  st_drop_geometry() 



path <- "/home/jt-miller/Pollen-Project/pollen-project-mapping/test-data-outputs-gators/specimen-info-datasets/"
  #write.csv(spatial_analysis_dataset, file = paste0("/home/jt-miller/Pollen-Project/pollen-project-mapping/test-data-outputs-gators/spatial-analysis-ready-datasets/", accepted_name_v_filestyle[[i]], ".csv"), row.names = FALSE)
  #write.csv(specimen_analysis_dataset, file.path(path,  paste0(accepted_name_v_filestyle[i], ".csv")), row.names = FALSE)
  #write.csv(specimen_analysis_dataset, file = path,  paste('myarray', i, 'csv', sep = '.'))
write.table(bees_regioned_df, file=paste0(path, accepted_name_v_filestyle[[i]],".txt"), row.names = FALSE, sep="\t", quote = FALSE)
  
print(paste0("Iteration: ", i, paste0(" of ", length(accepted_name_v)))) # Keep Track of where we are in the loop for my sanity 

} # The end of the download and cleaning loop

# Check output 
check <- fread("/home/jt-miller/Pollen-Project/pollen-project-mapping/test-data-outputs-gators/specimen-info-datasets/Atoposmia-abjecta.txt")

