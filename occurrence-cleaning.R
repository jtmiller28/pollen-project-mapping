### Occurrence Cleaning Script:

# Libraries
library(tidyverse)
library(taxize) # for using gbif-parse
library(sqldf)

# these truncated data sets were built with the following
# idigbio: cat occurrence-raw.txt | cut -f 35,36,42,43,46,47,49,55,56,57,68,80,90,95,103,104,105,118,130,140,170,171,172,175,177,199 > occurrence-raw-cut.txt
# gbif: cat verbatim.txt | cut -f 58,60,61,64,65,68,91,104,108,109,110,112,113,126,128,129,131,144,145,147,181,199,213,217,218,222 > occurrence-verbatim-cut.txt

idigbio_bees <- read.delim("/home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/idigbio-data/cut-data/occurrence-raw-cut.txt", quote = "")
gbif_bees <- read.delim("/home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/gbif-data/cut-data/occurrence-verbatim-cut.txt", quote = "")
amnh_bees <- read.delim("/home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/AMNH-Data/AEC-DBCNet-v1.0/globalbioticinteractions-AEC-DBCNet-55f1939/AEC-DBCNet_DwC-A20160308/AEC-DBCNet_DwC-A20160308/occurrences.tsv", quote = "")

#### Bring Datasets Together
idigbio_bees <- idigbio_bees %>% 
  dplyr::rename_with(~str_remove(., 'dwc.')) %>% # Remove dwc
  dplyr::mutate(dataAggregatorName = "idigbio")

gbif_bees <- gbif_bees %>% 
  dplyr::mutate(dataAggregatorName = "gbif") 

amnh_bees <- amnh_bees %>% 
  dplyr::select(associatedTaxa, basisOfRecord, coordinateUncertaintyInMeters, country, county, decimalLatitude, decimalLongitude, eventDate, genus, habitat, infraspecificEpithet, institutionCode, locality, occurrenceID, samplingProtocol, scientificName, scientificNameAuthorship, specificEpithet, stateProvince, year) %>% 
  dplyr::mutate(collectionCode = "Not in this Dataset", collectionID = "Not in this Dataset", day = "Not in this Dataset", identificationID = "Not in this Dataset", informationWithheld = "Not in this Dataset", month = "Not in this Dataset") %>% # Not a applicable field in this dataset
  dplyr::mutate(dataAggregatorName = "amnh")

bee_occurrences_agg <- rbind(idigbio_bees, gbif_bees, amnh_bees) # Aggregate the datasets together

# Remove non-unique occurrenceIDs as the uuid 
bee_occurrence_agg <- bee_occurrences_agg %>% 
  distinct(occurrenceID, .keep_all = TRUE)

# Write out txt file (commented out when not in use)
#write.table(bee_occurrence_agg, file = "/home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/aggregated-occurrences/bee-occurrence-verbatim-agg.txt", 
            #sep = "\t", row.names=FALSE, col.names=TRUE, quote=FALSE)

### Taxonomic Name Alignment: 

# It will be necessary to create a unique name list to feed into nomer, first lets create a df that only includes the SN + Authorship
bee_names <- distinct(bee_occurrence_agg, scientificName, scientificNameAuthorship, .keep_all = FALSE)
# Remove names that have no value in the scientificName field 
bee_names <- bee_names %>%
  filter(!scientificName == "")
bee_namest <- distinct(bee_occurrence_agg, scientificName, .keep_all = FALSE)

#write.table(bee_names, file = "/home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/aggregated-occurrences/parsed-names/uniq-bee-name-list.txt", row.names = FALSE, sep="\t", quote = FALSE)

# Use gbif-parse (called through the taxize package) Note: Beware http closures/fails, gbif parse's API fails intermittently 

parsed_names <- gbif_parse(bee_names$scientificName)

#write.table(parsed_names, file = "/home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/aggregated-occurrences/parsed-names/gbif-parsed-bee-names.txt", row.names = FALSE, sep="\t", quote = FALSE)

### Dealing with Uncertain names (Ambigious Species Boundaries)
uncertain_names <- bee_names %>% 
  filter(grepl('/', scientificName) | grepl(' or ', scientificName)) 

uncertain_name_v <- uncertain_names$scientificName

# Determine Priorities: Currently the priorities are similar to that of my western NA pull, porting over that cleaning bit, but this could be improved...
priorities <- bee_occurrence_agg %>% 
  filter(scientificName %in% uncertain_name_v) %>%
  group_by(scientificName) %>% 
  summarize(count = n())

associated_unc <- bee_occurrence_agg %>% 
  filter(str_detect(scientificName, 'Agapostemon angelicus|Agapostemon texanus|Agapostemon \\(Agapostemon\\) angelicus|Agapostemon \\(Agapostemon\\) texanus|Hylaeus \\(Prosopis\\) affinis|Hylaeus \\(Prosopis\\) modestus|Hylaeus affinis|Hylaeus modestus|Hylaeus affinis undefinable|Diadasia rinconis|Diadasia australis|Ceratina \\(Zadontomerus\\) calcarata|Ceratina \\(Zadontomerus\\) dupla|Ceratina calcarata|Ceratina dupla|Perdita exclamans|Perdita ashmeadi|Perdita \\(Perdita\\) exclamans|Perdita \\(Perdita\\) ashmeadi|Perdita \\(Perdita\\) triangulifera|Perdita triangulifera|Lasioglossum callidum|Hylaeus cf. affinis|Osmia clarescens|Osmia \\(Osmia\\) clarescens|Osmia sanrafaelae|Osmia \\(Osmia\\) sanrafaelae|Lasioglossum obnubilum|Lasioglossum \\(Lasioglossyn\\) obnubilum|Lasioglossum lillipute|Lasioglossum \\(Lasioglossum\\) lillipute|Lasioglossum ruficornis|Lasioglossum \\(Lasioglossum\\) ruficornis|Lasioglossum pulveris|Lasioglossum \\(Lasioglossum\\) pulveris|Hylaeus rudbeckiae|Hylaeus \\(Hylaeus rudbeckiae\\) rudbeckiae|Hylaeus granulatus|Hylaeus \\(Hylaeus\\) granulatus|Agapostemon coloradinus|Agapostemon \\(Agapostemon\\) coloradinus|Agapostemon \\(Agapostemon\\) virescens'))

associated_unc_df <- parsed_names %>% 
  filter(str_detect(scientificname, 'Agapostemon angelicus|Agapostemon texanus|Agapostemon \\(Agapostemon\\) angelicus|Agapostemon \\(Agapostemon\\) texanus|Hylaeus \\(Prosopis\\) affinis|Hylaeus \\(Prosopis\\) modestus|Hylaeus affinis|Hylaeus modestus|Hylaeus affinis undefinable|Diadasia rinconis|Diadasia australis|Ceratina \\(Zadontomerus\\) calcarata|Ceratina \\(Zadontomerus\\) dupla|Ceratina calcarata|Ceratina dupla|Perdita exclamans|Perdita ashmeadi|Perdita \\(Perdita\\) exclamans|Perdita \\(Perdita\\) ashmeadi|Perdita \\(Perdita\\) triangulifera|Perdita triangulifera|Lasioglossum callidum|Hylaeus cf. affinis|Osmia clarescens|Osmia \\(Osmia\\) clarescens|Osmia sanrafaelae|Osmia \\(Osmia\\) sanrafaelae|Lasioglossum obnubilum|Lasioglossum \\(Lasioglossyn\\) obnubilum|Lasioglossum lillipute|Lasioglossum \\(Lasioglossum\\) lillipute|Lasioglossum ruficornis|Lasioglossum \\(Lasioglossum\\) ruficornis|Lasioglossum pulveris|Lasioglossum \\(Lasioglossum\\) pulveris|Hylaeus rudbeckiae|Hylaeus \\(Hylaeus rudbeckiae\\) rudbeckiae|Hylaeus granulatus|Hylaeus \\(Hylaeus\\) granulatus|Agapostemon coloradinus|Agapostemon \\(Agapostemon\\) coloradinus|Agapostemon \\(Agapostemon\\) virescens')) %>% 
  mutate(resolvedNames = case_when(
    str_detect(scientificname, "Agapostemon angelicus|Agapostemon texanus|Agapostemon \\(Agapostemon\\) angelicus|Agapostemon \\(Agapostemon\\) texanus") ~ 'Agapostemon angelicus/texanus',
    str_detect(scientificname, "Hylaeus \\(Prosopis\\) affinis|Hylaeus \\(Prosopis\\) modestus|Hylaeus affinis|Hylaeus modestus|Hylaeus affinis undefinable|Hylaeus cf. affinis") ~ 'Hylaeus affinis/modestus',
    str_detect(scientificname, "Diadasia rinconis|Diadasia australis") ~ 'Diadasia rinconis/australis',
    str_detect(scientificname,"Ceratina \\(Zadontomerus\\) calcarata|Ceratina calcarata") ~ 'Ceratina calcarata',
    str_detect(scientificname,"Ceratina \\(Zadontomerus\\) dupla|Ceratina dupla") ~ 'Ceratina dupla',
    str_detect(scientificname, coll("Perdita (Perdita) exclamans/ashmeadi")) ~ as.character(NA),
    str_detect(scientificname, coll("Perdita (Perdita) exclamans/triangulifera")) ~ as.character(NA),
    str_detect(scientificname, coll("Perdita (Perdita) exclamans")) ~ 'Perdita exclamans',
    str_detect(scientificname, coll("Perdita exclamans")) ~ 'Perdita exclamans',
    str_detect(scientificname, coll("Perdita (Perdita) ashmeadi")) ~ 'Perdita ashmeadi',
    str_detect(scientificname, coll("Perdita ashmeadi")) ~ 'Perdita ashmeadi',
    str_detect(scientificname, coll("Perdita (Perdita) triangulifera")) ~ 'Perdita triangulifera',
    str_detect(scientificname, coll("Perdita triangulifera")) ~ 'Perdita triangulifera',
    str_detect(scientificname, "Lasioglossum callidum or new species|Lasioglossum callidum") ~ 'Lasioglossum callidum',
    str_detect(scientificname, "Osmia clarescens|Osmia \\(Osmia\\) clarescens|Osmia sanrafaelae|Osmia \\(Osmia\\) sanrafaelae") ~ 'Osmia clarescens/sanrafaelae',
    str_detect(scientificname, "Lasioglossum obnubilum|Lasioglossum \\(Lasioglossyn\\) obnubilum|Lasioglossum lillipute|Lasioglossum \\(Lasioglossum\\) lillipute") ~ 'Lasioglossum obnubilum/lilliputense',
    str_detect(scientificname, "Lasioglossum ruficornis|Lasioglossum \\(Lasioglossum\\) ruficornis|Lasioglossum pulveris|Lasioglossum \\(Lasioglossum\\) pulveris") ~ 'Lasioglossum pulveris',
    str_detect(scientificname, "Hylaeus \\(Hylaeus rudbeckiae\\) rudbeckiae|Hylaeus rudbeckiae|Hylaeus granulatus|Hylaeus \\(Hylaeus\\) granulatus") ~ 'Hylaeus rudbeckiae/granulatus',
    str_detect(scientificname, "Agapostemon coloradinus|Agapostemon \\(Agapostemon\\) coloradinus|Agapostemon \\(Agapostemon\\) virescens|Agapostemon virescens") ~ 'Agapostemon coloradinus/virescens'
  )
  )
# Remove these names from parsed names, reappend the associated_unc_df at end step of name alignment. 
removal_amb_v <- associated_unc_df$scientificname

parsed_names <- parsed_names %>% 
  filter(!scientificname %in% removal_amb_v)

bee_names <- bee_names %>% 
  filter(!scientificName %in% removal_amb_v) # Update for continuity sake

# Save this version as well...
#write.table(parsed_names, file = "/home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/aggregated-occurrences/parsed-names/gbif-parsed-bee-names-r.txt", row.names = FALSE, sep="\t", quote = FALSE)


# Re append Authorship where written out separately. Create an if-else statement, if ScientificNameAuthorship was not filled out by the providers, then look at the authorship column that was broken out by gbif-parse. If there is a author in plant_names$ScientificNameAuthorship prioritize this, if not use gbif_parsed_plants$authorship. No authorship present will result in NA.
parsed_names$authorship <- ifelse(bee_names$scientificNameAuthorship != "", 
                                      bee_names$scientificNameAuthorship, parsed_names$authorship)

tdf <- parsed_names %>% 
  filter(parsed == FALSE) # Placeholders, BOLD sequence Names, and Hybrids to throw out...

parsed_names <- parsed_names %>% 
  filter(!parsed == FALSE)

################################################################## COALESCE NAMES ###############################################################################
name_subset <- parsed_names # Grabbing ALL of the names
name_subset <- distinct(parsed_names, scientificname, authorship, .keep_all = TRUE ) # Run this prior to using the shell script.

write.table(name_subset, "/home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/aggregated-occurrences/taxa-alignment/name_subset.txt", row.names = FALSE, sep="\t", quote = FALSE)

# Version where authorship has WhiteSpace removed (Previous scripts have identified this as a problematic feature)
name_subset_wout_Awhitespace <- name_subset # create a copy
name_subset_wout_Awhitespace$authorship <- gsub("\\s+", '', name_subset$authorship) # remove whitespcae

write.table(name_subset_wout_Awhitespace, file = "/home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/aggregated-occurrences/taxa-alignment/name_subset-wout-Awhitespace.txt", row.names = FALSE, sep="\t", quote = FALSE)

# Run Nomer via command line: Uses a bash loop I created that uses Nomer on each version of our parsed names: full name with authorship, full name with authorship - whitespace(A), Full name without Authorship, Genus+SpecificEpithet
# cd /home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/aggregated-occurrences/taxa-alignment
# ./Nomer-loop.sh 

# CanonicalNameWithMarker + Authorship Resolution
CNWM_A_Res <- read.delim("/home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/aggregated-occurrences/taxa-alignment/CNWM-A-resolution.txt", header = TRUE, quote = "")

# Change NONE to relationName
CNWM_A_Res <- CNWM_A_Res %>% 
  rename(relationName = NONE, resolvedName = canonicalnamewithmarker.1)

# CanonicalNameWithMarker + Authorship WhiteSpace removed Resolution
CNWM_AWS_Res <- read.delim("/home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/aggregated-occurrences/taxa-alignment/CNWM-AwoutWSP-resolution.txt", header = TRUE, quote = "")
CNWM_AWS_Res <- CNWM_AWS_Res %>% 
  rename(relationName = NONE, resolvedName = canonicalnamewithmarker.1)

# CanonicalNameWithMarker without Authorship Resolution
CNWM_only <-read.delim("/home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/aggregated-occurrences/taxa-alignment//CNWM-resolution.txt", header = TRUE, quote = "")
CNWM_only_Res <- CNWM_only %>% 
  rename(relationName = NONE, resolvedName = canonicalnamewithmarker.1)

# CanonicalName Resolution
CN_only <-read.delim("/home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/aggregated-occurrences/taxa-alignment//CN-resolution.txt", header = TRUE, quote = "")

CN_only_Res <- CN_only %>% 
  rename(relationName = NONE, resolvedName = canonicalname.1)


# Genus + specificEpithet resolution
SP_only <-read.delim("/home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/aggregated-occurrences/taxa-alignment//SP-resolution.txt", header = TRUE, quote = "")

SP_only_Res <- SP_only %>% 
  rename(relationName = NONE, resolvedName = ID.1)

# Bring in base parsed table. Remove noninformative fields 
name_subset_table <- name_subset %>% 
  #select(!14:21) %>%  # removes excess fields
  distinct(scientificname, type, canonicalnamecomplete, .keep_all = TRUE)

# First, change all of the NONE values to NA (NOMER syntax Correction)
CNWM_A_Res[CNWM_A_Res == "NONE"] <- NA
CNWM_AWS_Res[CNWM_AWS_Res == "NONE"] <- NA
CNWM_only_Res[CNWM_only_Res == "NONE"] <- NA
CN_only_Res[CN_only_Res == "NONE"] <- NA
SP_only_Res[SP_only_Res == "NONE"] <- NA
# By default NOMER still returns a the original verbatim name inputted even when resolution fails. Change this syntax via a new field called resolvedNameCorrected by using a CASE statement to make it clear these are NOT resolutions.

CNWM_A_Res <- sqldf("SELECT *,
                     CASE
                     WHEN relationName NOT NULL THEN resolvedName
                     WHEN relationName ISNULL THEN NULL
                     END AS resolvedNameCorrected
                     FROM CNWM_A_Res")

CNWM_AWS_Res <- sqldf("SELECT *,
                     CASE
                     WHEN relationName NOT NULL THEN resolvedName
                     WHEN relationName ISNULL THEN NULL
                     END AS resolvedNameCorrected
                     FROM CNWM_AWS_Res")

CNWM_only_Res <- sqldf("SELECT *,
                     CASE
                     WHEN relationName NOT NULL THEN resolvedName
                     WHEN relationName ISNULL THEN NULL
                     END AS resolvedNameCorrected
                     FROM CNWM_only_Res")

CN_only_Res <- sqldf("SELECT *,
                     CASE
                     WHEN relationName NOT NULL THEN resolvedName
                     WHEN relationName ISNULL THEN NULL
                     END AS resolvedNameCorrected
                     FROM CN_only_Res")

SP_only_Res <- sqldf("SELECT *,
                     CASE
                     WHEN relationName NOT NULL THEN resolvedName
                     WHEN relationName ISNULL THEN NULL
                     END AS resolvedNameCorrected
                     FROM SP_only_Res")

# Additional Mutation required for name_table_subset to match IDs with SP_only_Res: Combine genusOrAbove + specificEpithet 
name_subset_table <- name_subset_table %>% 
  unite('species', c(genusorabove,specificepithet), sep = " ")

# Change the name of SP_only_Res to match 
SP_only_Res <- SP_only_Res %>% 
  rename(species = ID)


# Now try using COALESCE METHOD in SQL 

# First Combine our Tables....
Combined_names <- sqldf("SELECT DISTINCT name_table.canonicalnamewithmarker AS canonicalNameWithMarker, first_res.relationName AS firstRelation, first_res.resolvedNameCorrected AS firstResolvedName, name_table.scientificname AS verbatimScientificName, second_res.relationName AS secondRelation, second_res.resolvedNameCorrected AS secondResolvedName, third_res.relationName AS thirdRelation, third_res.resolvedNameCorrected AS thirdResolvedName, fourth_res.relationName AS fourthRelation, fourth_res.resolvedNameCorrected AS fourthResolvedName, fifth_res.relationName AS fifthRelation, fifth_res.resolvedNameCorrected AS fifthResolvedName
                                           FROM name_subset_table AS name_table
                                           LEFT JOIN CNWM_A_Res AS first_res ON name_table.canonicalnamewithmarker = first_res.canonicalnamewithmarker
                                           LEFT JOIN CNWM_AWS_Res AS second_res ON name_table.canonicalnamewithmarker = second_res.canonicalnamewithmarker
                                           LEFT JOIN CNWM_only_Res AS third_res ON name_table.canonicalnamewithmarker = third_res.canonicalnamewithmarker
                                           LEFT JOIN CN_only_Res AS fourth_res ON name_table.canonicalname = fourth_res.canonicalname
                                           LEFT JOIN SP_only_Res AS fifth_res ON name_table.species = fifth_res.species")

# Use COALESCE to Create a relationFound Field
coalesced_names <- sqldf("SELECT verbatimScientificName, 
                                  canonicalNameWithMarker, firstRelation, secondRelation, thirdRelation, fourthRelation, fifthRelation, firstResolvedName, secondResolvedName, thirdResolvedName, fourthResolvedName, fifthResolvedName,
                          COALESCE(firstRelation, secondRelation, thirdRelation, fourthRelation, fifthRelation) relationFound
                         
                          FROM Combined_names")
# Create a CASE statement to identify at which level relation was found using COALESCE via a field called whenRelationFound
coalesced_org <- sqldf("SELECT *, 
                       CASE 
                       WHEN firstRelation NOT NULL THEN 'CNWMandAuthor'
                       WHEN secondRelation NOT NULL THEN 'CNWMandAuthorWSP'
                       WHEN thirdRelation NOT NULL THEN 'CNWMonly'
                       WHEN fourthRelation NOT NULL THEN 'CNonly'
                       WHEN fifthRelation NOT NULL THEN 'SPonly'
                       END AS whenRelationFound
                       FROM coalesced_names")

# Create a CASE statement to identify what the final resolved Name should be via a field called finalResolution 
coalesced_resolution <- sqldf("SELECT DISTINCT *, 
                              CASE 
                              WHEN whenRelationFound = 'CNWMandAuthor' THEN firstResolvedName
                              WHEN whenRelationFound = 'CNWMandAuthorWSP' THEN secondResolvedName
                              WHEN whenRelationFound = 'CNWMonly' THEN thirdResolvedName
                              WHEN whenRelationFound = 'CNonly' THEN fourthResolvedName
                              WHEN whenRelationFound = 'SPonly' THEN fifthResolvedName
                              END AS finalResolution
                              FROM coalesced_org")
coalesced_resolution <- distinct(coalesced_resolution, verbatimScientificName, canonicalNameWithMarker, finalResolution, .keep_all = TRUE)

# Now we need to identify where multiple mappings Occur, and whether resolution should even happen for these circumstances (i.e. is author needed?)
coalesced_resolution <- coalesced_resolution %>% 
  group_by(verbatimScientificName) %>% 
  mutate(multipleMappings = case_when(
    !n_distinct(finalResolution) == 1 ~ TRUE,
    TRUE ~ FALSE))


# Now look at when relation is found relative to coalescent resolution. If its found at the same time, this is uninformative and therefore 'bad' to keep
multmaps <- coalesced_resolution %>% 
  filter(multipleMappings == TRUE) %>% 
  group_by(verbatimScientificName) %>% 
  filter(!n_distinct(whenRelationFound) == 1) # None found, Good!


#################################################################################################################################################################

gbif_headers <- read.delim("/home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/gbif-data/headers.txt")
# 58,60,61,64,65,68,90,103,107,108,109,111,112,125,126,127,129,138,139,140,171,189,203,207,208
# gbif: cat occurrence.txt | cut -f 58,60,61,64,65,68,90,103,107,108,109,111,112,125,126,127,129,138,139,140,171,189,203,207,208 > occurrence-cut.txt
gbif_test <- read.delim("/home/jt-miller/Pollen-Project/Pollen-DB/raw-data/Bee-Occurrence-Data/gbif-data/cut-data/occurrence-cut.txt")
#################################################################################################################################################################
