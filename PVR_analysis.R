
#### Project Information ----
#### Project Title: Quarry rock reef design features influence fish assemblage structure across a systematically heterogenous restoration reef
#### Authors: James W. Sturges & Jeremy T. Claisse
#### Last Updated: August 26th, 2024

#### Required Packages ----
library(Hmisc) # For data analysis, including descriptive statistics, data manipulation, and graphics
library(RColorBrewer) # Provides color palettes for visualizations
library(randomcoloR) # Generates distinct colors for use in plots
library(vegan) # For community ecology analyses, including ordination methods
library(tidyverse) # A collection of packages for data manipulation, visualization, and analysis
library(flextable) # For creating and formatting tables in R
library(ggrepel) # Provides geoms for ggplot2 to repel overlapping text labels
library(officer) # For manipulating Word and PowerPoint documents from R
library(wesanderson) # Provides Wes Anderson movie color palettes for visualizations
library(dendextend) # For extending dendrogram functionality in R
library(ggdendro) # Provides tools for creating dendrograms and dendrogram visualizations with ggplot2
library(hms) # For managing and manipulating time-of-day values
library(cluster) # For cluster analysis and partitioning
library(cowplot) # for making combined figures
library(png) # to read in module diagram file
library(magick) # to plot module diagram (w/ cowplot package)

#### Event Measure Observation File Cleaning ----

#Read in all fish observations form Event Measure observation text files
#raw Event Measure files are available in this repo but require licensed access keys to run the software
dat_EMObs = read.csv("data/ALL_Lengths.csv")

#Rename attribute columns from EM as meaningful reef characteristics
#Module_Side is published as either East/West or Inshore/Offshore depending on Module orientation relative to shore.
#Habitat_Type is either Mid_High, Mid_Medium, Mid_Low, or Ecotone_High, Ecotone_Medium, Ecotone_Low
#reflects zones (Ecotone or Reef)
#Module is the unique name of the 18 PVR modules
dat_EMObs <- dat_EMObs %>%
  rename(Module_Side = Attribute9,
         Habitat_Type = Attribute10,
         Module = OpCode) %>% 
  mutate(Genus_spp = str_c(Genus, Species, sep = " "),
         Habitat_Type = recode(Habitat_Type, 
                               High = "Mid_High", 
                               Medium = "Mid_Medium", 
                               Low = "Mid_Low"))

#Here we create a date time based on the Comment column
#Note that some observations will not parse 
#Those 291 rows have other comments that are not date times this is OK
dat_EMObs <- dat_EMObs %>%
  mutate(Start_Time = dmy_hms(Comment), 
         Video_Elapsed_hms = as_hms(Time * 60))

#Fill in that column so that every point for a given module has the same start time
dat_EMObs <- dat_EMObs %>%
  group_by(Module) %>%
  fill(Start_Time) %>%
  # min Video_Elapsed_hms should be Code == Time for each module
  mutate(Video_Sync_hms = min(Video_Elapsed_hms), 
         # subtract off sync time prior to filming clock in video       
         Video_Corrected_hms = Video_Elapsed_hms - Video_Sync_hms, 
         # add correct video time to clock time to get real date time
         Real_Time = Start_Time + Video_Corrected_hms)

# Calculate the transect duration time 
# Used to estimate how long we spent surveying each section 
# We need the time of each survey and the area viewed in the camera to calculate fish densities
# This created a df of just the Start and End points 
#(360 rows across all 18 Modules with 12 possible combinations of reef characteristics) 
Transect_Duration <- dat_EMObs %>%
  group_by(Module, Module_Side, Habitat_Type) %>%
  select(Module, Module_Side, Habitat_Type, Code, Real_Time) %>%
  filter(Code %in% c("Start", "End"))

#creates a table of ellapsed time
#Started with 10 'transects' per Module 
#(Ecotone, Mid, High, Medium, Low on each side) 
#Mid is all reef, Ecotone is all ecotone
Start_End_Time <- Transect_Duration %>% 
  pivot_wider(names_from = Code, values_from = Real_Time) %>% 
  mutate(Diff_s = End - Start) %>%
  ungroup()

# Calculates the average swim rate along the ecotone
# Module specific ectone swim rate is the average of both sides time
# We divided module ecotone times for both sides by by 48 meters to get an average swim rate per ecotone section 
# We can apply Module specific ectone swim rate to mid reef times
# to estimate distance swam over submodules assuming = swim rates during given Module
Avg_Eco_Time_Per_Mod <- Start_End_Time %>%
  group_by(Module) %>%
  filter(Habitat_Type %in% "Ecotone") %>%
  summarize(Mean_Ecotone_Time = mean(Diff_s))

# ecotone transects across all 18 modules
Avg_Eco_Time_Per_Mod <- Avg_Eco_Time_Per_Mod %>%
  mutate(Mean_Ecotone_Time_Num = as.numeric(Mean_Ecotone_Time))

# Calculates the average swim rate along the ecotone for each Module
# These values are multiplied with the transect times for Midlines & sub-Modules to estimate distances swam
Avg_Eco_Time_Per_Mod <- Avg_Eco_Time_Per_Mod %>%
  mutate(Eco_Swim_Rate_m_s = 48/Mean_Ecotone_Time_Num)

Start_End_Time <- Start_End_Time %>%
  left_join(Avg_Eco_Time_Per_Mod) %>%
  mutate(Dis_Swim_m = as.numeric(Diff_s) * Eco_Swim_Rate_m_s)

# Estimate distance with the mi  reef total time
Mid_Time_Per_Mod <- Start_End_Time %>%
  group_by(Module) %>%
  filter(Habitat_Type %in% "Mid")

Mid_Time_Per_Mod <- Mid_Time_Per_Mod %>%
  mutate(Diff_num = as.numeric(Diff_s))

# We divided the ecotone into three sections and assigned fish to these sections based on the observation time.
# This approach helps account for differences in the ecotone when it is adjacent to different submodules.
# For Offshore/West ecotones, the first ecotone section surveyed is adjacent to high submodules.
# For Inshore/East ecotones, the first ecotone section surveyed is adjacent to medium submodules.
Start_End_Time_w <- Start_End_Time  %>%
  filter(Habitat_Type == "Ecotone" & Module_Side == "West") %>% 
  mutate(Diff_third_s = Diff_s/3, 
         End_high = Start + Diff_third_s,
         End_low = Start + (2*Diff_third_s),
         End_medium = End)

#Here we modify the ecotone habitat type in 3 sections of equal length
#Sections are named for the submodule they are adjacent to
Start_End_Time_w_h <- Start_End_Time_w %>% 
  mutate(Habitat_Type = "Ecotone_High",
         Start = Start,
         End = End_high) %>% 
  select(Module:Diff_third_s)

Start_End_Time_w_m <- Start_End_Time_w %>% 
  mutate(Habitat_Type = "Ecotone_Medium",
         Start = End_low,
         End = End_medium) %>% 
  select(Module:Diff_third_s)

Start_End_Time_w_l <- Start_End_Time_w %>% 
  mutate(Habitat_Type = "Ecotone_Low",
         Start = End_high,
         End = End_low) %>% 
  select(Module:Diff_third_s)

#Going to do the same thing as above for the East/Inshore side
Start_End_Time_e <- Start_End_Time  %>%
  filter(Habitat_Type == "Ecotone" & Module_Side == "East") %>% 
  mutate(Diff_third_s = Diff_s/3, 
         End_high = End,
         End_low = Start + (2*Diff_third_s),
         End_medium = Start + Diff_third_s,)

Start_End_Time_e_h <- Start_End_Time_e %>% 
  mutate(Habitat_Type = "Ecotone_High",
         Start = End_low,
         End = End) %>% 
  select(Module:Diff_third_s)

Start_End_Time_e_m <- Start_End_Time_e %>% 
  mutate(Habitat_Type = "Ecotone_Medium",
         Start = Start,
         End = End_medium) %>% 
  select(Module:Diff_third_s)

Start_End_Time_e_l <- Start_End_Time_e %>% 
  mutate(Habitat_Type = "Ecotone_Low",
         Start = End_medium,
         End = End_low) %>% 
  select(Module:Diff_third_s)

#Create a new start_end_time table with each section
#Now there are 6 habitat/transect types on every module (high, medium, low reef or ecotone)
Start_End_Time_eco <- bind_rows(Start_End_Time_w_h, Start_End_Time_w_m, Start_End_Time_w_l, 
                                Start_End_Time_e_h, Start_End_Time_e_m, Start_End_Time_e_l) %>% 
  arrange(Module, Module_Side, Habitat_Type) %>% 
  mutate(Eco_Int = interval(start = Start, end = End), 
         Habitat_Type_eco = Habitat_Type) %>% 
  select(Module, Module_Side, Habitat_Type_eco, Eco_Int)

# Create updated version of reef characteristics in dat_EMObs main dataset 
dat_EMObs_eco <- dat_EMObs %>% 
  filter(Habitat_Type == "Ecotone") %>%
  # expand so row for each fish repeats 3x, one for each ecotone type  
  inner_join(Start_End_Time_eco) %>% 
  mutate(Habitat_Type = if_else(Real_Time %within% Eco_Int, Habitat_Type_eco, NA_character_)) %>% 
  drop_na(Habitat_Type)

dat_fish <- dat_EMObs %>% 
  bind_rows(dat_EMObs_eco)

dat_fish <- dat_fish %>%   
  filter(Habitat_Type %in% c("Ecotone_High", 
                             "Ecotone_Low",
                             "Ecotone_Medium", 
                             "Mid_High",
                             "Mid_Low", 
                             "Mid_Medium")) 
#filter
dat_fish <- dat_fish %>% 
  filter(!Code %in% c("Start","End")) 

# Set factor level order for tables, plots etc.
dat_fish <- dat_fish %>%
  mutate(Habitat_Type = fct_relevel(Habitat_Type, "Ecotone_High", 
                                    "Ecotone_Medium",
                                    "Ecotone_Low",
                                    "Mid_High", 
                                    "Mid_Medium",
                                    "Mid_Low")) %>%
  arrange(Module, Module_Side, Habitat_Type)

#going to take the Start_End_Time table and mutate it to get transect area
#the ecotone areas should set to 48m x 2m
#the swim rate for each module will be applied to the mid reef
Start_End_Mids <- Start_End_Time %>% 
  filter(Habitat_Type %in% c("Mid_High",
                             "Mid_Low",
                             "Mid_Medium"))

#We have already calculated the distance swam along the mid reef
#We take the distance swam in meters and multiple it by 4 m 
# (transect width cut off determined in by horizontal distance in Event Measure)
#the tsect_area_m2 is used for fish density calculations
Start_End_Mids <- Start_End_Mids %>% 
  mutate(tsect_area_m2 = Dis_Swim_m * 4)

#Creates a single table that had all transect areas
#There are 12 combinations of reef characteristics per Module (6 on each side)
#High, Medium, Low Ecotone or Mid Reef on the Inshore/East and Offshore/West side depending on orientation (paralle or perpendicular)
#All ecotone distances were manually set to 16 m
#All ecotone transect areas were set to 64 m^2
#The mid reef areas are based on module specific swim rates over estimated distances
Start_End_Complete <- Start_End_Mids %>% 
  bind_rows(Start_End_Time_eco)

#merges columns with NAs using coaleasce()
#Here we take the Habitat_Type_eco for ecotones and replace NAs in Habitat_Type
Start_End_Complete <- Start_End_Complete %>% 
  mutate(Habitat_Type = coalesce(Habitat_Type, Habitat_Type_eco))

#Start and End times for ecotones 
#Fill the start column using coalesce and int_start function
Start_End_Complete <- Start_End_Complete %>% 
  mutate(Start = coalesce(Start, int_start(Eco_Int)))

#Fill the End column with int_end function
Start_End_Complete <- Start_End_Complete %>% 
  mutate(End = coalesce(End, int_end(Eco_Int)))

#Manually set the ecotone distance to 16m for all segments 
Start_End_Complete <- Start_End_Complete %>% 
  mutate(Dis_Swim_m = replace_na(Dis_Swim_m, 16))

#Manually set the area survey to 64 square meters squared for ecotone sections
Start_End_Complete <- Start_End_Complete %>% 
  mutate(tsect_area_m2 = replace_na(tsect_area_m2, 64))

#arrange values by Module
Start_End_Complete <- Start_End_Complete %>% 
  arrange(Module)

#Fill in missing values from 
Start_End_Complete <- Start_End_Complete %>% 
  group_by(Module) %>% 
  fill(Mean_Ecotone_Time, Mean_Ecotone_Time_Num, Eco_Swim_Rate_m_s)

#recalculated the difference between start and end times 
Start_End_Complete <- Start_End_Complete %>% 
  mutate(Diff_s = End - Start)

#Then I dropped the last two columns 
Start_End_Complete <- Start_End_Complete %>% 
  select(-c("Habitat_Type_eco", "Eco_Int"))

#Determines that we swam more area on higher modules
ht_dist_st <- Start_End_Complete %>% 
  group_by(Habitat_Type) %>% 
  summarise(mean_dis = mean(Dis_Swim_m),
            sd_dis = sd(Dis_Swim_m))

#Merges the Start_End_complete table to manuscript fish dataframe
dat_fish <- 
  left_join(dat_fish, Start_End_Complete, by = c("Module", "Module_Side", "Habitat_Type"))

#calculate density of an individual fish as 1/area of initial observation
dat_fish <- dat_fish %>% 
  mutate(density_m2 = 1/tsect_area_m2)

# Further cleaning needed to remove Unknown species and stop points
dat_fish <- dat_fish %>% 
  filter(Genus_spp != "Pause_Start Pause_Start",
         Genus_spp != "Unknown Unknown")


#We transform fish density values to a more realistic context of fish/100m^2 
dat_fish <- dat_fish %>% 
  mutate(dens_100m2 = density_m2*100)


# designates Modules as either 3 or 4 m types 
#based on the maximum height of the high relief submodules
dat_fish <- dat_fish %>% 
  mutate(mid_high_hgt_m = (case_when(startsWith(Module, "2") ~ "4",
                                     startsWith(Module, "6") ~ "4",
                                     startsWith(Module, "8") ~ "4",
                                     startsWith(Module, "4") ~ "3",
                                     startsWith(Module, "5") ~ "3",
                                     startsWith(Module, "7") ~ "3")))

#Designates module orientation based on Figure 1 map and sonar scans
dat_fish <- dat_fish %>% 
  mutate(Orientation = case_when(startsWith(Module, "2A") ~ "Perpendicular",
                                 startsWith(Module, "2B") ~ "Parallel",
                                 startsWith(Module, "2C") ~ "Parallel",
                                 startsWith(Module, "4") ~ "Parallel",
                                 startsWith(Module, "5") ~ "Parallel",
                                 startsWith(Module, "6") ~ "Parallel",
                                 startsWith(Module, "7A") ~ "Parallel",
                                 startsWith(Module, "7B") ~ "Perpendicular",
                                 startsWith(Module, "7C") ~ "Perpendicular",
                                 startsWith(Module, "8") ~ "Perpendicular"))

# filters out any fish measured beyond 2 m on either side of the camera center point
dat_fish_2m_wide <- dat_fish %>% 
  filter(MidX <= 2000 & MidX >= -2000)


#Imports table of dive events
dive_dat <- read.csv("data/DiveEvent.csv", fileEncoding = 'UTF-8-BOM')

#selects desired coluumns for Table 1
dive_dat <- dive_dat %>% 
  select(Module, construction_date, construction_month, construction_group, survey_date, survey_time)


#creates a transect column that reflects 24 possible reef characteristics possible
#all module have the same 12 reef characteristics but veried by orientation 
transect_var <- dat_fish %>% 
  select(Module, Module_Side, Habitat_Type, Orientation, mid_high_hgt_m) %>% 
  mutate(transect = paste(Module, Module_Side, Habitat_Type)) %>% 
  distinct()

#splits habitat type into two characteristics 
#Ecotone vs Mid
#Sub-mods into three groups High, Medium, Low
transect_var <- transect_var %>% 
  separate(Habitat_Type, c("Zone", "Sub_Mod"), remove = F)

#and then add our field notes to transect variables
transect_var <- transect_var %>% 
  left_join(dive_dat)

transect_var <- transect_var %>% 
  mutate(Block = case_when(startsWith(Module, "2") ~ "2",
                           startsWith(Module, "4") ~ "4",
                           startsWith(Module, "5") ~ "5",
                           startsWith(Module, "6") ~ "6",
                           startsWith(Module, "7") ~ "7",
                           startsWith(Module, "8") ~ "8"))


## create reduced versions of transect_var for module specific Table 1 metrics
module_var <- transect_var %>% 
  select(Module, Orientation, mid_high_hgt_m, construction_date, survey_date, 
         survey_time, construction_group, Block) %>%
  distinct() %>% 
  arrange(Module)


# 11 focal species were used in this study
# These secies were all observed on at least 12/18 modules 
# and were the 11 most abundany by count
focal_spp <- c("Chromis punctipinnis",
               "Paralabrax clathratus",
               "Oxyjulis californica",
               "Semicossyphus pulcher",
               "Embiotoca jacksoni",
               "Hypsurus caryi",
               "Paralabrax nebulifer",
               "Girella nigricans",
               "Damalichthys vacca",
               "Sebastes serranoides",
               "Halichoeres semicinctus")


# Creates a new unique reef metric column that combines the Orientation 
dat_fish_2m_wide <- dat_fish_2m_wide %>% 
  mutate(os = paste(Module_Side, Orientation),
         ht_os = paste(os, Habitat_Type))



# When we want to use size class data we will only want to use fish that could be measure
# We commented the fish observations that could not be measured or measurements were above 20 mm in precision
# We remove those points for all length reports but may keep them for all density counts
dat_fish_l <- dat_fish_2m_wide %>% 
  filter(!Comment %in% "no vis right camera") %>% 
  filter(!Comment %in% "no vis left camera") %>% 
  filter(Precision < 20)

#remain code in this section cleans adds reef characteristics features to the lengths only dataset
dat_fish_l <- dat_fish_l %>% 
  separate(Habitat_Type, c("Zone", "Sub_Mod"), remove = F) %>% 
  mutate(Block = case_when(startsWith(Module, "2") ~ "2",
                           startsWith(Module, "4") ~ "4",
                           startsWith(Module, "5") ~ "5",
                           startsWith(Module, "6") ~ "6",
                           startsWith(Module, "7") ~ "7",
                           startsWith(Module, "8") ~ "8"))


dat_fish_l <- dat_fish_l %>% 
  mutate(transect = paste(Module, Module_Side, Habitat_Type),
         dens_100m2_4rt = dens_100m2^0.25) %>% 
  left_join(transect_var)


dat_fish_l <- dat_fish_l %>% 
  mutate(mid_high_hgt_m = as.character(mid_high_hgt_m))

dat_fish_l <- dat_fish_l %>% 
  mutate(Mid_High_3_4 = paste(Habitat_Type, mid_high_hgt_m))

dat_fish_l <- dat_fish_l %>% 
  mutate(Mid_High_3_4 = str_replace(Mid_High_3_4, " ", "_"))

dat_fish_l <- dat_fish_l %>% 
  mutate(Mid_High_3_4 = as.character(Mid_High_3_4))

dat_fish_l <- dat_fish_l %>% 
  mutate(Mid_High_3_4 = (case_when(startsWith(Mid_High_3_4, "Ecotone_High") ~ "Ecotone_High",
                                   startsWith(Mid_High_3_4, "Ecotone_Medium") ~ "Ecotone_Medium",
                                   startsWith(Mid_High_3_4, "Ecotone_Low") ~ "Ecotone_Low",
                                   startsWith(Mid_High_3_4, "Mid_High_3") ~ "Mid_High_3",
                                   startsWith(Mid_High_3_4, "Mid_High_4") ~ "Mid_High_4",
                                   startsWith(Mid_High_3_4, "Mid_Low") ~ "Mid_Low",
                                   startsWith(Mid_High_3_4, "Mid_Medium") ~ "Mid_Medium")))

#### Adjusting for Sampling Effort ----

# As expected we traveled further on taller modules
# We standardized ecotone survey lengths to 16 m and adjusted reef densities from estimated reef transect lengths
ht_dist_st <- ht_dist_st %>% 
  mutate(new_names = case_when(startsWith(Habitat_Type, "Mid_High") ~ "High Relief",
                               startsWith(Habitat_Type, "Ecotone_High") ~ "High Ecotone",
                               startsWith(Habitat_Type, "Mid_Low") ~ "Low Relief",
                               startsWith(Habitat_Type, "Ecotone_Low") ~ "Low Ecotone",
                               startsWith(Habitat_Type, "Mid_Medium") ~ "Medium Relief",
                               startsWith(Habitat_Type, "Ecotone_Medium") ~ "Medium Ecotone"))


#creates table of the the average length of 6 habitat/transect types
#table not reported in the manuscript but lengths are mentioned as text
ht_dist_ft <- flextable(ht_dist_st,
                        col_keys = c("new_names", "mean_dis",
                                     "sd_dis")) %>% 
  add_header_row(colwidths = 3, values = c("Estimated Transect Length (m)")) %>% 
  set_header_labels(new_names = "Habitat Type",
                    mean_dis = "Mean",
                    sd_dis = "sd") %>% 
  colformat_double(digits = 1) %>% 
  theme_box() %>% 
  align(align = "center") %>% 
  align(part = "header", align = "center") %>% 
  bg(j = "mean_dis", i = ~ mean_dis < 16.1, bg = "tan", part = "body") %>% 
  bg(j = "mean_dis", i = ~ mean_dis > 16.1, bg = "grey", part = "body") %>% 
  bg(j = "new_names", i = ~ mean_dis < 16.1, bg = "tan", part = "body") %>% 
  bg(j = "new_names", i = ~ mean_dis > 16.1, bg = "grey", part = "body") %>% 
  bg(j = "sd_dis", i = ~ mean_dis < 16.1, bg = "tan", part = "body") %>% 
  bg(j = "sd_dis", i = ~ mean_dis > 16.1, bg = "grey", part = "body")


# create new variables for Zone and current flow 

dat_fish_2m_wide <- dat_fish_2m_wide %>% 
  mutate(Zone = case_when(startsWith(Habitat_Type, "Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "Mid") ~ "Mid"))

dat_fish_2m_wide <- dat_fish_2m_wide %>% 
  filter(!c(Genus_spp %in% "Chromis punctipinnis"& Length > 260))

dat_fish_l <- dat_fish_l %>% 
  filter(!c(Genus_spp %in% "Chromis punctipinnis"& Length > 260))

dat_fish_2m_wide <- dat_fish_2m_wide %>% 
  arrange(Genus_spp, desc(Length))



dat_fish_2m_wide <- dat_fish_2m_wide %>% 
  mutate(current =  case_when(startsWith(Module_Side, "West") ~ "Upcurrent",
                              startsWith(Module_Side, "East") ~ "Downcurrent"))

#### Creating Primary Data Frame ----

# 31 species were observed over 216 "transects" on 18 modules
# creates density values for each species across each unique transect
dat_fish_t <- dat_fish_2m_wide %>% 
  group_by(Module, Module_Side, Habitat_Type, Genus_spp) %>% 
  summarise(dens_100m2 = sum(dens_100m2)) %>% 
  ungroup()

#Adds in 0's for fish absences
dat_fish_t <- dat_fish_t %>% 
  complete(Genus_spp, nesting(Module, Module_Side, Habitat_Type),  
           fill = list(dens_100m2 = 0)) %>% 
  arrange(Module, Module_Side, Habitat_Type, Genus_spp)  %>% 
  left_join(transect_var)

#relevel variables for reporting
dat_fish_t <- dat_fish_t %>%
  mutate(Module = factor(Module, levels = c("2A", "2B", "2C",
                                            "4D", "4B", "4C",
                                            "5A", "5B", "5C",
                                            "6A", "6D", "6C",
                                            "7A", "7B", "7C",
                                            "8A", "8B", "8C")))
dat_fish_t <- dat_fish_t %>%
  mutate(Habitat_Type = factor(Habitat_Type, levels = c("Ecotone_High",
                                                        "Ecotone_Medium",
                                                        "Ecotone_Low",
                                                        "Mid_High",
                                                        "Mid_Medium",
                                                        "Mid_Low")))
dat_fish_t <- dat_fish_t %>%
  mutate(Sub_Mod = factor(Sub_Mod, levels = c("High", "Medium", "Low")))

#creates unique factor columns
dat_fish_t <- dat_fish_t %>% 
  mutate(os = paste(Module_Side, Orientation),
         os_ht = paste(os, Habitat_Type))

#assigns a cluster group determined quantitatively in further sections
#needed earlier to generate summary tables as they appear in text
dat_fish_t <- dat_fish_t %>% 
  mutate(cluster = (case_when(startsWith(os_ht, "West Perpendicular Mid_High") ~ "Group 4",
                              startsWith(os_ht, "West Perpendicular Ecotone_High") ~ "Group 3",
                              startsWith(os_ht, "West Perpendicular Mid_Low") ~ "Group 3",
                              startsWith(os_ht, "West Perpendicular Ecotone_Low") ~ "Group 3",
                              startsWith(os_ht, "West Perpendicular Mid_Medium") ~ "Group 2",
                              startsWith(os_ht, "West Perpendicular Ecotone_Medium") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Mid_High") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Ecotone_High") ~ "Group 1",
                              startsWith(os_ht, "East Perpendicular Mid_Low") ~ "Group 4",
                              startsWith(os_ht, "East Perpendicular Ecotone_Low") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Mid_Medium") ~ "Group 4",
                              startsWith(os_ht, "East Perpendicular Ecotone_Medium") ~ "Group 3",
                              startsWith(os_ht, "West Parallel Mid_High") ~ "Group 2",
                              startsWith(os_ht, "West Parallel Ecotone_High") ~ "Group 2",
                              startsWith(os_ht, "West Parallel Mid_Low") ~ "Group 4",
                              startsWith(os_ht, "West Parallel Ecotone_Low") ~ "Group 2",
                              startsWith(os_ht, "West Parallel Mid_Medium") ~ "Group 2",
                              startsWith(os_ht, "West Parallel Ecotone_Medium") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Mid_High") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Ecotone_High") ~ "Group 4",
                              startsWith(os_ht, "East Parallel Mid_Low") ~ "Group 5",
                              startsWith(os_ht, "East Parallel Ecotone_Low") ~ "Group 5",
                              startsWith(os_ht, "East Parallel Mid_Medium") ~ "Group 5",
                              startsWith(os_ht, "East Parallel Ecotone_Medium") ~ "Group 5")))


#### Table 1: Module Characteristics and Survey Events ----

# List of all species observed in study and total abundance identified 
spp_ID <- dat_fish_2m_wide %>% 
  group_by(Genus_spp, Code) %>% 
  count(Genus_spp, sort = T) %>% 
  arrange(desc(n))

dat_fish_l = dat_fish_l %>% 
  mutate(Length = as.numeric(Length))

dat_fish_min_max_l <- dat_fish_l %>% 
  group_by(Genus_spp) %>% 
  summarise(min_length = min(Length),
            max_length = max(Length),
            mean_length = mean(Length),
            sd_length = sd(Length),
            median_length = median(Length))

dat_fish_l_obs <- dat_fish_l %>% 
  group_by(Genus_spp, .drop = F) %>% 
  count(Genus_spp, sort = T)

dat_fish_l_obs <- dat_fish_l_obs %>% 
  rename(measured = n)

spp_ID <- spp_ID %>% 
  left_join(dat_fish_l_obs)

spp_ID <- spp_ID %>% 
  left_join(dat_fish_min_max_l)

all_spp_mods_obs <- dat_fish_2m_wide %>% 
  group_by(Genus_spp) %>% 
  summarise(mods_obs = n_distinct(Module))

spp_ID <- spp_ID %>% 
  left_join(all_spp_mods_obs)


#create a table of module characteristics 
mod_ft <- flextable(module_var, 
                    col_keys = c("Module", "Orientation", "construction_group", "mid_high_hgt_m", "construction_date", "survey_date", "survey_time")) %>% 
  set_header_labels(Module = "Module",
                    Orientation = "Orientation",
                    construction_group = "Construction Phase",
                    mid_high_hgt_m = "Maximum Submodule Relief",
                    construction_date = "Construction Date",
                    survey_date = "Survey Date",
                    survey_time = "Survey Time"
  ) %>% 
  colformat_double(digits = 0) %>%
  #theme_box() %>%
  align(align = "center") %>%
  align(part = "header", align = "center")

mod_ft

write_csv(module_var, "tables/Table_1_module_&_survey_data.csv")

#### Table 2: All Species Observed w/ Length Distribution ----
#create a table of species identified
spp_ID_ft <- flextable(spp_ID,
                       col_keys = c("Genus_spp", "Code","mods_obs",
                                    "n", "measured", "min_length", "median_length", "max_length","mean_length", "sd_length")) %>%
  add_header_row(colwidths = c(1,1,1,1,1,5), values = c("Species Name", "Common Name", "Modules","Total", "Measured", "Total Length (mm)")) %>%
  set_header_labels(Genus_spp = "Species Name",
                    Code = "Common Name",
                    mods_obs = "Modules",
                    n = "Total",
                    measured = "Measured",
                    min_length = "Min",
                    mean_length = "Mean",
                    median_length = "Median",
                    max_length = "Max",
                    sd_length = "sd") %>% 
  colformat_double(digits = 0) %>%
  #theme_box() %>%
  align(align = "center") %>%
  align(part = "header", align = "center") %>%
  compose(j = "Genus_spp",
          value = as_paragraph(as_i(Genus_spp))) %>%
  merge_v(part = "header")

spp_ID_ft
write_csv(spp_ID, "tables/Table_2_spp_ID.csv")

# Focal species only summary table

dat_fish_min_max_l <- dat_fish_l %>% 
  group_by(Genus_spp) %>% 
  summarise(max_length = max(Length),
            min_length = min(Length),
            sd_length = sd(Length),
            mean_length = mean(Length),
            median_length = median(Length)) %>% 
  filter(Genus_spp %in% focal_spp)

dat_fish_min_max_l

dat_fish_l_obs <- dat_fish_l %>% 
  filter(Genus_spp %in% focal_spp) %>% 
  group_by(Genus_spp) %>% 
  count(Genus_spp, sort = T)

dat_fish_l_obs <- dat_fish_l_obs %>% 
  rename(measured = n)

dat_fish_l_obs

focal_spp_st <- dat_fish_2m_wide %>% 
  filter(Genus_spp %in% focal_spp) %>% 
  group_by(Genus_spp, Code) %>% 
  count(Genus_spp, sort = T) %>% 
  ungroup()

focal_spp_st <- focal_spp_st %>% 
  left_join(dat_fish_min_max_l)

focal_spp_st <- focal_spp_st %>% 
  left_join(dat_fish_l_obs)

focal_spp_mods_obs <- dat_fish_2m_wide %>% 
  filter(Genus_spp %in% focal_spp) %>% 
  group_by(Genus_spp) %>% 
  summarise(mods_obs = n_distinct(Module))

focal_spp_st <- focal_spp_st %>% 
  left_join(focal_spp_mods_obs)

spp_dens_ht <- dat_fish_2m_wide %>% 
  group_by(Genus_spp, Habitat_Type) %>% 
  summarise(Density_100m2 = sum(dens_100m2)) %>% 
  replace(is.na(.), 0)

focal_spp_dens_ht <- spp_dens_ht %>% 
  filter(Genus_spp %in% focal_spp)

focal_spp_dens_ht <- focal_spp_dens_ht %>% 
  pivot_wider(names_from = Habitat_Type, values_from = Density_100m2)

focal_spp_st <- focal_spp_st %>% 
  left_join(focal_spp_dens_ht)


focal_spp_ft <- flextable(focal_spp_st,
                          col_keys = c("Genus_spp", "Code", "mods_obs",
                                       "n", "measured", "min_length", "median_length", "max_length", "mean_length", "sd_length")) %>%
  add_header_row(colwidths = c(1,1,1,1,1,5), values = c("Species Name", "Common Name", "Modules", "Total", "Measured", "Total Length (mm)")) %>%
  set_header_labels(Genus_spp = "Species Name",
                    Code = "Common Name",
                    mods_obs = "Modules",
                    n = "Total",
                    measured = "Measured",
                    min_length = "Min",
                    median_length = "Median",
                    max_length = "Max",
                    mean_length = "Mean",
                    sd_length = "sd") %>% 
  colformat_double(digits = 0) %>%
  theme_box() %>%
  align(align = "center") %>%
  align(part = "header", align = "center") %>%
  compose(j = "Genus_spp",
          value = as_paragraph(as_i(Genus_spp))) %>%
  merge_v(part = "header")


# focal_spp_ft
# write_csv(focal_spp_st, "tables/focal_spp_st.csv")

#### Fish Density Calculations ----
# species level (across all transects)
dat_fish_t <- dat_fish_t %>% 
  mutate(current =  case_when(startsWith(os, "West") ~ "Up-current",
                              startsWith(os, "East") ~ "Down-current"))

dat_fish_t <- dat_fish_t %>% 
  mutate(Habitat_Type = str_replace_all(Habitat_Type, "Ecotone_High", "High Ecotone"),
         Habitat_Type = str_replace_all(Habitat_Type, "Ecotone_Medium", "Medium Ecotone"),
         Habitat_Type = str_replace_all(Habitat_Type, "Ecotone_Low", "Low Ecotone"),
         Habitat_Type = str_replace_all(Habitat_Type, "Mid_High", "High Relief"),
         Habitat_Type = str_replace_all(Habitat_Type, "Mid_Medium", "Medium Relief"),
         Habitat_Type = str_replace_all(Habitat_Type, "Mid_Low", "Low Relief"))


dens_sp <- dat_fish_t %>% 
  group_by(Genus_spp) %>%
  summarise(median_dens = median(dens_100m2), 
            mean_dens = mean(dens_100m2),
            sd_dens = sd(dens_100m2),
            max_dens = max(dens_100m2),
            min_dens = min(dens_100m2)) %>% 
  filter(Genus_spp %in% focal_spp) %>% 
  ungroup() %>% 
  arrange(desc(mean_dens))

set_flextable_defaults(font.family = "Arial", font.size = 9, digits = 1, padding = 0.1)

dens_sp_ft <- flextable(dens_sp,
                        col_keys = c("Genus_spp", "median_dens",
                                     "mean_dens", "sd_dens", "min_dens", "max_dens")) %>% 
  add_header_row(colwidths = c(1,5), values = c("Species Name", "Focal fish density (No./100 m2) ")) %>% 
  set_header_labels(Genus_spp = "Species Name",
                    median_dens = "Median",
                    mean_dens = "Mean",
                    sd_dens = "SD",
                    min_dens = "Min",
                    max_dens = "Max") %>% 
  colformat_double(digits = 1) %>% 
  theme_box() %>% 
  align(align = "center") %>% 
  align(part = "header", align = "center") %>% 
  compose(j = "Genus_spp",
          value = as_paragraph(as_i(Genus_spp))) %>% 
  merge_v(part = "header") 

#dens_sp_ft
# write_csv(dens_sp, "tables/dens_sp.csv")
# save_as_docx(dens_sp_ft, path = "dens_sp_ft.docx")

# sp dens by Zone
dens_sp_zone <- dat_fish_t %>% 
  group_by(Genus_spp, Zone) %>%
  summarise(median_dens = median(dens_100m2), 
            mean_dens = mean(dens_100m2),
            sd_dens = sd(dens_100m2),
            max_dens = max(dens_100m2),
            min_dens = min(dens_100m2)) %>% 
  filter(Genus_spp %in% focal_spp)

dens_sp_zone_ft <- flextable(dens_sp_zone,
                             col_keys = c("Genus_spp", "Zone", "median_dens",
                                          "mean_dens", "sd_dens", "max_dens", "min_dens")) %>% 
  add_header_row(colwidths = c(1, 1, 5), values = c("Species Name", "Zone", "Focal fish density (No./100 m2)")) %>% 
  set_header_labels(Genus_spp = "Species Name",
                    Zone = "Zone",
                    median_dens = "Median",
                    mean_dens = "Mean",
                    sd_dens = "Standard Deviation",
                    max_dens = "Max",
                    min_dens = "Min") %>% 
  colformat_double(digits = 1) %>% 
  theme_box() %>% 
  align(align = "center") %>% 
  align(part = "header", align = "center") %>% 
  compose(j = "Genus_spp",
          value = as_paragraph(as_i(Genus_spp))) %>% 
  merge_v(part = "header")

#dens_sp_zone_ft

# sp dens by Habitat_Type
dens_sp_ht <- dat_fish_t %>% 
  group_by(Genus_spp, Habitat_Type) %>%
  summarise(median_dens = median(dens_100m2), 
            mean_dens = mean(dens_100m2),
            sd_dens = sd(dens_100m2),
            max_dens = max(dens_100m2),
            min_dens = min(dens_100m2)) %>% 
  filter(Genus_spp %in% focal_spp)


dens_sp_ht_ft <- flextable(dens_sp_ht,
                           col_keys = c("Genus_spp", "Habitat_Type", "median_dens",
                                        "mean_dens", "sd_dens", "max_dens", "min_dens")) %>% 
  add_header_row(colwidths = c(1, 1, 5), values = c("Species Name", "Habitat Type", "Focal fish density (No./100 m2)")) %>% 
  set_header_labels(Genus_spp = "Species Name",
                    Habitat_Type = "Habitat Type",
                    median_dens = "Median",
                    mean_dens = "Mean",
                    sd_dens = "SD",
                    max_dens = "Max",
                    min_dens = "Min") %>% 
  colformat_double(digits = 1) %>% 
  theme_box() %>% 
  align(align = "center") %>% 
  align(part = "header", align = "center") %>% 
  compose(j = "Genus_spp",
          value = as_paragraph(as_i(Genus_spp))) %>% 
  merge_v(part = "header")

#dens_sp_ht_ft
# write_csv(dens_sp_ht, "tables/dens_sp_ht.csv")
# save_as_docx(dens_sp_ht_ft, path = "dens_sp_ht_ft.docx")

# sp dens by OS_HT
dens_sp_os_ht <- dat_fish_t %>% 
  group_by(Genus_spp, Orientation, current, Habitat_Type, os_ht) %>%
  summarise(median_dens = median(dens_100m2), 
            mean_dens = mean(dens_100m2),
            sd_dens = sd(dens_100m2),
            max_dens = max(dens_100m2),
            min_dens = min(dens_100m2)) %>% 
  filter(Genus_spp %in% focal_spp)


dens_sp_os_ht <- dens_sp_os_ht %>% 
  mutate(cluster = (case_when(startsWith(os_ht, "West Perpendicular Mid_High") ~ "Group 6",
                              startsWith(os_ht, "West Perpendicular Ecotone_High") ~ "Group 2",
                              startsWith(os_ht, "West Perpendicular Mid_Low") ~ "Group 3",
                              startsWith(os_ht, "West Perpendicular Ecotone_Low") ~ "Group 3",
                              startsWith(os_ht, "West Perpendicular Mid_Medium") ~ "Group 4",
                              startsWith(os_ht, "West Perpendicular Ecotone_Medium") ~ "Group 4",
                              startsWith(os_ht, "East Perpendicular Mid_High") ~ "Group 4",
                              startsWith(os_ht, "East Perpendicular Ecotone_High") ~ "Group 5",
                              startsWith(os_ht, "East Perpendicular Mid_Low") ~ "Group 2",
                              startsWith(os_ht, "East Perpendicular Ecotone_Low") ~ "Group 3",
                              startsWith(os_ht, "East Perpendicular Mid_Medium") ~ "Group 3",
                              startsWith(os_ht, "East Perpendicular Ecotone_Medium") ~ "Group 3",
                              startsWith(os_ht, "West Parallel Mid_High") ~ "Group 4",
                              startsWith(os_ht, "West Parallel Ecotone_High") ~ "Group 4",
                              startsWith(os_ht, "West Parallel Mid_Low") ~ "Group 3",
                              startsWith(os_ht, "West Parallel Ecotone_Low") ~ "Group 2",
                              startsWith(os_ht, "West Parallel Mid_Medium") ~ "Group 4",
                              startsWith(os_ht, "West Parallel Ecotone_Medium") ~ "Group 2",
                              startsWith(os_ht, "East Parallel Mid_High") ~ "Group 3",
                              startsWith(os_ht, "East Parallel Ecotone_High") ~ "Group 1",
                              startsWith(os_ht, "East Parallel Mid_Low") ~ "Group 2",
                              startsWith(os_ht, "East Parallel Ecotone_Low") ~ "Group 1",
                              startsWith(os_ht, "East Parallel Mid_Medium") ~ "Group 1",
                              startsWith(os_ht, "East Parallel Ecotone_Medium") ~ "Group 1")))

dens_sp_os_ht <- dens_sp_os_ht %>% 
  arrange(cluster)

dens_sp_os_ht <- dens_sp_os_ht %>% 
  mutate(os = paste(Orientation, current))

dens_sp_os_ht <- dens_sp_os_ht %>% 
  mutate(os = (case_when(startsWith(os, "Perpendicular Up-current") ~ "West",
                         startsWith(os, "Perpendicular Down-current") ~ "East",
                         startsWith(os, "Parallel Up-current") ~ "Offshore",
                         startsWith(os, "Parallel Down-current") ~ "Inshore")))

dens_sp_os_ht <- dens_sp_os_ht %>% 
  arrange(desc(Genus_spp))

dens_sp_os_ht_ft <- flextable(dens_sp_os_ht,
                              col_keys = c("Genus_spp", "Orientation", "os", "Habitat_Type", "cluster","min_dens", "median_dens","max_dens", "mean_dens", "sd_dens")) %>% 
  add_header_row(colwidths = c(1,1,1,1,1,5), values = c("Species Name", "Orientation", "Side", "Habitat Type", "Cluster", "Focal fish density (No./100 m2)")) %>% 
  set_header_labels(Genus_spp = "Species Name",
                    Orientation = "Orientation",
                    os = "Side",
                    Habitat_Type = "Habitat Type",
                    cluster = "Cluster",
                    min_dens = "Min",
                    median_dens = "Median",
                    max_dens = "Max",
                    mean_dens = "Mean",
                    sd_dens = "sd") %>% 
  colformat_double(digits = 1) %>% 
  padding(padding = 0.1) %>% 
  # theme_box() %>% 
  align(align = "center") %>% 
  align(part = "header", align = "center") %>% 
  compose(j = "Genus_spp",
          value = as_paragraph(as_i(Genus_spp))) %>% 
  merge_v(part = "header")

# dens_sp_os_ht_ft

# write_csv(dens_sp_os_ht, "tables/dens_sp_os_ht.csv")
# save_as_docx(dens_sp_os_ht_ft, path = "dens_sp_os_ht_ft.docx")


# ## SPP dens across haibtat types
# # sp dens by module
# dens_sp_mod <- dat_fish_t %>%
#   group_by(Genus_spp, Module) %>%
#   summarise(median_dens = median(dens_100m2),
#             mean_dens = mean(dens_100m2),
#             sd_dens = sd(dens_100m2),
#             max_dens = max(dens_100m2),
#             min_dens = min(dens_100m2)) %>%
#   filter(Genus_spp %in% focal_spp)
# 
# 
# ###CHANGE TO MODULE
# dens_sp_ht_ft <- flextable(dens_sp_ht,
#                            col_keys = c("Genus_spp", "Habitat_Type", "median_dens",
#                                         "mean_dens", "sd_dens", "max_dens", "min_dens")) %>%
#   add_header_row(colwidths = c(1, 1, 5), values = c("Species Name", "Habitat Type", "Focal fish density (No./100 m2)")) %>%
#   set_header_labels(Genus_spp = "Species Name",
#                     Habitat_Type = "Habitat Type",
#                     median_dens = "Median",
#                     mean_dens = "Mean",
#                     sd_dens = "Standard Deviation",
#                     max_dens = "Max",
#                     min_dens = "Min") %>%
#   colformat_double(digits = 1) %>%
#   theme_box() %>%
#   align(align = "center") %>%
#   align(part = "header", align = "center") %>%
#   compose(j = "Genus_spp",
#           value = as_paragraph(as_i(Genus_spp))) %>%
#   merge_v(part = "header")

# dens_sp_ht_ft

#### Figure S1: Module Assemblage NMDS ----
dat_fish_ht_mod_18 <- dat_fish_t %>% 
  group_by(Module, Genus_spp) %>% 
  summarise(dens_100m2 = mean(dens_100m2)) %>% 
  ungroup() %>% 
  left_join(
    distinct(select(transect_var, !c("Module_Side", "Habitat_Type", "Sub_Mod", "transect", "Zone")))
  )

wide_fish_ht_mod_18 <- dat_fish_ht_mod_18 %>% 
  mutate(dens_100m2_sqrt = dens_100m2^0.5) %>% 
  filter(Genus_spp %in% focal_spp) %>% 
  pivot_wider(id_cols = Module, names_from = Genus_spp, values_from = dens_100m2_sqrt)

names(wide_fish_ht_mod_18) <- str_replace_all(names(wide_fish_ht_mod_18), c(" " = "_"))



comm_fish_ht_mod_18 <- wide_fish_ht_mod_18 %>%
  column_to_rownames(var = "Module") %>%
  select(Chromis_punctipinnis:Semicossyphus_pulcher)


NMDS_comm_fish_ht_mod_18 <- metaMDS(comm_fish_ht_mod_18, 
                                    trymax = 200,
                                    distance = "bray",
                                    autotransform = F)


tibble_comm_fish_ht_mod_18 <- as_tibble(scores(NMDS_comm_fish_ht_mod_18$points), 
                                        rownames = ("Module"))


wide_fish_ht_mod_18 <- wide_fish_ht_mod_18 %>% 
  left_join(tibble_comm_fish_ht_mod_18)




wide_fish_ht_mod_18 <- wide_fish_ht_mod_18 %>% 
  left_join(
    distinct(select(transect_var, !c("Module_Side", "Habitat_Type", "Sub_Mod", "transect", "Zone")))
  )

wide_fish_ht_mod_18 <- wide_fish_ht_mod_18 %>% 
  mutate(construction_group = (case_when(startsWith(as.character(construction_group), "1") ~ "CP 1",
                                         startsWith(as.character(construction_group), "2") ~ "CP 2")))

wide_fish_ht_mod_18 <- wide_fish_ht_mod_18 %>% 
  mutate(mid_high_hgt_m = (case_when(startsWith(mid_high_hgt_m, "3") ~ "3 m",
                                     startsWith(mid_high_hgt_m, "4") ~ "4 m"))) 

plot_wide_fish_ht_mod_18 <- ggplot(wide_fish_ht_mod_18,
                                   aes(MDS1, MDS2)) +
  geom_text(aes(label = Module, vjust = 2, hjust = .2), show.legend = F) +
  geom_point(aes(shape = as_factor(construction_group), color = mid_high_hgt_m), size = 4) +
  theme_classic() +
  scale_colour_manual(values = c("#FF62BC", "#39B600")) +
  scale_shape_manual(values = c(15,16)) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid")) +
  labs(shape = "Construction Phase", color = "Maximum Vertical Relief")


# plot_wide_fish_ht_mod_18


hull_ht_mod_18 <- wide_fish_ht_mod_18 %>% 
  group_by(construction_group) %>% 
  slice(chull(MDS1, MDS2))

plot_wide_fish_ht_mod_18_hulls <- plot_wide_fish_ht_mod_18 +
  geom_polygon(data = hull_ht_mod_18,
               aes(x = MDS1, y = MDS2, 
                   fill = as_factor(construction_group),
                   group = as_factor(construction_group)), alpha = 0.2) +
  scale_fill_manual(values = c("#F8766D", "#00B0F6")) +
  geom_text(x = 0.5, y = 0.3, label = c(paste("2D Stress:" ,round(NMDS_comm_fish_ht_mod_18$stress,2), sep = " ")), color = "black") +
  guides(shape = guide_legend(title = "Construction Phase"),
         fill = guide_legend(title = "Construction Phase")) +
  theme(legend.position = "top")


plot_wide_fish_ht_mod_18_hulls



ggsave("figures/Figure_S1.png", plot_wide_fish_ht_mod_18_hulls,
       width = 6, height = 8, dpi = 600)


#### Figure S2: Module Assemblage Cluster Analysis ----
wide_fish_ht_mod_18 <- wide_fish_ht_mod_18 %>% 
  mutate(dend_lab = paste(Module, construction_group, mid_high_hgt_m))

wide_fish_ht_mod_18 <- wide_fish_ht_mod_18 %>% 
  mutate(dend_lab = str_replace(dend_lab, "CP 1 3 m", "(CP 1, 3 m)"),
         dend_lab = str_replace(dend_lab, "CP 2 3 m", "(CP 2, 3 m)"),
         dend_lab = str_replace(dend_lab, "CP 1 4 m", "(CP 1, 4 m)"),
         dend_lab = str_replace(dend_lab, "CP 2 4 m", "(CP 2, 4 m)"))


comm_fish_ht_mod_18 <- wide_fish_ht_mod_18 %>%
  column_to_rownames(var = "dend_lab") %>%
  select(Chromis_punctipinnis:Semicossyphus_pulcher)

#Create a distance matrix based on the community assemblages
dis.comm_fish_ht_mod_18 <- vegdist(comm_fish_ht_mod_18)

#Create a cluster dendrogram
clust.comm_fish_ht_mod_18 <- hclust(dis.comm_fish_ht_mod_18, "average")


#Add color labeles and branches based on the NMDS groups
mod_18_dendro <- color_labels(clust.comm_fish_ht_mod_18, col = "black", k = 1)

mod_18_dendro <- color_branches(mod_18_dendro, col = "black", k = 1) %>% 
  set("labels_cex", 1)
gg_mod_18_dend <- as.ggdend(mod_18_dendro)

plot_gg_mod_18_dend <- ggplot(gg_mod_18_dend, horiz = T, offset_labels = -0.01)
plot_gg_mod_18_dend <- plot_gg_mod_18_dend +
  theme_classic() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(), axis.text.x = element_text(size = 16), axis.title.x = element_text(size = 16)) +
  labs(y = "Bray-Curtis Dissimilarity") 


plot_gg_mod_18_dend


ggsave("figures/Figure_S2.png", plot_gg_mod_18_dend,
       width = 10, height = 10, dpi = 600)

#### PERMANOVA: Module Assemblage  ----

dis.comm_fish_ht_mod_18 <-vegdist(comm_fish_ht_mod_18)



ado.fish_ht_mod_18 <- adonis2(dis.comm_fish_ht_mod_18 ~  mid_high_hgt_m + construction_group, by = "margin", wide_fish_ht_mod_18)



ado_mod_18_table <- as.data.frame(ado.fish_ht_mod_18)
ado_mod_18_table <- ado_mod_18_table %>% 
  rownames_to_column() %>% 
  mutate(rowname = str_replace(rowname, "mid_high_hgt_m", "Maximum Vertical Relief"),
         rowname = str_replace(rowname, "construction_group", "Construction Group"))

set_flextable_defaults(font.family = "Arial", font.size = 9, digits = 2, padding = 0.1)

ado_mod_18_ft <- flextable(ado_mod_18_table,
                           col_keys = c("rowname", "Df"
                                        , "SumOfSqs", "R2", "F", "Pr(>F)")) %>% 
  set_header_labels(rowname = "Module Factors",
                    Df = "Df",
                    SumOfSqs = "Sum sq", 
                    R2 = "R2") %>% 
  align(align = "center") %>%
  align(part = "header", align = "center") %>%
  colformat_double(digits = 2) 



ado_mod_18_ft
# 
# write_csv(ado_mod_18_table, "tables/ado_mod_18_table.csv")
# save_as_docx(ado_mod_18_ft, path = "ado_mod_18_ft.docx")

#### Figure 2A: Submodule Assemblage NMDS ----
dat_fish_os_ht <- dat_fish_t %>% 
  group_by(Habitat_Type, os, Genus_spp) %>% 
  summarise(dens_100m2 = mean(dens_100m2)) %>% 
  ungroup()


wide_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(dens_100m2_sqrt = dens_100m2^0.5) %>% 
  filter(Genus_spp %in% focal_spp) %>% 
  pivot_wider(id_cols = Habitat_Type:os, names_from = Genus_spp, values_from = dens_100m2_sqrt) %>% 
  mutate(os_ht = paste(os, Habitat_Type))



#Need the column names to not have spaces
names(wide_fish_os_ht) <- str_replace_all(names(wide_fish_os_ht), c(" " = "_"))


comm_fish_os_ht <- wide_fish_os_ht %>%
  column_to_rownames(var = "os_ht") %>%
  select(Chromis_punctipinnis:Semicossyphus_pulcher)



NMDS_comm_fish_os_ht <- metaMDS(comm_fish_os_ht, 
                                trymax = 200,
                                distance = "bray",
                                autotransform = F)

# NMDS_comm_fish_os_ht

tibble_comm_fish_os_ht <- as_tibble(scores(NMDS_comm_fish_os_ht$points), 
                                    rownames = ("os_ht"))

wide_fish_os_ht <- wide_fish_os_ht %>% 
  left_join(tibble_comm_fish_os_ht)

wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(Habitat_Type = as.character(Habitat_Type))

wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(Zone = case_when(startsWith(Habitat_Type, "High Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "Medium Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "Low Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "High Relief") ~ "Midline",
                          startsWith(Habitat_Type, "Medium Relief") ~ "Midline",
                          startsWith(Habitat_Type, "Low Relief") ~ "Midline"))

### ADD CLUSTER ANALYSIS - WHICH DEFINES THESE GROUPS
wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(current =  case_when(startsWith(os, "West") ~ "Up-current",
                              startsWith(os, "East") ~ "Down-current"))

wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(Orientation =  case_when(startsWith(os, "West Par") ~ "Parallel",
                                  startsWith(os, "West Per") ~ "Perpendicular",
                                  startsWith(os, "East Par") ~ "Parallel",
                                  startsWith(os, "East Per") ~ "Perpendicular"))


wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(os_lab = paste(Orientation, current))

wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(os_lab = str_replace(os_lab, "Parallel Up-current", "Par. Offshore"),
         os_lab = str_replace(os_lab, "Parallel Down-current", "Par. Inshore"),
         os_lab = str_replace(os_lab, "Perpendicular Up-current", "Perp. West"),
         os_lab = str_replace(os_lab, "Perpendicular Down-current", "Perp. East"))



wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(Habitat_Type = str_replace_all(Habitat_Type, "Relief", "Reef"))

wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(dend_group = paste(os_lab, Habitat_Type))

cluster_df <- data.frame(
  dend_group = c("Par. Inshore High Ecotone", "Perp. East High Ecotone", "Par. Offshore High Ecotone", 
                 "Perp. West High Ecotone", "Par. Inshore High Reef", "Perp. East High Reef", 
                 "Par. Offshore High Reef", "Perp. West High Reef", "Par. Inshore Low Ecotone", 
                 "Perp. East Low Ecotone", "Par. Offshore Low Ecotone", "Perp. West Low Ecotone", 
                 "Par. Inshore Low Reef", "Perp. East Low Reef", "Par. Offshore Low Reef", 
                 "Perp. West Low Reef", "Par. Inshore Medium Ecotone", "Perp. East Medium Ecotone", 
                 "Par. Offshore Medium Ecotone", "Perp. West Medium Ecotone", "Par. Inshore Medium Reef", 
                 "Perp. East Medium Reef", "Par. Offshore Medium Reef", "Perp. West Medium Reef"),
  cluster_group = c(1, 2, 3, 4, 4, 3, 3, 1, 5, 3, 3, 4, 5, 1, 1, 4, 5, 4, 1, 3, 5, 1, 3, 3)
)

wide_fish_os_ht <- wide_fish_os_ht %>%
  left_join(cluster_df, by = "dend_group")

wide_fish_os_ht = wide_fish_os_ht %>% 
  mutate(cluster_ordered = (case_when(startsWith(as.character(cluster_group), "1") ~ "4",
                                      startsWith(as.character(cluster_group), "2") ~ "1",
                                      startsWith(as.character(cluster_group), "3") ~ "2",
                                      startsWith(as.character(cluster_group), "4") ~ "3",
                                      startsWith(as.character(cluster_group), "5") ~ "5")))



wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(Habitat_Type = factor(Habitat_Type, levels = c("High Reef",
                                                        "Medium Reef",
                                                        "Low Reef",
                                                        "High Ecotone",
                                                        "Medium Ecotone",
                                                        "Low Ecotone")))

plot_wide_fish_os_ht <- ggplot(wide_fish_os_ht,
                               aes(-MDS1, -MDS2)) +
  geom_text(aes(label = os_lab), vjust = 3, hjust = .4, size = 5) +
  geom_point(aes(color = as.factor(cluster_ordered), shape = Habitat_Type),size = 7, stroke = 3) +
  scale_shape_manual(values = c(15, 16, 17, 0, 1, 2)) +
  scale_color_manual(values = c("black", "coral2","springgreen4", "steelblue3","purple4")) +
  theme_classic() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = NA, color = "black", size = 1, linetype = "solid")) +
  theme(plot.margin = margin(1,2,1,1, "cm")) +
  theme(text = element_text(size = 16)) +
  guides(shape = guide_legend(title = "Habitat Type"),
         color = "none")

plot_wide_fish_os_ht = plot_wide_fish_os_ht +
  theme(legend.key = element_rect(fill = "white", colour = NA))
plot_wide_fish_os_ht =plot_wide_fish_os_ht +
  guides(shape = guide_legend(override.aes = list(stroke = 1)))
# plot_wide_fish_os_ht

hull_os_ht <- wide_fish_os_ht %>%
  group_by(cluster_ordered) %>%
  slice(chull(-MDS1, -MDS2))


plot_wide_fish_os_ht_hulls <- plot_wide_fish_os_ht +
  geom_polygon(data = hull_os_ht, aes(x = -MDS1, y = -MDS2, group = cluster_ordered, fill = cluster_ordered),alpha = 0.3)

# plot_wide_fish_os_ht_hulls

hull_os_ht <- wide_fish_os_ht %>% 
  group_by(cluster_ordered) %>% 
  slice(chull(-MDS1, -MDS2))



plot_wide_fish_os_ht_hulls <- plot_wide_fish_os_ht +
  geom_polygon(data = hull_os_ht, aes(x = -MDS1, y = -MDS2, group = as.factor(cluster_ordered), fill = as.factor(cluster_ordered)),alpha = 0.3) + 
  scale_fill_manual(values = c("black","coral2","springgreen4", "steelblue3","purple4")) +
  geom_text(x = -0.58, y = 0.24, label = c(paste("2D Stress:" ,round(NMDS_comm_fish_os_ht$stress,2), sep = " ")), color = "black", size = 8) +
  # guides(fill = guide_legend(title = "Cluster Group"))
  guides(fill = guide_legend(
    title = "Cluster Group",
    override.aes = list(alpha = 1)  # Set alpha to 1 to remove transparency in legend
  ),
  shape = guide_legend(
    title = "Habitat Type",
    nrow = 3, 
    ncol = 2,
    override.aes = list(stroke = 1)
  ))

# plot_wide_fish_os_ht_hulls

site_scores <- wide_fish_os_ht %>%
  select(Habitat_Type, os, Zone, MDS1, MDS2, cluster_ordered, os_ht) %>% 
  column_to_rownames(var = "os_ht")

os_ht_spp_fit <- envfit(NMDS_comm_fish_os_ht, comm_fish_os_ht, permutations = 999)

spp_scrs <- as_tibble(scores(os_ht_spp_fit, display = "vectors"), rownames = "Species") %>% 
  mutate(pval = os_ht_spp_fit$vectors$pvals) %>% 
  filter(pval <= 0.05 )

spp_scrs <- spp_scrs %>% 
  mutate(common_name = str_replace_all(Species, "_", " "))

spp_scrs <- spp_scrs %>% 
  mutate(common_name =  case_when(startsWith(common_name, "Ch") ~ "Blacksmith",
                                  startsWith(common_name, "Emb") ~ "Black Perch",
                                  startsWith(common_name, "Gir") ~ "Opaleye",
                                  startsWith(common_name, "Oxy") ~ "Senorita",
                                  common_name == "Paralabrax nebulifer" ~ "Barred Sand Bass",
                                  common_name == "Paralabrax clathratus" ~ "Kelp Bass",
                                  common_name == "Halichoeres semicinctus" ~ "Rock Wrasse",
                                  common_name == "Damalichthys vacca" ~ "Pile Perch",
                                  startsWith(common_name, "Seb") ~ "Olive Rockfish",
                                  startsWith(common_name, "Sem") ~ "CA Sheephead"))

plot_NMDS_os_ht_spp_vect <- plot_wide_fish_os_ht_hulls +
  geom_segment(data = spp_scrs, aes(x = 0, xend = -NMDS1*.275, y = 0, yend = -NMDS2*.27, shape = NULL),
               arrow = arrow(length = unit(.25, "cm")),
               color = "grey10", lwd = 0.3) +
#  geom_text(data = spp_scrs, aes(x = -NMDS1*.27, y = -NMDS2*.27, label = common_name, shape = NULL), fontface = "bold", color = "black", size = 7) +
  geom_text_repel(data = spp_scrs, aes(x = -NMDS1*.27, y = -NMDS2*.27, label = common_name, shape = NULL), fontface = "bold", color = "black", size = 7) +
  theme(legend.position = c(0.01, .93),
        legend.justification = c(0, 1),   
        legend.background = element_rect(color = "black", fill = "white", size = 0.5),
        legend.margin = margin(5, 5, 5, 5), legend.direction = "horizontal")



# plot_NMDS_os_ht_spp_vect
reverse_y_plot_NMDS_os_ht_spp_vect = plot_NMDS_os_ht_spp_vect + scale_y_reverse()

# reverse_y_plot_NMDS_os_ht_spp_vect
ggsave("figures/Figure_2A.png", reverse_y_plot_NMDS_os_ht_spp_vect,
       width = 16, height = 12, dpi = 600)
# ggsave("figures/submodule_assemblage_sqrt.png", plot_NMDS_os_ht_spp_vect,
#        width = 16, height = 12, dpi = 600)

spp_scrs <- spp_scrs %>% 
  arrange(pval)

spp_scrs

spp_scrs_ft <- flextable(spp_scrs,
                         col_keys = c("Species", "pval")) %>% 
  set_header_labels(Species = "NMDS1",
                    P = "pval") 

#spp_scrs_ft

#### Figure 2B: Submodule Assemblage Cluster Analysis ----

wide_fish_os_ht <- wide_fish_os_ht %>% 
  mutate(cluster_group = (case_when(startsWith(as.character(cluster_group), "1") ~ "Group 3",
                                    startsWith(as.character(cluster_group), "2") ~ "Group 5",
                                    startsWith(as.character(cluster_group), "3") ~ "Group 1",
                                    startsWith(as.character(cluster_group), "4") ~ "Group 2",
                                    startsWith(as.character(cluster_group), "5") ~ "Group 4")))

# Define colors for each category in 'cluster_2'
group_colors <- c("Group 1" = "coral2",
                  "Group 2" = "springgreen4",
                  "Group 3" = "steelblue3",
                  "Group 4" = "purple4",
                  "Group 5" = "black")

# Define shapes for each level in 'Module'
module_shapes <- c("Module 1" = 15,
                   "Module 2" = 16,
                   "Module 3" = 17,
                   "Module 4" = 0,
                   "Module 5" = 1,
                   "Module 6" = 2)


comm_fish_os_ht <- wide_fish_os_ht %>%
  column_to_rownames(var = "dend_group") %>%
  select(Chromis_punctipinnis:Semicossyphus_pulcher)

dis.comm_fish_os_ht <- vegdist(comm_fish_os_ht)

# Create a cluster dendrogram
clust.comm_fish_os_ht <- hclust(dis.comm_fish_os_ht, "average")

# Convert to a dendrogram object
os_ht_dendro <- as.dendrogram(clust.comm_fish_os_ht)

# Extract the order of the labels in the dendrogram
dendro_labels <- labels(os_ht_dendro)

# Map the reordered labels to their corresponding colors based on 'cluster_2'
label_color_mapping <- sapply(dendro_labels, function(label) {
  group <- wide_fish_os_ht[wide_fish_os_ht$dend_group == label, "cluster_group"][[1]]
  group_colors[group]
})

# Map the reordered labels to their corresponding shapes based on 'Module'
label_shape_mapping <- sapply(dendro_labels, function(label) {
  module <- wide_fish_os_ht[wide_fish_os_ht$dend_group == label, "Habitat_Type"][[1]]
  module_shapes[module]
})

# Modify the plotting pipeline with the correct order of colors and shapes
os_ht_dendro <- os_ht_dendro %>%
  set("leaves_pch", label_shape_mapping) %>%  # node point type based on module
  set("leaves_cex", 4) %>%  # node point size
  set("leaves_col", label_color_mapping) %>% # node point color
  set("labels_cex", 0.6) # reduce label size
# Plot the dendrogram again
# plot(os_ht_dendro)

# order.dendrogram(os_ht_dendro)
# plot(os_ht_dendro)

# # Define the new order
new_order2 <- c(5,12,18,6,3,11, 20,21,24,22,23,8,10,7,17,16,19,15,13,14,4,1,9,2)
# Rotate the dendrogram
dend_rotated <- rotate(os_ht_dendro, new_order2)
# 
# # Plot the rotated dendrogram
# plot(dend_rotated)

# Convert to ggplot object
gg_os_ht_dend <- as.ggdend(dend_rotated)

# Plot using ggplot2
plot_gg_os_ht_dend <- ggplot(gg_os_ht_dend, horiz = TRUE, offset_labels = -0.01) +
  theme_classic() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "none", axis.line.y = element_blank(), axis.title.x = element_text(vjust = -1.2, hjust = 0.5)) +
  theme(text = element_text(size = 24)) +
  scale_y_reverse(breaks = c(0.3, 0.2, 0.1, 0), expand = c(0, .1, 0, .1)) + 
  labs(y = "Bray-Curtis Dissimilarity")

# Plot the ggplot object
# print(plot_gg_os_ht_dend)

# # Save the plot
# ggsave("figures/submodule_dendrogram.png", plot_gg_os_ht_dend,
#        width = 18, height = 7.5, dpi = 600)


# # Convert to ggplot object
# gg_os_ht_dend <- as.ggdend(dend_rotated)

# Plot using ggplot2
plot_gg_os_ht_dend <- ggplot(gg_os_ht_dend, horiz = TRUE, offset_labels = -0.01) +
  theme_classic() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "none", axis.line.y = element_blank(), axis.title.x = element_text(vjust = -1.2, hjust = 0.5)) +
  theme(text = element_text(size = 24)) +
  scale_y_reverse(breaks = c(0.3, 0.2, 0.1, 0), expand = c(0, .1, 0, .1)) + 
  labs(y = "Bray-Curtis Dissimilarity")

# Plot the ggplot object
# print(plot_gg_os_ht_dend)

# Save the plot
ggsave("figures/Figure_2B.png", plot_gg_os_ht_dend,
       width = 18, height = 7.5, dpi = 600)


# Determine the optimal number of clusters using the silhouette method
sil_width <- numeric(10)
for (i in 2:10) {
  clusters <- cutree(clust.comm_fish_os_ht, k = i)
  sil <- silhouette(clusters, dist(comm_fish_os_ht))
  sil_width[i] <- mean(sil[, 3])
}

# Plot the silhouette width to determine the optimal number of clusters
# plot(1:10, sil_width, type = "b", xlab = "Number of Clusters", ylab = "Average Silhouette Width")
# abline(v = which.max(sil_width), col = "red", lty = 2)
# title(main = "Silhouette Method for Optimal Number of Clusters")


# define some clusters
mycl <- cutree(os_ht_dendro, h=0.23)

# as_tibble(mycl)

cluster_df <- data.frame(dend_group = names(mycl), cluster_group = mycl)

# # Join the cluster information with the wide_fish_os_ht data frame
# wide_fish_os_ht <- wide_fish_os_ht %>%
#   left_join(cluster_df, by = "dend_group")

# os_ht_dendro$height


#### Figure 2: Combined NMDS, Cluster, & Module Diagram ------------------------
set.seed(88) # set seed for reproducibility (so geom_text_repel doesn't move around)
#reformatted nMDS
plot_wide_fish_os_ht2 <- ggplot(wide_fish_os_ht, aes(-MDS1, -MDS2)) +
  geom_text(aes(label = os_lab),
            vjust = 2,
            hjust = .5,
            size = 3) +
  geom_point(aes(color = as.factor(cluster_ordered), shape = Habitat_Type),
             size = 3,
             stroke = 2) +
  scale_shape_manual(values = c(15, 16, 17, 0, 1, 2)) +
  scale_color_manual(values = c("black", "coral2", "springgreen4", "steelblue3", "purple4")) +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(
      fill = NA,
      color = "black",
      size = 1,
      linetype = "solid"
    )
  ) +
  theme(plot.margin = margin(1, 2, 1, 1, "cm")) +
  theme(text = element_text(size = 12)) +
  guides(shape = guide_legend(title = "Habitat Type"), color = "none") +
  theme(legend.key = element_rect(fill = "white", colour = NA)) +
  guides(shape = guide_legend(override.aes = list(stroke = 1))) +
  geom_polygon(
    data = hull_os_ht,
    aes(
      x = -MDS1,
      y = -MDS2,
      group = cluster_ordered,
      fill = cluster_ordered
    ),
    alpha = 0.3
  ) +
  geom_polygon(
    data = hull_os_ht,
    aes(
      x = -MDS1,
      y = -MDS2,
      group = as.factor(cluster_ordered),
      fill = as.factor(cluster_ordered)
    ),
    alpha = 0.3
  ) +
  scale_fill_manual(values = c("black", "coral2", "springgreen4", "steelblue3", "purple4")) +
  geom_text(
    x = 0.34,
    y = 0.24,
    label = c(paste(
      "2D Stress:" , round(NMDS_comm_fish_os_ht$stress, 2), sep = " "
    )),
    color = "black",
    size = 3
  ) +
  # guides(fill = guide_legend(title = "Cluster Group"))
  guides(
    fill = guide_legend(title = "Cluster", override.aes = list(alpha = 1)),
    #alpha to 1 remove transparency
    shape = guide_legend(title = NULL, 
                         nrow = 3,
                         ncol = 2,
                         override.aes = list(stroke = 1))
  ) +
  geom_segment(
    data = spp_scrs,
    aes(
      x = 0-0.09,
      xend = (-NMDS1 * .37)-0.09,
      y = 0,
      yend = -NMDS2 * .37,
      shape = NULL
    ),
    arrow = arrow(length = unit(.25, "cm")),
    color = "grey10",
    lwd = 0.3
  ) +
  geom_text_repel(
    data = spp_scrs,
    aes(
      x = (-NMDS1 * .38)-0.09,
      y = -NMDS2 * .38,
      label = common_name,
      shape = NULL
    ),
    fontface = "bold",
    color = "black",
    size = 4
  ) +
  theme(
    legend.position = c(0.01, .99),
    legend.justification = c(0, 1),
    legend.background = element_rect(
      color = "black",
      fill = "white",
      size = 0.16,
    ),
    legend.spacing = unit(0.1, 'cm'), 
    legend.direction = "horizontal",
    legend.margin = margin(1, 3, 1, 1),
    legend.text = element_text(margin = margin(t = 0, r = 0, b = 0, l = 1))
  ) +
  theme(plot.margin = margin(0.1, 1.0, 0.2, 0.1, "cm")) +
  scale_y_reverse()

# plot_wide_fish_os_ht2

## Reformat dendrogram

plot_gg_os_ht_dend2 <- ggplot(gg_os_ht_dend,
                              horiz = TRUE,
                              offset_labels = -0.01) +
  theme_classic() +
  theme(plot.margin = margin(0.1, 1.0, 0.2, 0.1, "cm")) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.title.x = element_text(vjust = -1.2, hjust = 0.5),
    text = element_text(size = 10)
  ) +
  scale_y_reverse(breaks = c(0.4, 0.3, 0.2, 0.1, 0),
                  expand = c(0, 0, 0, .25)) +
  labs(y = "Bray-Curtis Dissimilarity") +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA)) # to plot over module diagram


# plot_gg_os_ht_dend2


#read in module image file
mod_diagram <- readPNG("figures/diagrams/PVR2.png")

# combine plots
combined_plot <- ggdraw(plot_wide_fish_os_ht2, 
       ylim = c(-.9, 1), 
       xlim = c(0, .96)) +
  theme(plot.background = element_rect(fill="white", color = NA)) +
  # plot module diagram before dendrogram (so transparent background dendrogram can be on top of module diagram)
  draw_plot(
    ggdraw() + draw_image(mod_diagram),
    x = -0.3,
    y = -0.89,
    width = 0.9,
    height = 0.9
  ) +
  draw_plot(plot_gg_os_ht_dend2,
    x = 0.3,
    y = -0.9,
    width = 0.7,
    height = 0.90
  ) +
  draw_label(x = 0.92, 
             y = 0.95,
             "A", 
             size = 16,
             fontface = "bold") +
  draw_label(x = 0.92, 
             y = -0.01,
             "B", 
             size = 16,
             fontface = "bold") +
  draw_label(x = 0.28, 
             y = -0.01,
             "C", 
             size = 16,
             fontface = "bold")

combined_plot
### NOTE every time you run it the species vector labels will shift w/ randomization in gg_text_repel (but seems like it's better than geom_text where some always overlap)
ggsave("figures/Figure_2_combined.png", combined_plot, width = 7, height = 9, units = "in", dpi = 600)


# # Combine overview and inset maps
# plot_map_SiteType_combined <- ggdraw(plot_map_PV_overview_pre, 
#                                      ylim = c(-0.30, 1.2), 
#                                      xlim = c(0.1, 0.90)) +
#   theme(plot.background = element_rect(fill="white", color = NA)) +
#   draw_plot(
#     CA_Map_overview_pv_box,
#     x = .30,
#     y = .62,
#     width = .6,
#     height = .6
#   ) +
#   draw_plot(plot_reefing_area_outer_deep_post, 
#             x = 0.12, 
#             y = -0.30, 
#             width = 0.7, 
#             height = 0.7
#   ) +
#   draw_label(x = 0.6, 
#              y = 0.85,
#              "California", size = 14, angle = -43) +
#   draw_label(x = 0.4, 
#              y = -0.05,
#              "Reefing Area\nPost-Construction", size = 16) +
#   draw_label(x = 0.5, 
#              y = 0.6,
#              "Palos Verdes\nPeninsula", size = 16) +
#   #draw_line(x = c(0.41, 0.525), y = c(0.28, 0.285)) +
#   #draw_line(x = c(0.63, 0.568), y = c(0.095, 0.236))+
#   draw_line(x = c(0.51, 0.49), y = c(0.35, 0.26), arrow = arrow(type = "closed", length = unit(2.5, "mm")), size = 1)
# 
# 
# plot_map_SiteType_combined

#### PERMANOVA: Submodule Assemblage  ----
dis.comm_fish_os_ht <- vegdist(comm_fish_os_ht)

adonis2(dis.comm_fish_os_ht ~ cluster_ordered, wide_fish_os_ht)

wide_fish_os_ht2 <- wide_fish_os_ht |> 
  mutate(submod_relief = word(Habitat_Type, 1))

ado2.fish_os_ht_clus <-adonis2(dis.comm_fish_os_ht ~ Zone + submod_relief + os, by = "margin", wide_fish_os_ht2)

ado2.fish_os_ht_clus

### UPDATE BELOW
ado2.fish_os_ht_clus_table <- as.data.frame(ado2.fish_os_ht_clus)
ado2.fish_os_ht_clus_table <- ado2.fish_os_ht_clus_table %>% 
  rownames_to_column() %>% 
  mutate(rowname = str_replace(rowname, "Habitat_Type", "Habitat Type"),
         rowname = str_replace(rowname, "os", "Orientation & Side"),
         rowname = str_replace(rowname, "cluster_ordered", "Cluster"))

set_flextable_defaults(font.family = "Arial", font.size = 9, digits = 2, padding = 0.1)

ado_os_ht_ft <- flextable(ado2.fish_os_ht_clus_table,
                          col_keys = c("rowname", "Df"
                                       , "SumOfSqs", "R2", "F", "Pr(>F)")) %>% 
  set_header_labels(rowname = "Transect Characteristics",
                    Df = "Df",
                    SumOfSqs = "Sum sq", 
                    R2 = "R2") %>% 
  colformat_double(digits = 2) %>% 
  colformat_double(j = "Df", digits = 0) %>% 
  colformat_double(j = "Pr(>F)", digits = 3)



ado_os_ht_ft
# write_csv(ado2.fish_os_ht_clus_table, "tables/ado2.fish_os_ht_clus_table.csv")
# save_as_docx(ado_os_ht_ft, path = "ado_os_ht_ft.docx")


#### Figure 3: Fish Density Heatmap ----
dens_sp_order <- dens_sp_os_ht %>% 
  group_by(Genus_spp) %>% 
  summarise(mean_dens = mean(mean_dens)) %>% 
  arrange(mean_dens)

dens_sp_os_ht$Genus_spp <- factor(dens_sp_os_ht$Genus_spp, 
                                  levels = dens_sp_order$Genus_spp)

dens_sp_os_ht = dens_sp_os_ht %>% 
  mutate(Habitat_Type = str_replace_all(Habitat_Type, "Relief", "Reef"))

dens_sp_os_ht = dens_sp_os_ht %>% 
  mutate(dend_group = paste(Orientation, os, Habitat_Type))

cluster_df <- data.frame(
  dend_group = c("Parallel Inshore High Ecotone", "Perpendicular East High Ecotone", "Parallel Offshore High Ecotone", 
                 "Perpendicular West High Ecotone", "Parallel Inshore High Reef", "Perpendicular East High Reef", 
                 "Parallel Offshore High Reef", "Perpendicular West High Reef", "Parallel Inshore Low Ecotone", 
                 "Perpendicular East Low Ecotone", "Parallel Offshore Low Ecotone", "Perpendicular West Low Ecotone", 
                 "Parallel Inshore Low Reef", "Perpendicular East Low Reef", "Parallel Offshore Low Reef", 
                 "Perpendicular West Low Reef", "Parallel Inshore Medium Ecotone", "Perpendicular East Medium Ecotone", 
                 "Parallel Offshore Medium Ecotone", "Perpendicular West Medium Ecotone", "Parallel Inshore Medium Reef", 
                 "Perpendicular East Medium Reef", "Parallel Offshore Medium Reef", "Perpendicular West Medium Reef"),
  cluster_group = c(1, 2, 3, 4, 4, 3, 3, 1, 5, 3, 3, 4, 5, 1, 1, 4, 5, 4, 1, 3, 5, 1, 3, 3)
)


dens_sp_os_ht <- dens_sp_os_ht %>%
  left_join(cluster_df, by = "dend_group")

dens_sp_os_ht = dens_sp_os_ht %>% 
  mutate(cluster_ordered = (case_when(startsWith(as.character(cluster_group), "1") ~ "4",
                                      startsWith(as.character(cluster_group), "2") ~ "1",
                                      startsWith(as.character(cluster_group), "3") ~ "2",
                                      startsWith(as.character(cluster_group), "4") ~ "3",
                                      startsWith(as.character(cluster_group), "5") ~ "5")))


dens_sp_clust_heatmap <- dens_sp_os_ht %>% 
  group_by(cluster_ordered, Genus_spp) %>% 
  summarise(mean_clust_dens = mean(mean_dens), 
            min_clust_dens = min(mean_dens),
            max_clust_dens = max(mean_dens),
            count_tt = n())

group_color_lab <- c("black","coral2","springgreen4", "steelblue3","purple4")

heatmap_sp_clust<-dens_sp_clust_heatmap %>%
  ggplot(aes(x=cluster_ordered, y = Genus_spp)) +
  geom_tile(aes(fill = mean_clust_dens ), position = "identity", colour = "black") +
  scale_fill_gradientn(trans= "log1p", 
                       colors = c("white","red","dark blue"),
                       #colours = wes_palette("Zissou1", 100, type = "continuous"),
                       limits=c(0, 200), 
                       breaks = c(0, 1, 3, 6, 12, 25, 50, 100, 200),
                       name= expression(paste("Density\n(No./100",m^{2},")"))) + #name= expression(density~(No.~100m,^{-2}))) +
  theme_bw() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 18)) + 
  theme(axis.text.y = element_text(face = "bold", color = "black"), axis.text.x = element_text(face = "bold", color = group_color_lab)) +
  ylab(NULL) +
  xlab(NULL) +
  
  geom_text(aes(label = paste(round(mean_clust_dens, 1), "\n", 
                              paste("(", 
                                    round(min_clust_dens, 1), 
                                    ", ", 
                                    round(max_clust_dens, 1), 
                                    ")", sep = ""))), 
            size = 2.2, 
            fontface = "bold") +
  scale_y_discrete(labels = c("Rock Wrasse","Olive Rockfish","Pile Perch","Opaleye","Barred Sand Bass","Rainbow Seaperch", "Black Perch","California Sheephead","Kelp Bass","Señorita","Blacksmith"))


heatmap_sp_clust

ggsave(heatmap_sp_clust, file="figures/Figure_3.png", width=6, height=6, dpi=600) 



# #### SPP dens across haibtat types
# # sp dens by module
# dens_sp_mod <- dat_fish_t %>% 
#   group_by(Genus_spp, Module) %>%
#   summarise(median_dens = median(dens_100m2), 
#             mean_dens = mean(dens_100m2),
#             sd_dens = sd(dens_100m2),
#             max_dens = max(dens_100m2),
#             min_dens = min(dens_100m2)) %>% 
#   filter(Genus_spp %in% focal_spp)
# 
# 
# ###CHANGE TO MODULE
# dens_sp_ht_ft <- flextable(dens_sp_ht,
#                            col_keys = c("Genus_spp", "Habitat_Type", "median_dens",
#                                         "mean_dens", "sd_dens", "max_dens", "min_dens")) %>% 
#   add_header_row(colwidths = c(1, 1, 5), values = c("Species Name", "Habitat Type", "Focal fish density (No./100 m2)")) %>% 
#   set_header_labels(Genus_spp = "Species Name",
#                     Habitat_Type = "Habitat Type",
#                     median_dens = "Median",
#                     mean_dens = "Mean",
#                     sd_dens = "Standard Deviation",
#                     max_dens = "Max",
#                     min_dens = "Min") %>% 
#   colformat_double(digits = 1) %>% 
#   theme_box() %>% 
#   align(align = "center") %>% 
#   align(part = "header", align = "center") %>% 
#   compose(j = "Genus_spp",
#           value = as_paragraph(as_i(Genus_spp))) %>% 
#   merge_v(part = "header")
# 
# # dens_sp_ht_ft
#### Figure 4: Community Composition Stacked Bar Plot ----
#start with the mean density per species per transect type 744 points (31 spp 24 transect types)
dat_fish_os_ht <- dat_fish_os_ht %>%
  mutate(os_ht = paste(os, Habitat_Type))


dat_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(Zone = case_when(startsWith(Habitat_Type, "High Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "Medium Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "Low Ecotone") ~ "Ecotone",
                          startsWith(Habitat_Type, "High Relief") ~ "Midline",
                          startsWith(Habitat_Type, "Medium Relief") ~ "Midline",
                          startsWith(Habitat_Type, "Low Relief") ~ "Midline"))

### ADD CLUSTER ANALYSIS - WHICH DEFINES THESE GROUPS
dat_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(current =  case_when(startsWith(os, "West") ~ "Up-current",
                              startsWith(os, "East") ~ "Down-current"))

dat_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(Orientation =  case_when(startsWith(os, "West Par") ~ "Parallel",
                                  startsWith(os, "West Per") ~ "Perpendicular",
                                  startsWith(os, "East Par") ~ "Parallel",
                                  startsWith(os, "East Per") ~ "Perpendicular"))


dat_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(os_lab = paste(Orientation, current))

dat_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(os_lab = str_replace(os_lab, "Parallel Up-current", "Parallel Offshore"),
         os_lab = str_replace(os_lab, "Parallel Down-current", "Parallel Inshore"),
         os_lab = str_replace(os_lab, "Perpendicular Up-current", "Perpendicular West"),
         os_lab = str_replace(os_lab, "Perpendicular Down-current", "Perpendicular East"))



dat_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(Habitat_Type = str_replace_all(Habitat_Type, "Relief", "Reef"))

dat_fish_os_ht <- dat_fish_os_ht %>% 
  mutate(dend_group = paste(os_lab, Habitat_Type))

cluster_df <- data.frame(
  dend_group = c("Parallel Inshore High Ecotone", "Perpendicular East High Ecotone", "Parallel Offshore High Ecotone", 
                 "Perpendicular West High Ecotone", "Parallel Inshore High Reef", "Perpendicular East High Reef", 
                 "Parallel Offshore High Reef", "Perpendicular West High Reef", "Parallel Inshore Low Ecotone", 
                 "Perpendicular East Low Ecotone", "Parallel Offshore Low Ecotone", "Perpendicular West Low Ecotone", 
                 "Parallel Inshore Low Reef", "Perpendicular East Low Reef", "Parallel Offshore Low Reef", 
                 "Perpendicular West Low Reef", "Parallel Inshore Medium Ecotone", "Perpendicular East Medium Ecotone", 
                 "Parallel Offshore Medium Ecotone", "Perpendicular West Medium Ecotone", "Parallel Inshore Medium Reef", 
                 "Perpendicular East Medium Reef", "Parallel Offshore Medium Reef", "Perpendicular West Medium Reef"),
  cluster_group = c(1, 2, 3, 4, 4, 3, 3, 1, 5, 3, 3, 4, 5, 1, 1, 4, 5, 4, 1, 3, 5, 1, 3, 3)
)

dat_fish_os_ht <- dat_fish_os_ht %>%
  left_join(cluster_df, by = "dend_group")


clust_col <- c("black",
               "coral2","coral2","coral2","coral2","coral2","coral2","coral2","coral2",
               "springgreen4","springgreen4","springgreen4","springgreen4","springgreen4",
               "steelblue3","steelblue3","steelblue3","steelblue3","steelblue3","steelblue3",
               "purple4","purple4","purple4","purple4")



plot_dens_total_os_ht <- dat_fish_os_ht %>%
  mutate(dend_group = fct_rev(fct_relevel(dend_group,
                                  
                                  "Perpendicular East High Ecotone",
                                  
                                  "Parallel Offshore Low Ecotone",
                                  "Parallel Offshore High Ecotone",
                                  "Perpendicular East Low Ecotone",
                                  "Perpendicular West Medium Ecotone",
                                  "Perpendicular East High Reef",
                                  "Perpendicular West Medium Reef",
                                  "Parallel Offshore Medium Reef",
                                  "Parallel Offshore High Reef",
                                  
                                  "Perpendicular West High Ecotone",
                                  "Perpendicular East Medium Ecotone",
                                  "Perpendicular West Low Ecotone", 
                                  "Parallel Inshore High Reef",
                                  "Perpendicular West Low Reef",
                                  
                                  "Perpendicular West High Reef",
                                  "Perpendicular East Medium Reef",
                                  "Parallel Offshore Low Reef",
                                  "Parallel Inshore High Ecotone",
                                  "Perpendicular East Low Reef",
                                  "Parallel Offshore Medium Ecotone",
                                  
                                  
                                  
                                  "Parallel Inshore Low Reef",
                                  "Parallel Inshore Low Ecotone",
                                  "Parallel Inshore Medium Reef",
                                  "Parallel Inshore Medium Ecotone"
                                  ))) %>%
  filter(Genus_spp %in% focal_spp) %>%
  group_by(dend_group, Genus_spp) %>%
  summarize(total_dens = sum(dens_100m2), .groups = 'drop') %>%
  ungroup() |> 
  ggplot(aes(x = dend_group, y = total_dens, fill = Genus_spp)) +
  geom_bar(stat = "identity", colour = "black") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Transect Type", y = expression(paste("Mean Density (No./100",m^{2},")"))) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  guides(fill = guide_legend(title = "Focal Fish Species")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_brewer(palette = "Set3", labels = c("Blacksmith",
                                                 "Señorita",
                                                 "Kelp Bass",
                                                 "California Sheephead",
                                                 "Black Perch",
                                                 "Rainbow Seaperch",
                                                 "Barred Sand Bass",
                                                 "Opaleye",
                                                 "Pile Perch",
                                                 "Rock Wrasse",
                                                 "Olive Rockfish")) +
  theme(
    legend.position = c(.757, .37),
    legend.direction = "vertical", 
    legend.background = element_rect(fill = "white", color = "black"), 
    legend.text = element_text(size = 16),
    axis.text.y = element_text(color = rev(clust_col))  # Set y-axis (flipped x-axis) label colors
  )

plot_dens_total_os_ht

ggsave("figures/Figure_4.png", plot_dens_total_os_ht,
       width = 8, height = 6, dpi = 600)

#extra proportional stacked bar blot 

# plot_prop_dens_total_os_ht <- dat_fish_os_ht %>%
#   mutate(dend_group = fct_relevel(dend_group,
#                                   
#                                   "Perpendicular East High Ecotone",
#                                   
#                                   "Parallel Offshore Low Ecotone",
#                                   "Parallel Offshore High Ecotone",
#                                   "Perpendicular East Low Ecotone",
#                                   "Perpendicular West Medium Ecotone",
#                                   "Perpendicular East High Reef",
#                                   "Perpendicular West Medium Reef",
#                                   "Parallel Offshore Medium Reef",
#                                   "Parallel Offshore High Reef",
#                                   
#                                   "Perpendicular West High Ecotone",
#                                   "Perpendicular East Medium Ecotone",
#                                   "Perpendicular West Low Ecotone", 
#                                   "Parallel Inshore High Reef",
#                                   "Perpendicular West Low Reef",
#                                   
#                                   "Perpendicular West High Reef",
#                                   "Perpendicular East Medium Reef",
#                                   "Parallel Offshore Low Reef",
#                                   "Parallel Inshore High Ecotone",
#                                   "Perpendicular East Low Reef",
#                                   "Parallel Offshore Medium Ecotone",
#                                   
#                                   
#                                   
#                                   "Parallel Inshore Low Reef",
#                                   "Parallel Inshore Low Ecotone",
#                                   "Parallel Inshore Medium Reef",
#                                   "Parallel Inshore Medium Ecotone"
#   )) %>%
#   filter(Genus_spp %in% focal_spp) %>%
#   group_by(dend_group, Genus_spp) %>%
#   summarize(total_dens = sum(dens_100m2), .groups = 'drop') %>%
#   group_by(dend_group) %>%
#   mutate(prop_dens = total_dens / sum(total_dens)) %>%
#   ungroup() %>%
#   ggplot(aes(x = dend_group, y = prop_dens, fill = Genus_spp)) +
#   geom_bar(stat = "identity", colour = "black") +
#   coord_flip() +
#   theme_classic() +
#   theme(legend.position = "top") +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   labs(x = "Reef Design Features", y = "Proportion of Total Density") +
#   theme(legend.text = element_text(size = 12, face = "bold")) +
#   guides(fill = guide_legend(title = "Focal Fish Species")) +
#   scale_x_discrete(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0), labels = scales::percent) +
#   scale_fill_brewer(palette = "Set3", labels = c("Blacksmith",
#                                                  "Señorita",
#                                                  "Kelp Bass",
#                                                  "California Sheephead",
#                                                  "Black Perch",
#                                                  "Rainbow Seaperch",
#                                                  "Barred Sand Bass",
#                                                  "Opaleye",
#                                                  "Pile Perch",
#                                                  "Rock Wrasse",
#                                                  "Olive Rockfish")) +
#   theme(
#     legend.position = c(.857, .797),
#     legend.direction = "vertical", 
#     legend.background = element_rect(fill = "white", color = "black"), 
#     legend.text = element_text(size = 16),
#     axis.text.y = element_text(color = clust_col)  # Set y-axis (flipped x-axis) label colors
#   )
# 
# plot_prop_dens_total_os_ht
# 
# ggsave("figures/fish_density_proprtional_stack.png", plot_prop_dens_total_os_ht,
#        width = 12, height = 8, dpi = 600)

# extra Species Specific Habitat Use 
# 
# dat_fish_spp <- dat_fish_t %>% 
#   filter(Genus_spp %in% focal_spp)
# 
# dat_fish_spp = dat_fish_spp %>% 
#   mutate(os_ht = paste(Module_Side, Orientation, Habitat_Type, sep = " "))
# 
# dat_fish_spp <- dat_fish_spp %>% 
#   mutate(Zone = case_when(startsWith(Habitat_Type, "High Ecotone") ~ "Ecotone",
#                           startsWith(Habitat_Type, "Medium Ecotone") ~ "Ecotone",
#                           startsWith(Habitat_Type, "Low Ecotone") ~ "Ecotone",
#                           startsWith(Habitat_Type, "High Relief") ~ "Midline",
#                           startsWith(Habitat_Type, "Medium Relief") ~ "Midline",
#                           startsWith(Habitat_Type, "Low Relief") ~ "Midline"))
# 
# 
# dat_fish_spp <- dat_fish_spp %>% 
#   mutate(Orientation =  case_when(startsWith(os, "West Par") ~ "Parallel",
#                                   startsWith(os, "West Per") ~ "Perpendicular",
#                                   startsWith(os, "East Par") ~ "Parallel",
#                                   startsWith(os, "East Per") ~ "Perpendicular"))
# 
# 
# dat_fish_spp <- dat_fish_spp %>% 
#   mutate(os_lab = paste(Orientation, current))
# 
# dat_fish_spp <- dat_fish_spp %>% 
#   mutate(os_lab = str_replace(os_lab, "Parallel Up-current", "Parallel Offshore"),
#          os_lab = str_replace(os_lab, "Parallel Down-current", "Parallel Inshore"),
#          os_lab = str_replace(os_lab, "Perpendicular Up-current", "Perpendicular West"),
#          os_lab = str_replace(os_lab, "Perpendicular Down-current", "Perpendicular East"))
# 
# 
# 
# dat_fish_spp <- dat_fish_spp %>% 
#   mutate(Habitat_Type = str_replace_all(Habitat_Type, "Relief", "Reef"))
# 
# dat_fish_spp <- dat_fish_spp %>% 
#   mutate(dend_group = paste(os_lab, Habitat_Type))
# 
# # Get unique species
# species <- unique(dat_fish_spp$Genus_spp)
# species_plots <- list()
# 
# group_color_lab <- c("Group 1" = "black", "Group 2" = "coral2", 
#                      "Group 3" = "springgreen4", "Group 4" = "steelblue3", 
#                      "Group 5" = "purple4")
# 
# for(species_ in species){
#   species_plots[[species_]] <- dat_fish_spp %>%
#     filter(Genus_spp == species_) %>%
#     mutate(dend_group = fct_relevel(dend_group,
#                                     
#                                     "Perpendicular East High Ecotone",
#                                     
#                                     "Parallel Offshore Low Ecotone",
#                                     "Parallel Offshore High Ecotone",
#                                     "Perpendicular East Low Ecotone",
#                                     "Perpendicular West Medium Ecotone",
#                                     "Perpendicular East High Reef",
#                                     "Perpendicular West Medium Reef",
#                                     "Parallel Offshore Medium Reef",
#                                     "Parallel Offshore High Reef",
#                                     
#                                     "Perpendicular West High Ecotone",
#                                     "Perpendicular East Medium Ecotone",
#                                     "Perpendicular West Low Ecotone", 
#                                     "Parallel Inshore High Reef",
#                                     "Perpendicular West Low Reef",
#                                     
#                                     "Perpendicular West High Reef",
#                                     "Perpendicular East Medium Reef",
#                                     "Parallel Offshore Low Reef",
#                                     "Parallel Inshore High Ecotone",
#                                     "Perpendicular East Low Reef",
#                                     "Parallel Offshore Medium Ecotone",
#                                     
#                                     
#                                     
#                                     "Parallel Inshore Low Reef",
#                                     "Parallel Inshore Low Ecotone",
#                                     "Parallel Inshore Medium Reef",
#                                     "Parallel Inshore Medium Ecotone"
#     )) %>%
#     mutate(Habitat_Type = factor(Habitat_Type, levels = c("High Reef",
#                                                           "Medium Reef",
#                                                           "Low Reef",
#                                                           "High Ecotone",
#                                                           "Medium Ecotone",
#                                                           "Low Ecotone"))) %>% 
#     ggplot(aes(x = dend_group, y = dens_100m2, color = cluster, shape = Habitat_Type)) +
#     scale_shape_manual(values = c(15, 16, 17, 0, 1, 2)) +
#     geom_crossbar(stat = "summary", fun.data = mean_cl_boot,
#                   fill = "gray", alpha = 0.5, width = 0.4) +
#     geom_point(position = position_jitter()) +
#     theme_classic() +
#     ggtitle(species_) +
#     scale_color_manual(values = group_color_lab) +
#     guides(shape = guide_legend(title = "Habitat Type", nrow = 3, 
#                                 ncol = 2,), color = guide_legend(title = "Cluster Group")) +
#     theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 9,color = clust_col),legend.position = c(0.65, 0.85), legend.direction = "horizontal", legend.box = "vertical") +
#     labs(x = "Reef Design Features", y = expression(paste("Fish Density (No./100", m^2, ")")))
#   
#   print(species_plots[[species_]])
#   
#   ggsave(paste0("figures/species_plots/spp_density_plot_", species_, ".png"), species_plots[[species_]],
#          width = 9, height = 8, dpi = 600)
# }
