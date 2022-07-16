################################################
# R CODE EXAMPLES TO LEARN FASTER!             #
################################################

#### SECTION 0 : GENERAL COMMENTS & SHORTCUTS ####

# R is case sensitive! I try to use the following structure : 
# - TABLES : All in capital letters
# - New variables : Upper case the first letter of each word (i.e. Prm_Prop_Total)

# To RUN code : Select what you want to run and press CTRL + ENTER
# To CLEAR the console : CTRL + L
# To remove only 1 table from the environment : rm(DATA)
# Find & Replace : CTRL + F (Select the lines of codes where you want to replace BEFORE CTRL + F)

# SHORTCUTS
# <- : ALT + - (ALT and minus sign)
# %>% : CTRL + SHIFT + M

# In the paths, the slashes need to be on this side /////// 

# EQUAL in R : == (2 signs)
# NOT : ! (i.e. not equal : !=)
# OR : |

#### SECTION 1 : LOAD PACKAGES ####

# Model factory's function to install and load the most usefull packages (NOTE : the source function reads the entire code without opening it)

LOAD.H2O <- FALSE
source("S:/Pricing/Pricing Regions/02 Ontario/01 Auto/01 PPA/2020-03 - Classification/02 - Modeling/PHASE-2-MODEL/Functions/000_LoadLibrariesCheckpoint.R")

# Packages that I use the most

library(fst)
library(dplyr)
library(plotly)
library(data.table)
library(readr)

#### SECTION 2 : LOAD DATA ####

# When working with large datasets (Inforce, Trx, etc.) : convert them into fst at the beggining of the project
# To read csv : use fread (NOT read.csv)

DATA <- read_fst("S:/Pricing/Pricing Regions/02 Ontario/01 Auto/01 PPA/2020-03 - Classification/01 - Data Preparation/Inforce/Inforce_20190731_final_VRG.fst")

DATA_PREM <- fread("S:/Pricing/Pricing Regions/02 Ontario/01 Auto/01 PPA/2020-03 - Classification/01 - Data Preparation/Inforce/Inforce_Onlvl_Prem_20190731_Subset.csv")

#### SECTION 3 : PRACTICAL FUNCTIONS ####

## 3.1 Basic functions

names(DATA)                 # Get all the names of the variables in the data
unique(DATA$Clt_Insurer_Tx) # Unique values of a variable
summary(DATA$Dri_Age_Nb)    # Quickly get a few stats
class(DATA$Dri_Age_Nb)      # Type of the variable or dataset

## 3.2 Cut function : Useful to bucket a variable

# - Option 1 : Define how the variable will be bucketed -> indicate break points
(DATA$Dri_Age_Nb_Bucket1 <- cut(DATA$Dri_Age_Nb, breaks = c(16, seq(from = 20, to = 80, by = 5), 100), include.lowest = TRUE))

# - Option 2 : Create n groups -> R will select the break points
(DATA$Dri_Age_Nb_Bucket2 <- cut(DATA$Dri_Age_Nb, breaks = 10))

## 3.3 pmin & pmax : useful to cap losses

# Let's pretend I have a sample of claims to cap at 5 000
(CLAIMS <- seq(from = 0, to = 10000, by = 1000))

min(CLAIMS, 5000)  # WRONG : min will extract the overall min between my claims and 5000
pmin(CLAIMS, 5000) # GOOD : pmin will calculate the "parallel" minimum between each claim amount and 5000

## 3.4 Quick check on frequency

# Example : Number of drivers with age > 50 years old
sum(DATA$Dri_Age_Nb > 50) # DATA$Dri_Age_Nb : returns a vector of TRUE or FALSE (TRUE = 1, FALSE = 0)

## 3.5 Paste function : usefull to automate and simplify R codes

COVERAGES <- c("AB", "COL", "CMP", "DC")
(VRG_NAMES <- paste("VRG", COVERAGES, "T0", sep = "_"))

## 3.6 Sample : To create a random sample (Useful to feed the CASES sheet in the calculator)

set.seed(123) # To obtain the same sample every time
SAMPLE <- DATA[sample(1:nrow(DATA), size = 1000), c("Pol_Policy_No", "Veh_Id_No", "Dri_Id_No")]

#### SECTION 4 : DATA MANIPULATION ####

## To extract in a dataset : DATA["lines", "columns"]
DATA[DATA$Pol_Policy_No == "37609540", c("VRG_COL_T0", "VRG_CMP_T0", "VRG_DC_T0", "VRG_AB_T0")]
DATA[DATA$Pol_Policy_No == "37609540", VRG_NAMES] # Same output

# Merging datasets with left_join : without any condition, R will find the unique key at the most granular level by itself
DATA <- left_join(DATA, DATA_PREM)

# To create multiple variables : mutate
DATA <- DATA %>% 
  mutate(Prm_Curr_Total_Uncap_Final = Prm_Curr_AB_Uncap_Final + Prm_Curr_BI_Uncap_Final + Prm_Curr_COLL_Uncap_Final + Prm_Curr_COMP_Uncap_Final + Prm_Curr_DC_Uncap_Final + DATA$Prm_Curr_PD_Uncap_Final,
         Prm_Prop_Total_Uncap_Final = Prm_Prop_AB_Uncap_Final + Prm_Prop_BI_Uncap_Final + Prm_Prop_COLL_Uncap_Final + Prm_Prop_COMP_Uncap_Final + Prm_Prop_DC_Uncap_Final + DATA$Prm_Prop_PD_Uncap_Final,
         Dislocation_Pct = Prm_Prop_Total_Uncap_Final / Prm_Curr_Total_Uncap_Final - 1)

# Quick validation : Proposed premiums should be 3% higher than current  
# Note : in many functions, can add na.rm yo remove the NA in the calculation (otherwise the sum would = NA)
sum(DATA$Prm_Prop_Total_Uncap_Final, na.rm = TRUE) / sum(DATA$Prm_Curr_Total_Uncap_Final, na.rm = TRUE)

#### SECTION 5 : AGGREGATE DATA BY VARIABLE ####

## 5.1 Aggregate current and proposed premiums by insurer 

# Current premiums
VAR_CURR <- c("Prm_Curr_BI_Uncap_Final", "Prm_Curr_PD_Uncap_Final", "Prm_Curr_DC_Uncap_Final", "Prm_Curr_AB_Uncap_Final",
              "Prm_Curr_COLL_Uncap_Final", "Prm_Curr_COMP_Uncap_Final", "Prm_Curr_UA_Uncap_Final", "Prm_Curr_UI_Uncap_Final")

AGGREGATE_PREM_CURR <- DATA %>%
  group_by(Clt_Insurer_Tx) %>%
  summarise_at(VAR_CURR, sum, na.rm = TRUE)

# Proposed premiums
VAR_PROP <- c("Prm_Prop_BI_Uncap_Final", "Prm_Prop_PD_Uncap_Final", "Prm_Prop_DC_Uncap_Final", "Prm_Prop_AB_Uncap_Final",
              "Prm_Prop_COLL_Uncap_Final", "Prm_Prop_COMP_Uncap_Final", "Prm_Prop_UA_Uncap_Final", "Prm_Prop_UI_Uncap_Final")

AGGREGATE_PREM_PROP <- DATA %>%
  group_by(Clt_Insurer_Tx) %>%
  summarise_at(VAR_PROP, sum, na.rm = TRUE) # summarise_at allows to apply the sum on a vector of variable

# VALIDATION :
# - Validated that premiums are equal to sheet Data_Radar from file : S:\Pricing\Pricing Regions\02 Ontario\01 Auto\01 PPA\2020-03 - Classification\Filing\PIC\PIC - Section 2.xlsm
# - Current and proposed premiums are all OK

## 5.2 Aggregate data by vehicle (Need to sum the premiums so they are at a vehicle level)

# NOTE : It takes a while to run so no need to process it!

DATA_VEH <- DATA %>% 
  group_by(Clt_Opsite_Tx, Pol_Policy_No, Pol_Eff_Dt, Trx_Ent_Dt, Veh_Id_No) %>% 
  mutate(Prm_Veh_Prop_Total_Uncap_Final = sum(Prm_Prop_Total_Uncap_Final),
         Prm_Veh_Curr_Total_Uncap_Final = sum(Prm_Curr_Total_Uncap_Final)) %>% 
  filter(Dri_Type_Cd == "P")

#### SECTION 6 : GRAPHS & AUTOMATION ####

## 6.1 Create function to generate graphs by variable

# RESOURCE : https://plot.ly/r/

# GOAL : Graph average premium change by variable & separate NB and RN

# STEP 1 : Create the table with values to plot
# STEP 2 : Using the function plot_ly : create the graph and play with the parameters so it looks the way you want

FCT_PRM_CHG_BY_VAR <- function(DATA, BY.VAR)
{
  df <- DATA[, c("Pol_Business_Cd", BY.VAR, "Dislocation_Pct", "Prm_Prop_Total_Uncap_Final", "Prm_Curr_Total_Uncap_Final")]
  names(df) <- c("Pol_Business_Cd", "BY.VAR", "Dislocation_Pct","Prm_Prop_Total_Uncap_Final","Prm_Curr_Total_Uncap_Final")
  
  # STEP 1 : Create the tables to plot
  
  df_plot_NB <- df %>%
    filter(Pol_Business_Cd == "NB") %>%
    group_by(BY.VAR) %>%
    summarise(Avg_Prm_Chg_NB = sum(Prm_Prop_Total_Uncap_Final, na.rm = T) / sum(Prm_Curr_Total_Uncap_Final, na.rm = T) - 1,
              Nb_Exposure_NB = n())
  
  df_plot_RN <- df %>%
    filter(Pol_Business_Cd == "RN") %>%
    group_by(BY.VAR) %>%
    summarise(Avg_Prm_Chg_RN = sum(Prm_Prop_Total_Uncap_Final, na.rm = T) / sum(Prm_Curr_Total_Uncap_Final, na.rm = T) - 1,
              Nb_Exposure_RN = n())
  
  df_plot <- merge(df_plot_NB, df_plot_RN, by = "BY.VAR", sort = FALSE) # Final table to plot
  
  # STEP 2 : Create the graph
  
  p <- plot_ly(data = df_plot,
               width = 1100, 
               height = 800) %>%
    
    add_trace(x = ~BY.VAR, y = ~Nb_Exposure_RN, type = "bar", opacity = 0.3, name = "Exposure RN") %>%
    
    add_trace(x = ~BY.VAR, y = ~Nb_Exposure_NB, type = "bar", opacity = 0.3, name = "Exposure NB") %>%
    
    add_trace(x = ~BY.VAR, y = ~Avg_Prm_Chg_RN, type = "scatter", yaxis = "y2", mode = "lines+markers",
              marker = list(size = 8, color = "rgb(36, 51, 108)"), name = "Avg_Prem_Chg RN", line = list(color = "rgb(36, 51, 108)")) %>%
    
    add_trace(x = ~BY.VAR, y = ~Avg_Prm_Chg_NB, type = "scatter", yaxis = "y2", mode = "lines+markers",
              marker = list(size = 8, color = "rgb(225, 135, 86)"), name = "Avg_Prem_Chg NB", line = list(color = "rgb(225, 150, 74)")) %>%
    
    layout(barmode = "stack", # Other barmodes : "overlay", "group"
           xaxis = list(title = paste(BY.VAR), dtick = 1, position = 0, showgrid = TRUE), 
           yaxis2 = list(title = "Average Premium Change", side = "left", overlaying = "y", tickformat = "%", dtick = 0.05),
           yaxis = list(title = "Exposure", side = "right"),
           title = paste("Average Premium Change by", BY.VAR),
           legend  = list(x = 0.1,
                          y = -0.12,
                          orientation = 'h',
                          font = list(size = 12)))  
  
  return(p) # The function will only output the graph
}

## 6.2 Generate graphs for multiple variables

# STEP 1 : Bucket variables (if needed)

DATA$Veh_Price_Am_Bucket <- cut(DATA$Veh_Price_Am, breaks = c(seq(from = 0, to = 150000, by = 10000), Inf), right = FALSE, dig.lab = 10)
DATA$Veh_Age_Nb_Bucket <- ifelse(DATA$Veh_Age_Nb >= 20, 20, DATA$Veh_Age_Nb)

# STEP 2 : Create loop to generate graphs

VAR.NEEDED <- c("Veh_Age_Nb_Bucket", "Dri_Age_Nb", "Veh_Price_Am_Bucket")

# Need to create an empty list first : that's where the graphs will be stored
Graphs_Prm_Chg_By_Var <- vector("list", length(VAR.NEEDED))
names(Graphs_Prm_Chg_By_Var) <- VAR.NEEDED

for (i in 1:length(VAR.NEEDED))
{
  Graphs_Prm_Chg_By_Var[[i]] <- FCT_PRM_CHG_BY_VAR(DATA = DATA, BY.VAR = VAR.NEEDED[i])
}

# NOTE : To extract from a list : need double brackets [[ ]]

## 6.3 Export graphs automatically

PATH.GRAPH <- "S:/Pricing/Pricing Regions/02 Ontario/06 Other/R Ressources/Graphs/"

for (i in 1:length(VAR.NEEDED))
{
  htmlwidgets::saveWidget(as_widget(Graphs_Prm_Chg_By_Var[[i]]), paste(PATH.GRAPH, "Avg Prm Change - ", VAR.NEEDED[[i]], ".html", sep = ""))
}

#### SECTION 7 : EXPORT DATA ####

# Export in csv
write_csv(SAMPLE, "S:/Pricing/Pricing Regions/02 Ontario/06 Other/R Ressources/Data/Sample_inf_20190731.csv")

# Export in fst
write_fst(SAMPLE, "S:/Pricing/Pricing Regions/02 Ontario/06 Other/R Ressources/Data/Sample_inf_20190731.fst")



