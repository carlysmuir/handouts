## Tidy Concept 
#https://www.youtube.com/embed/CQkmZbPzKZs?vp=1080
trial <- read.delim(sep = ',', header = TRUE, text = "
block, drug, control, placebo
    1, 0.22,    0.58,    0.31
    2, 0.12,    0.98,    0.47
    3, 0.42,    0.19,    0.40
")

## Pivot wide to long 
#making the table longer by making single rows for each observation
library(tidyr)
#trial is the df, cols is the columns we are going to pivot into longer format
#How will you name cols in new table names_to
#values to, is the column that now holds the variables
tidy_trial <- pivot_longer(trial,
                  cols = c(drug, control, placebo),
                  names_to = 'treatment',
                  values_to = 'response')

## Pivot long to wide 
#multiple rows for each observation and you want one row for each observation
survey <- read.delim(sep = ',', header = TRUE, text = "
participant,   attr, val
1          ,    age,  24
2          ,    age,  57
3          ,    age,  13
1          , income,  30
2          , income,  60
")

#names_from pulls from cols as they exist, in this case attributes column
tidy_survey <- pivot_wider(survey,
                   names_from = attr,
                   values_from = val)

#missing data, values_fill allows you to specify what you want NA (missing data) to be
tidy_survey <- pivot_wider(survey,
                           names_from = attr,
                           values_from = val,
                           values_fill = 0)


#https://www.youtube.com/embed/4VF5ezcEU1c?vp=1080
## Sample Data 
library(data.table)
cbp <- fread('data/cbp15co.csv')

cbp <- fread(
  'data/cbp15co.csv',
  colClasses = c(
    FIPSTATE = 'character',
    FIPSCTY = 'character'
  ))

acs <- fread(
  'data/ACS/sector_ACS_15_5YR_S2413.csv',
  colClasses = c(FIPS = 'character'))

## dplyr Functions 

library(dplyr)
#We want to filter cbp df by the observations
#with 4 dashes in the NAICS columns, but
#not include 5 dashes bc we use ! to indicate not
cbp2 <- filter(cbp,
  grepl('----', NAICS),
  !grepl('------', NAICS))

library(stringr)
#filter by the col and then the pattern. NAICS by strings with number [0-9] and
#we wants exactly 2 numbers followed by 4 dashes
cbp2 <- filter(cbp,
  str_detect(NAICS, '[0-9]{2}----'))

#mutate will alter columns, rewrite old column or create new
#This will combine state and cty to create a new col
#column name will be FIPS which will be a combination of cols
cbp3 <- mutate(cbp2,
  FIPS = str_c(FIPSTATE, FIPSCTY))

#multiple arguments for multiple cols
#you want to remove any NAICS values with a certain pattern
#in this case any value in NAICS with -+ (any number of ---)
cbp3 <- mutate(cbp2,
  FIPS = str_c(FIPSTATE, FIPSCTY),
  NAICS = str_remove(NAICS, '-+'))

#advantafe of dplyr, all dplyr functions have the same syntax
#they take df as first argument and then return a df
#we can chain these together, so the output of one going into another
#we do this with pipes %>%

cbp <- cbp %>%
  filter(
    str_detect(NAICS, '[0-9]{2}----')
  ) %>%
  mutate(
    FIPS = str_c(FIPSTATE, FIPSCTY),
    NAICS = str_remove(NAICS, '-+')
  )
  
  
#select is a filter for cols, filter is for rows
#we are selecting from the cbp df and selecting the
#FIPS column, NAICS column, and those starting with N
cbp <- cbp %>%
  select(
    FIPS,
    NAICS,
    starts_with('N')
  )


#https://www.youtube.com/embed/gMi1bbobSCQ?vp=1080
## Join
#many to one join: combine cbp df to sector df. Many sectors with same NAICS 
#code in cbp, but only one row for each NAICS code in sector
sector <- fread(
  'data/ACS/sector_naics.csv',
  colClasses = c(NAICS = 'character'))

#join based on NAICS column..cols have to have same name
cbp <- cbp %>%
  inner_join(sector)
#will only include rows where NAICS code is in both dfs

## Group By 
#Sectors have multiple different NAICS codes, but we want just 1 for all
# we will do a split/apply/combine to get rid of redundancy 
#(multiple sectors for each county)
cbp_grouped <- cbp %>%
  group_by(FIPS, Sector)

## Summarize 
#sum the number of establishments in N cols so that there is 
#only one val for unique FIPS and NAICS
#sum columns starting with N, but not NAICS
cbp <- cbp %>%
  group_by(FIPS, Sector) %>%
  select(starts_with('N'), -NAICS) %>%
  summarise_all(sum)
#split into subsets using group_by
# we applied select to summarize them
#combine happens automatically

acs_cbp <- cbp %>%
  inner_join(acs)
