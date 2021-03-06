# load all R packages
# library(reshape2)
library(data.table)
library(tidyverse)
library(ggrepel)
library(scales)
library(stringr)
library(migest)
library(RColorBrewer)
library(circlize)
library(choroplethr)
library(choroplethrMaps)
library(likert) # for likert plot
library(shiny)
library(shinydashboard)
library(plotly)
library(sunburstR)
library(gridExtra)

# plotly Sankey use json data as input (07/23/2020)
library(rjson)

# read in data
dataGroup <- read.csv("sourceData.csv")
colnames(dataGroup)[colnames(dataGroup)=="degree_cip"] <- "degree_category"

# count of alumni
alnCount = nrow(dataGroup)

# get intitute name
nameInstitute <- dataGroup$institute[1]

# convert all factor columns to character columns 
# https://stackoverflow.com/questions/47633241/convert-all-factor-columns-to-character-in-a-data-frame-without-affecting-non-fa
dataGroup <- dataGroup %>% mutate_if(is.factor, as.character)

### convert long entry names to short entry names for data visualization
# convert long name of job sector to short name
dataGroup$job_sector[dataGroup$job_sector == 'Independent/self-employed'] <- "Indep./self-employed"

# convert long name of job specifics to short name
dataGroup$specifics[dataGroup$specifics == 'Science administration/project management'] <- "Science admin./PMT"
dataGroup$specifics[dataGroup$specifics == 'Science writing or communications'] <- 'Science writing or comm.'
dataGroup$specifics[dataGroup$specifics == 'Additional postdoctoral training'] <- 'Additional postdoc'

# (3/22/18) replace "Unknown" with "Degree Unknown" in "degree_category"
dataGroup$degree_category[dataGroup$degree_category == 'Unknown'] <- "Degree Unknown"

# Convert job functions which are not in top 12 to 'REST COMBINED' for better data visualization
tblJobSpec <- table(dataGroup[,"specifics"])
nameRestSpec <- names(tail(sort(tblJobSpec, decreasing = TRUE), -12))
dataGroup$specifics[dataGroup$specifics %in% nameRestSpec] <- "REST COMBINED"

# convert character columns back to factor columns 
dataGroup <- dataGroup %>% mutate_if(is.character, as.factor)

# get the time period data for UI
timePeriods <- unique(sort(dataGroup$years))
timePeriods <- as.character(timePeriods)
timeYrs <- unlist(strsplit(timePeriods,'-'))
timeYrs <- as.numeric(timeYrs)
minYrs <- min(timeYrs)
maxYrs <- max(timeYrs)
totalYrs <- paste0(minYrs,'-',maxYrs)
trendYrs <- paste0(timePeriods, collapse = ",")

# prepare choice 
choiceAll <- 'Whole time (2000-2019)'
choiceYrs <- paste0('Left NIEHS in ', timePeriods)
choiceYrs <- c(choiceAll, choiceYrs)



##### Definition for colors
# gender colors
genderColors = c("#D81B60", "#4F94CD")
# visiting colors
visitColors = c("#47427e","#6cc06d")

#>>> general career plot start
colorJSect = c("Academic institution"="#94363a",
               "For-profit company"="#f19493",
               "Government agency"="#b15426",
               "Indep./self-employed"="#f285a8",
               "Non-profit organization"="#933761",
               "Unknown or Undecided"="#da9a54",
               "All sectors"="#000000")

colorJType = c("Management"="#24594d",
               "Non-tenure track faculty" ="#8acfbf",
               "Professional staff"="#255a2d",
               "Support staff"="#23787a",
               "Tenure track faculty"="#6cc06d",
               "Trainee" ="#5e7e37", 
               "Unknown or Undecided"="#89c658",
               "All types"="#000000")

colorJSpec = c("Additional postdoc"="#225f7b",
               "Computation/informatics"="#a792c5",
               "Consulting"="#655F9F",
               "Grants management"="#5655A9",
               "Primarily applied research"="#8b9fd1",
               "Primarily basic research"="#243e7d",
               "Primarily clinical research"="#747fbe",
               "Primarily clinical practice"="#7c74be",
               "Primarily teaching"="#6b417d",
               "Regulatory affairs"="#23787a",
               "REST COMBINED"="#47427e",
               "Science admin./PMT"="#85357a",
               "Science writing or comm."="#c06aaa",
               "Technical/customer support"="#4bc4d2",
               "Unknown or Undecided"="#8dbbd8",
               "All specifics"="#000000")



# demographical percentage count
tGender = table(dataGroup[,"gender"])
tMalePct = round(1 - as.numeric(tGender["Female"]) / alnCount, 3) * 100
tCitizen = table(dataGroup[,"citizenship"])
tCitizenPct = round(1- as.numeric(tCitizen["International"]) / alnCount, 3) * 100

# general career count
tGnrSector = table(dataGroup[,"job_sector"])
tGnrSectorPct = round(tGnrSector / alnCount, 3) * 100
tGnrType = table(dataGroup[,"job_type"])
tGnrTypePct = round(tGnrType / alnCount, 3) * 100
tGnrSpecifics = table(dataGroup[,"specifics"])
tGnrSpecPct = round(tGnrSpecifics / alnCount, 3) * 100

# training time
cntAvgTime = round(mean(dataGroup$months_postdoc,na.rm=TRUE),1)
cntAcdTime = round(mean(dataGroup$months_postdoc[dataGroup$job_sector == "Academic institution"],na.rm=TRUE),1)
cntGvnTime = round(mean(dataGroup$months_postdoc[dataGroup$job_sector == "Government agency"],na.rm=TRUE),1)
cntPrfTime = round(mean(dataGroup$months_postdoc[dataGroup$job_sector == "For-profit company"],na.rm=TRUE),1)

# use dplyr to summarize gender information
# http://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
gend4All <- dataGroup %>% group_by(gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)
gend4Sect <- dataGroup %>% group_by(job_sector, gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)
gend4Type <- dataGroup %>% group_by(job_type, gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)
gend4Spec <- dataGroup %>% group_by(specifics, gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)

gendYrAll <- dataGroup %>% group_by(years,gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)
gendYrSect <- dataGroup %>% group_by(years,job_sector, gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)
gendYrType <- dataGroup %>% group_by(years,job_type, gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)
gendYrSpec <- dataGroup %>% group_by(years,specifics, gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)


# use dplyr to summarize citizenship information
# http://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
citi4All <- dataGroup %>% group_by(citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)
citi4Sect <- dataGroup %>% group_by(job_sector, citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)
citi4Type <- dataGroup %>% group_by(job_type, citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)
citi4Spec <- dataGroup %>% group_by(specifics, citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)

citiYrAll <- dataGroup %>% group_by(years,citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)
citiYrSect <- dataGroup %>% group_by(years,job_sector, citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)
citiYrType <- dataGroup %>% group_by(years,job_type, citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)
citiYrSpec <- dataGroup %>% group_by(years,specifics, citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)


# summary country origin information
# keep 0 count in summary:
#   http://stackoverflow.com/questions/25956178
#   http://stackoverflow.com/questions/16073918
#   http://stackoverflow.com/questions/22523131
#     complete(b, fill = list(count_a = 0))
dcoAll <- dataGroup %>% group_by(country_origin) %>% summarise (Postdoc = n()) %>% complete(country_origin, fill = list(Postdoc = 0))
colnames(dcoAll)[colnames(dcoAll)=="country_origin"] <- "Country"
dcoYrs <- dataGroup %>% group_by(years,country_origin) %>% summarise (Postdoc = n()) %>% complete(country_origin, fill = list(Postdoc = 0))
colnames(dcoYrs)[colnames(dcoYrs)=="country_origin"] <- "Country"

# sort country by posdoc number
## https://trinkerrstuff.wordpress.com/2013/08/14/how-do-i-re-arrange-ordering-a-plot-revisited/
srtCTRYori <- factor(dcoYrs$Country, levels=dcoYrs$Country[order(unique(dcoYrs$Postdoc))])
dcoYrs$Country <- srtCTRYori
srtCTRYori4 <- factor(dcoAll$Country, levels=dcoAll$Country[order(unique(dcoAll$Postdoc))])
dcoAll$Country <- srtCTRYori4

# summary country job information
dcjAll <- dataGroup %>% group_by(job_country) %>% summarise (Postdoc = n()) %>% complete(job_country, fill = list(Postdoc = 0))
colnames(dcjAll)[colnames(dcjAll)=="job_country"] <- "Country"
dcjYrs <- dataGroup %>% group_by(years,job_country) %>% summarise (Postdoc = n()) %>% complete(job_country, fill = list(Postdoc = 0))
colnames(dcjYrs)[colnames(dcjYrs)=="job_country"] <- "Country"

# sort country by posdoc number
# http://rstudio-pubs-static.s3.amazonaws.com/7433_4537ea5073dc4162950abb715f513469.html
#   x$name <- factor(x$name, levels = x$name[order(x$val)])
srtCTRYjob <- factor(dcjYrs$Country, levels=dcjYrs$Country[order(unique(dcjYrs$Postdoc))])
dcjYrs$Country <- srtCTRYjob
srtCTRYjob4 <- factor(dcjAll$Country, levels=dcjAll$Country[order(unique(dcjAll$Postdoc))])
dcjAll$Country <- srtCTRYjob4

# make the matrix for circular plotting of postdoc migration

# Keep countries with large number of postdocs:
#   United States, China, Japan, South Korea, India
#   Canada, Germany, United Kingdom
# Group the rest countries to continents:
#   Europe, Asia, Africa, Australasia, Middle & South America
cnvtCountry <- fread("country_region.csv")

convertCountry <- function(dfin, countryTable) {
  df <- data.table(dfin)
  df$country_origin <- as.character(df$country_origin)
  df$job_country <- as.character(df$job_country)
  
  setkey(countryTable,Country)
  setkey(df,country_origin)
  df$country_origin <- df[countryTable, nomatch=0]$Region
  
  setkey(df,job_country)
  df$job_country <- df[countryTable, nomatch=0]$Region
  
  df$country_origin <- factor(df$country_origin)
  df$job_country <- factor(df$job_country)
  return(df)
}

mgrCountry <- convertCountry(dataGroup,cnvtCountry)
mgrCountryGroup <- mgrCountry %>% group_by(years,country_origin,job_country) %>% summarise (n = n())
mgrCountryAll <- mgrCountry %>% group_by(country_origin,job_country) %>% summarise (n = n())

makeCircMD <- function(idct) {
  # remove 
  dct <- idct
  dct <- dct[!(dct$job_country == 'Unknown'),]
  dct <- dct[!(dct$country_origin == 'Unknown'),]
  
  uniTop <- unique(c(as.character(dct$country_origin), as.character(dct$job_country)))
  mtrx <- matrix(0L,length(uniTop), length(uniTop))
  dimnames(mtrx) <- list(orig = uniTop, dest=uniTop)
  
  # map count to m matrix
  for(i in 1:nrow(dct)) {
    rw <- dct[i,]
    c <- as.character(rw$country_origin)
    j <- as.character(rw$job_country)
    mtrx[c, j] = rw$n
  }
  
  return(mtrx)
}

# job positions by state
dataUSA <- dataGroup[dataGroup$job_country == 'United States',]
dataUSA$job_state <- factor(tolower(dataUSA$job_state),exclude=NULL)
stateNM <- tolower(state.name)
misSTNM <- stateNM[!(stateNM %in% dataUSA$job_state)]
levels(dataUSA$job_state) <- c(levels(dataUSA$job_state),misSTNM)

dfStateAll <- dataUSA %>% group_by(job_state) %>% summarise (value = n()) %>% complete(job_state, fill = list(value = 0))
colnames(dfStateAll)[colnames(dfStateAll)=="job_state"] <- "region"
dfStateAll$value <- as.integer(dfStateAll$value)
dfStateYrs <- dataUSA %>% group_by(years,job_state) %>% summarise (value = n()) %>% complete(job_state, fill = list(value = 0))
colnames(dfStateYrs)[colnames(dfStateYrs)=="job_state"] <- "region"
dfStateYrs$value <- as.integer(dfStateYrs$value)

# summary job sector (where did they go) information
dfJobSectAll <- dataGroup %>% group_by(job_sector) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
colnames(dfJobSectAll) <- c('Sector','Count','Postdoc')
dfJobSectYrs <- dataGroup %>% group_by(years, job_sector) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
colnames(dfJobSectYrs) <- c('years','Sector','Count','Postdoc')

# sort job sector by posdoc number
# srtSECTjob <- factor(dfJobSectYrs$Sector, levels = dfJobSectYrs$Sector[order(dfJobSectYrs$Postdoc, decreasing=TRUE)])
# dfJobSectYrs$Sector <- srtSECTjob
# srtSECTjob4 <- factor(dfJobSectAll$Sector, levels = dfJobSectAll$Sector[order(dfJobSectAll$Postdoc, decreasing=TRUE)])
# dfJobSectAll$Sector <- srtSECTjob4


# summary job type (What is the level of their position) information
dfJobTypeAll <- dataGroup %>% group_by(job_type) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
colnames(dfJobTypeAll) <- c('Type','Count','Postdoc')
dfJobTypeYrs <- dataGroup %>% group_by(years, job_type) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
colnames(dfJobTypeYrs) <- c('years','Type','Count','Postdoc')

# sort job type by posdoc number
# srtTYPEjob <- factor(dfJobTypeYrs$Type, levels = dfJobTypeYrs$Type[order(dfJobTypeYrs$Postdoc, decreasing=TRUE)])
# dfJobTypeYrs$Type <- srtTYPEjob
# srtTYPEjob4 <- factor(dfJobTypeAll$Type, levels = dfJobTypeAll$Type[order(dfJobTypeAll$Postdoc, decreasing=TRUE)])
# dfJobTypeAll$Type <- srtTYPEjob4


# summary job specificity (what are they doing) information
dfJobSpecAll <- dataGroup %>% group_by(specifics) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
colnames(dfJobSpecAll) <- c('Specifics','Count','Postdoc')
dfJobSpecYrs <- dataGroup %>% group_by(years, specifics) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
colnames(dfJobSpecYrs) <- c('years','Specifics','Count','Postdoc')

# sort job specificity by posdoc number
# srtSPECjob <- factor(dfJobSpecYrs$Specifics, levels = dfJobSpecYrs$Specifics[order(dfJobSpecYrs$Postdoc, decreasing=TRUE)])
# dfJobSpecYrs$Specifics <- srtSPECjob
# srtSPECjob4 <- factor(dfJobSpecAll$Specifics, levels = dfJobSpecAll$Specifics[order(dfJobSpecAll$Postdoc, decreasing=TRUE)])
# dfJobSpecAll$Specifics <- srtSPECjob4


# average time in NIEHS by job sector
dfTimeSectAll <- dataGroup %>% group_by(job_sector) %>% summarise(avg_time=round(mean(months_postdoc, na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n())
dfTimeSectAll$job_sector <- as.character(dfTimeSectAll$job_sector)
dfTimeSectAll <- bind_rows(dfTimeSectAll, dataGroup %>% summarise(avg_time=round(mean(months_postdoc, na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n()) %>% mutate(job_sector="All sectors"))
dfTimeSectYrs <- dataGroup %>% group_by(years, job_sector) %>% summarise(avg_time=round(mean(months_postdoc, na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n())
# (10/06/2016): change from rbind to bind_rows (dplyr) to fix the error of combine two data frames into list
#   http://stackoverflow.com/questions/3402371/combine-two-data-frames-by-rows-rbind-when-they-have-different-sets-of-columns
# dfTimeSectYrs <- rbind(dfTimeSectYrs, dataGroup %>% group_by(years) %>% summarise(avg_time=round(mean(months_postdoc, na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n()) %>% mutate(job_sector="All sectors"))
dfTimeSectYrs$job_sector <- as.character(dfTimeSectYrs$job_sector)
dfTimeSectYrs <- bind_rows(dfTimeSectYrs, dataGroup %>% group_by(years) %>% summarise(avg_time=round(mean(months_postdoc, na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n()) %>% mutate(job_sector="All sectors"))


# average time in NIEHS by job type
dfTimeTypeAll <- dataGroup %>% group_by(job_type) %>% summarise(avg_time=round(mean(months_postdoc, na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n())
dfTimeTypeAll$job_type <- as.character(dfTimeTypeAll$job_type)
dfTimeTypeAll <- bind_rows(dfTimeTypeAll, dataGroup %>% summarise(avg_time=round(mean(months_postdoc, na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n()) %>% mutate(job_type="All types"))
dfTimeTypeYrs <- dataGroup %>% group_by(years, job_type) %>% summarise(avg_time=round(mean(months_postdoc, na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n())
dfTimeTypeYrs$job_type <- as.character(dfTimeTypeYrs$job_type)
dfTimeTypeYrs <- bind_rows(dfTimeTypeYrs, dataGroup %>% group_by(years) %>% summarise(avg_time=round(mean(months_postdoc, na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n()) %>% mutate(job_type="All types"))


# average time in NIEHS by job specifics
dfTimeSpecAll <- dataGroup %>% group_by(specifics) %>% summarise(avg_time=round(mean(months_postdoc,na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n())
dfTimeSpecAll$specifics <- as.character(dfTimeSpecAll$specifics)
dfTimeSpecAll <- bind_rows(dfTimeSpecAll, dataGroup %>% summarise(avg_time=round(mean(months_postdoc,na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n()) %>% mutate(specifics="All specifics"))
dfTimeSpecYrs <- dataGroup %>% group_by(years, specifics) %>% summarise(avg_time=round(mean(months_postdoc,na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n())
dfTimeSpecYrs$specifics <- as.character(dfTimeSpecYrs$specifics)
dfTimeSpecYrs <- bind_rows(dfTimeSpecYrs, dataGroup %>% group_by(years) %>% summarise(avg_time=round(mean(months_postdoc,na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n()) %>% mutate(specifics="All specifics"))


# average time in NIEHS by degree field
dfTimeDegrAll <- dataGroup %>% group_by(degree_category) %>% summarise(avg_time=round(mean(months_postdoc,na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n())
dfTimeDegrAll$degree_category <- as.character(dfTimeDegrAll$degree_category)
dfTimeDegrAll <- bind_rows(dfTimeDegrAll, dataGroup %>% summarise(avg_time=round(mean(months_postdoc,na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n()) %>% mutate(degree_category="All degree"))
dfTimeDegrYrs <- dataGroup %>% group_by(years, degree_category) %>% summarise(avg_time=round(mean(months_postdoc,na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n())
dfTimeDegrYrs$degree_category <- as.character(dfTimeDegrYrs$degree_category)
dfTimeDegrYrs <- bind_rows(dfTimeDegrYrs, dataGroup %>% group_by(years) %>% summarise(avg_time=round(mean(months_postdoc,na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n()) %>% mutate(degree_category="All degree"))


# training time by gender and country
gendTimeAll <- dataGroup %>% group_by(gender) %>% summarise(avg_time=round(mean(months_postdoc,na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n())
gendTimeGrp <- dataGroup %>% group_by(years, gender) %>% summarise(avg_time=round(mean(months_postdoc,na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n())
gendTimeCtryAll <- mgrCountry %>% group_by(country_origin,gender) %>% summarise(avg_time=round(mean(months_postdoc,na.rm=TRUE),digits = 1), min_time=min(months_postdoc,na.rm=TRUE), max_time=max(months_postdoc,na.rm=TRUE), num_data=n())


# count the degree_category vs job_sector, job_type, specifics
dgcSectAll <- dataGroup %>% group_by(degree_category, job_sector) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
dgcTypeAll <- dataGroup %>% group_by(degree_category, job_type) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
dgcSpecAll <- dataGroup %>% group_by(degree_category, specifics) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))

dgcSectYr <- dataGroup %>% group_by(years,degree_category,job_sector) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
dgcTypeYr <- dataGroup %>% group_by(years,degree_category,job_type) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
dgcSpecYr <- dataGroup %>% group_by(years,degree_category,specifics) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
