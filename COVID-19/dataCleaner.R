# 22/01/2020 to yesterday

# Setting the working directory
setwd("~/Documents/A-tracking-of-2019-nCoV/COVID-19/")

#####  LIBRARIES  #####
# loading library for string operations
library(stringr)
library(RCurl)

library(AUCRF)
library(randomForest)
library(RFmarkerDetector) # random forest  ---> for autoscale()


#########################################


check.Confirmed = NULL
check.Deaths = NULL
check.Recovered = NULL

# loading the datasets
if(url.exists("https://raviprakashravi.cf/")){  # files for US are no longer being updated   --->    url.exists("https://raviprakashravi.cf/")
  
  # names
  confirmLocation = "Johns H. University/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
  deathsLocation = "Johns H. University/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
  recoveredLocation = "Johns H. University/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
  
  ##########################
  
  # having backup
  write.csv(read.csv(confirmLocation), "Johns H. University/csse_covid_19_time_series/backup/time_series_19-covid-Confirmed.csv")
  write.csv(read.csv(deathsLocation), "Johns H. University/csse_covid_19_time_series/backup/time_series_19-covid-Deaths.csv")
  write.csv(read.csv(recoveredLocation), "Johns H. University/csse_covid_19_time_series/backup/time_series_19-covid-Recovered.csv")
  
  # loading new data
  check.Confirmed <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
  check.Deaths <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
  check.Recovered <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))
  
  # saving new data
  write.csv(check.Confirmed, confirmLocation, row.names = F)
  write.csv(check.Deaths, deathsLocation, row.names = F)
  write.csv(check.Recovered, recoveredLocation, row.names = F)
  
} else {  # Internet is NOT available!!!
  
  check.Confirmed = read.csv("Johns H. University/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
  check.Deaths = read.csv("Johns H. University/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
  check.Recovered = read.csv("Johns H. University/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
}  

india.conf = check.Confirmed[which(str_detect(check.Confirmed$Country.Region, "India")),]

write.csv(india.conf, file = "~/Documents/COVID-19-India/archived/time_series_data/jhu_format/time_series_19-covid-jhu-Confirmed.csv", row.names = FALSE)

#check.Confirmed = read.csv("Johns H. University/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
#check.Deaths = read.csv("Johns H. University/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
#check.Recovered = read.csv("Johns H. University/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

#################################################

# removing NAs

for (i in 1:nrow(check.Confirmed)) {
  for (j in 5:ncol(check.Confirmed)) {
    if(j==5) {
      check.Confirmed[i,j] = ifelse(is.na(check.Confirmed[i, j]), 0, check.Confirmed[i,j])
    } else {
      if(is.na(check.Confirmed[i, j])){
        check.Confirmed[i,j] = check.Confirmed[i, (j-1)]
      } else if(check.Confirmed[i, (j-1)] > check.Confirmed[i, j]){
        check.Confirmed[i,j] = check.Confirmed[i, (j-1)]
      }
    }
  }
}

for (i in 1:nrow(check.Deaths)) {
  for (j in 5:ncol(check.Deaths)) {
    if(j==5) {
      check.Deaths[i,j] = ifelse(is.na(check.Deaths[i, j]), 0, check.Deaths[i,j])
    } else {
      if(is.na(check.Deaths[i, j])){
        check.Deaths[i,j] = check.Deaths[i, (j-1)]
      } else if(check.Deaths[i, (j-1)] > check.Deaths[i, j]){
        check.Deaths[i,j] = check.Deaths[i, (j-1)]
      }
    }
  }
}

for (i in 1:nrow(check.Recovered)) {
  for (j in 5:ncol(check.Recovered)) {
    if(j==5) {
      check.Recovered[i,j] = ifelse(is.na(check.Recovered[i, j]), 0, check.Recovered[i,j])
    } else {
      if(is.na(check.Recovered[i, j])){
        check.Recovered[i,j] = check.Recovered[i, (j-1)]
      } else if(check.Recovered[i, (j-1)] > check.Recovered[i, j]){
        check.Recovered[i,j] = check.Recovered[i, (j-1)]
      }
    }
  }
}

####################
#View(check.Confirmed)
#View(check.Deaths)
#View(check.Recovered)



###############################

# replacing in states
states = as.character(check.Confirmed$Province.State)
states.levels = as.character(levels(check.Confirmed$Province.State))

states[states %in% ""] = "Others"
states[states %in% "From Diamond Princess"] = "Diamond Princess"
states.levels[states.levels %in% ""] = "Others"
states.levels = states.levels[!states.levels %in% "From Diamond Princess"]

# replacing in countries
countries = as.character(check.Confirmed$Country.Region)
countries.levels = as.character(levels(check.Confirmed$Country.Region))

countries[countries %in% "US"] = "United States"
countries[countries %in% "Burma"] = "Myanmar"
countries[countries %in% "Czechia"] = "Czech Republic"
countries[countries %in% "Cote d'Ivoire"] = "Ivory Coast"
countries[countries %in% "UK"] = "United Kingdom"
countries[countries %in% "Taiwan*"] = "Taiwan"
countries[countries %in% "The Bahamas"] = "Bahamas"
countries[countries %in% "Gambia, The"] = "Gambia"
countries[countries %in% "Korea, South"] = "South Korea"
countries[countries %in% c("Congo (Brazzaville)", "Congo (Kinshasa)", "Republic of the Congo")] = "Democratic Republic of the Congo"
###
countries.levels[countries.levels %in% "US"] = "United States"
countries.levels[countries.levels %in% "Burma"] = "Myanmar"
countries.levels[countries.levels %in% "Czechia"] = "Czech Republic"
countries.levels[countries.levels %in% "Cote d'Ivoire"] = "Ivory Coast"
countries.levels[countries.levels %in% "UK"] = "United Kingdom"
countries.levels[countries.levels %in% "Taiwan*"] = "Taiwan"
countries.levels[countries.levels %in% "The Bahamas"] = "Bahamas"
countries.levels[countries.levels %in% "Gambia, The"] = "Gambia"
countries.levels[countries.levels %in% "Korea, South"] = "South Korea"

countries.levels = countries.levels[!countries.levels %in% c("Congo (Brazzaville)", "Congo (Kinshasa)", "Republic of the Congo")]
countries.levels = c(countries.levels, "Democratic Republic of the Congo")

###############################

# rectified fectors
states.factor  = factor(c(states), levels = c(states.levels))
countries.factor  = factor(countries, levels = countries.levels)


# editing factors in datasets
check.Confirmed = cbind(
  Province.State = states.factor,
  Country.Region = countries.factor,
  check.Confirmed[,3:ncol(check.Confirmed)]
)

check.Deaths = cbind(
  Province.State = states.factor,
  Country.Region = countries.factor,
  check.Deaths[,3:ncol(check.Deaths)]
)


###################################################################################
#######   Not Used - Recovered
###################################################################################

# replacing in states
states = as.character(check.Recovered$Province.State)
states.levels = as.character(levels(check.Recovered$Province.State))

states[states %in% ""] = "Others"
states[states %in% "From Diamond Princess"] = "Diamond Princess"
states.levels[states.levels %in% ""] = "Others"
states.levels = states.levels[!states.levels %in% "From Diamond Princess"]

countries = as.character(check.Recovered$Country.Region)
countries.levels = as.character(levels(check.Recovered$Country.Region))

countries[countries %in% "US"] = "United States"
countries[countries %in% "UK"] = "United Kingdom"
countries[countries %in% "Taiwan*"] = "Taiwan"
countries[countries %in% "The Bahamas"] = "Bahamas"
countries[countries %in% "Gambia, The"] = "Gambia"
countries[countries %in% "Korea, South"] = "South Korea"
countries[countries %in% c("Congo (Brazzaville)", "Congo (Kinshasa)", "Republic of the Congo")] = "Democratic Republic of the Congo"
###
countries.levels[countries.levels %in% "US"] = "United States"
countries.levels[countries.levels %in% "UK"] = "United Kingdom"
countries.levels[countries.levels %in% "Taiwan*"] = "Taiwan"
countries.levels[countries.levels %in% "The Bahamas"] = "Bahamas"
countries.levels[countries.levels %in% "Gambia, The"] = "Gambia"
countries.levels[countries.levels %in% "Korea, South"] = "South Korea"

countries.levels = countries.levels[!countries.levels %in% c("Congo (Brazzaville)", "Congo (Kinshasa)", "Republic of the Congo")]
countries.levels = c(countries.levels, "Democratic Republic of the Congo")


# check.Recovered = cbind(
#   Province.State = states.factor,
#   Country.Region = countries.factor,
#   check.Recovered[,3:ncol(check.Recovered)]
# )


#View(check.Confirmed)
#View(check.Deaths)
#View(check.Recovered)

###################################################################################
#######   Not Used - Recovered
###################################################################################


###############################
#str(check.Confirmed)

# Removing outlier i.e. Diamond.Princess
Diamond.Princess.Confirmed = check.Confirmed[ which(str_detect(check.Confirmed$Country.Region, "Cruise Ship", negate = F)), ]
check.Confirmed = check.Confirmed[ which(str_detect(check.Confirmed$Country.Region, "Cruise Ship", negate = T)), ]

Diamond.Princess.Deaths = check.Deaths[ which(str_detect(check.Deaths$Country.Region, "Cruise Ship", negate = F)),]
check.Deaths = check.Deaths[ which(str_detect(check.Deaths$Country.Region, "Cruise Ship", negate = T)), ]

# Diamond.Princess.Recovered = check.Recovered[ which(str_detect(check.Recovered$Country.Region, "Cruise Ship", negate = F)), ]
# check.Recovered = check.Recovered[ which(str_detect(check.Recovered$Country.Region, "Cruise Ship", negate = T)), ]



## Rectifying Row sequences
row.names(check.Confirmed) <- NULL
row.names(check.Deaths) <- NULL
row.names(check.Recovered) <- NULL



## Closed cases (i.e. Recovered or Death cases)
#cases.Closed = cbind(check.Confirmed[,1:4],  (check.Deaths[,5:ncol(check.Deaths)] + check.Recovered[,5:ncol(check.Recovered)]))
## Active cases 
cases.Active = cbind(check.Confirmed[,1:4],  (check.Confirmed[,5:ncol(check.Confirmed)] - check.Deaths[,5:ncol(check.Deaths)])) # cases.Closed[,5:ncol(cases.Closed)]))


###  When and Where COVID-19 ever.Affected / highly.Affected  --->  excluding Diamond Princess
ever.Affected = cases.Active
# Unit scaling
for (i in row.names(ever.Affected)) {
  for (j in 5:ncol(ever.Affected)) {
    if(ever.Affected[i,j] != 0)
      ever.Affected[i,j] = 1
  }
}

highly.Affected = check.Deaths
# Unit scaling
for (i in row.names(highly.Affected)) {
  for (j in 5:ncol(highly.Affected)) {
    if(highly.Affected[i,j] != 0)
      highly.Affected[i,j] = 1
  }
}

## Replacing country Name from Denmark to Greenland for Greeland
target = rbind(ever.Affected[which(str_detect(ever.Affected$Province.State, "Greenland")),], ever.Affected[which(str_detect(ever.Affected$Province.State, "French Guiana")),])
target$Country.Region = target$Province.State
ever.Affected = rbind(ever.Affected[which(str_detect(ever.Affected$Province.State, "Greenland", negate = T)),], ever.Affected[which(str_detect(ever.Affected$Province.State, "French Guiana", negate = T)),], target)

target = rbind(highly.Affected[which(str_detect(highly.Affected$Province.State, "Greenland")),], highly.Affected[which(str_detect(highly.Affected$Province.State, "French Guiana")),])
target$Country.Region = target$Province.State
highly.Affected = rbind(highly.Affected[which(str_detect(highly.Affected$Province.State, "Greenland", negate = T)),], highly.Affected[which(str_detect(highly.Affected$Province.State, "French Guiana", negate = T)),], target)

#View(ever.Affected)
#View(highly.Affected)


# Removing outlier i.e. Hubei
Hubei.Confirmed = check.Confirmed[ which(str_detect(check.Confirmed$Province.State, "Hubei", negate = F)), ]
check.Confirmed = check.Confirmed[ which(str_detect(check.Confirmed$Province.State, "Hubei", negate = T)), ]

Hubei.Deaths = check.Deaths[ which(str_detect(check.Deaths$Province.State, "Hubei", negate = F)),]
check.Deaths = check.Deaths[ which(str_detect(check.Deaths$Province.State, "Hubei", negate = T)), ]

# Hubei.Recovered = check.Recovered[ which(str_detect(check.Recovered$Province.State, "Hubei", negate = F)), ]
# check.Recovered = check.Recovered[ which(str_detect(check.Recovered$Province.State, "Hubei", negate = T)), ]



## Rectifying Row sequences
row.names(check.Confirmed) <- NULL
row.names(check.Deaths) <- NULL
# row.names(check.Recovered) <- NULL

row.names(Diamond.Princess.Confirmed) <- NULL
row.names(Diamond.Princess.Deaths) <- NULL
# row.names(Diamond.Princess.Recovered) <- NULL

row.names(Hubei.Confirmed) <- NULL
row.names(Hubei.Deaths) <- NULL
# row.names(Hubei.Recovered) <- NULL


###


#----------------------------------------------#




# creating new .csv file after cleaning the data

## Diamond Princess
write.csv(Diamond.Princess.Confirmed, file = "cleaned/Diamond-Princess/time_series_19-covid-Confirmed.csv", row.names = FALSE)
write.csv(Diamond.Princess.Deaths, file = "cleaned/Diamond-Princess/time_series_19-covid-Deaths.csv", row.names = FALSE)
# write.csv(Diamond.Princess.Recovered, file = "cleaned/Diamond-Princess/time_series_19-covid-Recovered.csv", row.names = FALSE)

## Hubei
write.csv(Hubei.Confirmed, file = "cleaned/Hubei/time_series_19-covid-Confirmed.csv", row.names = FALSE)
write.csv(Hubei.Deaths, file = "cleaned/Hubei/time_series_19-covid-Deaths.csv", row.names = FALSE)
# write.csv(Hubei.Recovered, file = "cleaned/Hubei/time_series_19-covid-Recovered.csv", row.names = FALSE)


## Main files
write.csv(check.Confirmed, file = "cleaned/time_series_19-covid-Confirmed.csv", row.names = FALSE)
write.csv(check.Deaths, file = "cleaned/time_series_19-covid-Deaths.csv", row.names = FALSE)
# write.csv(check.Recovered, file = "cleaned/time_series_19-covid-Recovered.csv", row.names = FALSE)

###   For map plot & gif
write.csv(ever.Affected, file = "cleaned/ever.Affected.csv", row.names = FALSE)
write.csv(highly.Affected, file = "cleaned/highly.Affected.csv", row.names = FALSE)



#############################################################


cleaned.Confirmed <- read.csv("cleaned/time_series_19-covid-Confirmed.csv")
cleaned.Deaths <- read.csv("cleaned/time_series_19-covid-Deaths.csv")
# cleaned.Recovered <- read.csv("cleaned/time_series_19-covid-Recovered.csv")

cleaned.ever.Affected <- read.csv("cleaned/ever.Affected.csv")
cleaned.highly.Affected <- read.csv("cleaned/highly.Affected.csv")

#str(cleaned.Confirmed)


#View(cleaned.Confirmed)
#View(cleaned.Deaths)
#View(cleaned.Recovered)

#View(cleaned.ever.Affected)
#View(cleaned.highly.Affected)

##########    ENDS     ###########


