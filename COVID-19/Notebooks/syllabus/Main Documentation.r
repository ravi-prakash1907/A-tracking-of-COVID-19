# Setting the working directory
options(warn=-1)
setwd("~/Documents/A-tracking-of-2019-nCoV/COVID-19/")

#####  Loading LIBRARIES  #####
library(stringr)

library(AUCRF)
library(randomForest)
library(RFmarkerDetector)
library(ggplot2)

library(caret)
library(mlbench)
library(kernlab)

# loading raw data
check.Confirmed = read.csv("Notebooks/syllabus/static/raw/time_series_19-covid-Confirmed.csv")
check.Deaths = read.csv("Notebooks/syllabus/static/raw/time_series_19-covid-Deaths.csv")
check.Recovered = read.csv("Notebooks/syllabus/static/raw/time_series_19-covid-Recovered.csv")

# view sample
head(check.Confirmed)
head(check.Deaths)
head(check.Recovered)

# columns
cat("Number of columns in all 3 datasets:-\n\n")

matrix(
    c("Confirmed", "Deaths", "Recovered", ncol(check.Confirmed), ncol(check.Deaths), ncol(check.Recovered)),
    nrow = 2, ncol = 3, byrow = T
)

# Dimention
cat("Dimentions of datasets:-\n")   # same for all 3
dim(check.Confirmed)


######################
# columns' name
#cat("Name of columns in all 3 datasets:-\n")

#colnames(check.Confirmed)
#colnames(check.Deaths)
#colnames(check.Recovered)

str(check.Confirmed)
#str(check.Deaths)
#str(check.Recovered)

# showing data of Hubei
cat("\nA sample data of a location from \"Confirmed Cases\'\" dataset:\n")
check.Confirmed[which(str_detect(check.Confirmed$Province.State, "Hubei")),]

# sample NA values
check.Confirmed[which(str_detect(check.Confirmed$Country.Region, "Cruise Ship")),]

# sample wrong data "french guiana" the data value can not decrease on next day
check.Confirmed[which(str_detect(check.Confirmed$Province.State, "French Guiana")),]

# sample blank data ---> State (to be replaced by 'Others')
head(check.Recovered)

# removing NAs, replacing incorrect values

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





# replace blanks and incorrect country/state names

# replacing in states
states = as.character(check.Confirmed$Province.State)
states.levels = as.character(levels(check.Confirmed$Province.State))

states[states %in% ""] = "Others"
states.levels[states.levels %in% ""] = "Others"


#######
states[states %in% "From Diamond Princess"] = "Diamond Princess"
states.levels = states.levels[!states.levels %in% "From Diamond Princess"]


# replacing in countries
countries = as.character(check.Confirmed$Country.Region)
countries.levels = as.character(levels(check.Confirmed$Country.Region))

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
###############################

# rectified fectors
states.factor  = factor(c(states), levels = c(states.levels))
countries.factor  = factor(countries, levels = countries.levels)


## CUZ' INITIAL 4 COLUMNS ARE COMMON IN ALL 3 DATASETS ##

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

check.Recovered = cbind(
                    Province.State = states.factor,
                    Country.Region = countries.factor,
                    check.Recovered[,3:ncol(check.Recovered)]
                  )



# sample NA val
check.Confirmed[which(str_detect(check.Confirmed$Country.Region, "Cruise Ship")),]

# sample wrong data "french guiana"
check.Confirmed[which(str_detect(check.Confirmed$Province.State, "French Guiana")),]

# sample blank data ---> State (replaced by 'Other')
head(check.Recovered)

# total cases of different catagories on the daily basis
dailyConfirmed = apply(check.Confirmed[,5:ncol(check.Confirmed)], 2, sum)
dailyDeaths = apply(check.Deaths[,5:ncol(check.Deaths)], 2, sum)
dailyRecovered = apply(check.Recovered[,5:ncol(check.Recovered)], 2, sum)

# active cases
dailyActiveCases = dailyConfirmed - (dailyDeaths + dailyRecovered)

# day count till March 21st
days = 1:length(dailyConfirmed)

trendDF = data.frame(
                Day = days,
                Confirmed = dailyConfirmed,
                Deaths = dailyDeaths,
                Recovered = dailyRecovered,
                Active.Cases = dailyActiveCases)
head(trendDF, 5)

options(repr.plot.width=12, repr.plot.height=10)

ggplot(trendDF, aes(x = Day, y = Active.Cases)) +
    # Active cases in LIGHTBLUE
    geom_point(color = "lightblue") +
    geom_line(color = "lightblue") +

    # Confirmed Cases - RED
    geom_point(aes(y = Confirmed), color = "red", size=3) +
    geom_line(aes(y = Confirmed), color = "red", size=1) +
    # Death Cases - BLACK
    geom_point(aes(y = Deaths), color = "black", size=3) +
    geom_line(aes(y = Deaths), color = "black", size=1) +
    # Recovered Cases - GREEN
    geom_point(aes(y = Recovered), color = "green", size=3) +
    geom_line(aes(y = Recovered), color = "green", size=1) +

    labs(x="Days Count", y="Cases Count") +
    theme(
              text = element_text(family = "Gill Sans")
              ,plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
              ,plot.subtitle = element_text(size = 25, family = "Courier", face = "bold", hjust = 0.5)
              ,axis.text = element_text(size = 12)
              ,axis.title = element_text(size = 20)
      )


allConfirmed = as.numeric(check.Confirmed[,ncol(check.Confirmed)])
allDeaths = as.numeric(check.Deaths[,ncol(check.Deaths)])
allRecoveries = as.numeric(check.Recovered[,ncol(check.Recovered)])

# finding the mean of each catagories
meanConfirmed = mean(allConfirmed)
meanDeaths = mean(allDeaths)
meanRecovered = mean(allRecoveries)

meanConfirmed
meanDeaths
meanRecovered

# once for Confirmed Cases
lambda = 1/meanConfirmed
lambda

ggplot(trendDF, aes(x = Day, y = Active.Cases)) +
    # Active cases in BLUE
    geom_point(color = "blue") +
    geom_line(color = "blue")

# removing diamond princess
Diamond.Princess.Confirmed = check.Confirmed[ which(str_detect(check.Confirmed$Country.Region, "Cruise Ship", negate = F)), ]
check.Confirmed = check.Confirmed[ which(str_detect(check.Confirmed$Country.Region, "Cruise Ship", negate = T)), ]

Diamond.Princess.Deaths = check.Deaths[ which(str_detect(check.Deaths$Country.Region, "Cruise Ship", negate = F)),]
check.Deaths = check.Deaths[ which(str_detect(check.Deaths$Country.Region, "Cruise Ship", negate = T)), ]

Diamond.Princess.Recovered = check.Recovered[ which(str_detect(check.Recovered$Country.Region, "Cruise Ship", negate = F)), ]
check.Recovered = check.Recovered[ which(str_detect(check.Recovered$Country.Region, "Cruise Ship", negate = T)), ]

## Rectifying Row sequences
row.names(check.Confirmed) <- NULL
row.names(check.Deaths) <- NULL
row.names(check.Recovered) <- NULL

# Let's check whether Diamond Princess is still at row 166 or not
check.Confirmed[166,]
#check.Deaths[166,]
#check.Recovered[166,]


# also checking dimention
cat("\nEarlier dimention: 468 X 62\n\n")    # as we saw initially

cat("New dimention: ", dim(check.Confirmed))


# in UNIT SCALING, all the data has either 0 or 1 value

ever.Affected = check.Confirmed

# Unit scaling
for (i in row.names(ever.Affected)) {
  for (j in 5:ncol(ever.Affected)) {
    if(ever.Affected[i,j] != 0)
      ever.Affected[i,j] = 1
  }
}

head(ever.Affected)


# Let's visualize our data to varify:
library(ggplot2)

options(repr.plot.width=14, repr.plot.height=8)
ggplot(check.Confirmed) +
  geom_point(aes(x=check.Confirmed$Province.State, y=check.Confirmed$X1.29.20), color="red", size=2) +
  theme(
          text = element_text(family = "Gill Sans")
          ,plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
          ,plot.subtitle = element_text(size = 25, family = "Courier", face = "bold", hjust = 0.5)
          ,axis.text = element_text(size = 12)
          ,axis.title = element_text(size = 20)
          ,axis.text.x = element_blank()
  )

cat("\n\n")

#ggplot(check.Deaths) +
#  geom_point(aes(x=check.Deaths$Province.State, y=check.Deaths$X1.29.20), color="red", size=2)#

#ggplot(check.Recovered) +
#  geom_point(aes(x=check.Recovered$Province.State, y=check.Recovered$X1.29.20), color="red", size=2)
  

# Let's find the name of this outlier:-

check.Confirmed[which(check.Confirmed$X1.29.20 > 400), c("Province.State", "Country.Region")]
#check.Deaths[which(check.Deaths$X1.29.20 > 15), c("Province.State", "Country.Region")]
#check.Recovered[which(check.Recovered$X1.29.20 > 20), c("Province.State", "Country.Region")]

# Here we are trying to compare the mean values of everyday, including and excluding Hubei province
With.Hubei = as.numeric(apply(check.Confirmed[,5:ncol(check.Confirmed)], 2, mean))

exceptHubei = check.Confirmed[ which(str_detect(check.Confirmed$Province.State, "Hubei", negate = T)), ]
Without.Hubei = as.numeric(apply(exceptHubei[,5:ncol(exceptHubei)], 2, mean))

# creating a dataframe for comperision
Mean.Comparision.Table = data.frame(
              "Date" = as.character(colnames(check.Confirmed)[5:ncol(check.Confirmed)]),
              "With Hubei" = c(With.Hubei),
              "Without Hubei" = c(Without.Hubei))

tail(Mean.Comparision.Table, 10)

# let's remove Hubei from our dataset:
Hubei.Confirmed = check.Confirmed[ which(str_detect(check.Confirmed$Province.State, "Hubei", negate = F)), ]
check.Confirmed = check.Confirmed[ which(str_detect(check.Confirmed$Province.State, "Hubei", negate = T)), ]

Hubei.Deaths = check.Deaths[ which(str_detect(check.Deaths$Province.State, "Hubei", negate = F)),]
check.Deaths = check.Deaths[ which(str_detect(check.Deaths$Province.State, "Hubei", negate = T)), ]

Hubei.Recovered = check.Recovered[ which(str_detect(check.Recovered$Province.State, "Hubei", negate = F)), ]
check.Recovered = check.Recovered[ which(str_detect(check.Recovered$Province.State, "Hubei", negate = T)), ]


## Rectifying Row sequences
row.names(check.Confirmed) <- NULL
row.names(check.Deaths) <- NULL
row.names(check.Recovered) <- NULL

Hubei.Confirmed

# Let's check the once dimention more

# also checking dimention
cat("\nEarlier dimention: 467 X 62\n\n")    # after removing Cruis Ship

cat("New dimention: ", dim(check.Confirmed))

# Let's visualize once more
library(ggplot2)

options(repr.plot.width=14, repr.plot.height=8)
ggplot(check.Confirmed) +
  geom_point(aes(x=check.Confirmed$Province.State, y=check.Confirmed$X1.29.20), color="red", size=2) +
  theme(
          text = element_text(family = "Gill Sans")
          ,plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
          ,plot.subtitle = element_text(size = 25, family = "Courier", face = "bold", hjust = 0.5)
          ,axis.text = element_text(size = 12)
          ,axis.title = element_text(size = 20)
          ,axis.text.x = element_blank()
  )

cat("\n\n")


# Let's find them out, too:-
check.Confirmed[which(check.Confirmed$X1.29.20 > 100), c("Province.State", "Country.Region")]

# Checking for mean comperision
With.China = as.numeric(apply(check.Confirmed[,5:ncol(check.Confirmed)], 2, mean))

exceptChina = check.Confirmed[ which(str_detect(check.Confirmed$Country.Region, "China", negate = T)), ]
Without.China = as.numeric(apply(exceptChina[,5:ncol(exceptChina)], 2, mean))

# comperision
Mean.Comparision.Table = data.frame(
              "Date" = as.character(colnames(check.Confirmed)[5:ncol(check.Confirmed)]),
              "With China" = c(With.China),
              "Without China" = c(Without.China))

head(Mean.Comparision.Table, 10)

# We've already saved the cleaned version of the all the files
# Loading the files in order to transform the dataset(s)

# loading raw data - from source
Confirmed = read.csv("Notebooks/syllabus/static/cleaned/time_series_19-covid-Confirmed.csv")
Deaths = read.csv("Notebooks/syllabus/static/cleaned/time_series_19-covid-Deaths.csv")
Recovered = read.csv("Notebooks/syllabus/static/cleaned/time_series_19-covid-Recovered.csv")

Hubei.Confirmed = read.csv("Notebooks/syllabus/static/cleaned/Hubei/time_series_19-covid-Confirmed.csv")
Hubei.Deaths = read.csv("Notebooks/syllabus/static/cleaned/Hubei/time_series_19-covid-Deaths.csv")
Hubei.Recovered = read.csv("Notebooks/syllabus/static/cleaned/Hubei/time_series_19-covid-Recovered.csv")

Diamond.Princess.Confirmed = read.csv("Notebooks/syllabus/static/cleaned/Diamond-Princess/time_series_19-covid-Confirmed.csv")
Diamond.Princess.Deaths = read.csv("Notebooks/syllabus/static/cleaned/Diamond-Princess/time_series_19-covid-Deaths.csv")
Diamond.Princess.Recovered = read.csv("Notebooks/syllabus/static/cleaned/Diamond-Princess/time_series_19-covid-Recovered.csv")

# as known, all of these files have same set of columns,
# the only things that differ are data values in dates' columns

# Let's see any one dataset's structure (as all are similer)
str(Hubei.Recovered)

# We need Countries' data:

# It's because: many states have very few cases
tail(Confirmed)

# Most of the states' name is not identified
unknown = nrow(Recovered[which(str_detect(Recovered$Province.State, "Others")),])
cat(unknown, "/", nrow(Recovered), " States are NOT identified")

# Ultimatly, any precaution/cure or action is more likely be taken onto the country level, rather than the individual state, as it's the case of a severe Epidemic
# Only then it would be much easier for us to make any possible estimate for the world as well, due to not having really a huge data about each and every single state of the countries.

Countries = levels(Confirmed$Country.Region)

cat("\nTotal number of affected countries: ", nlevels(Confirmed$Country.Region), "\n\n\nCountries:")
head(as.matrix(Countries), 5) # top 5 countries (in sorted list-namewise)

## Functions for extracting required data


# finds the total cases reported in given country 
    # (by Adding all the data of different states in it)
country.aggregate.daily  <-  function(dfName, country) {
  
  df <- get(dfName)
  df = df[which(str_detect(df$Country.Region, country)),]
  df = cbind(States = df[,1], Country = df[,2], df[,5:ncol(df)])     # ELEMINATING LATITUDE/LONGITUDE Col.
  
  row.names(df) <- NULL    
    
  temp = df                                             # all states' data of a country
  df = temp[1,] 
  
  df[3:ncol(temp)] = apply(   temp[,3:ncol(temp)],
                            2,
                            sum
                        )                               # applying sum of all the states' values
  df = df[2:ncol(df)]                                   # removing column 'States'  
  row.names(df) <- NULL  
  return(df)
}



# generated a dataframe having required data arranged Country-Wise 
    # (by appending every single country's data)
countries.daily <-  function(dfName, cList) {
  
  n = length(cList)       # number of countries
  
  flag = 0
  
  for (i in cList) {
    
    if(flag == 0) {
      df = country.aggregate.daily(dfName, i)
      flag = 1
    } else {
      temp = country.aggregate.daily(dfName, i)
      df = rbind(df, temp)
    }    
  }
  
  row.names(df) <- NULL  
  return(df)
}

China.Confirmed = country.aggregate.daily("Confirmed", "China")
World.Confirmed = countries.daily("Confirmed", Countries)

China.Confirmed
cat("\n\n")
head(World.Confirmed)

China.Deaths = country.aggregate.daily("Deaths", "China")
World.Deaths = countries.daily("Deaths", Countries)
China.Recovered = country.aggregate.daily("Recovered", "China")
World.Recovered = countries.daily("Recovered", Countries)

## Functions

countries.daily.bulk.summary = function(cList) { # date wise country data
  
  # structure of resulting dataset (initially blank)
  df <- data.frame(
    Country = NULL,
    Day = NULL,           # day no.
    Date = NULL,
    Confirmed = NULL,
    Deaths = NULL,
    Recovered = NULL
  )
  
  # calculating all countries' data (date wise) through iteration
  for(i in cList) {
    this.one.confirmed = country.aggregate.daily("Confirmed", i)
    this.one.deaths = country.aggregate.daily("Deaths", i)
    this.one.recovered = country.aggregate.daily("Recovered", i)
    
    times = ncol(this.one.confirmed)-1      # no. of days
    day = 1:times
    d = as.Date("21-01-2020", format(c("%d-%m-%Y")))
    
    date = as.character((day + d), format(c("%d-%m-%Y")))      # its lenngth is equal to --> no. of days
    date = factor(c(date), levels = date)
    
    #max(Deaths.temp[1,5:ncol(Deaths.temp)])
    confirmed = as.numeric(this.one.confirmed[1,2:ncol(this.one.confirmed)])
    
    deaths = as.numeric(this.one.deaths[1,2:ncol(this.one.deaths)])
    
    recovered = as.numeric(this.one.recovered[1,2:ncol(this.one.recovered)])
    
    dataset <- data.frame(
      Country = rep(i, times),
      Day = factor(c(1:length(date)), levels = 1:length(date)),
      Date = date,
      Confirmed = confirmed,
      Deaths = deaths,
      Recovered = recovered
    )
    
    # joining this country
    df = rbind(df, dataset)
  }
    
  return(df)
}


bulk = countries.daily.bulk.summary(Countries)
head(bulk)

bulk$Active.Cases = bulk$Confirmed - (bulk$Deaths + bulk$Recovered)
bulk$Closed.Cases = bulk$Deaths + bulk$Recovered
tail(bulk)

# Analysing the Pooled data
str(bulk)

# filtering out the China
China.dataset = bulk[which(str_detect(bulk$Country, 'China')),]

# World Pooled dataset (except china)
bulk = bulk[which(str_detect(bulk$Country, 'China', negate=T)),] # updating bulk itself

head(China.dataset)

## Load both datewise-datasets (world & FOUR)
# includes data of all the countries
all = read.csv('Notebooks/syllabus/static/pooled/countryWise_bulk_summary.csv')

# includes data of four majour location
four = read.csv('Notebooks/syllabus/static/pooled/Four_dataset_locationWise.csv')

str(all)

cat("\n\n")

str(four)

# Initially we plot dataset with majour Locations
options(repr.plot.width=16, repr.plot.height=8)
withChina<-ggplot(four, aes(x=Day, y=Confirmed, color=Day)) +
  geom_boxplot(aes(group=Day)) +
  labs(title="Including China") +
  theme_classic() +
  theme(
          text = element_text(family = "Gill Sans")
          ,plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
          ,plot.subtitle = element_text(size = 25, family = "Courier", face = "bold", hjust = 0.5)
          ,axis.text = element_text(size = 12)
          ,axis.title = element_text(size = 20)
  )

cat("\n\n")
withChina

options(repr.plot.width=16, repr.plot.height=8)

chinaAlone <- ggplot(four[which(str_detect(four$Location, "China", negate=F)),], aes(x=Day, y=Confirmed, color=Day)) +
  geom_point(aes(group=Day)) +
  labs(title="Only China") +
  theme_classic() +
  theme(
          text = element_text(family = "Gill Sans")
          ,plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
          ,plot.subtitle = element_text(size = 25, family = "Courier", face = "bold", hjust = 0.5)
          ,axis.text = element_text(size = 12)
          ,axis.title = element_text(size = 20)
  )

withoutChina <- ggplot(four[which(str_detect(four$Location, "China", negate=T)),], aes(x=Day, y=Confirmed, color=Day)) +
  geom_boxplot(aes(group=Day)) +
  labs(title="Excluding China") +
  theme_classic() +
  theme(
          text = element_text(family = "Gill Sans")
          ,plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
          ,plot.subtitle = element_text(size = 25, family = "Courier", face = "bold", hjust = 0.5)
          ,axis.text = element_text(size = 12)
          ,axis.title = element_text(size = 20)
  )

cat("\n\n")
chinaAlone
cat("\n\n")
withoutChina

# Let's view few of the rows in existing datasets
head(all)
head(four)

# calculate the percent (using Confirmed cases as total)
percent <- function(dfName){
    get(dfName) -> df
    part <- NULL
    
    for(i in 1:nrow(df)) {
        val = df[i,"Active.Cases"]
        Total = df[i,"Confirmed"]
        
        
        if(i == 1)
            if(val==0)
                part = 0
            else
                part = as.numeric((val*100)/Total)
        else
            if(val==0)
                part = c(part, 0)
            else
                part <- c(part, as.numeric((val*100)/Total))
    }
        
    return(part)
}

# CASES -> percentage
four$'percent_active' = percent("four")     # Active cases, out of every 100 Confirmed cases
four$'percent_closed' = 100-percent("four") # Closed cases, out of every 100 Confirmed cases


all$'percent_active' = percent("all")     # Active cases, out of every 100 Confirmed cases
all$'percent_closed' = 100-percent("all") # Closed cases, out of every 100 Confirmed cases


# Look onto the structure whether the things are updated or not
str(all)

cat("\n\n")

str(four)


# extracting the desired dataset
extractDatases <- function(region){
    if(region %in% c("Hubei", "World", "Diamond Princess")) {
    temp = four[which(str_detect(four$Location, region)),]
    row.names(temp) <- NULL
} else {
    temp = all[which(str_detect(all$Country, region)),]
    row.names(temp) <- NULL
}

return(temp)
}


# country i.e. to be used throughout the analysis
rName = "China" # without hubei

# filtering out desired country/location 
region1 = extractDatases(rName)

# so we are missing something, when we have outliers saperatly,
    # better is that we join Hubei data in China so that our Countries' dataset don't have any vulnarability

# joining Hubei for complete data of china
region2 = extractDatases("Hubei")
region = cbind(region1[,1:3], region1[,4:8]+region2[,4:8])

# filtering out desired country/location 

region$'percent_active' = percent("region")     # Active cases, out of every 100 Confirmed cases
region$'percent_closed' = 100-percent("region") # Closed cases, out of every 100 Confirmed cases

head(region)

region=region[,c(-1, -3)]
head(region, 10)

# setting the theme
theme_set(theme_classic())
# setting plot size
options(repr.plot.width=8, repr.plot.height=8)

## REGRASSIONs

# converting from Factor
region$Day <- as.numeric(as.character(region$Day))

x <- as.matrix(region[,1:6, 8])
y <- as.matrix(region[,7])

end = "\n\n#############################################################\n\n\n"

cat("\n\nLinear Regression:\n------------------\n")
# fit model
fit <- lm(percent_active~., region)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, region)
# summarize accuracy
mse <- sqrt(mean((region$percent_active - predictions)^2))
cat("RMSE: ", mse)



cat(end, "k-Nearest Neighbors:\n--------------------\n")
# load the libraries
# fit model
fit <- knnreg(x, y, k=3)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, x)
# summarize accuracy
mse <- sqrt(mean((y - predictions)^2))
cat("RMSE: ", mse)



cat(end, "Support Vector Machine:\n-----------------------\n")
# fit model
fit <- ksvm(percent_active~., region, kernel="rbfdot")
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, region)
# summarize accuracy
mse <- sqrt(mean((y - predictions)^2))
cat("RMSE: ", mse)




#############################################################

# Visualizing the available data as a scatter plot to see how the Active Cases(%) in China has varied over days, since 22nd January, 2020

# Day vs Active Cases(%)
region.scatter.plot <- ggplot(region, aes(x = region$Day, y = region$percent_active)) +
                        geom_point() +
                        labs( x = "Days", y = "Active Cases (%)") +
                        theme(
                              text = element_text(family = "Gill Sans")
                              ,plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
                              ,plot.subtitle = element_text(size = 25, family = "Courier", face = "bold", hjust = 0.5)
                              ,axis.text = element_text(size = 12)
                              ,axis.title = element_text(size = 20)
                              )
              
region.scatter.plot

set.seed(20) # generages same set of random sample every time

training.samples <- region$Day %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- region[training.samples, ]
test.data <- region[-training.samples, ]

# Dimentions of the splitted datasets
dim(train.data)
dim(test.data)

head(train.data, 3)
head(test.data, 3)

# model
fit.svmk <- ksvm(percent_active~Day, train.data, kernel="rbfdot")
#summary(fit.svmk)

predictions <- fit.svmk %>% predict(train.data)

# Model performance
svm.predictions <- data.frame(
  RMSE = RMSE(predictions, train.data$Day),
  R2 = R2(predictions, train.data$Day)
)

svmk.trained = cbind( # Prediction for training data
            train.data[,c("Day", "percent_active")],
            Pridicted_percent_active = predict(fit.svmk, train.data)
          )

svmk.tested = cbind(  # Prediction for tested data
            test.data[,c("Day", "percent_active")],
            Pridicted_percent_active = predict(fit.svmk, test.data)
         )


tail(svmk.trained, 5)
tail(svmk.tested, 5)

# Visualizations

svmk.trainer <- ggplot(train.data, aes(Day, percent_active) ) +
              geom_point() +
              geom_smooth(data=svmk.trained, method="loess", size=0) +
              geom_line(data = svmk.trained, aes(Day, Pridicted_percent_active), color="#3366fe", size=1) +   # polynomial function
  
              # decoration
              labs( x = "Days", y = "Active Cases (%)", title = paste("\nTraining plot", rName, sep = " - ") ) +
              theme( plot.title = element_text(size = 20, face = "bold")) 

svmk.tester <- ggplot(test.data, aes(Day, percent_active) ) +
              geom_point() +
              geom_smooth(data=svmk.tested, method="loess", size=0) +
              geom_line(data = svmk.tested, aes(Day, Pridicted_percent_active), color="#3366fe", size=1) +   # polynomial function
  
              # decoration
              labs( x = "Days", y = "Active Cases (%)", title = paste("\nTesting plot", rName, sep = " - ") ) +
              theme( plot.title = element_text(size = 20, face = "bold")) 



#  Plotting
svmk.trainer
svmk.tester

x <- as.matrix(train.data[,1])
y <- as.matrix(train.data[,7])

# fit model
fit.knn <- knnreg(x, y, k=3)
#summary(fit.knn)

predictions <- fit.knn %>% predict(x)

knn.predictions <- data.frame(
  RMSE = RMSE(predictions, x),
  R2 = R2(predictions, x)
)

knn.trained = cbind( # Prediction for training data
            train.data[,c("Day", "percent_active")],
            Pridicted_percent_active = predict(fit.knn, as.matrix(train.data[,1]))
          )

knn.tested = cbind(  # Prediction for tested data
            test.data[,c("Day", "percent_active")],
            Pridicted_percent_active = predict(fit.knn, as.matrix(test.data[,1]))
         )


tail(knn.trained, 5)
tail(knn.tested, 5)

# Visualizations
knn.trainer <- ggplot(train.data, aes(Day, percent_active) ) +
              geom_point() +
              geom_smooth(data=knn.tested, method="loess", size=0) +
              geom_line(data = knn.trained, aes(Day, Pridicted_percent_active), color="#3366fe", size=1) +   # polynomial function
  
              # decoration
              labs( x = "Days", y = "Active Cases (%)", title = paste("\nTraining plot", rName, sep = " - ") ) +
              theme( plot.title = element_text(size = 20, face = "bold")) 

knn.tester <- ggplot(test.data, aes(Day, percent_active) ) +
              geom_point() +
              geom_smooth(data=knn.tested, method="loess", size=0) +
              geom_line(data = knn.tested, aes(Day, Pridicted_percent_active), color="#3366fe", size=1) +   # polynomial function
  
              # decoration
              labs( x = "Days", y = "Active Cases (%)", title = paste("\nTesting plot", rName, sep = " - ") ) +
              theme( plot.title = element_text(size = 20, face = "bold")) 



# Plotting
knn.trainer
knn.tester

# building linear model
fit.lm = lm(Day ~ percent_active, data = train.data)
#summary(fit.lm)

# Predicting
predictions <- fit.lm %>% predict(train.data)

# Model performance
lm.predictions <- data.frame(
  RMSE = RMSE(predictions, train.data$Day),
  R2 = R2(predictions, train.data$Day)
)

lm.trained = cbind( # Prediction for training data
            train.data[,c("Day", "percent_active")],
            Pridicted_percent_active = predict(fit.lm, train.data)
          )

lm.tested = cbind(  # Prediction for tested data
            test.data[,c("Day", "percent_active")],
            Pridicted_percent_active = predict(fit.lm, test.data)
         )


tail(lm.trained, 5)
tail(lm.tested, 5)

# Visualizations
lm.trainer <- ggplot(train.data, aes(Day, percent_active) ) +
              geom_point() +
              stat_smooth(method = lm, formula = y ~ x) +   # linear function
  
              # decoration
              labs( x = "Days", y = "Active Cases (%)", title = paste("\nTraining plot", rName, sep = " - ") ) +
              theme( plot.title = element_text(size = 20, face = "bold")) 

lm.tester <- ggplot(test.data, aes(Day, percent_active) ) +
              geom_point() +
              stat_smooth(method = lm, formula = y ~ x) +   # linear function
  
              # decoration
              labs( x = "Days", y = "Active Cases (%)", title = paste("\nTesting plot", rName, sep = " - ") ) +
              theme( plot.title = element_text(size = 20, face = "bold")) 

# Plotting
lm.trainer
lm.tester

# Model performance
plm.predictions = data.frame(
  Degree = NULL,
  RMSE = NULL,
  RSE = NULL,
  R2 = NULL
)


for(deg in 1:20){
    
    # building polynomial model
    fit.plm = lm(percent_active ~ poly(Day, deg, raw = TRUE), data = train.data)
    #summary(fit.plm)

    
    #Residual Standard error (Like Standard Deviation)
    k=length(fit.plm$coefficients)-1
    #Multiple R-Squared (Coefficient of Determination)
    SSyy=sum((train.data$percent_active-mean(train.data$percent_active))**2)
    
    SSE=sum(fit.plm$residuals**2)
    n=length(fit.plm$residuals)
    
    
    # final
    rmse = sqrt(SSE/(n-1))
    rse = sqrt(SSE/(n-(1+k))) #Residual Standard Error
    r2 = (SSyy-SSE)/SSyy
    
    temp <- data.frame(
                        Degree = deg,
                        RMSE = rmse,
                        RSE = rse,
                        R2 = r2
                      )
    
    plm.predictions = rbind(plm.predictions, temp)
}

plm.predictions

deg = 16

# building polynomial model
fit.plm = lm(percent_active ~ poly(Day, deg, raw = TRUE), data = train.data)
#summary(fit.plm)

plm.trained = cbind( # Prediction for training data
            train.data[,c("Day", "percent_active")],
            Pridicted_percent_active = predict(fit.plm, train.data)  # predicting the values
          )

plm.tested = cbind(  # Prediction for tested data
            test.data[,c("Day", "percent_active")],
            Pridicted_percent_active = predict(fit.plm, test.data)   # predicting the values
         )


tail(plm.trained, 5)
tail(plm.tested, 5)

# Visualizations of the predictions for Training as well as Testing datasets to see fit
plm.trainer <- ggplot(train.data, aes(Day, percent_active) ) +
              geom_point() +
              stat_smooth(method = lm, formula = y ~ poly(x, deg, raw = TRUE)) +   # polynomial function
  
              # decoration
              labs( x = "Days", y = "Active Cases (%)", title = paste("\nTraining plot", rName, sep = " - ") ) +
              theme( plot.title = element_text(size = 20, face = "bold")) 

plm.tester <- ggplot(test.data, aes(Day, percent_active) ) +
              geom_point() +
              stat_smooth(method = lm, formula = y ~ poly(x, deg, raw = TRUE)) +   # polynomial function
  
              # decoration
              labs( x = "Days", y = "Active Cases (%)", title = paste("\nTesting plot", rName, sep = " - ") ) +
              theme( plot.title = element_text(size = 20, face = "bold")) 


# Plotting
plm.trainer
plm.tester

# Available data for 59th day (i.e. 20-03-2020)
temp.test = data.frame(
                        Day = 59:61,
                        Confirmed = c(81251, NaN, NaN),
                        Deaths = c(3253, NaN, NaN),
                        Recovered = c(71266, NaN, NaN),
                        Active.Cases = c(6732, NaN, NaN),
                        Closed.Cases = c(74519, NaN, NaN),
                        percent_active =  c(8.285436, NaN, NaN),
                        percent_closed = c(91.7145, NaN, NaN)
                     )

temp.test

# Now, we'll find the Active case(%) from all the four algorithms
temp.required <- temp.test[,c(1, 7)]
temp.required$"Polynomial Model" <- predict(fit.plm, temp.test)

temp.required

plm.predictions[which(plm.predictions$Degree == 11),]

deg = 11

# building polynomial model
fit.plm = lm(percent_active ~ poly(Day, deg, raw = TRUE), data = train.data)
#summary(fit.plm)

########################

## Prediction table
plm.trained = cbind( # Prediction for training data
            train.data[,c("Day", "percent_active")],
            Pridicted_percent_active = predict(fit.plm, train.data)  # predicting the values
          )

plm.tested = cbind(  # Prediction for tested data
            test.data[,c("Day", "percent_active")],
            Pridicted_percent_active = predict(fit.plm, test.data)   # predicting the values
         )


########################

# Graphs for prediction
plm.trainer <- ggplot(train.data, aes(Day, percent_active) ) +
              geom_point() +
              stat_smooth(method = lm, formula = y ~ poly(x, deg, raw = TRUE)) +   # polynomial function
  
              # decoration
              labs( x = "Days", y = "Active Cases (%)", title = paste("\nTraining plot", rName, sep = " - ") ) +
              theme( plot.title = element_text(size = 20, face = "bold")) 

plm.tester <- ggplot(test.data, aes(Day, percent_active) ) +
              geom_point() +
              stat_smooth(method = lm, formula = y ~ poly(x, deg, raw = TRUE)) +   # polynomial function
  
              # decoration
              labs( x = "Days", y = "Active Cases (%)", title = paste("\nTesting plot", rName, sep = " - ") ) +
              theme( plot.title = element_text(size = 20, face = "bold")) 


# Prediction table at degree = 6
tail(plm.trained, 5)
tail(plm.tested, 5)

# New visualization for prediction
plm.trainer
plm.tester

temp.required$"Polynomial Model" <- predict(fit.plm, temp.test)
temp.required

# Let's evaluate and compare the 4 model algorithms

# Performance (Accurecy) table
Accuracy.Table = data.frame(
                    Algo = c("Support Vector Machine Regression",
                             "k-Nearest Neighbour Regression",
                             "Linear Regression",
                             "Polynomial Regression"),
                    RMSE = c(svm.predictions$RMSE[1],
                             knn.predictions$RMSE[1],
                             lm.predictions$RMSE[1],
                             plm.predictions[deg, 'RMSE']),
                    R2 = c(svm.predictions$R2[1],
                             knn.predictions$R2[1],
                             lm.predictions$R2[1],
                             plm.predictions[deg, 'R2'])
                 )

Accuracy.Table

# Creating a comparision table to compare the predicted values by all four models
Prediction = cbind(
                "Day" = test.data$Day,
                "Actual Active Case(%)" = test.data$percent_active,
                "Predicted by SVMK" = svmk.tested$Pridicted_percent_active,
                "Predicted by kNN" = knn.tested$Pridicted_percent_active,
                "Predicted by LM" = lm.tested$Pridicted_percent_active,
                "Predicted by Poly LM" = plm.tested$Pridicted_percent_active
             )
Prediction

# Appending 59th day's data to our training dataset
train.data = rbind(train.data, temp.test[1,])            # RUN ONCE!!

# Renaming row sequence
row.names(train.data) <- NULL
tail(train.data)

# training once more on updated training dataset
fit.plm = lm(percent_active ~ poly(Day, deg, raw = TRUE), data = train.data)

# At this point, we can clearly see that we should one among Polynomial Regession and KNN algo.
# But, we also know that Polynomial Regression better fits into trend as compared to KNN, as per the visualization, above.
# So, we find that Polynomial regression is best

# Hence, we'll be choosing Polynomial Regression with Degree = 16

# now we are ready to go for the last step in our COVID-19 analysis i.e. Deployment & hence, to get the status of China, in few of the upcoming days:

# Summary of trained model
summary(fit.plm)

# Accurecy
plm.predictions[which(plm.predictions$Degree == deg),]

# Preparing our dataset to store next 7 days' estimate

d = as.Date("21/01/2020", format(c("%d/%m/%Y")))
date = as.character((c(1:8) + d), format(c("%d/%m/%Y")))

finalEstimate <- data.frame(
                             Day = 59:66,
                             Date = date
                           )


finalEstimate$"Estimated Active Case(%)" = predict(fit.plm, finalEstimate)

finalEstimate
