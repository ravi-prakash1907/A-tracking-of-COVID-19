# 22/01/2020 to yesterday

# Setting the working directory
setwd("~/Documents/A-tracking-of-2019-nCoV/COVID-19/")

#####  LIBRARIES  #####
# loading library for string operations
library(stringr)
library(RFmarkerDetector) # random forest  ---> for autoscale()


## replace new time series files first, then run following command -----> 'n your dataset is updated
check.Confirmed = read.csv("Johns H. University/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", header = TRUE)
check.Deaths = read.csv("Johns H. University/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
check.Recovered = read.csv("Johns H. University/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

# removing NAs

# new files
for (i in 1:nrow(check.Confirmed)) {
  for (j in 5:ncol(check.Confirmed)) {
    if(j==5) {
      check.Confirmed[i,j] = ifelse(is.na(check.Confirmed[i, j]), 0, check.Confirmed[i,j])
    } else {
      check.Confirmed[i,j] = ifelse(is.na(check.Confirmed[i, j]), check.Confirmed[i, (j-1)], check.Confirmed[i,j])
    }
  }
}

for (i in 1:nrow(check.Deaths)) {
  for (j in 5:ncol(check.Deaths)) {
    if(j==5) {
      check.Deaths[i,j] = ifelse(is.na(check.Deaths[i, j]), 0, check.Deaths[i,j])
    } else {
      check.Deaths[i,j] = ifelse(is.na(check.Deaths[i, j]), check.Deaths[i, (j-1)], check.Deaths[i,j])
    }
  }
}

for (i in 1:nrow(check.Recovered)) {
  for (j in 5:ncol(check.Recovered)) {
    if(j==5) {
      check.Recovered[i,j] = ifelse(is.na(check.Recovered[i, j]), 0, check.Recovered[i,j])
    } else {
      check.Recovered[i,j] = ifelse(is.na(check.Recovered[i, j]), check.Recovered[i, (j-1)], check.Recovered[i,j])
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
countries[countries %in% "UK"] = "United Kingdom"
countries.levels[countries.levels %in% "US"] = "United States"
countries.levels[countries.levels %in% "UK"] = "United Kingdom"

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

check.Recovered = cbind(
                    Province.State = states.factor,
                    Country.Region = countries.factor,
                    check.Recovered[,3:ncol(check.Recovered)]
                  )






#View(check.Confirmed)
#View(check.Deaths)
#View(check.Recovered)





###############################
#str(check.Confirmed)

## Closed cases (i.e. Recovered or Death cases)
cases.Closed = cbind(check.Confirmed[,1:4],  (check.Deaths[,5:ncol(check.Deaths)] + check.Recovered[,5:ncol(check.Recovered)]))
## Active cases 
cases.Active = cbind(check.Confirmed[,1:4],  (check.Confirmed[,5:ncol(check.Confirmed)] - cases.Closed[,5:ncol(cases.Closed)]))


# Removing outlier i.e. Diamond.Princess
Diamond.Princess.Confirmed = check.Confirmed[ which(str_detect(check.Confirmed$Province.State, "Diamond Princess", negate = F)), ]
check.Confirmed = check.Confirmed[ which(str_detect(check.Confirmed$Province.State, "Diamond Princess", negate = T)), ]

Diamond.Princess.Deaths = check.Deaths[ which(str_detect(check.Deaths$Province.State, "Diamond Princess", negate = F)),]
check.Deaths = check.Deaths[ which(str_detect(check.Deaths$Province.State, "Diamond Princess", negate = T)), ]

Diamond.Princess.Recovered = check.Recovered[ which(str_detect(check.Recovered$Province.State, "Diamond Princess", negate = F)), ]
check.Recovered = check.Recovered[ which(str_detect(check.Recovered$Province.State, "Diamond Princess", negate = T)), ]


## Rectifying Row sequences
row.names(check.Confirmed) <- NULL
row.names(check.Deaths) <- NULL
row.names(check.Recovered) <- NULL


###  When and Where COVID-19 ever.Affected / still.Affected  --->  excluding Diamond Princess
ever.Affected = check.Confirmed
# Unit scaling
for (i in row.names(ever.Affected)) {
  for (j in 5:ncol(ever.Affected)) {
    if(ever.Affected[i,j] != 0)
      ever.Affected[i,j] = 1
  }
}

still.Affected = check.Confirmed
# Unit scaling
for (i in row.names(still.Affected)) {
  for (j in 5:ncol(still.Affected)) {
    if(still.Affected[i,j] != 0)
      still.Affected[i,j] = 1
  }
}

#View(ever.Affected)
#View(still.Affected)


# Removing outlier i.e. Hubei
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

row.names(Diamond.Princess.Confirmed) <- NULL
row.names(Diamond.Princess.Deaths) <- NULL
row.names(Diamond.Princess.Recovered) <- NULL

row.names(Hubei.Confirmed) <- NULL
row.names(Hubei.Deaths) <- NULL
row.names(Hubei.Recovered) <- NULL


###


#----------------------------------------------#




# creating new .csv file after cleaning the data

## Diamond Princess
write.csv(Diamond.Princess.Confirmed, file = "cleaned/Diamond-Princess/time_series_19-covid-Confirmed.csv", row.names = FALSE)
write.csv(Diamond.Princess.Deaths, file = "cleaned/Diamond-Princess/time_series_19-covid-Recovered.csv", row.names = FALSE)
write.csv(Diamond.Princess.Recovered, file = "cleaned/Diamond-Princess/time_series_19-covid-Deaths.csv", row.names = FALSE)

## Hubei
write.csv(Hubei.Confirmed, file = "cleaned/Hubei/time_series_19-covid-Confirmed.csv", row.names = FALSE)
write.csv(Hubei.Deaths, file = "cleaned/Hubei/time_series_19-covid-Recovered.csv", row.names = FALSE)
write.csv(Hubei.Recovered, file = "cleaned/Hubei/time_series_19-covid-Deaths.csv", row.names = FALSE)


## Main files
write.csv(check.Confirmed, file = "cleaned/time_series_19-covid-Confirmed.csv", row.names = FALSE)
write.csv(check.Recovered, file = "cleaned/time_series_19-covid-Recovered.csv", row.names = FALSE)
write.csv(check.Deaths, file = "cleaned/time_series_19-covid-Deaths.csv", row.names = FALSE)

###   For map plot & gif
write.csv(ever.Affected, file = "cleaned/ever.Affected.csv", row.names = FALSE)
write.csv(still.Affected, file = "cleaned/still.Affected.csv", row.names = FALSE)



#############################################################


cleaned.Confirmed <- read.csv("cleaned/time_series_19-covid-Confirmed.csv")
cleaned.Deaths <- read.csv("cleaned/time_series_19-covid-Deaths.csv")
cleaned.Recovered <- read.csv("cleaned/time_series_19-covid-Recovered.csv")

cleaned.ever.Affected <- read.csv("cleaned/ever.Affected.csv")
cleaned.still.Affected <- read.csv("cleaned/still.Affected.csv")

#str(cleaned.Confirmed)


#View(cleaned.Confirmed)
#View(cleaned.Deaths)
#View(cleaned.Recovered)

#View(cleaned.ever.Affected)
#View(cleaned.still.Affected)

##########    ENDS     ###########


