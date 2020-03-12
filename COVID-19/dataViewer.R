#To view all the available datasets

# Setting the working directory
setwd("~/Documents/A-tracking-of-2019-nCoV/COVID-19/")

## THE BASE DATASETs



##############################################################################################################


##    China (except Hubei)
China.States.daily.Confirmed = read.csv("ready_to_use/COVID-19/China/China_States_daily_Confirmed.csv")
China.States.daily.Deaths = read.csv("ready_to_use/COVID-19/China/China_States_daily_Deaths.csv")
China.States.daily.Recovered = read.csv("ready_to_use/COVID-19/China/China_States_daily_Recovered.csv")
China.Aggregate.daily.Confirmed = read.csv("ready_to_use/COVID-19/China/China_Aggregate_daily_Confirmed.csv")
China.Aggregate.daily.Deaths = read.csv("ready_to_use/COVID-19/China/China_Aggregate_daily_Deaths.csv")
China.Aggregate.daily.Recovered = read.csv("ready_to_use/COVID-19/China/China_Aggregate_daily_Recovered.csv")
China.States.summary = read.csv("ready_to_use/COVID-19/China/China_States_summary.csv")
China.Aggregate.summary = read.csv("ready_to_use/COVID-19/China/China_Aggregate_summary.csv")
China.dataset.dateWise = read.csv("ready_to_use/COVID-19/China/China_dataset_dateWise_summary.csv")

View(China.States.daily.Confirmed)
View(China.States.daily.Deaths)
View(China.States.daily.Recovered)
View(China.Aggregate.daily.Confirmed)
View(China.Aggregate.daily.Deaths)
View(China.Aggregate.daily.Recovered)
View(China.States.summary)
View(China.Aggregate.summary)
View(China.dataset.dateWise)



##    Diamond Princess (only)
Diamond.Princess.daily.Confirmed = read.csv("ready_to_use/COVID-19/Cruise/Diamond_Princess_daily_Confirmed.csv")
Diamond.Princess.daily.Deaths = read.csv("ready_to_use/COVID-19/Cruise/Diamond_Princess_daily_Deaths.csv")
Diamond.Princess.daily.Recovered = read.csv("ready_to_use/COVID-19/Cruise/Diamond_Princess_daily_Recovered.csv")
Diamond.Princess.summary = read.csv("ready_to_use/COVID-19/Cruise/Diamond_Princess_summary.csv")
Diamond.Princess.dataset.dateWise = read.csv("ready_to_use/COVID-19/Cruise/Diamond_Princess_dataset_dateWise_summary.csv")

View(Diamond.Princess.daily.Confirmed)
View(Diamond.Princess.daily.Deaths)
View(Diamond.Princess.daily.Recovered)
View(Diamond.Princess.summary)
View(Diamond.Princess.dataset.dateWise)



##    FOUR
FOUR.daily.Confirmed = read.csv("ready_to_use/COVID-19/FOUR/Four_daily_Confirmed.csv")
FOUR.daily.Deaths = read.csv("ready_to_use/COVID-19/FOUR/Four_daily_Deaths.csv")
FOUR.daily.Recovered = read.csv("ready_to_use/COVID-19/FOUR/Four_daily_Recovered.csv")
FOUR.summary = read.csv("ready_to_use/COVID-19/FOUR/Four_Summary.csv")
FOUR.dataset.dateWise = read.csv("ready_to_use/COVID-19/FOUR/Four_dataset_dateWise.csv")
FOUR.dataset.locationWise = read.csv("ready_to_use/COVID-19/FOUR/Four_dataset_locationWise.csv")

View(FOUR.daily.Confirmed)
View(FOUR.daily.Deaths)
View(FOUR.daily.Recovered)
View(FOUR.summary)
View(FOUR.dataset.dateWise)
View(FOUR.dataset.locationWise)


##    Hubei (only)
Hubei.daily.Confirmed = read.csv("ready_to_use/COVID-19/Hubei/Hubei_daily_Confirmed.csv")
Hubei.daily.Deaths = read.csv("ready_to_use/COVID-19/Hubei/Hubei_daily_Deaths.csv")
Hubei.daily.Recovered = read.csv("ready_to_use/COVID-19/Hubei/Hubei_daily_Recovered.csv")
Hubei.summary = read.csv("ready_to_use/COVID-19/Hubei/Hubei_summary.csv")
Hubei.dataset.dateWise = read.csv("ready_to_use/COVID-19/Hubei/Hubei_dataset_dateWise_summary.csv")

View(Hubei.daily.Confirmed)
View(Hubei.daily.Deaths)
View(Hubei.daily.Recovered)
View(Hubei.summary)
View(Hubei.dataset.dateWise)


##    Mixed
All.Countries.daily.Confirmed = read.csv("ready_to_use/COVID-19/Mixed/All_Countries_daily_Confirmed.csv")
All.Countries.daily.Deaths = read.csv("ready_to_use/COVID-19/Mixed/All_Countries_daily_Deaths.csv")
All.Countries.daily.Recovered = read.csv("ready_to_use/COVID-19/Mixed/All_Countries_daily_Recovered.csv")
All.Countries.summary = read.csv("ready_to_use/COVID-19/Mixed/All_Countries_summary.csv")
Mixed.summary = read.csv("ready_to_use/COVID-19/Mixed/bulk_summary.csv")
Mixed.countryWise.summary = read.csv("ready_to_use/COVID-19/Mixed/countryWise_bulk_summary.csv")
Mixed.dateWise.summary = read.csv("ready_to_use/COVID-19/Mixed/dateWise_bulk_summary.csv")

View(All.Countries.daily.Confirmed)
View(All.Countries.daily.Deaths)
View(All.Countries.daily.Recovered)
View(All.Countries.summary)
View(Mixed.summary)
View(Mixed.countryWise.summary)
View(Mixed.dateWise.summary)


##    World (except China)
World.Countries.daily.Confirmed = read.csv("ready_to_use/COVID-19/World/World_Countries_daily_Confirmed.csv")
World.Countries.daily.Deaths = read.csv("ready_to_use/COVID-19/World/World_Countries_daily_Deaths.csv")
World.Countries.daily.Recovered = read.csv("ready_to_use/COVID-19/World/World_Countries_daily_Recovered.csv")
World.Aggregate.daily.Confirmed = read.csv("ready_to_use/COVID-19/World/World_Aggregate_daily_Confirmed.csv")
World.Aggregate.daily.Deaths = read.csv("ready_to_use/COVID-19/World/World_Aggregate_daily_Deaths.csv")
World.Aggregate.daily.Recovered = read.csv("ready_to_use/COVID-19/World/World_Aggregate_daily_Recovered.csv")
World.Countries.summary = read.csv("ready_to_use/COVID-19/World/World_Countries_summary.csv")
World.Aggregate.summary = read.csv("ready_to_use/COVID-19/World/World_Aggregate_summary.csv")
World.dataset.dateWise = read.csv("ready_to_use/COVID-19/World/World_dataset_dateWise_summary.csv")

View(World.Countries.daily.Confirmed)
View(World.Countries.daily.Deaths)
View(World.Countries.daily.Recovered)
View(World.Aggregate.daily.Confirmed)
View(World.Aggregate.daily.Deaths)
View(World.Aggregate.daily.Recovered)
View(World.Countries.summary)
View(World.Aggregate.summary)
View(World.dataset.dateWise)



##############################################################################################################

##    Other Temp   (11)

