library(lubridate)
library(ggmap)

##### Data From: https://data.world/jamesgray/missing-children-in-the-us
# missing.persons = read.csv("https://query.data.world/s/57mcfy9olvjsi3imujwdh9xoq",header=T)
# missing.persons$location = paste(missing.persons$missingfromcity,", ",
#                                  missing.persons$missingfromstate,", ",
#                                  missing.persons$missingfromcountry,
#                                  sep='')
#####

##### Grabbed all places, geo coded, wrote to a csv to save time
#places = unique(missing.persons$location)
#places.latlon = geocode(places)
#places.df = cbind(places,places.latlon)
#write.csv(places.df,'places_latlon.csv',row.names=FALSE)
#places = read.csv('places_latlon.csv')
######

###### Merged data and created new file
# df = merge(missing.persons,places,by.x='location',by.y='places')
# write.csv(df,'missing_persons_data_joined.csv',row.names=FALSE)
######

df = read.csv('missing_persons_data_joined.csv')
df$age = round(as.numeric((today() - as.Date(df$birthdate))/365),1)
df$ageGoneMissingFrom = round(as.numeric((as.Date(df$missingfromdate) - as.Date(df$birthdate))/365),1)
df$ageGoneMissingReported = round(as.numeric((as.Date(df$missingreporteddate) - as.Date(df$birthdate))/365),1)
df$missingToReportedYears = round(as.numeric((as.Date(df$missingreporteddate) - as.Date(df$missingfromdate))/365),1)
df$currentYearsMissing = round(as.numeric((today() - as.Date(df$missingfromdate))/365),1)
df$missingReportedHour = hour(df$missingreporteddate)
df$missingReportedDayOfWeek = wday(df$missingreporteddate,label=TRUE) 
df$missingFromHour = hour(df$missingfromdate)
df$missingFromDayOfWeek = wday(df$missingfromdate,label=TRUE)
df$childName = paste(df$childfirstname, df$childlastname)


library(googleVis)
p = gvisGeoChart(df,"missingfromstate",colorvar="ageGoneMissingReported",options=list(region="US", 
                                                                     displayMode="regions", 
                                                                     resolution="provinces",
                                                                     width=600, height=400))

plot(p)

GeoStates <- gvisGeoChart(states, "state.name", "Illiteracy",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)