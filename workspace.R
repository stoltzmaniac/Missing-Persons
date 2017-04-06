library(lubridate)
library(dplyr)
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
df$age = floor(as.numeric((today() - as.Date(df$birthdate))/365))
df$birthMonth = month(as.Date(df$birthdate),label=TRUE)
df$birthYear = year(as.Date(df$birthdate))

df$ageGoneMissingFrom = floor(as.numeric((as.Date(df$missingfromdate) - as.Date(df$birthdate))/365))
df$ageGoneMissingReported = floor(as.numeric((as.Date(df$missingreporteddate) - as.Date(df$birthdate))/365))

df$missingToReportedYears = floor(as.numeric((as.Date(df$missingreporteddate) - as.Date(df$missingfromdate))/365))
df$currentYearsMissing = floor(as.numeric((today() - as.Date(df$missingfromdate))/365))

df$missingReportedHour = hour(df$missingreporteddate)
df$missingReportedDayOfWeek = wday(df$missingreporteddate,label=TRUE) 
df$missingReportedMonth = month(df$missingreporteddate,label=TRUE)
df$missingReportedYear = year(df$missingreporteddate) 

df$missingFromHour = hour(df$missingfromdate)
df$missingFromDayOfWeek = wday(df$missingfromdate,label=TRUE)
df$missingFromMonth = month(df$missingfromdate,label=TRUE)
df$missingFromYear = year(df$missingfromdate) 

df$childName = paste(df$childfirstname, df$childlastname)

df$ageDecade = "0-9"
df$ageDecade[df$age >= 10] = "10-19"
df$ageDecade[df$age >= 20] = "20-29"
df$ageDecade[df$age >= 30] = "30-39"
df$ageDecade[df$age >= 40] = "40-49"
df$ageDecade[df$age >= 50] = "50-59"
df$ageDecade = as.factor(df$ageDecade)

df$ageGoneMissingFromDecade = "0-9"
df$ageGoneMissingFromDecade[df$ageGoneMissingFrom >= 10] = "10-19"
df$ageGoneMissingFromDecade[df$ageGoneMissingFrom >= 20] = "20-29"
df$ageGoneMissingFromDecade[df$ageGoneMissingFrom >= 30] = "30-39"
df$ageGoneMissingFromDecade[df$ageGoneMissingFrom >= 40] = "40-49"
df$ageGoneMissingFromDecade[df$ageGoneMissingFrom >= 50] = "50-59"
df$ageGoneMissingFromDecade = as.factor(df$ageGoneMissingFromDecade)

df$ageGoneMissingReportedDecade = "0-9"
df$ageGoneMissingReportedDecade[df$ageGoneMissingReported >= 10] = "10-19"
df$ageGoneMissingReportedDecade[df$ageGoneMissingReported >= 20] = "20-29"
df$ageGoneMissingReportedDecade[df$ageGoneMissingReported >= 30] = "30-39"
df$ageGoneMissingReportedDecade[df$ageGoneMissingReported >= 40] = "40-49"
df$ageGoneMissingReportedDecade[df$ageGoneMissingReported >= 50] = "50-59"
df$ageGoneMissingReportedDecade = as.factor(df$ageGoneMissingReportedDecade)

p = ggplot(df,aes(x=missingFromYear))
p + geom_line(stat='count')
p + geom_line(stat='count',aes(col=casetype))
###Notice how casetype shows the massive spike in endangered runaway, gotta remove for analysis



df.backup = df

er = df %>% filter(casetype == 'Endangered Runaway')
df = df %>% filter(casetype != 'Endangered Runaway')

##summary(er$age) shows us that most are around 17 years old, but min is 2 and max is 63... kind of ridiculous

#p = ggplot(df, aes(x=ageGoneMissingFrom,y=ageGoneMissingReported))
#p + geom_point(aes(col=ageDecade),alpha=.5) + geom_abline(slope=1,col='red')

#p = ggplot(df,aes(x=missingReportedYear))
#p + geom_line(stat='count')


p = ggplot(df,aes(x=missingFromYear))
p + geom_line(stat='count')
p + geom_line(stat='count',aes(col=sex,fill=sex))
p + geom_area(position='fill',stat='count',aes(col=sex,fill=sex)) + geom_abline(intercept=0.5,slope=0,col='black',size=3)

p = ggplot(df %>% group_by(ageGoneMissingReportedDecade),aes(x=ageGoneMissingReportedDecade))
p + geom_bar(position='fill',stat='count',aes(col=sex,fill=sex))
p + geom_bar(stat='count',aes(col=sex,fill=sex))

p = ggplot(df %>% group_by(ageGoneMissingReportedDecade),aes(x=ageGoneMissingReportedDecade))
p + geom_bar(position='fill',stat='count',aes(col=sex,fill=sex))

p = ggplot(df,aes(x=birthYear))
p + geom_area(position='fill',stat='count',aes(col=sex,fill=sex))

p = ggplot(df,aes(x=birthYear))
p + geom_area(position='fill',stat='count',aes(col=haircolor,fill=haircolor))

p = ggplot(df,aes(x=ageGoneMissingFrom))
p + geom_area(stat='count',aes(col=haircolor,fill=haircolor))
p + geom_area(stat='count',aes(col=sex,fill=sex))
p + geom_area(position='fill',stat='count',aes(col=sex,fill=sex)) + geom_abline(intercept=0.5,slope=0,col='black',size=3)
p + geom_area(position='fill',stat='count',aes(col=eyecolor,fill=eyecolor))
p + geom_area(stat='count',aes(col=casetype,fill=casetype))




library(googleVis)

tmp = df %>% 
  filter(missingfromcountry == 'United States') %>% 
  group_by(missingfromstate) %>%
  summarise(meanAgeGoneMissingReported = round(mean(ageGoneMissingReported, na.rm=TRUE),0), 
            medianAgeGoneMissingReported = round(median(ageGoneMissingReported, na.rm=TRUE),0),
            minAgeGoneMissingReported = round(min(ageGoneMissingReported, na.rm=TRUE),0),
            maxAgeGoneMissingReported = round(max(ageGoneMissingReported, na.rm=TRUE),0))

p = gvisGeoChart(tmp,"missingfromstate",colorvar="medianAgeGoneMissingReported",options=list(region="US",
                                                                     displayMode="regions",
                                                                     resolution="provinces",
                                                                     colors="['red','yellow','green']",
                                                                     width=1000, height=600))

plot(p)

p = gvisGeoChart(tmp,"missingfromstate",colorvar="meanAgeGoneMissingReported",options=list(region="US",
                                                                                             displayMode="regions",
                                                                                             resolution="provinces",
                                                                                             colors="['red','yellow','green']",
                                                                                             width=1000, height=600))
plot(p)

p = gvisGeoChart(tmp,"missingfromstate",colorvar="minAgeGoneMissingReported",options=list(region="US",
                                                                                           displayMode="regions",
                                                                                           resolution="provinces",
                                                                                           colors="['red','yellow','green']",
                                                                                           width=1000, height=600))
plot(p)

p = gvisGeoChart(tmp,"missingfromstate",colorvar="maxAgeGoneMissingReported",options=list(region="US",
                                                                                          displayMode="regions",
                                                                                          resolution="provinces",
                                                                                          colors="['red','yellow','green']",
                                                                                          width=1000, height=600))
plot(p)
##
