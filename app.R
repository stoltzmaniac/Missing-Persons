library(shiny)
library(lubridate)
library(ggmap)
library(leaflet)


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
df$yearsOld = round(as.numeric((today() - as.Date(df$birthdate))/365),1)
df$ageGoneMissing = round(as.numeric((today() - df$birthdate)/365),1)
df$yearsMissing = round(as.numeric((today() - as.Date(df$missingfromdate))/365),1)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
}

shinyApp(ui, server)