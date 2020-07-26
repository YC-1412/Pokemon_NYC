## ----read data-------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(sp)
library(geojsonio)

dat <- read_csv('./pokemon-spawns.csv')
dat2 <- read_csv('./pogo.csv')


## ----data cleaning---------------------------------------------------------------------------------
# preselect a small region around New York
# because the for loop for maping lat and lng to states takes long
# convert time to machine-readable format

# location of NY (roughly)
loc = c(40.7,-73.9)   # center
r = c(0.3,0.4)        # radius


dat <- dat %>% 
  mutate('appear_intvl' = (.$encounter_ms-.$disppear_ms)/60000,
         'lat' = round(.$lat,digits = 3),
         'lng' = round(.$lng,digits = 3)) %>% 
  mutate('appear_time' = 
           format(as.POSIXct(.$encounter_ms/1000, 
                           origin="1970-01-01", 
                           tz="America/New_York"),"%H:%M")) %>% 
  filter(appear_intvl>0,
         appear_time <= format(strptime('14:30',"%H:%M"),"%H:%M"),
         lat>loc[1]-r[1],lat<loc[1]+r[1],
         lng>loc[2]-r[2],lng<loc[2]+r[2]) %>% 
  select(c('num','name','lat','lng','appear_intvl','appear_time'))


## ----map state info--------------------------------------------------------------------------------
# read in usa state map information
usa <- geojson_read(
  "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_500k.json",
  what = "sp")

# map state info to each point by its lat and lng
for (i in 1:nrow(dat)) {
  coords <- c(dat$lng[i], dat$lat[i])
  if(any(is.na(coords))) next
  point <- sp::SpatialPoints(
    matrix(
      coords,
      nrow = 1
    )
  )
  sp::proj4string(point) <- sp::proj4string(usa)
  polygon_check <- sp::over(point, usa)
  dat$state[i] <- as.character(polygon_check$NAME)
}

# select points only in NY
dat <- dat %>% filter(state == 'New York')


## ----pokemon type----------------------------------------------------------------------------------
# get pokemon type info from pogo dataset
dat2 <- dat2 %>% 
  mutate('num' = Pokedex) %>% 
  select(num,Primary)

# compute rarity and average appearing time
# combine location dataset with pogo dataset
# rarity calculation: sum up all the appearing time all over NY. 
# <30mins: rare, 30-60mins: uncommon, 60mins: common
dat <- dat %>% group_by(num,name,lat,lng) %>% 
  summarise('ave_appear_time' = sum(appear_intvl)/n(),
            'total_appear_time'= sum(appear_intvl)) %>% 
  mutate('freq'= 'uncommon',
         'freq' = ifelse(total_appear_time<30,'rare',freq),
         'freq' = ifelse(total_appear_time>60,'common',freq)) %>% 
  # filter(ave_appear_time>5) %>% 
  left_join(dat2,by='num')


## ----visualization---------------------------------------------------------------------------------
library(plotly)
library(RColorBrewer)

# here for more info https://docs.mapbox.com/help/how-mapbox-works/access-tokens/
# Sys.setenv("MAPBOX_TOKEN" = 'personal token') # for Orca

mycolors = c(brewer.pal(name="Set1", n = 9), brewer.pal(name="Accent", n = 5))

# sizeref: smaller, larger points
fig <- dat %>% plot_ly(lat = ~lat,
                       lon = ~lng,
                       marker = list(size = ~ave_appear_time,
                                     sizeref = 0.2,
                                     sizemode = 'area'),
                       color = ~Primary,
                       colors = mycolors,
                       type = 'scattermapbox',
                       mode = "markers",
                       hoverinfo = 'text',
                       hovertext = ~paste('</br> ID:',num,
                                         '</br> Name:',name,
                                         '</br> Type:',Primary,
                                         '</br> Appear time:',
                                         round(ave_appear_time,digits = 0),'mins',
                                         '</br> Location:',lng,'°,',lat,'°'))
# center: make sure the map shows all the points when being loaded
# zoom: larger, more details
fig <- fig %>%
  layout(
    mapbox = list(
      # style = 'open-street-map',
      style = 'carto-positron',
      zoom =11,
      center = list(lon = loc[2]-r[2]/4, lat = loc[1]+r[1]/6)),
    title = 'Pokemon Distribution')

fig

