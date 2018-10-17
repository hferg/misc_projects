
# Explore the various UFO datasets to see which will be most
# fun/interesting.

# libs
library(mapdata)
library(ggplot2)
library(ggmap)
library(maps)

# make a no axes theme
mintheme <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_blank()
  )

# read in the data.
# start with UFO scrubbed.
ds <- read.csv("./data/ufo_scrubbed.csv", stringsAsFactors = FALSE)
nds <- data.frame(
  x = as.numeric(ds$longitude), 
  y = as.numeric(ds$latitude),
  duration = ds$duration..seconds,
  shape = ds$shape,
  datetime = ds$datetime)
# cut this down to the US
usa <- map_data("usa")
usa_ufo <- nds[nds$x <= max(usa$long) & nds$x >= min(usa$long), ]
usa_ufo <- usa_ufo[usa_ufo$y <= max(usa$lat) & usa_ufo$y >= min(usa$lat), ]

# get states, and add the land, water, total areas and population.
states <- map_data("state")
area <- read.csv("./data/us_state_areas.csv")
popn <- read.csv("./data/us_state_populations.csv")
for (i in 1:nrow(area)) {
states$total_area[states$region == tolower(area$state[i])] <- 
    area$total_km2[i]
  states$land_area[states$region == tolower(area$state[i])] <- 
    area$land_km2[i]
  states$water_area[states$region == tolower(area$state[i])] <- 
    area$water_km2[i]    
}
for (i in 1:nrow(popn)) {
states$population[states$region == tolower(popn$State.or.territory[i])] <- 
    popn$pop_2017[i]  
}

# people per area.
states$pop_per_area <- states$population / states$total_area

# assign a state to each of the US sightings.
  # I think I need to do this in stages because of a "too many connections"
  # error. The error seems to be a bug in the package? The built-in package
  # might actually be better, and then if it returns an NA, use revgeo?

# first use map.where
for (i in 1:nrow(usa_ufo)) {
  # use maps::map.where since it's faster, and then lookup on photon if
  # map.where returns NA for state (which it sometimes does if near the coast)
  x <- maps::map.where(database = "state", usa_ufo[i, "x"], usa_ufo[i, "y"])
  if (is.na(x)) {
    url <- paste0("http://photon.komoot.de/reverse?lon=", 
      usa_ufo[i, "x"], "&lat=", usa_ufo[i, "y"])
    data <- RCurl::getURL(url)
    info <- jsonlite::fromJSON(data)
    x <- info$features$properties$state
  }   
  if (is.null(x)) {
    usa_ufo$state[i] <- NA
  } else {
    usa_ufo$state[i] <- x
  }
}

# sount sightings for each state.
sts <- unique(states$region)
states$sightings <- 0
for (i in seq_along(sts)) {
  states[states$region == sts[i], "sightings"] <- 
    sum(tolower(usa_ufo$state) == sts[i], na.rm = TRUE)
}

# Now some measures of sightings per x
states$sightings_per_capita <- states$sightings / states$population
states$sightings_per_area <- states$sightings / states$total_area
states$sightings_per_water <- states$sightings / as.numeric(states$water_area)


write.csv(usa_ufo, file = "ufo_states.csv")

# check that we can plot properly...
p_base <- ggplot(data = states, mapping = aes(x = long,
    y = lat,
    group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")
  
p_base +
  geom_polygon(data = states,
      aes(fill = sightings_per_capita),
    color = "white") +
  mintheme



# now get those state counts into the states data frame
# for plotting. Need to sum the number in each state, then pop that into the
# column for that state.


uu_sp <- usa_ufo
sp::coordinates(uu_sp) <- ~ x + y

# Get a map and let's start plotting the data a bit...
# Map help here:
  # http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

ggplot() + 
  geom_polygon(
    data = usa, aes(
      x=long, 
      y = lat, 
      group = group)
    ) + 
  coord_fixed(1.3)

# Then things to look at include...
# Sightings over time.
# Sightings per state.
# Sightings per capita.
# Sighting shape over space (different colour point for shape).
# Sighting duration over space and time.
# Sightings vs native psilocybe mushrooms in state (ha!)
# streamgraph of ufo shape over time.
  # ufo shape over space?!
# Sightings near to water?

# Maybe take a closer look at some of the more sighting-dense states?


wrld <- raster::raster("./data/wrld_10arcmin_0.grd")
cnts <- table(cellFromXY(wrld, nds))
wrld[as.numeric(names(cnts))] <- cnts
plot(wrld)

# the above is no good, because the counts are so varied!
# 1) get a bigger world grid.
# 2) bin the data into orders of magnitude.

# bins
log10_ceiling <- function(x) {
    ceiling(log10(x))
}

cnts2 <- log10_ceiling(cnts)
wrld[as.numeric(names(cnts2))] <- cnts2
plot(wrld, col = viridis::viridis(5))
