library(ggplot2)
library(grid)
library(maps)
library(plyr)

##############################################################################
# STEP 1. Transform the input data of tweets, their times, and geolocations. #
##############################################################################

# Read in the file containing when and where people tweeted that:
# - they lost power
# - they still have no power
# - their power turned back on
# I probably can't share the full set of data I have, so this is just an example.
d = read.csv("example_sandy_data.tsv", header = T, comment.char = '', quote = '')
d$which = factor(d$which, levels = c("Just lost power!", "Still no power...", "Power is back on!"), ordered = T)

# Filter to tweets around the North-East New England area.
MIN_LONGITUDE = -85
MAX_LONGITUDE = -65
MIN_LATITUDE = 35
MAX_LATITUDE = 48

d = subset(d, longitude >= MIN_LONGITUDE & longitude <= MAX_LONGITUDE & latitude >= MIN_LATITUDE & latitude <= MAX_LATITUDE)

# Count the number of tweets in the same geographic area.
RADIUS = 5 # Bucket tweets within 2 * RADIUS latitude/longitude degrees of each other.
d = transform(d, latitude = round(RADIUS * latitude) / RADIUS, longitude = round(RADIUS * longitude) / RADIUS)
d = ddply(d, .(day, hour, longitude, latitude, which), summarise, count = length(longitude))

#####################################
# STEP 2. Plot the tweets on a map. #
#####################################

# Map code partially taken from https://github.com/hadley/ggplot2/wiki/Flying-over-the-usa.

# Plots three maps, one for each type of tweet, showing where people
# just lost power, still had no power, or just got their power turned on,
# for the specified day and hour.
plotMap = function(my_day, my_hour) {
  opts = opts(panel.grid.minor = theme_blank(),
              panel.grid.major = theme_blank(),
              panel.background = theme_blank(),
              axis.title.x = theme_blank(),
              axis.title.y = theme_blank(),
              axis.line = theme_blank(),
              axis.ticks = theme_blank(),
              axis.text.y = theme_text(colour="#FFFFFF"),
              axis.text.x = theme_text(colour = "#FFFFFF"))

  data = subset(d, day == my_day & hour == my_hour)

  base = qplot(longitude, latitude, data = data,
               xlim = c(MIN_LONGITUDE, MAX_LONGITUDE), ylim = c(MIN_LATITUDE, MAX_LATITUDE),
               alpha = I(0))

  q = base + borders("state", size = 0.2, fill = I("grey85"), colour = I("white")) +
             geom_jitter(alpha = 0.65, aes(x = longitude, y = latitude, colour = which, size = count)) +
             scale_colour_manual(values = c("#D7191C", "#FF7F00", "#2C7BB6")) +
             guides(colour = FALSE, size = FALSE) +
             scale_size(range = c(3, 15)) +
             facet_grid(. ~ which) +
             opts

  return(q)
}

# Plots a clock showing the time of the tweets.
# Code copied from
plotClock = function(day, hour, minute) {
  # Hour and minute hands.
  hour = hour %% 12
  minute = minute
  face = data.frame(cbind(x = c(rep((hour + minute / 60) / 12, 2), rep(minute / 60, 2)), y = c(0,0.55, 0,0.85)))
  face$id = c("hour", "hour", "min", "min")

  # Clock frame.
  clock = data.frame(cbind(x = seq(0, 1, length = 100), y = rep(1, 100)))
  clock$id = "clock"

  face = rbind(face,clock)
  base = qplot(x, y, geom = "line", data = face, colour = I("grey65"), group = id, xlim = c(0,1), xlab = "", ylab = "")

  title = paste("October ", day, ", ", if (hour == 0) 12 else hour, if (hour <= 11) "am" else "pm", sep = "")
  q = base +
      coord_polar() +
      scale_x_continuous(breaks = seq(0,1, length = 5), labels = rep("", 5)) +
      opts(plot.margin = unit(rep(0,4), "lines"),
           panel.background = theme_blank(),
           panel.grid.minor = theme_blank(),
           axis.title.x = theme_blank(),
           axis.title.y = theme_blank(),
           axis.line = theme_blank(),
           axis.ticks = theme_blank(),
           axis.text.y   =  theme_text(colour = "#FFFFFF", lineheight = -10, size = 0),
           axis.text.x   =  theme_text(colour = "#FFFFFF", lineheight = -10, size = 0),
           title = title,
           plot.title = theme_text(size = 9))

  return(q)
}

# Plots the map of tweets along with the clock showing their time.
plotMapWithClock = function(my_day, my_hour) {
  map = plotMap(my_day, my_hour) +
        opts(title = "Power Outages during Hurricane Sandy",
             plot.title = theme_text(size = 25, face = 'bold', vjust = 2))
  print(map)

  clock = plotClock(my_day, my_hour, 0)
  vp2 = viewport(x = 1, y = 0.075, height = unit(5, "cm"), width =  unit(5, "cm"), just = c("right", "bottom"))
  print(clock, vp = vp2)
}

# Make a PNG for each map on the specified day and hour.
for (i in 8:23) {
  png(paste("sandy_oct29_", i, ".png", sep = ""), height = 600, width = 1200)
  plotMapWithClock(29, i)
  dev.off()
}
for (i in 0:23) {
  png(paste("sandy_oct30_", i, ".png", sep = ""), height = 600, width = 1200)
  plotMapWithClock(30, i)
  dev.off()
}