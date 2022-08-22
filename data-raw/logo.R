library(gsheet)
library(hexSticker)
library(scales)
library(sp)
library(tidyverse)
library("rnaturalearth")
library("rnaturalearthdata")

library(ggplot2)

worldtilegrid <- read.csv("https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv")

View(worldtilegrid)
ggplot(worldtilegrid, aes(x = x, y = y)) + geom_text(aes(label = alpha.2))

worldgrid <- ggplot(worldtilegrid, aes(xmin = x, ymin = y, xmax = x + 1, ymax = y + 1))
worldgrid + geom_rect()

worldgrid + geom_rect(color = "darkgray", fill=NA) + mytheme +
  geom_text(aes(x = x, y = y, label = alpha.2), color = "#ffffff", alpha = 0.5, nudge_x = 0.5, nudge_y = -0.5, size = 0.5) +
  scale_y_reverse()

mytheme <- theme_minimal() + theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank())
worldgrid + geom_rect(color = "darkgray", fill=NA) + mytheme


p_0 <- worldgrid + geom_rect(color = "darkgray", fill=NA) + mytheme +
  geom_text(aes(x = x, y = y, label = alpha.2), color = "#ffffff", alpha = 0.5, nudge_x = 0.5, nudge_y = -0.5, size = 0.5) +
  scale_y_reverse()

################

# Load ggplot2
library(ggplot2)

# Very basic chart
basic <- ggplot( mtcars , aes(x=mpg, y=wt)) +
  geom_point(color = "red", alpha = 0.5, nudge_x = 0.5, nudge_y = 0.5, size = 1)
basic



p <- basic + theme(
  panel.grid.major = element_line(colour = "darkgray", linetype = "dotted"),
  panel.grid.minor = element_line(colour = "darkgray", size = 0.5)
)

sticker(p_0,
        package="GridDER",
        p_size=22,
        p_x = 1,
        p_y = 1.6,
        s_x=1,
        s_y=0.8,
        s_width=1.70,
        s_height= 1.5,
        h_size = 0.85,
        p_color = "#080808",
        h_fill = "#f5f5f5",
        h_color = "#080808",
        filename="inst/logo.png")


#ggplot(worldtilegrid, aes(x = x, y = y)) + geom_point()


################


