remove(list=ls())
assign("last.warning", NULL, envir = baseenv())
library(tidyverse)
library(ggplot2)
#install.packages("devtools")
library(devtools)
#install_github("iobis/robis")
library(robis)
#help(package = "robis") #accessing OBIS data
#?occurrence
library(dplyr)
spdata=occurrence("Pennatulacea")
spdata
spdatasub= spdata[, c("scientificName", "maximumDepthInMeters", "decimalLatitude", "decimalLongitude")] #subset by column


A_data= filter(spdatasub, grepl('Anthoptilum', scientificName)) #all the Anthptilum including Anthoptilum sp.     
A_data = A_data[order(A_data$scientificName),]

A_data1=A_data
A = A_data1 %>% group_by(scientificName) %>% summarise(avg.depth = mean( "maximunDepthInMeters", na.rm = T))

anthoptilum_data1= transform(anthoptilum_data1, Name = gsub(".*? (.+)", "A.\\1", scientificName))
#TO REMOVE GENUS NAME AND ALSO KEEP GENUS FOR THOSE WHERE THE SAMPLES ARE NOT IDENTIFIED TO SPECIES LEVEL
#\\1 lets it keep everything after space after replacing the first word with A.


anthoptilum_data1= transform(anthoptilum_data1, Name = gsub("Anthoptilum", "Anthoptilum sp.", Name))
#changing ANTHOPTILUM to ANTHOPTILUM sp.

pl1=ggplot(anthoptilum_data1, aes(x=Name, y=maximumDepthInMeters, fill=Name) ) +
  geom_boxplot() + scale_y_reverse(lim=c(4200,0),breaks=seq(0,4200,200),expand = c(0, 0)) +scale_x_discrete(position="top") + theme_classic( base_size = 13) + theme(axis.text.x = element_text(face="italic"), legend.position = "none") + labs(title="Depth profile for Anthoptilum", x="",y="Depth m")
plot(pl1)
