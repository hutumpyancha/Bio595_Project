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

spdatasub= spdata[, c("scientificName", "maximumDepthInMeters", "decimalLatitude", "decimalLongitude")] #subset by column


A_data= filter(spdatasub, grepl('Anthoptilum', scientificName)) #all the Anthptilum including Anthoptilum sp.     


U_data= filter(spdatasub, grepl('Umbellula', scientificName))
P_data= filter(spdatasub, grepl('Pennatula', scientificName))

V_data= rbind(A_data, U_data, P_data )


V_data = V_data[order(V_data$scientificName),]

V_data$genus= str_split_fixed(string = V_data$scientificName, pattern = " ", n = 2)[,1]
V_data$species= str_split_fixed(V_data$scientificName, " ", 2)[,2]
g <- str_sub(string = V_data$genus, start = 1, end = 1)
V_data$genus.period <- str_c(g,".",sep = "")



V = V_data %>% group_by(scientificName) %>% summarise(avg.depth = mean( maximumDepthInMeters, na.rm = T))

#V_data$species= sub("", "sp.", V_data$species)

V_data$species[V_data$species==""]<-"sp."

V_data$genus.species= paste(V_data$genus, "", V_data$species)
V_data$genus.sp= paste(V_data$genus.period, "", V_data$species)


taxon.fc <- function(x){

  if(str_detect(string = "species", pattern = "sp.")){
    taxon = V_data$genus.sp
    
    
  } else if(str_detect(string = "species", pattern != ("sp."))){
    taxon = V_data$genus.species
  } else {}
  
  return(taxon)
  
}

taxon.fc1 <- function(x){
  
  if(V_data$species == "sp."){
    taxon = V_data$genus.sp
    
    
  } else if(V_data$species != "sp."){
    taxon = V_data$genus.species
  } else {}
  
  return(taxon)
  
}


V_data$taxon = NA
for(i in 1:nrow(V_data)){
  V_data[i,]$taxon <- taxon.fc1(x = V_data[i,])
}



p= ggplot(V_data, aes(scientificName)) + geom_histogram(stat="count") + theme_classic( base_size = 13)

dir.create("V_figures")
ddply(.data = V_data, .variables = c("genus"), function(x){
  
  t <- unique(x$scientificName)
  
  plot1 <- ggplot(anthoptilum_data1, aes(x=Name, y=maximumDepthInMeters, fill=Name) ) +
    geom_boxplot() + scale_y_reverse(lim=c(4200,0),breaks=seq(0,4200,200),expand = c(0, 0)) +
    scale_x_discrete(position="top") + theme_classic( base_size = 13) + 
    theme(axis.text.x = element_text(face="italic"), legend.position = "none") + 
    labs(title="Depth profile", x="",y="Depth m")
  
  ggsave(filename = paste0('V_figures/',t,'.png'),
         plot = plot1, width = 4, height = 3, units = 'in',
         dpi = 300)
  
}, .inform = T, .progress = "text")



