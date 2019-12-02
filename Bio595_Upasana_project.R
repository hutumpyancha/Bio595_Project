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
library(plyr)
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

V_data$species[V_data$species==""]<-"sp."

V_data$genus.species= paste(V_data$genus, "", V_data$species)
V_data$genus.sp= paste(V_data$genus.period, "", V_data$species)

V_data1= subset(V_data, genus != "Pennatulacea")

V = V_data1 %>% group_by(scientificName) %>% summarise(avg.depth = mean( maximumDepthInMeters, na.rm = T))




taxon.fc <- function(x){
  
  if(V_data$species[i] == "sp."){
    taxon = V_data1$genus.species[i]
    

    
  } else {taxon = V_data1$genus.sp[i]}
  
  return(taxon)
  
}

i=2
V_data1$taxon = NA
for(i in 1:nrow(V_data1)){
  V_data1$taxon[i] <- taxon.fc(x = V_data1[i,])
}



p= ggplot(V_data1, aes(genus, fill=genus)) + geom_histogram(stat="count") + theme_classic( base_size = 13)
plot(p)

dir.create("V_figures")
ddply(.data = V_data1, .variables = c("genus"), function(y){
  
  t = unique(y$genus)
  
  pl = ggplot(y, aes(x=taxon, y=maximumDepthInMeters, fill=taxon) ) +
    geom_boxplot() + stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
    stat_summary(fun.y=mean, geom="point")+ scale_y_reverse(lim=c(6000,0),breaks=seq(0,6000,500),expand = c(0, 0)) +
    scale_x_discrete(position="top") + theme_classic( base_size = 8) + 
    theme(axis.text.x = element_text(angle=90, size= 5, face="italic"), axis.text.y = element_text(size =8),legend.position = "none") + 
    labs(title="Depth profile", x="",y="Depth m")
  
  ggsave(filename = paste0('V_figures/',t,'.png'),
         plot = pl, width = 4, height = 3, units = 'in',
         dpi = 600)
  
},  .progress = "text")





