#R Final Project: Pods

#This is saved in my Github repository:
#(https://github.com/thefishauthority/RFinal.git)

#Now first we read in our data set: 
library(readxl)

#read_excel(file.choose()) #this didn't work because its a CSV
pods=read.csv(file.choose())

#check our file:
head(pods)
str(pods)

#'data.frame':	3007 obs. of  9 variables:
# $ Species       : chr  "Lepidactylus" "Lepidactylus" "Lepidactylus" "Lepidactylus" ...
#$ Sex           : chr  "Males" "Females" "Ovi_females" "Juveniles" ...
#$ Transect_Num  : int  1 1 1 1 1 1 1 1 1 1 ...
#$ Abundance     : int  0 0 0 0 0 0 0 0 0 0 ...
#$ Position      : int  0 0 0 0 0 0 0 0 0 0 ...
#$ Month         : chr  "May" "May" "May" "May" ...
#$ Temperature.. : chr  "27" "27" "27" "27" ...
#$ Salinity..ppt.: chr  "18" "18" "18" "18" ...
#$ X             : chr  "" "" "" "" ...

#So here we have a data fram with multiple species, sex classes, transects
#numbers, positions, months, temperatures, salinities, and x?

#first we can delete the column x because it's empty

pods=pods[,1:8]
head(pods) #the empty column is gone

#So what species do we have?
unique(pods$Species) #5 unique species plus an empty value? 

#What if we want to view the data by species?

species.pods=pods[order(pods$Species),]
species.pods
unique(species.pods$Species) #so now our species are sorted alphabetically

summary(species.pods)#this gives us a summary of each column and type- this df
#is 7 characters too long as downloaded. 

ddply(.data = pods, .variables = c("Species"), function(x){
  
  Abundance.Month.plots <- ggplot(data = pods, aes(x = Month, y= Abundance)) +
    geom_point() +
    ggtitle("Abundance v Month")
  
  png(file = paste0("plots/", unique(pods$Species)), 
      width = 2.5, height = 2.5, units = "in", res = 150)
  plot(Abundance.Month.plots)
  dev.off()
  
}, .progress = "text", .inform = T) #having issues with this


Species.plots <- ggplot(data = pods, aes(x = Species, y= Abundance)) +
  geom_point() +
  stat_smooth(method = "lm") +
  ggtitle("Species at Sea Rim TX")
Species.plots

Abundance.plots <- ggplot(data = pods, aes(x = Position, y= Abundance)) +
  geom_point() +
  ggtitle("Abundance v Postion")

head(pods)

















?subset
pods1=subset(x=pods, subset = Species=='Lepidactylus')
head(pods1)
unique(pods1$Species) #So now we have subet the data to our focus species, Lepidactylus

#But we need to rename lepidactylus (it's a new Genus, yknow)

new.species="Cryptohaustorius"
pods1$Species=new.species
head(pods1)

#Now because this all worked, save your work in Github (This has the commit message "hwody")
#so now we have two data frames, pods for all species and pods1 for crypto


library(dplyr)
library(tidyverse)

#lets convert our temperatures to F for context

tempswap=function(x){ #another custom function
  x=32+(x*9/5)
    return(x)
}

tempswap(100) #okay so this function works

farenheit= c()
pods1$Temperature..=as.numeric(pods1$Temperature..)

for(i in 1:length(pods1$Temperature..)){
  farenheit=tempswap(pods1$Temperature..[i])
  print(farenheit)
} 

#so lets try some summary of categories
library(plyr)
detach(tidyverse)
detach(dplyr)

pods.abund.summary=ddply(.data=pods1, 
                            .variables=c("Month","Position"),
                            .fun=summarise, mean.abund=mean(Abundance))
pods.abund.summary #this makes an average abundance for each 
                  #position for each month
            #weirdly the months are out of order?

#what about by sex class?
pods.sex.summary=ddply(.data=pods1, 
                       .variables=c("Month","Sex"),
                       .fun=summarise, mean.abund=mean(Abundance))
pods.sex.summary #this was really helpful- mean abundance for each month. 
pods.sex.summary2=ddply(.data=pods1, 
                       .variables=c("Month","Sex"),
                       .fun=summarise, Abund=sum(Abundance), SD.Abund=sd(Abundance))
pods.sex.summary2 #this is great! summarize each sex class per month
#Remember to plot this 

pods.sex=ggplot(data=pods1, aes(x=Sex, y=Abundance))+
  geom_boxplot()
pods.sex

pods.sex2=ggplot(data=pods.sex.summary2, aes(x=Sex, y=Abund))+
  geom_boxplot()
pods.sex2 #now these two plots are similar but scaled differently- because of less points?

#lets make a histogram 
pods.pos.abund=ggplot(data=pods1, aes(x=Position))+
  geom_histogram()
pods.pos.abund #so here you can see the grand position vs abundance

#Lets manipulate the data a bit-
#first subset the data for transect 1
pods2=subset(x=pods1, subset=Transect_Num=="1")
unique(pods2$Transect_Num)
#Now get rid of transect number and environmental columns
pods3=pods2[,c(1:2,4:6)]
head(pods3)
str(pods3)
pods4=spread(data=pods3, key=Position, value=Abundance)
head(pods4)
pods4
pods5=spread(data=pods3, key=Sex, value=Abundance)
pods5 #a different way of looking at our data for the same position by sex

#This gave us a cool new format with the Positions as columns
model=aov(formula=Month~Sex, data=pods4)

#So one of our questions- are sex class ratios different for each month? 
#(yes, but prove it)
unique(pods1$Abundance)
unique(pods1$Month)
head(pods1)
tail(pods1)
model=aov(formula=Month~Abundance, data=pods1)#this didnt work either?
summary(model)

#Test the effect of month on sex/ abundance

model.1=aov(formula=Month~Sex, data=pods1) #this didn't work 
#because the y is categorical
summary(model.1)
