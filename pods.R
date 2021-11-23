#R Final Project: Pods

#This is saved in my Github repository:
#(https://github.com/thefishauthority/RFinal.git)

#Now first we read in our data set: 
library(readxl)

read_excel(file.choose()) #this didn't work because its a CSV
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

?subset
pods1=subset(x=pods, subset = Species=='Lepidactylus')
head(pods1)
unique(pods1$Species) #So now we have subet the data to our focus species, Lepidactylus

#But we need to rename lepidactylus (it's a new Genus, yknow)

new.species="Cryptohaustorius"
pods1$Species=new.species
head(pods1)

#Now because this all worked, save your work in Github