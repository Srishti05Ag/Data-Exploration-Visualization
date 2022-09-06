# Importing the dataset
Rawset <- read.csv("dataset.csv")

# Duplicating the dataset
working_rawset <- Rawset 
working_rawset

# Importing Essential libraries 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotrix)

# Checking the structure
str(working_rawset)

# Viewing the dataset
View(working_rawset)

# Renaming the Column
colnames(working_rawset)[1] <- "Address"
colnames(working_rawset)[2] <- "Name"
colnames(working_rawset)[3] <- "Online_Order"
colnames(working_rawset)[4] <- "Book_Table"
colnames(working_rawset)[6] <- "Votes"
colnames(working_rawset)[8] <- "Dish_Liked"
colnames(working_rawset)[9] <- "Cuisines"
str(working_rawset)

# Dropping the NA values
working_rawset <- drop_na(working_rawset)

############### Analyzing the data through visualization ##############

# Checking the summary
summary(working_rawset)
write.csv(working_rawset, file = "clean_dataset.csv")
View(working_rawset)

# Visualization the average price per restaurant type for two people
working_rawset %>%
  filter(!str_detect(Restaurant_Type, ',')) %>%
  ggplot(aes(Restaurant_Type , y = Cost_for_2_people , fill = Restaurant_Type)) + stat_summary(fun = "mean", geom = "col") + theme(axis.text.x = element_text(angle = 90), legend.position="none") +  xlab("Restaurant Type") + ylab("Cost for 2 people") + ggtitle("Average cost for each restaurant type")+ theme(plot.title = element_text(hjust = 0.5))

# Visualization the average price per Locality for two people
ggplot(working_rawset,aes(Locality , y = Cost_for_2_people , fill = Locality)) + stat_summary(fun = "mean", geom = "col") + theme(axis.text.x = element_text(angle = 90), legend.position="none")


set_palette(p, "Set1")
p <- working_rawset %>%
  ggplot(aes(Locality, Cost_for_2_people, fill = Locality)) + theme_bw() + ylab("Cost for 2 people (in INR)") +
  geom_boxplot() +
  ggtitle("Cost per locality") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 90), legend.position="none")+scale_y_continuous(breaks=c(0,2000,4000,6000))
theme(plot.title = element_text(hjust=0.5, face = "bold", colour = "#386CB0", size = 16)) +
  xlab("") 
p

# People who order Online
table(working_rawset$Online_Order)
lbls <- paste(names(table(working_rawset$Online_Order)), round((table(working_rawset$Online_Order)/23259)*100), "%")
pie(table(working_rawset$Online_Order), lables = lbls, main = "Online Order")

# People who Book Table  
table(working_rawset$Book_Table)
lbls1 <- paste(names(table(working_rawset$Book_Table)), round((table(working_rawset$Book_Table)/23259)*100), "%")
pie(table(working_rawset$Book_Table), lables = lbls1, main = "Book Table")

# Rating as per price for 2 people
ggplot(working_rawset, aes(x = Rating, y = Cost_for_2_people, color = Locality )) + stat_summary(fun = "mean", geom = "point") + theme(axis.text.x = element_text(angle = 90))

# Visualization for cost in dense graph
ggplot(working_rawset,aes(x=Cost_for_2_people))+ geom_density(adjust=1/2,color="midnightblue",fill="lightblue")+scale_x_continuous(breaks = seq(0,6000,1000)) + xlab("Cost for 2 people (in INR)") + ylab("Density") + ggtitle("Density Vs Cost graph")+ theme(plot.title = element_text(hjust = 0.5))

# Visualization for cuisines for cost 
working_rawset %>%
  filter(!str_detect(Cuisines, ',')) %>%
  ggplot(aes(Cuisines , y = Cost_for_2_people , fill = Cuisines)) + stat_summary(fun = "mean", geom = "col") + theme(axis.text.x = element_text(angle = 90), legend.position="none")+ ylab("Cost for 2 people (in INR)") +ggtitle("Cost Vs Cuisines")+ theme(plot.title = element_text(hjust = 0.5))

# Visualization for Cuisines per Price for two people in certain localities
working_rawset %>%
  filter(!str_detect(Cuisines, ',')) %>%
  filter(Locality=="BTM"|Locality=="Church Street"|Locality=="Indiranagar"|Locality=="Jayanagar"|Locality=="JP Nagar"|Locality=="MG Road") %>%
  ggplot(aes(x = Cuisines, y = Cost_for_2_people, color = Locality, size = Locality, shape=Locality )) + stat_summary(fun = "mean", geom = "point") + theme(axis.text.x = element_text(angle = 90, size=12), axis.title=element_text(size=14), axis.text.y = element_text(size=12), legend.text=element_text(size=12), legend.title = element_text(size=14)) + xlab("Cuisines") + ylab("Cost for 2 people (in INR)") + ggtitle("Cost Vs Cuisines") + theme(plot.title = element_text(size=18, face="bold", hjust = 0.5))


