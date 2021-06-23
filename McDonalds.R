#Hackathon 

setwd("C:/Users/mchoudhu/Documents/********")
library(readxl)
mcd=read.csv("Mcdonald+.csv",header=TRUE)
summary(mcd)
attach(mcd)

#1
a = table(Category) #frequencytable
a

barplot(a, 
        xlab = "Category", 
        ylab = "Variety (Number of Items in a category)", col = "Pink", 
        main = "Depiction of variety within each category")
  #The categories with the highest and lowest of varieties can be easily spotted.

#2
boxplot(mcd[,c(4:24)], xlab="Variable", ylab = "Value", col = 'Pink', plot = TRUE, varwidth = TRUE, main="All attributes")
  #We can see that most variables have outliers, except 3, which are Saturated Fat, Saturated Fat (% Daily Value) and Dietary Fiber

#3.
install.packages("corrplot")
library(corrplot)
corrplot(cor(mcd[,4:24]),order = "hclust",type = "upper",col = c("white","black"), bg = "lightblue")
 #The items with bigger circles of black have the most correlation


#4
install.packages("dplyr")
library(dplyr)
chol = mcd %>% group_by(Category) %>% summarise(Cholesterol....Daily.Value.=mean(Cholesterol....Daily.Value.))
chol = data.frame(chol)
library(ggplot2)
my_palette <- c("#00abbd", "#055499", "#132241", "#9bca3c", "#c3c3c3",
                "#ff5a00", "#e91365", "#ff921f","#cc0000")
ggplot(chol, aes(x=Category, y= Cholesterol....Daily.Value., fill= Category, label=Cholesterol....Daily.Value.))+
  geom_bar(stat = "identity",width = 0.5)+coord_flip()+theme_bw()+
  scale_fill_manual(values = my_palette)+
  theme(legend.position = "none")+
  xlab("")+ylab("Percentage of Cholestoral in a diet (Daily Intake)")+ggtitle("Category vs. % of Cholestoral in a diet (% daily value)")
#The categories have been depicted with their mean % cholestoral in a diet


#5

  sodintake=data.frame(head(sort(Sodium,decreasing = TRUE),10))
  sodintake
  
  
  Sod <- data.frame(head(sort(Sodium,decreasing = TRUE),10))
  #head(mcd,10)
  
  
  newdata <- head(mcd[order(-Sodium),],10) 


ggplot(newdata, aes(x=Item, y= Sodium, fill= Category, label=Sodium))+
  geom_bar(stat = "identity")+coord_flip()+theme_bw()+
  scale_fill_manual(values = my_palette)+theme(legend.position = "top")+
  geom_text(hjust=1.3, colour = "white")+
  xlab("")+
  ggtitle("Top 20 products with the most Sodium")
#The items with top 10 values of Sodium component have been plotted here

#6
newdatasfat = head(mcd[order(-Saturated.Fat),],4)

ggplot(newdatasfat, aes(x=Item, y= Saturated.Fat, fill= Category, label=Saturated.Fat))+
  geom_bar(stat = "identity",width = 0.4)+facet_wrap(~ Category, scales = "free_x")+theme_bw()+
  scale_fill_manual(values = my_palette)+theme(legend.position = "top")+
  geom_text(hjust=0, colour = "white", nudge_x = -.03,nudge_y = -10)+
  xlab("")+theme(legend.position = "none")+ylab("Level of Saturated Fat")+
  ggtitle("The 4 Food Items with the most amount of Saturated Fat")

#The top 4 food items in terms of saturated fat have been shown here along with the Category they belong to.







