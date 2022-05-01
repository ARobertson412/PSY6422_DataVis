#Load necessary packages for the subsequent script
library(here)
library(tidyverse)
library(ggExtra)
library(dplyr)
library(plotly)
library(gganimate)
library(gifski)
library(png)

#Load data into workspace
Data <- read.csv(here("Data", "VideoGameSales2016.csv"))

#Check it loaded correctly
head(Data)

#Create new dataframe containing only Name, Critic_Score, User_Score and Global_Sales
Data2 <- cbind(Data[c("Name", "Global_Sales","Critic_Score", "User_Score")])

#Check it has loaded using the head() function
head(Data2)

#Check what class of data critic and user scores are stored as
class(Data2$Critic_Score)
class(Data2$User_Score)

#Convert both user and critic scores to numeric
Data2$Critic_Score <- as.numeric(Data2$Critic_Score)
Data2$User_Score <- as.numeric(Data2$User_Score) #This occasionally flags a warning in R, but has no effect so can be ignored

#Check the number of complete cases which have values for critic and user score both individually and together
length(na.omit(Data2$Critic_Score))
length(na.omit(Data2$User_Score))
length(na.omit(Data2$Critic_Score & Data2$User_Score))

#Convert the User_Score values into the same scale as the Critic_Score
Data2 <- Data2 %>%
  mutate(User_Score = User_Score * 10)
#The head() function checks that this has been done correctly
head(Data2)

#Create a new variable containing the average review score of critics and users for a given game
Data2$Overall_Score <- (Data2$User_Score + Data2$Critic_Score) / 2

#Create a smoothed line graph of review scores and global sale figures
g <- ggplot(data = na.omit(subset(Data2, Overall_Score >20 & Overall_Score <95)), aes(x = Data2$Overall_Score, y = Data2$Global_Sales))
g2 <- g + geom_smooth(aes(x = Overall_Score, y = Global_Sales), method = "loess",colour = 'darkred', size = .7, se = FALSE) +
  #Limit xaxis
  xlim(0, 100)+
  #Create title and axis labels
  labs(x = "Review Score", y = "Global sales (units/millions)", title = "Game Review Scores and Global Sales")+
  #Specify title text size and location
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))+
  #Change axis text
  theme(axis.text = element_text(color = 'black', size = 6))+
  #Change background to white
  theme(panel.background = element_rect(fill = 'white'))

#View plot
g2

#Save plot
ggsave(here("figs", "fig1.png"), g2, width = 5, height = 3.5)

#Create a joint scatter and histogram plot using the ggExtra package
#This compared the global unit sales of a game with the overall review score
#Specify dataframe and axes
plot_center = ggplot(Data2, aes(x = Overall_Score, y = Global_Sales))+ 
  #Create scatterplot and specify point colour and size
  geom_point(color = 'darkred', alpha = .3, size = .8, stroke = 0)+ 
    #Create title and axis labels
    labs(x = "Review Score", y = "Global sales (units/millions)", title = "Game Review Scores and Global Sales")+
    #Specify title text size and location
    theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))+
    #Change axis text
    theme(axis.text = element_text(color = 'black', size = 6))+
    #Change background to white
    theme(panel.background = element_rect(fill = 'white'))

#joins a histogram to the above scatterplot and saves to figs folder
ggsave(here("figs", file = "fig2.png"),ggMarginal(plot_center, type = "histogram", color = 'black', fill = 'darkred'), width = 5, height = 3.5) 

#Examining the range, mean and SD of the global sale data
range(Data2$Global_Sales)
mean(Data2$Global_Sales)
sd(Data2$Global_Sales)

#Determining the average +/- 1SD to set the axis limits on future graphs
mean(Data2$Global_Sales)+sd(Data2$Global_Sales)

#Joint plot as above, restricting shown data to global sales less than 2.1 million units
plot_center = ggplot(data = subset(Data2, Global_Sales < 2.1), aes(x = Overall_Score, y = Global_Sales))+ 
  #Create scatterplot and specify point colour and size
  geom_point(color = 'darkred', alpha = .3, size = .8, stroke = 0)+ 
    #Create title and axis labels
    labs(x = "Review Score", y = "Global sales (units/millions)", title = "Game Review Scores and Global Sales")+
    #Specify title text size and location
    theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))+
    #Change axis text
    theme(axis.text = element_text(color = 'black', size = 6))+
    #Change background to white
    theme(panel.background = element_rect(fill = 'white'))

#joins a histogram to the above scatterplot and saves to figs folder
ggsave(here("figs", file = "fig3.png"),ggMarginal(plot_center, type = "histogram", 
                          color = 'black', fill = 'darkred'), width = 5, height = 3.5) 


#Create an interactive scatterplot of the overall/user/critic vs global sales
intplot <- plot_ly(data = Data2, x = Data2$Overall_Score, y = Data2$Global_Sales, 
                   #Add hover labels to individual points on the plot
                   text = ~paste("Review Score: ", Data2$Overall_Score, 
                                 "<br>Global Sales: ", Data2$Global_Sales, 
                                 "<br>Game: ", Data2$Name), 
                          hoverinfo = "text",
            #Specify type of plot
            type = "scatter", mode = "markers", 
            #Specify point aesthetics
            marker = list(size = 4,
                          color = 'rgba(170, 30, 15, .7)',
                          line = list(color = 'rgba(100, 30, 15, .7)',
                                      width = 1)))
#Add title and axis labels
intplot <- intplot %>%
  layout(title = "Global Sales and Review Scores",
         xaxis = list(title = "Review Score", range = c(0, 100)),
         yaxis = list(title = "Global Sales (per million units)", range = c(0, 85)),
         font = list(color = '#21130d',
                     family = 'sans serif',
                     size = 14))
#Format graph background
intplot <- intplot %>%
  layout(plot_bgcolor = 'rgb(254, 254, 254)') %>%
  layout(paper_bgcolor = 'rgb(254, 254, 254)') %>%
  layout(xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE)) %>%
  layout(margin = list(b = 50, l = 50, r = 50, t = 50))

#Display interactive plot
intplot


#Animate the smoothed g2 plot created earlier (line 45)
anim <- g2 +
  transition_reveal(Overall_Score)

#View animation - nframe and duration specifications allow running without it error-ing and crashing
animate(anim, nframes = 35, duration = 7)

