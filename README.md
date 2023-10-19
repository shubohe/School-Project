# School-Project
R.studio school project
# read.csv
data <- read.csv("Video_Games_Sales_as_at_22_Dec_2016_upload.csv")

# delete NAs
data <- data[rowSums(is.na(data) | data == "") != ncol(data),]

# write.csv
write.csv(data, "Video_Games_Sales_as_at_22_Dec_2016_upload.csv", row.names = FALSE)



install.packages("ggplot2")
install.packages("dplyr")
install.packages("stargazer")
library(stargazer)
library(ggplot2)
library(dplyr)
install.packages("rmarkdown")
library(rmarkdown)
install.packages("tinytex")
tinytex::install_tinytex()

# 1st: Observe original data
Video.Game<-read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")
summary(Video.Game)
dim(Video.Game)
View(Video.Game)
sapply(Video.Game,class)
str(Video.Game)
stargazer(Video.Game, type="text")

# 2nd: Filter and sample data
VG.Sample <- read.csv("Video_Games_Sales_as_at_22_Dec_2016_upload.csv")
VG.Sample <- na.omit(VG.Sample)
VG_Summary<-summary(VG.Sample)
dim(VG.Sample)
View(VG.Sample)
attach(Video.Game)
attach(VG.Sample)
VG.df<-data.frame(VG.Sample)
stargazer(VG.Sample,title=VG_Summary,type="text")
stargazer(VG.Sample[1:10,],  type="text")

# Regression and Prediction Model Build
Original_model<-lm(Global_Sales~ Name+Year_of_Release+Genre+Publisher+NA_Sales+EU_Sales+JP_Sales+Other_Sales+Critic_Score+Critic_Count+User_Score+User_Count+Developer+Rating)
summary(Original_model) # overll original 


# Type change to be prepared for Numeical Prediction
Critic_Score<-as.numeric(VG.Sample$Critic_Score)
Critic_Count<-as.numeric(VG.Sample$Critic_Count)
User_Score<-as.numeric(VG.Sample$User_Score)
User_Count<-as.numeric(VG.Sample$User_Count)
class(Critic_Score)
class(Critic_Count)
class(User_Score)
class(User_Count)

summary(VG.Sample)


# Filtered Dara Model
linear_model <- lm(Global_Sales ~ User_Score+User_Count+Critic_Score+Critic_Count,data=VG.Sample)        
summary(linear_model)
linear_model2 <- lm(Global_Sales ~ Genre+User_Score+User_Count+Critic_Score+Critic_Count,data=VG.Sample)        
summary(linear_model2)

stargazer(VG.Sample, type="text")
stargazer(linear_model,type="text")
stargazer(VG.Sample[1:4,], summary=FALSE, rownames=FALSE, type="text")

@@@ 
# Convert "genre" variable to a factor with single level
VG.Sample$genre <- factor("Overall Genre", levels = unique(VG.Sample$genre))

# Use stargazer to output summary statistics or regression results
stargazer(VG.Sample, title = "Summary Statistics", type = "text")








### !!!!!
# summary(Video.Game$UserScore)# excel class,NULL >> Numbers 
# Publisher1<-as.factor(Publisher)
# summary(Publisher1) # filer first, 


linear_model5<-lm(Global_Sales~ UserScore,data=Video.Game)
plot(linear_model5)
summary(linear_model5)






linear_model1<-lm(NA_Sales~UserScore+UserCount+CriticScore+CriticCount,data=Video.Game)
linear_model2<-lm(EU_Sales~UserScore+UserCount+CriticScore+CriticCount,data=Video.Game)
linear_model3<-lm(JP_Sales~UserScore+UserCount+CriticScore+CriticCount,data=Video.Game)
linear_model4<-lm(Other_Sales~UserScore+UserCount+CriticScore+CriticCount,data=Video.Game)


par(mfrow=c(2,2))
plot(linear_model)
plot(linear_model1)
plot(linear_model2)
plot(linear_model3)
plot(linear_model4)


plot(Year_of_Release,Global_Sales, xlabel=Year_of_Release, ylabel=Global_Sales)



# sapply(Video.Game[,1:7],var)
par(mfrow=c(2,2))
ggplot(Video.Game, aes(x = Year_of_Release, y = NA_Sales)) +
  geom_point(alpha = 0.5, size = 1)
ggplot(Video.Game, aes(x = Year_of_Release, y = EU_Sales)) +
  geom_point(alpha = 0.5, size = 10)
ggplot(Video.Game, aes(x = Year_of_Release, y = JP_Sales)) +
  geom_point(alpha = 0.5, size = 1)
ggplot(Video.Game, aes(x = Year_of_Release, y = Other_Sales)) +
  geom_point(alpha = 0.5, size = 1)






linear_model2<-glm(Global_Sales ~ UserScore+UserCount+CriticScore+CriticCount,data=Video.Game)
summary(linear_model)

NA_model<-lm(NA_Sales ~ Platform+Year_of_Release+Genre+Publisher+Rating,data=Video.Game)        
summary(NA_model)
EU_model<-lm(EU_Sales ~ Platform+Year_of_Release+Genre+Publisher+Rating,data=Video.Game)        
summary(EU_model)
JP_model<-lm(JP_Sales ~ Platform+Year_of_Release+Genre+Publisher+Rating,data=Video.Game)        
summary(JP_model)
JP_model1<-lm(JP_Sales ~ Publisher,data=Video.Game)        
summary(JP_model1)
Other_model<-lm(Other_Sales ~ Platform+Year_of_Release+Genre+Publisher+Rating,data=Video.Game)        
summary(Other_model)
















levels(Video.Game$Publisher)

Video.Game <- Video.Game[order(Video.Game$Year_of_Release), ]
summary(Video.Game)



Video.Game$Platform <- factor(Video.Game$Platform)
Video.Game$Publisher <- factor(Video.Game$Publisher)


plot(Video.Game)
table(Video.Game$Year_of_Release)

Year.Sales<-Video.Game %>% 
  group_by(Year_of_Release) %>% 
  summarize(Total_Sales = sum(Global_Sales)) %>% 
  arrange(Year_of_Release) %>% 
  mutate(Total_Sales = sprintf("%.2f", Total_Sales)) %>% 
  print(n = Inf)



total_na_sales <- sum(Video.Game$NA_Sales, na.rm = TRUE)
total_na_sales
total_eu_sales <- sum(Video.Game$EU_Sales, na.rm = TRUE)
total_eu_sales
total_jp_sales <- sum(Video.Game$JP_Sales, na.rm = TRUE)
total_jp_sales
total_other_sales <- sum(Video.Game$Other_Sales, na.rm = TRUE)
total_other_sales
total_global_sales <- sum(Video.Game$Global_Sales, na.rm = TRUE)
total_global_sales


table(Video.Game$Platform)

Platform.sales<-Video.Game %>% 
  group_by(Platform) %>% 
  summarize(Total_Sales = sum(Global_Sales)) %>% 
  arrange(Platform) %>% 
  mutate(Total_Sales = sprintf("%.2f", Total_Sales)) %>% 
  print(n = Inf)









VG.Rep <- subset(Video.Game, select = -Rating)


sapply(Video.Game[,1:7],var)
par(mfrow=c(2,2))
ggplot(Video.Game, aes(x = Year_of_Release, y = NA_Sales)) +
  geom_point(alpha = 0.5, size = 1)
ggplot(Video.Game, aes(x = Year_of_Release, y = EU_Sales)) +
  geom_point(alpha = 0.5, size = 10)
ggplot(Video.Game, aes(x = Year_of_Release, y = JP_Sales)) +
  geom_point(alpha = 0.5, size = 1)
ggplot(Video.Game, aes(x = Year_of_Release, y = Other_Sales)) +
  geom_point(alpha = 0.5, size = 1)

# Fit linear regression model
JP_model2 <- lm(JP_Sales ~ Publisher, data = Video.Game)
# Get coefficient table
coef_table <- summary(JP_model2)$coef
# Filter for Nintendo category
nintendo_coefs <- coef_table[rownames(coef_table) == "PublisherNintendo", ]
# Print coefficient table for Nintendo category
print(nintendo_coefs)
# the publisher in their country may not correlated 


subset.data.frame()
par(mfrow=c(2,2))
pairs(Auto[,1:7])

Video.Game[is.na(Video.Game)] <- 0
tail(Video.Game,10)
summary(Video.Game)
scatt.plot(Video.Game)










Video.Game_subset <- Video.Game[, -which(names(Video.Game) == "Critic_Count")]
