# ========================= Read Data ============================================
getwd() # find the current directory
#reading the Dataset downloaded from Kaggle into a variable 
datafile<- read.csv("FIFA_19_Players.csv",stringsAsFactors = FALSE) # converts the vectors 
head(datafile,1)

# ========================== Descriptive Statistics ================================
#We use the table() function to calculate player_average the occurrences of unique values of a variable and display the frequency table of the categories of that variable.
#To print a frequency distribution of the "age" column.
freq.table <- table(datafile$age) 
freq.table


library("dplyr")
#data extraction with dplyr************
#1)Select Function
#To select the following columns
mycols <- select(datafile, short_name, age)
head(mycols, 5) 

#2)Filter Function
#To print the last 5 rows where age is 20
myrows <- filter(datafile, age == 20)

tail(myrows,5)

#3)Arrange Function
#To arrange age in ascending order and display only the age column

datafile %>% arrange(age) %>% select(age) 


#4)Group By Function
#To find the mean of capital.gain column grouped according to the "workclass" column 
datafile %>% group_by(club) %>% summarise(Mean= mean(weight_kg))


# =================== Missing Values =========================================

#checking missing values
is.na(datafile$nation_jersey_number)
sum(is.na(datafile$nation_jersey_number))

#filling the missing values with string 
datafile$nation_jersey_number <- ifelse(is.na(datafile$nation_jersey_number), 
                                        'Not played yet', datafile$nation_jersey_number)
is.na(datafile$nation_jersey_number)
#thus we replaced all the null values in nation_jersey_number


#we need to change -- to NA
datafile <- datafile %>%
  mutate(nation_position = replace(nation_position, nation_position ==  "-", NA))

head(datafile$nation_position,10)

##using mutate to add new column
datafile <- datafile %>%
  mutate(player_average= (as.numeric(pace)+as.numeric(shooting)+as.numeric(passing) +as.numeric(dribbling)+as.numeric(defending)+as.numeric(physic))/6)
head(datafile$player_average, 5)

#replacing nan values with goalkeeping states for average
datafile$player_average <- ifelse(is.na(datafile$player_average), 
                                  ((as.numeric(datafile$gk_diving)+	as.numeric(datafile$gk_handling)+as.numeric(datafile$gk_kicking)+as.numeric(datafile$gk_reflexes)	+as.numeric(datafile$gk_speed)+as.numeric(	datafile$gk_positioning))/6), datafile$player_average)
sum(is.na(datafile$player_average))

head(datafile$player_average, 5)



# =================== EDA ======================================================

class(datafile)
dim(datafile) #dimension
names(datafile)
object.size(datafile)
head(datafile)
tail(datafile)
summary(datafile)
str(datafile)


#returns mean. missing values are removed, if #na.rm=TRUE. 
mean(datafile$player_average, na.rm=TRUE)
median(datafile$player_average, na.rm=TRUE)


#returns range (minimum and maximum) of object. 
# missing values are removed, if #na.rm=TRUE. 
range(datafile$player_average,na.rm=TRUE)
 
quantile(datafile$player_average, probs=seq(0,1,0.25),na.rm=TRUE)
fivenum(datafile$player_average)




#Subsetting
#Subsetting data 
#Subsetting rows using filter
#install.packages("tidyr") #for pipe operator
library(tidyr)

#install.packages("dplyr") #for distinct function
library(dplyr)

#Extract rows that meet logical criteria overall >85

overall_85 <- filter(datafile, datafile$overall>85)
overall_85

#Extract rows that meet logical criteria age>30
age_30 <- filter(datafile, datafile$age>30)
age_30

#OR
age_301<- datafile %>% filter(age >30)
age_301

#Subsetting columns using select

select(datafile, short_name, age) 

#select columns which contain "+"
select(datafile, contains("+"))

#select columns ending with "g"
select(datafile, ends_with("g"))

#select columns starting with 'movement'
select(datafile, starts_with("movement")) 

#select columns except player_url and long_name
select(datafile, -player_url,-long_name) 

#Count number of rows
count(datafile, wt = overall) 

#group data by specific column
group_by(datafile, club) %>%  count()

datafile   %>%   group_by(club)   %>%   summarise(avg=mean(age)) 


#Reshaping data
library(dslabs)
str(datafile)

tidy_data <- datafile %>% 
 filter(age %in% c(30)) %>%
 select(sofifa_id,short_name,long_name, player_average,player_positions,nationality,club)
head(tidy_data,5)


new_tidy_data <- tidy_data %>%
 gather(key, value ,6,7)

new_tidy_data


# spread tidy data to generate wide data
new_wide_data <- new_tidy_data %>% spread(key,value)
View(new_wide_data)

#4)Separate and Unit
#the separate function splits one column into two or more columns 
#at a specified character that separates the variables.


# separate on underscores
seperated_tidy_data <- tidy_data %>% separate(long_name, c("first_name", "other_name"),fill="right", extra="merge")
seperated_tidy_data





# =============== Regression ===============================================

input <- datafile[,c("age","overall","potential","player_average")]
# Create the relationship model.
model <- lm(age~overall+potential+player_average, data = input)
# Show the model.
print(model)
# Get the Intercept and coefficients as vector
# elements.
a<- coef(model)[1]
xoverall<- coef(model)[2]
xpotential<- coef(model)[3]
xplayer_average<- coef(model)[4]

print(a)
print(xoverall)
print(xpotential)
print(xplayer_average)
newdata <- data.frame(overall=80,potential=95,player_average=87)
Y <- predict(model,newdata)
print(Y)



# ================================ Logistic Regression  ==============================

library(RWeka)
library(dslabs)
datafile$preferred_foot <- as.factor(datafile$preferred_foot)
print(datafile$preferred_foot)
trainingData <- datafile[1:7783,]
#print(trainingData)
testiningData <- datafile[7784:15566,]
#print(testiningData)
# # # train
modelLogiR <- Logistic(trainingData$preferred_foot ~ trainingData$height_cm+trainingData$weight_kg , data = trainingData)
print(modelLogiR)
# # #prediction on the testing data
predictions <- predict(modelLogiR, newdata = testiningData)
#print(predictions)
#predictions
# # get the confusion matrix - actual and predicted
table(testiningData$preferred_foot,predictions)
# #Accuracy calculations
cm = as.matrix(table(testiningData$preferred_foot,predictions))
# #total number of elements
n = sum(cm)
n 
# #diagonal elements - correctly classified
diag = diag(cm)
diag
# #Accuracy
accuracy = sum(diag) / n
# #Print accuracy
accuracy




# ====================== Data Visualisation =============================

#scatterplot: gives the idea of bivariate analysis
#i.e. how one variable changes with respect to changes in another variable

plot(datafile$overall ~ datafile$player_average,xlab="player_average", ylab = "overall", main="overall vs player_averagenitude", col="blue",pch=20)
#Histogram  :  Univariate Analysis

hist(datafile$overall, xlab = "overall", ylab = "Frequency", main = "Histogram of overall", col = "tomato")

#to know different colors provided by R plotting system
colors()

#Boxplot:  How a continuous variable changes with categorical variable

boxplot(datafile$overall ~datafile$player_average, xlab = "player_average", ylab = "overall", main="overall of player_averagenitude", col="rosybrown2")


#ggplot2
#install.packages("ggplot2")
library(ggplot2)
library(dplyr)


ggplot(data=datafile, aes(x= overall, y=player_average, colors='overall', shape=overall))+ geom_point()+  scale_shape_identity() #scale_shape for accepting integers
ggplot(data=datafile,aes(x=player_average))+ geom_histogram(bins=30, fill="palegreen3",color="green")

library("dslabs")

ggplot(data=datafile, aes(x=overall, y= player_average))+ geom_point()
ggplot(data=datafile, aes(x=overall, y= player_average))+ geom_point(alpha=0.1)

# change the size of the points
p <- datafile %>% ggplot()+
  geom_point(aes(x = overall, y = player_average), size=.1)+
  geom_text(aes(x = overall, y = player_average,label=player_average)) #label indicates the player_average
p


p1 <- datafile %>% ggplot()+
  geom_point(aes(x = overall, y = player_average), size=.1)+
  geom_text(aes(x = overall, y = player_average,label=ifelse(player_average>80, player_average,''))) #label indicates the player_average only above 100
p1

#Add labels and title
t <- datafile %>% ggplot(aes(overall, player_average))+
  geom_point(size = .3) +
  geom_text(aes(label=ifelse(player_average>110, player_average,''))) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("overall of Player (log scale)") +
  ylab("player_average (log scale)") +
  ggtitle("Overall vs Player_average")
t
#to change the color of points
t + geom_point(size = .3, color = "blue")

#Add a line with average player_average per overallnitude
# define average player rate
u <- datafile %>%
  summarise(rate_player_average = sum(player_average) / sum(overall) ) %>%
  pull(rate_player_average)

class(u)


# basic line with average player_average per overallnitude
t + geom_point( size = .3) +
  geom_abline(intercept = log10(u))    # slope is default of 1

# change line to dashed and dark grey, line under points
t + geom_abline(intercept = log10(u), lty = 2, color = "darkgrey") +
  geom_point(size = .3)



summary(datafile)


