#Import Dataset

edudata <- read.csv("C:/Users/elian/OneDrive/Escritorio/Statistical Inference/Project/highered_00002.csv.gz")
View(edudata)

#Adjusting Salary Column into x hundreds of thousands of US dollars

edudata$SALARY <- edudata$SALARY/1000

#Plots of for Age and Salary

hist(edudata$AGE, main = "Age of Graduate Students", xlab =  "Age" )
hist(edudata$SALARY, main = "Annual Salary of Graduate Students", xlab = "Annual Salary (USD$1,000)")

#Plot for trimmed Salary
hist(edudata$SALARY[edudata$SALARY<=1500], main = "Annual Salary of Graduate Students that Earn USD$150,000 or Less", xlab = "Annual Salary (USD$1,000)")

#Creating New Column for Gender with Labels

edudata$genderlabel[edudata$GENDER==1] <- "Female"
edudata$genderlabel[edudata$GENDER==2] <- "Male"

barplot(table(edudata$genderlabel), main = "Gender of Graduate Students", xlab = "Gender", ylab = "Frequency")

#Creating New Column for Race with Labels and Barplo

edudata$racelabel[edudata$RACETH==1] <- "Asian"
edudata$racelabel[edudata$RACETH==2] <- "White"
edudata$racelabel[edudata$RACETH==3] <- "Under-repressented minorities"

barplot(table(edudata$racelabel), main = "Race of Graduate Students", xlab = "Race", ylab = "Frequency")

#Creating New Column for Type of Degree with Labels and Barplot

edudata$degreelabel[edudata$DGRDG==1] <- "Bachelor"
edudata$degreelabel[edudata$DGRDG==2] <- "Master"
edudata$degreelabel[edudata$DGRDG==3] <- "Doctorate"
edudata$degreelabel[edudata$DGRDG==4] <- "Professional Certificate"

barplot(table(edudata$degreelabel), main = "Type of Degree of Graduate Students", xlab = "Type of Degree", ylab = "Frequency")

#Creating new column for Citizenship status and barplot

edudata$citizenlabel[edudata$CTZUSIN==0] <- "Not Citizen"
edudata$citizenlabel[edudata$CTZUSIN==1] <- "Citizen"

barplot(table(edudata$citizenlabel), main = "Citizenship Status of Graduate Students", xlab = "Citizenship Status", ylab = "Frequency")

#Values 9999998 and 9999999 in Salary are Logical skipt and Missing data respectively
#Eliminating outliers from Salary by creating a subset of original database

edudata2 <- subset(edudata, SALARY < 9999.998)

#Value 96 in age means missing
#Eliminating outliers from Age by creating a subset of edudata2

edudata3 <- subset(edudata2, AGE <= 96)

#Ploting all histograms and barplots again with new dataset edudata3

hist(edudata3$AGE, main = "Age of Graduate Students", xlab =  "Age" )
hist(edudata3$SALARY, main = "Annual Salary of Graduate Students", xlab = "Annual Salary (USD$1,000)")

#Creating New Column for Gender with Labels

edudata3$genderlabel[edudata3$GENDER==1] <- "Female"
edudata3$genderlabel[edudata3$GENDER==2] <- "Male"

barplot(table(edudata3$genderlabel), main = "Gender of Graduate Students", xlab = "Gender", ylab = "Frequency")

#Creating New Column for Race with Labels and Barplo

edudata3$racelabel[edudata3$RACETH==1] <- "Asian"
edudata3$racelabel[edudata3$RACETH==2] <- "White"
edudata3$racelabel[edudata3$RACETH==3] <- "Under-repressented minorities"

barplot(table(edudata3$racelabel), main = "Race of Graduate Students", xlab = "Race", ylab = "Frequency")

#Creating New Column for Type of Degree with Labels and Barplot

edudata3$degreelabel[edudata3$DGRDG==1] <- "Bachelor"
edudata3$degreelabel[edudata3$DGRDG==2] <- "Master"
edudata3$degreelabel[edudata3$DGRDG==3] <- "Doctorate"
edudata3$degreelabel[edudata3$DGRDG==4] <- "Professional Certificate"

barplot(table(edudata3$degreelabel), main = "Type of Degree of Graduate Students", xlab = "Type of Degree", ylab = "Frequency")

#Creating new column for Citizenship status and barplot

edudata3$citizenlabel[edudata3$CTZUSIN==0] <- "Not Citizen"
edudata3$citizenlabel[edudata3$CTZUSIN==1] <- "Citizen"

barplot(table(edudata3$citizenlabel), main = "Citizenship Status of Graduate Students", xlab = "Citizenship Status", ylab = "Frequency")

#Analyzing statistics of Age

mean(edudata3$AGE)
median(edudata3$AGE)
sd(edudata3$AGE)
variance_age <- sd(edudata3$AGE)*sd(edudata3$AGE)
variance_age

#Analyzing statistics of Salary

mean(edudata3$SALARY)
median(edudata3$SALARY)
sd(edudata3$SALARY)
variance_salary <- sd(edudata3$SALARY)*sd(edudata3$SALARY)
variance_salary

#Evaluation of linear correlation of Age and Salary

plot(SALARY~AGE, data = edudata3, xlab = "Age", ylab = "Annual Salary (Thousands of USD)", main = "Annual Salary by Age of Graduate Students", col="gray")
abline(v=mean(edudata3$AGE), col="blue")
abline(h=mean(edudata3$SALARY), col="blue")

abline(a=mean(edudata3$SALARY)-1.5*mean(edudata3$AGE), b=1.5, col= "red")
abline(a=mean(edudata3$SALARY)+1.5*mean(edudata3$AGE), b=-1.5, col= "red")

cor(edudata3$AGE,edudata3$SALARY)

#Creating subsets of edudata3 by type of degree

bachelors  <- subset(edudata3, DGRDG==1)
masters    <- subset(edudata3, DGRDG==2)
doctorates <- subset(edudata3, DGRDG==3)
profcert   <- subset(edudata3, DGRDG==4)

#Evaluating linear correlation of Age and Salary for bachelors

plot(SALARY~AGE, data = bachelors, xlab = "Age", ylab = "Annual Salary (Thousands of USD)", main = "Annual Salary by Age", col="gray")
abline(v=mean(bachelors$AGE), col="blue")
abline(h=mean(bachelors$SALARY), col="blue")

abline(a=mean(bachelors$SALARY)-1.5*mean(bachelors$AGE), b=1.5, col= "red")
abline(a=mean(bachelors$SALARY)+1.5*mean(bachelors$AGE), b=-1.5, col= "red")

cor(bachelors$AGE,bachelors$SALARY)

#Evaluating linear correlation of Age and Salary for masters

plot(SALARY~AGE, data = masters, xlab = "Age", ylab = "Annual Salary (Thousands of USD)", main = "Annual Salary by Age", col="gray")
abline(v=mean(masters$AGE), col="blue")
abline(h=mean(masters$SALARY), col="blue")

abline(a=mean(masters$SALARY)-1.5*mean(masters$AGE), b=1.5, col= "red")
abline(a=mean(masters$SALARY)+1.5*mean(masters$AGE), b=-1.5, col= "red")

cor(masters$AGE,masters$SALARY)

#Evaluating linear correlation of Age and Salary for doctorates

plot(SALARY~AGE, data = doctorates, xlab = "Age", ylab = "Annual Salary (Thousands of USD)", main = "Annual Salary by Age", col="gray")
abline(v=mean(doctorates$AGE), col="blue")
abline(h=mean(doctorates$SALARY), col="blue")

abline(a=mean(doctorates$SALARY)-1.5*mean(doctorates$AGE), b=1.5, col= "red")
abline(a=mean(doctorates$SALARY)+1.5*mean(doctorates$AGE), b=-1.5, col= "red")

cor(doctorates$AGE,doctorates$SALARY)

#Evaluating linear correlation of Age and Salary for professional certificates

plot(SALARY~AGE, data = profcert, xlab = "Age", ylab = "Annual Salary (Thousands of USD)", main = "Annual Salary by Age", col="gray")
abline(v=mean(profcert$AGE), col="blue")
abline(h=mean(profcert$SALARY), col="blue")

abline(a=mean(profcert$SALARY)-1.5*mean(profcert$AGE), b=1.5, col= "red")
abline(a=mean(profcert$SALARY)+1.5*mean(profcert$AGE), b=-1.5, col= "red")

cor(profcert$AGE,profcert$SALARY)

#95% Confidence Interval of Mean Age

minmeanage <- mean(edudata3$AGE)+qnorm(0.05/2)*sd(edudata3$AGE)/sqrt(nrow(edudata3))
maxmeanage <- mean(edudata3$AGE)-qnorm(0.05/2)*sd(edudata3$AGE)/sqrt(nrow(edudata3))
minmeanage
maxmeanage

#95% Confidence Interval of Mean Salary

minmeansalary <- mean(edudata3$SALARY)+qnorm(0.05/2)*sd(edudata3$SALARY)/sqrt(nrow(edudata3))
maxmeansalary <- mean(edudata3$SALARY)-qnorm(0.05/2)*sd(edudata3$SALARY)/sqrt(nrow(edudata3))
minmeansalary
maxmeansalary

#Graph of confidence interval Mean Age

hist(edudata3$AGE, xlim= c(15,85), main = "Age of Graduate Students", xlab =  "Age" )
abline(v= mean(edudata3$AGE), col="red")
abline(v= minmeanage, col = "blue")
abline(v= maxmeanage, col = "blue")

#Graph of confidence interval Mean Salary 

hist(edudata3$SALARY, xlim = c(-10,160),main = "Annual Salary of Graduate Students", xlab = "Annual Salary (USD$1,000)")
abline(v= mean(edudata3$SALARY), col="red")
abline(v= minmeansalary, col = "blue")
abline(v= maxmeansalary, col = "blue")

#I'm creaing a linear regression for salary. In order to do so, I'll subset the dataframe for only people who are employed
#Also taking out observations with logical skip for job satisfaction

edudata4 <- subset(edudata3, LFSTAT == 1)
edudata5 <- subset(edudata4, JOBSATIS != 98)

#Creating dummy variable for gender
#Notice Gender = 1 when female, and = 2 when male

edudata5$male <- edudata5$GENDER - 1

#Creating dummy variable for highest degree earned (combining masters and phd)

edudata5$graduate[edudata5$DGRDG >=2] <- 1
edudata5$graduate[edudata5$DGRDG <2] <- 0

#Creating dummy variables for race

edudata5$minority[edudata5$RACETH==3] <- 1
edudata5$minority[edudata5$RACETH!=3] <- 0

#Creating dummy variables for job satisfaction
#1 = very satisfied, 2= somewhat satisfied, 3= somewhat dissatisfied, 4= very dissatisfied

edudata5$satisfied[edudata5$JOBSATIS <= 2] <- 1
edudata5$satisfied[edudata5$JOBSATIS  > 2] <- 0

#creating linear regression

linearmodel <- lm(SALARY~AGE+male+CTZUSIN+graduate+minority+satisfied , data = edudata5)
summary(linearmodel)

linearmodel2 <- lm(SALARY~AGE+male+graduate+minority+satisfied , data = edudata5)
summary(linearmodel2)

#Graph residuals

hist(linearmodel$residuals, main="Linear Model Residuals", xlab = "Residuals")

plot(linearmodel$residuals~edudata5$SALARY, main="Salary vs. Residuals", xlab="Salary (USD$1000)", ylab = "Residuals", col="gray")

cor(edudata5$SALARY,linearmodel$residuals)

#Hypothesis testing

#Claim 1: The mean salary for women is less than the mean salary for men

#Creating subsets for men and women (using dummy variables from previous analysis)

mendata   <- subset(edudata5, male==1)
womendata <- subset(edudata5, male==0)

mendata_size   <- nrow(mendata)
womendata_size <- nrow(womendata)

mendata_mean   <- mean(mendata$SALARY)
womendata_mean <- mean(womendata$SALARY)

mendata_sd   <- sd(mendata$SALARY)
womendata_sd <- sd(womendata$SALARY)

#Assuming the population standard deviation for men and women's salary is different:

df1 <- ((mendata_sd^2)/mendata_size + (womendata_sd^2)/womendata_size)/(((mendata_sd^2)/mendata_size)/(mendata_size-1) + ((womendata_sd^2)/womendata_size)/(womendata_size-1))
test_statistic1 <- (womendata_mean -  mendata_mean)/sqrt((mendata_sd^2)/mendata_size + (womendata_sd^2)/womendata_size)

#P-value

pt(test_statistic1,df1)

#Claim 2: the proportion of men and women who graduated with a bachelor degree is the same (0.5)

#Creating subset of bachelor degree

bachelordegree <- subset(edudata5, DGRDG==1)
bachelordegree_size <- nrow(bachelordegree)

proportion_women <- nrow(subset(bachelordegree,male==0))/bachelordegree_size

test_statistic2 <- (proportion_women - 0.5)/sqrt((0.5*(1 - 0.5))/bachelordegree_size)

#P-value

pnorm(test_statistic2)

#Claim 3: Race has no influence in salaries (mean salary is equal for all races)

edudata5$race[edudata5$RACETH== 2] <- "White"
edudata5$race[edudata5$RACETH== 1] <- "Asian"
edudata5$race[edudata5$RACETH== 3] <- "Minorities"

#Descriptive statistics

asian_mean       <-mean(subset(edudata5, RACETH==1)$SALARY)
white_mean       <-mean(subset(edudata5, RACETH==2)$SALARY)
minorities_mean  <-mean(subset(edudata5, RACETH==3)$SALARY)

asian_size      <- nrow(subset(edudata5, RACETH==1))
white_size      <- nrow(subset(edudata5, RACETH==2))
minorities_size <- nrow(subset(edudata5, RACETH==3))

#Creating mx2 (categories and data) dataset for ANOVA

salaries <-data.frame(edudata5$race, edudata5$SALARY)
View(salaries)

library(plyr)
salaries <- rename(salaries, c("edudata5.race" = "Race", "edudata5.SALARY" = "Salary"))

#ANOVA
salaries_aov = aov(Salary ~ Race, data = salaries)
summary(salaries_aov)