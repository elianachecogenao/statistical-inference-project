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

