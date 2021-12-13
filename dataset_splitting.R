library(mltools) #for one-hot encoding the categorical variables
library(data.table)

# Coursera dataset
coursera<-read.csv("/Users/narendraomprakash/Desktop/Narendra/Semester-V-FALL2021/Data Visualization/J-Component/coursera-course-detail-data.csv")

## Checking for null values
## 0 null values observed.
sum(is.na(coursera))

## Dropping url column-not required for model
coursera<-coursera[,-c(1,3)]

## Removing the courses with Name=None (i.e., courses without a page)
## 6 courses without page or Name=None
coursera<-coursera[!(coursera$Name=="None"),]

## Factorizing difficulty column:{None, Beginner Level, Intermediate Level, Advanced Level}
coursera$Difficulty.f<-factor(coursera$Difficulty,levels=c("None","Beginner Level","Intermediate Level","Advanced Level"),ordered=TRUE)


## Pre-processed datasert for Visualisation of Coursera Dataset
## Removing the unfactored column of difficulty
coursera_visualisation<-coursera[,-c(3)]
write.csv(coursera_visualisation,"/Users/narendraomprakash/Desktop/Narendra/Semester-V-FALL2021/Data Visualization/J-Component/coursera_visualisation.csv", row.names = FALSE)


## One-Hot Encoding the Difficulty categorical variable
coursera$Difficulty<-factor(coursera$Difficulty,levels=c("None","Beginner Level","Intermediate Level","Advanced Level"))
coursera_ml <- one_hot(as.data.table(coursera,cols=c("Difficulty")))

## Pre-processed dataset for ML model of Coursera dataset
coursera_ml<-coursera_ml[,-c(8)]
write.csv(coursera_ml,"/Users/narendraomprakash/Desktop/Narendra/Semester-V-FALL2021/Data Visualization/J-Component/coursera_recommendation.csv", row.names = FALSE)


## -------------------PRE-PROCESSING OF COURSERA DATASET ENDS --------------------------------------- ##

# Udemy Dataset
udemy<-read.csv("/Users/narendraomprakash/Desktop/Narendra/Semester-V-FALL2021/Data Visualization/J-Component/udemy_courses.csv")

## Checking for null values
## 0 null values observed.
sum(is.na(udemy))

## Dropping id,url and timestamp column,-not required for model
udemy<-udemy[,-c(1,3,11)]


## Factorizing is_paid,level,subject for visualisation
udemy$is_paid.f<-factor(udemy$is_paid,levels=c("True","False"))
udemy$level.f<-factor(udemy$level,levels=c("All Levels","Beginner Level","Intermediate Level","Expert Level"),ordered=TRUE)
udemy$subject.f<-factor(udemy$subject,levels=c("Business Finance","Graphic Design","Musical Instruments","Web Development"))

## Pre-processed Dataset for Visualisation of Udemy Courses
udemy_visualisation<-udemy[,-c(2,7,9)]
write.csv(udemy_visualisation,"/Users/narendraomprakash/Desktop/Narendra/Semester-V-FALL2021/Data Visualization/J-Component/udemy_visualisation.csv", row.names = FALSE)

## One-Hot Encoding the Categorical variables
udemy$is_paid<-factor(udemy$is_paid,levels=c("True","False"))
udemy$level<-factor(udemy$level,levels=c("All Levels","Beginner Level","Intermediate Level","Expert Level"))
udemy$subject<-factor(udemy$subject,levels=c("Business Finance","Graphic Design","Musical Instruments","Web Development"))

udemy_ml <- one_hot(as.data.table(udemy,cols=c("is_paid","level","subject")))

# Pre-processed Dataset for ML Model for Udemy Courses
udemy_ml<-udemy_ml[,-c(17,18,19,20,21,22,23)]

write.csv(udemy_ml,"/Users/narendraomprakash/Desktop/Narendra/Semester-V-FALL2021/Data Visualization/J-Component/udemy_recommendation.csv", row.names = FALSE)

## -------------------PRE-PROCESSING OF UDEMY DATASET ENDS --------------------------------------- ##
