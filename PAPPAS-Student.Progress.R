library(readxl)
install.packages("tidyverse")
library(tidyverse)
install.packages("pheatmap")
grades <- file.choose()
#Now,lets import the excel file we need to work on
students_grades<-read_excel(grades)
View(students_grades)
#Firstly,lets adjust a bit the dataset
colnames(students_grades)[1:2]<-c("Exams",
                                  "Repeated Exams")
View(students_grades)
colnames(students_grades)[4:7]<-c("Homework Assignments-1",
                                  "Homework Assignments-2",
                                  "Homework Assignments-3",
                                  "Homework Assignments-4")
View(students_grades)
#Also, we can do a bit of data cleaning by removing unnecessary rows and columns
colnames(students_grades)[10:17]<-c("Compulsory Activities-1",
                                  "Compulsory Activities-2",
                                  "Compulsory Activities-3",
                                  "Compulsory Activities-4",
                                  "Compulsory Activities-5",
                                  "Compulsory Activities-6",
                                  "Compulsory Activities-7",
                                  "Compulsory Activities-8")
View(students_grades)

colnames(students_grades)[19:28]<-c("Optional Activities-1",
                                    "Optional Activities-2",
                                    "Optional Activities-3",
                                    "Optional Activities-4",
                                    "Optional Activities-5",
                                    "Optional Activities-6",
                                    "Optional Activities-7",
                                    "Optional Activities-8",
                                    "Optional Activities-9",
                                    "Optional Activities-10")

View(students_grades)

#Now,we will remove columns with the missing values
students_grades<-students_grades[,-c(3,8,9,18)]
View(students_grades)

#Also,we will delete row one because it is not important for our data by the time we have set the correct titles
students_grades<-students_grades[-1,]
View(students_grades)

#Now,lets check for unique values on our dataset
unique(students_grades$Exams)



#Now lets work on "-1" and "-" values
#Firstly, lets do it for "-1"
students_grades[students_grades==-1]<-NA
View(students_grades)
#Secondly, lets do it for "-"
students_grades[students_grades=="-"]<-NA
View(students_grades)


#Now, lets work on filtering the missing data
#Firstly,lets check after the modifications how many NA columns do we have in this dataset

# Filter students based on exam participation
first_time_students <- students_grades %>%
  filter(!is.na(Exams))

repeated_exam_students <- students_grades %>%
  filter(!is.na(`Repeated Exams`))

no_show_students <- students_grades %>%
  filter(is.na(Exams) & is.na(`Repeated Exams`))

Both_exam_students<-students_grades %>%
  filter(!is.na(Exams) & !is.na(`Repeated Exams`))


# Print counts
cat("Students who took the exam for the first time:", nrow(first_time_students), "\n")
cat("Students who took the repeated exam:", nrow(repeated_exam_students), "\n")
cat("Students who did not show up for either exam:", nrow(no_show_students), "\n")
cat("Students who gave the exam two times:",nrow(Both_exam_students))

# Add the binary.1 column
library(dplyr)

students_grades <- students_grades %>%
  mutate(
    binary.1 = ifelse(
      # Condition 1: Participated in either Exams or Repeated Exams
      (!is.na(Exams) | !is.na(`Repeated Exams`)) &
        # Condition 2: No missing values in Compulsory Activities
        rowSums(across(starts_with("Compulsory Activities"), is.na)) == 0 &
        # Condition 3: No missing values in Homework Assignments
        rowSums(across(starts_with("Homework Assignments"), is.na)) == 0,
      1, 0  # Assign 1 if conditions met, else 0
    )
  )

#Now lets make some conclusions from the binary.1 column
nrow(students_grades[students_grades$binary.1==1,])
nrow(students_grades[students_grades$binary.1 == 1 & is.na(students_grades$Exams) & students_grades$`Repeated Exams` >= 5, ])
nrow(students_grades[students_grades$binary.1 == 1 & is.na(students_grades$`Repeated Exams`) & students_grades$Exams >= 5, ])
nrow(students_grades[students_grades$binary.1 ==1 & is.na(students_grades$Exams) & is.na(students_grades$`Repeated Exams`),])
View(students_grades)

#Now lets create the binary.2 column only for the optional activities.if a student has done half of them(5), then in the column it will be inserted the value of 1
#else(<5) it will be inserted the value of zero
library(dplyr)

students_grades <- students_grades %>%
  mutate(
    binary.2 = ifelse(
      # Check if the student has done at least 5 optional activities
      rowSums(across(starts_with("Optional Activities"), ~ !is.na(.))) >= 5 &
        # Check if the student participated in either Exams or Repeated Exams
        (!is.na(Exams) | !is.na(`Repeated Exams`)),
      1, 0  # Assign 1 if conditions met, otherwise 0
    )
  )

library(dplyr)

# Number of rows where binary.2 == 1
nrow(students_grades %>% filter(binary.2 == 1))

# Number of rows where binary.2 == 1 and Exams >=5 (with NA handling)
nrow(students_grades %>% filter(binary.2 == 1, !is.na(Exams), Exams >= 5))

# Number of rows where binary.2 == 1 and Repeated Exams >= 5 (with NA handling)
nrow(students_grades %>% filter(binary.2 == 1, !is.na(`Repeated Exams`), `Repeated Exams` >=5))

View(students_grades)
sum(nrow(students_grades[students_grades$binary.2==1,]))
sum(nrow(students_grades[ !is.na(students_grades$Exams) & students_grades$Exams>=5,]))

#Now lets convert all 24 columns to numeric
students_grades[]<-lapply(students_grades, as.numeric)
View(students_grades) #Now the dataset contains numeric columns

sum(nrow(students_grades[ !is.na(students_grades$Exams)   &    students_grades$Exams>=5,]))
sum(nrow(students_grades[!is.na(students_grades$`Repeated Exams`) & students_grades$`Repeated Exams`>=5,]))

#Now lets find the mean for each column and assign the NA value to it for each column
students_grades$Exams[is.na(students_grades$Exams)]<-mean(students_grades$Exams, na.rm=TRUE)
students_grades$`Repeated Exams`[is.na(students_grades$`Repeated Exams`)]<-mean(students_grades$`Repeated Exams`, na.rm=TRUE)
students_grades$`Homework Assignments-1`[is.na(students_grades$`Homework Assignments-1`)]<-mean(students_grades$`Homework Assignments-1`, na.rm=TRUE)
students_grades$`Homework Assignments-2`[is.na(students_grades$`Homework Assignments-2`)]<-mean(students_grades$`Homework Assignments-2`, na.rm=TRUE)
students_grades$`Homework Assignments-3`[is.na(students_grades$`Homework Assignments-3`)]<-mean(students_grades$`Homework Assignments-3`, na.rm=TRUE)
students_grades$`Homework Assignments-4`[is.na(students_grades$`Homework Assignments-4`)]<-mean(students_grades$`Homework Assignments-4`, na.rm=TRUE)
students_grades$`Compulsory Activities-1`[is.na(students_grades$`Compulsory Activities-1`)]<-mean(students_grades$`Compulsory Activities-1`,na.rm=TRUE)
students_grades$`Compulsory Activities-2`[is.na(students_grades$`Compulsory Activities-2`)]<-mean(students_grades$`Compulsory Activities-2`, na.rm=TRUE)
students_grades$`Compulsory Activities-3`[is.na(students_grades$`Compulsory Activities-3`)]<-mean(students_grades$`Compulsory Activities-3`, na.rm=TRUE)
students_grades$`Compulsory Activities-4`[is.na(students_grades$`Compulsory Activities-4`)]<-mean(students_grades$`Compulsory Activities-4`, na.rm=TRUE)
students_grades$`Compulsory Activities-5`[is.na(students_grades$`Compulsory Activities-5`)]<-mean(students_grades$`Compulsory Activities-5`, na.rm=TRUE)
students_grades$`Compulsory Activities-6`[is.na(students_grades$`Compulsory Activities-6`)]<-mean(students_grades$`Compulsory Activities-6`, na.rm=TRUE)
students_grades$`Compulsory Activities-7`[is.na(students_grades$`Compulsory Activities-7`)]<-mean(students_grades$`Compulsory Activities-7`, na.rm=TRUE)
students_grades$`Compulsory Activities-8`[is.na(students_grades$`Compulsory Activities-8`)]<-mean(students_grades$`Compulsory Activities-8`, na.rm=TRUE)

#Now lets work on optional activities
students_grades$`Optional Activities-1`[is.na(students_grades$`Optional Activities-1`)]<-mean(students_grades$`Optional Activities-1`,na.rm=TRUE)
students_grades$`Optional Activities-2`[is.na(students_grades$`Optional Activities-2`)]<-mean(students_grades$`Optional Activities-2`, na.rm=TRUE)
students_grades$`Optional Activities-3`[is.na(students_grades$`Optional Activities-3`)]<-mean(students_grades$`Optional Activities-3`, na.rm=TRUE)
students_grades$`Optional Activities-4`[is.na(students_grades$`Optional Activities-4`)]<-mean(students_grades$`Optional Activities-4`, na.rm=TRUE)
students_grades$`Optional Activities-5`[is.na(students_grades$`Optional Activities-5`)]<-mean(students_grades$`Optional Activities-5`, na.rm=TRUE)
students_grades$`Optional Activities-6`[is.na(students_grades$`Optional Activities-6`)]<-mean(students_grades$`Optional Activities-6`, na.rm=TRUE)
students_grades$`Optional Activities-7`[is.na(students_grades$`Optional Activities-7`)]<-mean(students_grades$`Optional Activities-7`, na.rm=TRUE)
students_grades$`Optional Activities-8`[is.na(students_grades$`Optional Activities-8`)]<-mean(students_grades$`Optional Activities-8`, na.rm=TRUE)
students_grades$`Optional Activities-9`[is.na(students_grades$`Optional Activities-9`)]<-mean(students_grades$`Optional Activities-9`,na.rm=TRUE)
students_grades$`Optional Activities-10`[is.na(students_grades$`Optional Activities-10`)]<-mean(students_grades$`Optional Activities-10`, na.rm=TRUE)


View(students_grades)

mean(students_grades$Exams)

#Now lets try to create a plot for exams and homework assignments
library(ggplot2)
# Scatter plot for Exam vs Homework
ggplot(students_grades, aes(x = as.numeric(`Homework Assignments-1`), y =as.numeric(Exams), colour = as.numeric(Exams))) +
  geom_point(alpha = 0.6) +
  geom_vline(xintercept = mean(students_grades$`Homework Assignments-1`), linetype = "dashed", color = "red", size = 1) +  # Vertical line at mean homework grade
  geom_hline(yintercept = mean(students_grades$Exams), linetype = "dashed", color = "blue", size = 1) + 
  labs(title = "Exam Grades vs Homework Grades", x = "Homework Assignments-1", y = "Exam Grade") +
  theme_minimal()

ggplot(students_grades, aes(x = as.numeric(`Homework Assignments-2`), y = as.numeric(Exams), colour = as.numeric(Exams))) +
  geom_point(alpha = 0.9) +
  geom_vline(xintercept=mean(students_grades$`Homework Assignments-2`), linetype="dashed",color="green",size=1) +
  geom_hline(yintercept = mean(students_grades$Exams), linetype = "dashed", color = "blue", size = 1) +
  labs(title = "Exam Grades vs Homework Grades", x = "Homework Assignments-2", y = "Exam Grade") +
  theme_minimal()

ggplot(students_grades, aes(x = as.numeric(`Homework Assignments-3`), y =as.numeric(Exams), colour = as.numeric(`Repeated Exams`))) +
  geom_point(alpha = 0.6) +
  geom_vline(xintercept = mean(students_grades$`Homework Assignments-1`),linetype="dashed",color="purple", size=1) +
  geom_hline(yintercept = mean(students_grades$Exams), linetype = "dashed", color = "blue", size = 1) +
  labs(title = " Repeated Exam Grades vs Homework Grades", x = "Homework Grade", y = "Exam Grade") +
  theme_minimal()

ggplot(students_grades, aes(x =as.numeric(Exams), y = as.numeric(`Homework Assignments-4`), colour = as.numeric(Exams))) +
  geom_point(alpha = 0.9, colour="red") +
  geom_vline(xintercept = mean(students_grades$`Homework Assignments-4`),linetype="dashed",color="purple", size=1) +
  geom_hline(yintercept = mean(students_grades$Exams), linetype = "dashed", color = "blue", size = 1) +
  labs(title = "Exam Grades vs Homework Grades", x = "Homework Assignment-4", y = "Exam Grade") +
  theme_minimal()

#Lets also do a histogram to see how scores are spread across 
# Histogram for Exam grades
ggplot(students_grades, aes(x =as.numeric( Exams))) + 
  geom_histogram(bins = 30, fill = "green", color = "black") +
  geom_vline(xintercept = mean(students_grades$Exams), linetype="dashed",color="black",size=1)
  labs(title = "Distribution of Exam Grades", x = "Exam Grade", y = "Frequency")

#Histogram for repeated exam grades
ggplot(students_grades,aes(x=as.numeric(`Repeated Exams`))) +
  geom_histogram(bins=30,fill="red",color="black") +
  geom_vline(xintercept = mean(students_grades$`Repeated Exams`), linetype="dashed",color="black",size=1)
  labs(title = "Distribution of  Repeated  Exam Grades", x = "Exam Grade", y = "Frequency")


mean(students_grades$Exams)
mean(students_grades$`Repeated Exams`)
median(students_grades$Exams)
median(students_grades$`Repeated Exams`)

#Now lets convert all 24 columns to numeric
students_grades[]<-lapply(students_grades, as.numeric)
View(students_grades) #Now the dataset contains numeric columns


#Now,lets create a correlation matrix out of this dataset.However we will include only the Homework and compulsory assignments
summary(students_grades)
ncol(students_grades)
str(students_grades)
students_grades_correlation<-cor(students_grades[,-c(25,26)],method="pearson")
students_grades_correlation

#Now lets do a heatmap in order to visualize the correlations between them
# Install and load the corrplot package
library(pheatmap)
pheatmap(students_grades_correlation,
         color = colorRampPalette(c("yellow", "white", "orange"))(50),
         display_numbers = TRUE,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "Correlation Matrix Heatmap")

summary(students_grades)


##Now lets apply the KNN algorithm on this dataset
library(class)
# Scale the feature columns (columns 1:4) for the iris dataset.
students_grades_scaled <- students_grades
students_grades_scaled[,-c(2,15:26)] <- scale(students_grades[, -c(2,15:26)])
View(students_grades_scaled)

# Split the scaled data into training and test sets.
set.seed(202)
idx_knn <- sample(1:nrow(students_grades_scaled), size = 0.7 * nrow(students_grades_scaled))
knn_train <- students_grades_scaled[idx_knn, ]
knn_test  <- students_grades_scaled[-idx_knn, ]

# Convert exam scores to pass/fail (1 if >= 5, 0 otherwise)
train_y <- ifelse(students_grades[idx_knn, "Exams"] >= 5, 1, 0)
test_y <- ifelse(students_grades[-idx_knn, "Exams"] >= 5, 1, 0)

# Separate features and labels.
train_x <- knn_train[, -1]
train_y <- as.factor(train_y)
test_x  <- knn_test[,-1]
test_y  <- as.factor(test_y)

# Apply k-NN with k = 5 and print the confusion matrix.
knn_preds <- knn(train_x, test_x, cl = train_y, k = 5)
length(knn_preds)
knn_cm <- table(Predicted = knn_preds, Actual = test_y)
print(knn_cm)

# Calculate and print the accuracy.
knn_acc <- sum(diag(knn_cm)) / sum(knn_cm)
cat("k-NN Accuracy (scaled iris, k=5):", round(knn_acc, 3), "\n")


# Loop over k values from 1 to 10 to see how accuracy changes.
acc_k <- numeric(10)
for (k in 1:10) {
  tmp_preds <- knn(train_x, test_x, cl = train_y, k = k)
  tmp_cm <- table(Predicted = factor(tmp_preds, levels = levels(train_y)),
                  Actual = factor(test_y, levels = levels(train_y)))
  acc_k[k] <- sum(diag(tmp_cm)) / sum(tmp_cm)
}
print(acc_k)
# Plot accuracy vs. k to help decide the best value of k.
plot(1:10, acc_k, type = "b",
     main = "k-NN Accuracy vs. k (Scaled Iris)",
     xlab = "k", ylab = "Accuracy",
     pch = 19, col = "red")
cat("So we have the best accuracy for k=4")

# Add a vertical line to indicate the best k value on the plot
best_k <- which.max(acc_k)  # Find the index of the highest accuracy
plot(1:10, acc_k, type = "b",
     main = "k-NN Accuracy vs. k (Scaled Data)",
     xlab = "k", ylab = "Accuracy",
     pch = 19, col = "red")
abline(v = best_k, col = "blue", lty = 2)  # Dashed vertical line for best k



# Ensure test set is correctly indexed
students_grades_test <- students_grades[-idx_knn, ]

# Add predictions to the test set
students_grades_test$Predicted_Exam_Pass <- knn_preds
students_grades_test$Exams[abs(students_grades_test$Exams - 5.550495) < 0.0001]<-0


View(students_grades_test)



##NOW LETS TRY A MULTIPLE LINERAR REGRESSION MODEL TO CHECK
students_grades_without_exams<-students_grades[,-c(1,2,15:26)]
View(students_grades_without_exams)
students_grades_predictions<-lm(Exams~ `Homework Assignments-1`+`Homework Assignments-2`+
                                  +`Homework Assignments-3`+`Homework Assignments-4` + `Compulsory Activities-1` +
                                  `Compulsory Activities-2` + `Compulsory Activities-3` + `Compulsory Activities-4`+
                                  `Compulsory Activities-5` + `Compulsory Activities-6` + `Compulsory Activities-7` +
                                  `Compulsory Activities-8`,
                                  data=students_grades)

summary(students_grades_predictions)
#Now, lets do some comparision between the actual and the predicted variables 
predicted_values<-predict(students_grades_predictions)

library(ggplot2)
#Lets compare actual vs predicted
plot(predicted_values,students_grades$Exams)
ggplot(students_grades, aes(x = as.numeric(predicted_values), y =as.numeric(Exams), colour = as.numeric(Exams))) +
  geom_point(alpha = 0.6) +
  geom_vline(xintercept = median(predicted_values), linetype = "dashed", color = "red", size = 1) +  # Vertical line at mean homework grade
  geom_hline(yintercept = median(students_grades$Exams), linetype = "dashed", color = "blue", size = 1) + 
  labs(title = "Exam Grades vs Predictor Grades", x = "Predictor Grades", y = "Exam Grade") +
  theme_minimal()





#Now lets apply the logistic regression on this dataset
#Firstly, we will add a new column to our dataset with no values
students_grades$Exams_log<-NA
View(students_grades)
library(dplyr)

students_grades <- students_grades %>%
  mutate(Exams_log = as.integer(Exams >= 5 | `Repeated Exams` >= 5))
View(students_grades)
students_grades$Exams[abs(students_grades$Exams - 5.550495) < 0.0001] <- 0

students_grades$Exams_log 
View(students_grades)
students_grades$Exams_log <- factor(students_grades$Exams_log, levels = c(0,1), labels = c("Fail","Pass"))
View(students_grades)
log_model <- glm(Exams_log ~ `Homework Assignments-1`+ `Homework Assignments-2`
                   +`Homework Assignments-3`+ `Homework Assignments-4` +
                   `Compulsory Activities-2` + `Compulsory Activities-1`, data = students_grades, family = binomial)
students_grades$log_probs <- predict(log_model, type = "response")
students_grades$log_pred <- ifelse(students_grades$log_probs > 0.5, "Pass", "Fail")
summary(log_model)

colnames(students_grades)
nrow(students_grades[students_grades$Exams_log==students_grades$log_pred,])
nrow(students_grades[students_grades$Exams_log=="Fail" & students_grades$log_pred=="Fail",])

#Now lets only for Hom.Assignment 1 and Hom.Assignment 3
students_grades$Homework_Mean <- rowMeans(students_grades[, c(3:6)], na.rm = TRUE)
View(students_grades)
students_grades$Compulsory_Mean<-rowMeans(students_grades[,c(7:14)])
View(students_grades)
#Now, lets do also one column for the mean of optional activities

students_grades$Optional_mean<-rowMeans(students_grades[,c(15:24)])
log_model_revised<-glm(Exams_log~ `Homework_Mean` + `Compulsory_Mean` + `Optional_mean`, data=students_grades,family=binomial)
students_grades$log_probs_revised <- predict(log_model_revised, type = "response")
students_grades$log_pred_revised <- ifelse(students_grades$log_probs_revised > 0.5, "Pass", "Fail")
View(students_grades)
summary(log_model_revised)




View(students_grades)


#Now lets apply the K-Means
#Firstly, lets apply the K-means clustering for students with Exams and repeated Exams

#So, that means that in total more than 50% of the students have achieved to pass the exams
#99/159
filtered_rows <- nrow(students_grades[round(students_grades$Exams, 1) == 0.0 & 
                                   round(students_grades$`Repeated Exams`, 6) == 4.497297, ])
filtered_rows
View(students_grades)



#Now,for the k-means i will take a subgroup of this specific dataset
ncol(students_grades)
View(students_grades)
students_grades_appliedToKMeans<-students_grades[,c(1,2,30,31,32)]
View(students_grades_appliedToKMeans)
students_grades_appliedToKMeans$`Repeated Exams`[abs(students_grades_appliedToKMeans$`Repeated Exams` - 4.497297) < 0.0001] <- 0

View(students_grades_appliedToKMeans)


#Now, lets apply the K-Means clustering on this dataset
# Define the optimal number of clusters
wss<-numeric(15)
for(k in 1:15) {
  wss[k]<-sum(kmeans(students_grades_appliedToKMeans,centers=k,nstart=25)$withinss)
}
plot(1:15,wss,type="b",xlab = "Number of clusters",ylab="Within Sum of Squares",pch=19,col="red")
k_means<-kmeans(students_grades_appliedToKMeans,7,nstart=25)
k_means
c(wss[4],sum(k_means$withinss))
library(factoextra)
fviz_cluster(k_means,data=students_grades_appliedToKMeans,geom = "point")







#Now, lets work also a bit on the decision trees
install.packages(c("MASS","rpart"))
library(MASS)
library(rpart)
#Now lets estimate the data
head(students_grades)
summary(students_grades)
#Now we create the prediction in regard to exams
final_prediction_withthis<-rpart(Exams~.,data=students_grades,method="anova")
#Now lets try to create a plot of the final predictions
plot(final_prediction_withthis,margin=0.1)
text(final_prediction_withthis, use.n=TRUE)
predictions<-predict(final_prediction_withthis,students_grades)
actual<-students_grades$Exams
mean((predictions- actual)^2)
summary(predictions)
head(final_prediction_withthis)
