# Student-Performance-Analysis-Predicting-Academic-Success-using-Machine-Learning
An end-to-end R project analyzing how student participation in homework, compulsory activities, and optional tasks impacts exam performance. Featuring data cleaning, K-Means clustering, and predictive modeling with KNN and Logistic Regression.
## Student Progress & Performance Analysis ##

## 🎓Project Overview ##
As a Master’s student, I developed this project to investigate how continuous assessment and various student activities impact final module grades. By exploring a dataset of 159 students, I applied both supervised and unsupervised machine learning techniques to identify behavioral patterns and predict academic success.

## 💡 Key Insights ##
- **The "Success" Factor:** Participation in optional activities is a powerful predictor; 97% of students who completed at least half of the optional activities passed the course.
- **Consistency Matters:** Out of the students who completed all optional and compulsory activities, 76% succeeded in the final exams.
- **Attendance Gaps:** 44 students did not participate in either the primary or repeated exam sessions.
- **Activity Influence:** Homework assignments showed a moderate correlation with exam grades (up to 0.30), while compulsory activities had less individual impact on repeated exam results.

## 🛠️ Technical Workflow1 ##
# Data Preparation & Cleaning Formatting: #
Renamed columns for readability and removed unnecessary structural rows/columns.Preprocessing: Handled missing values represented by "-1" or "-" by converting them to NA and performing column-wise mean imputation.
# Feature Engineering: #
Created binary indicators for full and partial participation (binary.1 and binary.2) and calculated mean scores for homework, compulsory, and optional tasks.
# Exploratory Data Analysis (EDA) #
- **Correlation Analysis:** Developed a heatmap to visualize relationships between all activities and exam scores.
- **Visualizations:** Created histograms and scatter plots using ggplot2 to identify grade distributions and correlations between homework and exams.
- **Machine Learning ModelsKNN Classification:** Achieved 83.3% accuracy in predicting pass/fail status using an optimized k=4
- **Logistic Regression:** Reached up to 93% predictive accuracy for passing students.
- **Linear Regression:** Identified Homework Assignments 2 and 4 as significant predictors, though the overall variance explained was low (R^2 ~=22%).
- **K-Means Clustering:** Grouped students into 7 distinct clusters to separate top performers, at-risk students, and those who only succeeded in repeated exams.

## 📂 Project Structurescripts ##
Contains the full R analysis script (PAPPAS-Student.Progress.R).
- **output:** High-resolution plots including the Correlation Heatmap and K-Means Clusters.
- **Student_Grades_Project_Report.docx:** Full academic report detailing the methodology and conclusions.
- **📊 Tools & LibrariesLanguage:** R Data Manipulation: dplyr, tidyverse Visualization: ggplot2, pheatmap Modeling: class (KNN), stats (Linear/Logistic Regression), rpart (Decision Trees) 
