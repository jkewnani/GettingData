
### Introduction

This repository contains files for Coursera "Getting and Cleaning Data" Course Project. The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 

### Files 
 1. CodeBook.md: Description of data and transformations
 2. run_analysis.R: R script for cleaning data
  
### Data Set
The data set, "Human Activity Recognition Using Smartphones", was obtained from [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).

### Execution
`run_analysis.R` performs the data transformations to clean the data and outputs `tidy_data.txt`, an  independent tidy data set with the average of each variable for each activity and each subject. The required R libraries are `dplyr`, `data.table`, `reshape2`.

