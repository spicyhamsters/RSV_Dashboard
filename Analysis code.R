############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
#CART  


# Level	Approx. Weekly Hospitalization Rate (per 100,000 children 0–4)	Notes / Source Basis
# Baseline (off-season)	<1	Sporadic RSV hospitalizations outside typical fall–winter season.
# Alert (rising activity)	5–10	Indicates sustained increase, often corresponds to PCR positivity ≥3%.
# Epidemic (season onset)	15–25	Consistent with epidemic thresholds identified in CDC RSV-NET data; signals start of RSV season.
# Severe epidemic / surge	≥50	Seen in the U.S. 2022 surge; overwhelmed pediatric hospitals in some states.
# Extreme surge (historic high)	>100	Peak rates in infants <6 months during 2022 RSV surge (CDC RSV-NET reports).
# Category	Rate (/100k)
# off-season	0
# Alert 1-10
# Epidmic (outbreak season onset)	25
# High	50
# Extreme surge 100

# Example thresholds (you can adjust based on literature or public health guidelines)
# Low: 0–5 per 100k
# Moderate: 6–20 per 100k
# High: >20 per 100k

###################################
#Select data that is wanted########
###################################

sub1<-m3[m3$Age.Category %in% c("0-4 years"), ]
sub1<-sub1[!is.na(sub1$Rate), ]
sub1<-sub1[, !(names(sub1) %in% c("Week.ending.date","Age.Category", "activity_level","Sex", "Race","year", "day", "State", "State.Territory_WVAL"))]
sub1$RSV_season <- factor(ifelse(sub1$month %in% c(11, 12, 1, 2, 3, 4),
                                 "Season", "OffSeason"))

# Remove month column
sub1$month <- NULL
#sub1$month<-as.factor(sub1$month)
sub1$RSV_season<-as.factor(sub1$RSV_season)
sub1 <- na.omit(sub1)
#sub1<-sub1[sub1$State != "Colorado",]

library(dplyr)
library(tidyr)

# Select numeric variables
numeric_vars <- sub1 %>% select(where(is.numeric))

# Compute min, max, median ignoring NAs
summary_stats <- numeric_vars %>%
  summarise(across(everything(), list(
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE)
  )))

# Reshape into tidy table, splitting at the last underscore
tidy_summary <- summary_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Stat"),
    names_pattern = "^(.*)_(min|max|median)$",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Stat,
    values_from = Value
  ) %>%
  arrange(Variable)

# View the tidy table
tidy_summary

# Variable         min      max   median
# <chr>          <dbl>    <dbl>    <dbl>
#   1 CO            0        0.766    0.321 
# 2 NO2           0.613   29.9      9.90  
# 3 Ozone         0.0103   0.0616   0.0307
# 4 PM10          6.05    93.4     19.3   
# 5 PM2.5         2.07    61.4      7.32  
# 6 PRECTOTCORR   0       15.8      1.40  
# 7 PS           77.7    103.      98.0   
# 8 QV2M          0.609   17.3      5.74  
# 9 RH2M         21.2     97.5     69.7   
# 10 Rate          0      178.       2.7   
# 11 SO2           0        4.51     0.554 
# 12 T2M         -20.7     32.2     11.9   
# 13 T2MDEW      -21.2     22.5      4.12  
# 14 T2MWET      -21.0     25.4      8.06  
# 15 TS          -21.1     32.9     11.7   
# 16 WD10MN       25.0    320.     216.    
# 17 WS10M         1.05     7.17     3.39  
# 18 WS2M          0.231    5.16     2.09  
# 19 WVAL          0.81    28.2      2.46  
##################################
# Load libraries#################
################################
library(caret)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(pROC)
library(reshape2)

# Categorize Rate into three classes
sub1$Rate_cat <- cut(
  sub1$Rate,
  breaks = c(-Inf, 5, 20, Inf),
  labels = c("Low.risk", "Alert", "Epidemic"),
  right = TRUE
)

###################################
# Exclude 'Rate' from predictors
#################################
# predictors <- setdiff(names(sub1), c("Rate", "Rate_cat", "RH2M", "T2MWET", "T2MDEW","TS", "WS10M","SO2", "PS"))
# 
# predictors <- setdiff(names(sub1), c("Rate", "Rate_cat", "T2MWET", "RH2M", "WS10M", "TS", "T2MDEW"))
# predictors <- setdiff(names(sub1), c("Rate", "Rate_cat", "PS", "SO2")) #OK model
# predictors <- setdiff(names(sub1), c("Rate", "Rate_cat", "PS", "SO2", "T2MDEW", "T2MWET", "RH2M"))
# predictors <- setdiff(names(sub1), c("Rate", "Rate_cat", "PS", "TS", "T2MWET", "PM2.5", "RH2M"))
# predictors <- setdiff(names(sub1), c("Rate", "Rate_cat", "PS"))
# predictors <- setdiff(names(sub1), c("Rate", "Rate_cat", "T2MWET", "T2MDEW", "TS")) #looks ok
# predictors <- setdiff(names(sub1), c("Rate", "Rate_cat"))

predictors <- setdiff(names(sub1), c("Rate", "Rate_cat", "T2MWET", "T2MDEW", "TS", "WS2M")) 
#Train data set created
train_data <- sub1[, c(predictors, "Rate_cat")]

# Convert outcome to factor make sure number rep of classes isnt confused w/ number
train_data$Rate_cat <- as.factor(train_data$Rate_cat)
train_data$RSV_season <- as.factor(train_data$RSV_season)
##########################
# Train/Test Split (80/20)
###########################
set.seed(123)
train_index <- createDataPartition(train_data$Rate_cat, p = 0.8, list = FALSE)
train_set <- train_data[train_index, ]
test_set  <- train_data[-train_index, ]

library(dplyr)
library(knitr)

# Function to summarize a dataset
summarize_dataset <- function(df) {
  df %>%
    summarise(
      across(everything(), list(
        Mean = ~if(is.numeric(.)) mean(.) else NA,
        SD   = ~if(is.numeric(.)) sd(.) else NA,
        Min  = ~if(is.numeric(.)) min(.) else NA,
        Max  = ~if(is.numeric(.)) max(.) else NA,
        Levels = ~if(is.factor(.) | is.character(.)) paste(levels(as.factor(.)), collapse = ", ") else NA), 
        .names = "{.col}_{.fn}")) %>% t() %>%  # transpose for readability
    as.data.frame() %>%
    rename(Summary = V1)}

# Summarize training and testing sets
train_summary <- summarize_dataset(train_set)
test_summary  <- summarize_dataset(test_set)

# Combine into a single table
summary_table <- cbind(
  Variable = rownames(train_summary),
  Train = train_summary$Summary,
  Test  = test_summary$Summary
)

# Display table
kable(summary_table, caption = "Summary Statistics for Training and Testing Sets")

# Table: Summary Statistics for Training and Testing Sets
# 
# |Variable           |Train                     |Test                      |
#   |:------------------|:-------------------------|:-------------------------|
#   |T2M_Mean           |11.82784                  |10.45824                  |
#   |T2M_SD             |10.07492                  |10.52123                  |
#   |T2M_Min            |-17.11429                 |-20.71                    |
#   |T2M_Max            |32.17857                  |29.49857                  |
#   |T2M_Levels         |NA                        |NA                        |
#   |QV2M_Mean          |6.626601                  |6.32902                   |
#   |QV2M_SD            |3.752978                  |3.667353                  |
#   |QV2M_Min           |0.6542857                 |0.6085714                 |
#   |QV2M_Max           |17.27                     |15.5                      |
#   |QV2M_Levels        |NA                        |NA                        |
#   |RH2M_Mean          |65.78503                  |67.03176                  |
#   |RH2M_SD            |16.18411                  |16.00397                  |
#   |RH2M_Min           |21.18429                  |25.07                     |
#   |RH2M_Max           |97.51                     |96.54286                  |
#   |RH2M_Levels        |NA                        |NA                        |
#   |PRECTOTCORR_Mean   |2.161868                  |2.336354                  |
#   |PRECTOTCORR_SD     |2.451939                  |2.629326                  |
#   |PRECTOTCORR_Min    |0                         |0                         |
#   |PRECTOTCORR_Max    |15.76286                  |14.11143                  |
#   |PRECTOTCORR_Levels |NA                        |NA                        |
#   |PS_Mean            |92.72207                  |92.25446                  |
#   |PS_SD              |8.918301                  |8.862772                  |
#   |PS_Min             |77.95571                  |77.71714                  |
#   |PS_Max             |102.67                    |102.1371                  |
#   |PS_Levels          |NA                        |NA                        |
#   |WS10M_Mean         |3.508139                  |3.571259                  |
#   |WS10M_SD           |1.116968                  |1.138736                  |
#   |WS10M_Min          |1.047143                  |1.534286                  |
#   |WS10M_Max          |7.167143                  |7.007143                  |
#   |WS10M_Levels       |NA                        |NA                        |
#   |WD10MN_Mean        |210.7813                  |213.5429                  |
#   |WD10MN_SD          |45.60446                  |47.72509                  |
#   |WD10MN_Min         |25.02857                  |77.91429                  |
#   |WD10MN_Max         |320.1286                  |305.8571                  |
#   |WD10MN_Levels      |NA                        |NA                        |
#   |CO_Mean            |0.3246587                 |0.3155496                 |
#   |CO_SD              |0.1268488                 |0.1180595                 |
#   |CO_Min             |0                         |0.05015126                |
#   |CO_Max             |0.7663821                 |0.6378069                 |
#   |CO_Levels          |NA                        |NA                        |
#   |NO2_Mean           |10.29992                  |9.938726                  |
#   |NO2_SD             |5.135224                  |5.001335                  |
#   |NO2_Min            |0.6127033                 |0.7156056                 |
#   |NO2_Max            |28.92867                  |29.90534                  |
#   |NO2_Levels         |NA                        |NA                        |
#   |Ozone_Mean         |0.03120393                |0.03127822                |
#   |Ozone_SD           |0.009041281               |0.009168706               |
#   |Ozone_Min          |0.0102503                 |0.01248014                |
#   |Ozone_Max          |0.0616413                 |0.05999402                |
#   |Ozone_Levels       |NA                        |NA                        |
#   |PM10_Mean          |21.87804                  |20.86826                  |
#   |PM10_SD            |9.927094                  |9.448822                  |
#   |PM10_Min           |6.05439                   |7.691234                  |
#   |PM10_Max           |93.39633                  |64.35116                  |
#   |PM10_Levels        |NA                        |NA                        |
#   |PM2.5_Mean         |8.08878                   |7.797808                  |
#   |PM2.5_SD           |4.181437                  |3.819233                  |
#   |PM2.5_Min          |2.360728                  |2.068472                  |
#   |PM2.5_Max          |61.37354                  |29.32157                  |
#   |PM2.5_Levels       |NA                        |NA                        |
#   |SO2_Mean           |0.6014013                 |0.6413668                 |
#   |SO2_SD             |0.3956394                 |0.4759572                 |
#   |SO2_Min            |0                         |0                         |
#   |SO2_Max            |2.458843                  |4.511273                  |
#   |SO2_Levels         |NA                        |NA                        |
#   |WVAL_Mean          |4.956359                  |5.347143                  |
#   |WVAL_SD            |4.86875                   |5.454267                  |
#   |WVAL_Min           |0.88                      |0.81                      |
#   |WVAL_Max           |28.16                     |22.36                     |
#   |WVAL_Levels        |NA                        |NA                        |
#   |RSV_season_Mean    |NA                        |NA                        |
#   |RSV_season_SD      |NA                        |NA                        |
#   |RSV_season_Min     |NA                        |NA                        |
#   |RSV_season_Max     |NA                        |NA                        |
#   |RSV_season_Levels  |OffSeason, Season         |OffSeason, Season         |
#   |Rate_cat_Mean      |NA                        |NA                        |
#   |Rate_cat_SD        |NA                        |NA                        |
#   |Rate_cat_Min       |NA                        |NA                        |
#   |Rate_cat_Max       |NA                        |NA                        |
#   |Rate_cat_Levels    |Low.risk, Alert, Epidemic |Low.risk, Alert, Epidemic |

# Count of each category
counts <- table(test_set$Rate_cat)

# Percentages
percentages <- prop.table(counts) * 100

# Combine into a table
data.frame(Category = names(counts), Count = as.vector(counts), Percent = round(percentages, 2))

# Category Count Percent.Var1 Percent.Freq
# 1 Low.risk   126     Low.risk        60.00
# 2    Alert    40        Alert        19.05
# 3 Epidemic    44     Epidemic        20.95

# Count of each category
counts <- table(train_set$Rate_cat)

# Percentages
percentages <- prop.table(counts) * 100

# Combine into a table
data.frame(Category = names(counts), Count = as.vector(counts), Percent = round(percentages, 2))


# Category Count Percent.Var1 Percent.Freq
# 1 Low.risk   505     Low.risk        59.69
# 2    Alert   164        Alert        19.39
# 3 Epidemic   177     Epidemic        20.92


# Count of each category
counts <- table(train_set$RSV_season)

# Percentages
percentages <- prop.table(counts) * 100

# Combine into a table
data.frame(Category = names(counts), Count = as.vector(counts), Percent = round(percentages, 2))

# Category Count Percent.Var1 Percent.Freq
# 1 OffSeason   405    OffSeason        47.87
# 2    Season   441       Season        52.13

# Count of each category
counts <- table(test_set$RSV_season)

# Percentages
percentages <- prop.table(counts) * 100

# Combine into a table
data.frame(Category = names(counts), Count = as.vector(counts), Percent = round(percentages, 2))

# Category Count Percent.Var1 Percent.Freq
# 1 OffSeason   103    OffSeason        49.05
# 2    Season   107       Season        50.95

###################
# Set up 10-fold CV
######################
ctrl <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE,
  classProbs = TRUE,        
  savePredictions = TRUE
)

# train_set <- na.omit(train_set)
# test_set  <- na.omit(test_set)
###################
# Train CART model#
###################
set.seed(123)  # fixes randomness for CV folds
cart_cv_class <- train(
  Rate_cat ~ .,
  data = train_set,
  method = "rpart",
  trControl = ctrl,
  tuneLength = 10,
  metric = "Accuracy",
  control = rpart.control(
    maxdepth = 5,
    minsplit = 20,
    cp = 0.01
  )
)

######################
# Visualize final tree
######################
final_tree <- cart_cv_class$finalModel

# Make sure your response variable is a factor in the correct order
# For example: low = 1st, medium = 2nd, epidemic = 3rd
final_tree$y <- factor(final_tree$y, levels = c("Low risk", "Alert", "Epidemic"))

# Define box colors as a list in the same order as the factor levels
color_list <- list("green", "orange", "red")

#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/CART_tree.jpg", width = 800, height = 600)
pdf("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/CART_tree.pdf", width = 7, height = 5)
rpart.plot(final_tree, box.palette = color_list,  cex = 0.8 ,legend.cex = 0.8)   # scale legend text separately)
dev.off()

# Plot the tree
rpart.plot(final_tree,
           main = "Final CART Tree",
           type = 3,
           extra = 101,
           fallen.leaves = TRUE,
           box.palette = color_list,
           shadow.col = "gray",
           branch.lty = 3,
           cex = 0.8)

#####################
# Variable Importance
#####################
var_imp <- varImp(cart_cv_class)
plot(var_imp, main="Variable Importance (CART)")

# Extract variable importance
var_imp <- final_tree$variable.importance

# Sort in decreasing order
var_imp <- sort(var_imp, decreasing = TRUE)

# Print top variables
print(var_imp)

# Barplot
barplot(var_imp,
        main = "CART Variable Importance",
        col = "skyblue",
        las = 2,  # rotates labels
        cex.names = 0.8)


#Top 4 variables: WVAL, QV2M T2M RSV_season

##############################################################################
# Predictions & Confusion Matrix which sees how well Cart predicts on test set
##############################################################################
pred_class_test <- predict(cart_cv_class, newdata = test_set)
confusion_test <- table(Predicted = pred_class_test, Actual = test_set$Rate_cat)
accuracy_test <- sum(diag(confusion_test)) / sum(confusion_test)
cat("Test Accuracy:", accuracy_test, "\n")

#Test Accuracy: 0.7666667 

# Class-wise accuracy
# class_accuracy <- diag(confusion_test) / colSums(confusion_test)
# class_accuracy
# 
# # Print in a readable way
# for(cls in names(class_accuracy)){
#   cat("Accuracy for class", cls, ":", round(class_accuracy[cls], 3), "\n")
# }

# Confusion Matrix Heatmap (Shows concordance and discordance/which ones are right and which ones are wrong)
conf_mat <- as.data.frame(confusion_test)
colnames(conf_mat) <- c("Predicted", "Actual", "Freq")

#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/CART_confusion.jpg", width = 800, height = 600)

# ggplot(conf_mat, aes(x = Actual, y = Predicted, fill = Freq)) +
#   geom_tile(color = "white") +
#   geom_text(aes(label = Freq), color = "black", size = 5) +
#   scale_fill_gradient(low = "white", high = "steelblue") +
#   theme_minimal() 
# #+labs(title = "Confusion Matrix Heatmap", fill = "Count")

ggplot(conf_mat, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "",
    fill = "Count",
    x = "Actual",
    y = "Predicted"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 18),   # x-axis label
    axis.title.y = element_text(size = 18),   # y-axis label
    axis.text.x = element_text(size = 14, face = "bold"),    # x-axis tick labels ("Low Alert", "Epidemic")
    axis.text.y = element_text(size = 14, face = "bold"),    # y-axis tick labels
    legend.title = element_text(size = 16, face = "bold"),   # legend title
    legend.text = element_text(size = 14)                    # legend tick labels
  )
ggsave("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/CART_confusion.pdf", width = 7, height = 5)
#dev.off()

# Get the unique classes
classes <- unique(c(conf_mat$Actual, conf_mat$Predicted))

# Initialize vectors to store results
sensitivity <- numeric(length(classes))
specificity <- numeric(length(classes))

# Loop over each class
for (i in seq_along(classes)) {
  class <- classes[i]
  
  # True positives (TP)
  TP <- conf_mat$Freq[conf_mat$Actual == class & conf_mat$Predicted == class]
  if(length(TP) == 0) TP <- 0
  
  # False negatives (FN)
  FN <- sum(conf_mat$Freq[conf_mat$Actual == class & conf_mat$Predicted != class])
  
  # False positives (FP)
  FP <- sum(conf_mat$Freq[conf_mat$Actual != class & conf_mat$Predicted == class])
  
  # True negatives (TN)
  TN <- sum(conf_mat$Freq[conf_mat$Actual != class & conf_mat$Predicted != class])
  
  # Sensitivity = TP / (TP + FN)
  sensitivity[i] <- TP / (TP + FN)
  
  # Specificity = TN / (TN + FP)
  specificity[i] <- TN / (TN + FP)
}

# Combine results into a table
results <- data.frame(
  Class = classes,
  Sensitivity = round(sensitivity, 3),
  Specificity = round(specificity, 3)
)

print(results)

# Class Sensitivity Specificity
# 1 Low.risk       0.952       0.738
# 2    Alert       0.125       0.965
# 3 Epidemic       0.818       0.873


###########################
# ROC Curves (One-vs-All)
#########################
pred_probs_test <- predict(cart_cv_class, newdata = test_set, type = "prob")

# Low risk
actual_low <- ifelse(test_set$Rate_cat == "Low.risk", 1, 0)
roc_low <- roc(response = actual_low, predictor = pred_probs_test[,"Low.risk"])

# Alert
actual_alert <- ifelse(test_set$Rate_cat == "Alert", 1, 0)
roc_alert <- roc(response = actual_alert, predictor = pred_probs_test[,"Alert"])

# Epidemic
actual_epi <- ifelse(test_set$Rate_cat == "Epidemic", 1, 0)
roc_epi <- roc(response = actual_epi, predictor = pred_probs_test[,"Epidemic"])

# Plot ROC
library(ggplot2)

# Convert each ROC curve into a data frame
roc_low_df <- data.frame(
  fpr = 1 - roc_low$specificities,
  tpr = roc_low$sensitivities,
  model = "Low risk"
)

roc_alert_df <- data.frame(
  fpr = 1 - roc_alert$specificities,
  tpr = roc_alert$sensitivities,
  model = "Alert"
)

roc_epi_df <- data.frame(
  fpr = 1 - roc_epi$specificities,
  tpr = roc_epi$sensitivities,
  model = "Epidemic"
)

# Combine into one data frame
roc_df <- rbind(roc_low_df, roc_alert_df, roc_epi_df)

# Plot with custom colors
#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/CART_roc.jpg", width = 800, height = 600)
library(ggplot2)

ggplot(roc_df, aes(x = fpr, y = tpr, color = model)) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +   # force square box
  scale_color_manual(values = c("Low risk" = "green",
                                "Alert" = "orange",
                                "Epidemic" = "red")) +
  labs(x = "False Positive Rate", y = "True Positive Rate",
       color = "Class") +  # legend title
  theme_minimal(base_size = 16) +  # increases base text size for axis labels
  theme(
    axis.title.x = element_text(size = 18),  # x-axis label
    axis.title.y = element_text(size = 18),  # y-axis label
    axis.text = element_text(size = 14),                    # tick labels
    legend.title = element_text(size = 16, face = "bold"),  # legend title
    legend.text = element_text(size = 14),                  # legend items
    panel.grid.minor = element_blank()
  ) +
  # Add AUC text with larger font
  annotate("text", x = 0.65, y = 0.25, label = paste0("AUC=", round(auc(roc_low),3)),
           color = "green", size = 6, fontface = "bold") +
  annotate("text", x = 0.65, y = 0.18, label = paste0("AUC=", round(auc(roc_alert),3)),
           color = "orange", size = 6, fontface = "bold") +
  annotate("text", x = 0.65, y = 0.11, label = paste0("AUC=", round(auc(roc_epi),3)),
           color = "red", size = 6, fontface = "bold")
#dev.off()
ggsave("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/CART_roc.pdf", width = 7, height = 5)
# AUCs (ARea under Curve)
cat("AUC Low risk:", auc(roc_low), "\n")
cat("AUC Alert:", auc(roc_alert), "\n")
cat("AUC Epidemic:", auc(roc_epi), "\n")

#AUC Low risk: 0.8621504
#AUC Alert: 0.6735294
#AUC Epidemic: 0.894989

##################################
# F1-Score for Multi-class
##################################
library(MLmetrics)

# Convert factors to numeric labels if needed
actual <- test_set$Rate_cat
predicted <- pred_class_test

# Compute F1-score for each class
f1_scores <- sapply(levels(actual), function(cls){
  F1_Score(y_true = actual, y_pred = predicted, positive = cls)
})

# Weighted F1-score
class_counts <- table(actual)
weighted_f1 <- sum(f1_scores * class_counts / sum(class_counts))

# Print results
cat("F1-score per class:\n")
print(f1_scores)
cat("Weighted F1-score:", round(weighted_f1,3), "\n")

# Low.risk     Alert  Epidemic 
# 0.8955224 0.1960784 0.7128713 
#Weighted F1-score: 0.724

saveRDS(cart_cv_class, file = "C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/cart_model.rds")

# Install/load required packages
# install.packages("iml")
# install.packages("data.table")
# library(iml)
# library(data.table)
# 
# # ------------------------
# # Train CART model
# # ------------------------
# library(rpart)
# cart_model <- rpart(Rate_cat ~ ., data = train_set, method = "class")
# 
# # ------------------------
# # Wrap in Predictor object
# # ------------------------
# # Make sure target variable is excluded from features
# X <- train_set[, !names(train_set) %in% c("Rate_cat")]
# y <- train_set$Rate_cat
# 
# predictor <- Predictor$new(
#   model = cart_model,
#   data = X,
#   y = y,
#   type = "prob"   # use class probabilities
# )
# 
# ###############
# # SHAP values
# ########################
# shapley <- Shapley$new(
#   predictor = predictor,
#   x.interest = X[1:50, ]   # take a subset for speed
# )
# 
# ###########################
# # Beeswarm-style SHAP plot
# ###########################
# library(ggplot2)
# 
# # iml has feature importance + Shapley plots
# plot(shapley)  # per-instance breakdown
# 
# # Aggregate SHAP values for beeswarm-style
# shapley_values <- shapley$results
# head(shapley_values)
# 
# # Plot beeswarm with ggplot2
# ggplot(shapley_values, aes(x = phi, y = feature, color = feature.value)) +
#   geom_point(alpha = 0.6) +
#   theme_minimal() +
#   labs(title = "Beeswarm SHAP Plot", x = "SHAP value (impact)", y = "Feature")
# 
# shap_df <- shapley_values
# 
# # Compute mean absolute SHAP per feature for variable importance
# shap_varimp <- shap_df %>%
#   group_by(feature) %>%
#   summarise(mean_abs_shap = mean(abs(phi))) %>%
#   arrange(desc(mean_abs_shap))
# 
# # Create bar plot
# ggplot(shap_varimp, aes(x = reorder(feature, mean_abs_shap), y = mean_abs_shap)) +
#   geom_bar(stat = "identity", fill = "forestgreen") +
#   coord_flip() +  # horizontal bars
#   theme_minimal() +
#   labs(
#     title = "Variable Importance (Mean Absolute SHAP)",
#     x = "Feature",
#     y = "Mean |SHAP Value|"
#   )
####################################
#Random Forest
# ----------------------------
# Load libraries
# ----------------------------
library(caret)
library(randomForest)
library(ggplot2)
library(pROC)
library(reshape2)

# ----------------------------
# Train Random Forest
# ----------------------------
set.seed(123)
rf_cv_class <- train(
  Rate_cat ~ .,
  data = train_set,
  method = "rf",
  trControl = ctrl,
  tuneLength = 5,       # try different mtry values
  metric = "Accuracy"
)

# ----------------------------
# Variable Importance
# ----------------------------
var_imp <- varImp(rf_cv_class)
plot(var_imp, main="Variable Importance (Random Forest)")

# WVAL, T2M Ozone, QV2M, 
# ----------------------------
# Predictions & Confusion Matrix
# ----------------------------
pred_class_test <- predict(rf_cv_class, newdata = test_set)
confusion_test <- table(Predicted = pred_class_test, Actual = test_set$Rate_cat)
accuracy_test <- sum(diag(confusion_test)) / sum(confusion_test)
cat("Test Accuracy:", accuracy_test, "\n")

#Test Accuracy: 0.8095238

# Confusion Matrix Heatmap
conf_mat <- as.data.frame(confusion_test)
colnames(conf_mat) <- c("Predicted", "Actual", "Freq")

#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/RF_confusion.jpg", width = 800, height = 600)
ggplot(conf_mat, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "",
    fill = "Count",
    x = "Actual",
    y = "Predicted"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 18),   # x-axis label
    axis.title.y = element_text(size = 18),   # y-axis label
    axis.text.x = element_text(size = 14, face = "bold"),    # x-axis tick labels ("Low Alert", "Epidemic")
    axis.text.y = element_text(size = 14, face = "bold"),    # y-axis tick labels
    legend.title = element_text(size = 16, face = "bold"),   # legend title
    legend.text = element_text(size = 14)                    # legend tick labels
  )
#dev.off()
ggsave("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/RF_confusion.pdf", width = 7, height = 5)

# Get the unique classes
classes <- unique(c(conf_mat$Actual, conf_mat$Predicted))

# Initialize vectors to store results
sensitivity <- numeric(length(classes))
specificity <- numeric(length(classes))

# Loop over each class
for (i in seq_along(classes)) {
  class <- classes[i]
  
  # True positives (TP)
  TP <- conf_mat$Freq[conf_mat$Actual == class & conf_mat$Predicted == class]
  if(length(TP) == 0) TP <- 0
  
  # False negatives (FN)
  FN <- sum(conf_mat$Freq[conf_mat$Actual == class & conf_mat$Predicted != class])
  
  # False positives (FP)
  FP <- sum(conf_mat$Freq[conf_mat$Actual != class & conf_mat$Predicted == class])
  
  # True negatives (TN)
  TN <- sum(conf_mat$Freq[conf_mat$Actual != class & conf_mat$Predicted != class])
  
  # Sensitivity = TP / (TP + FN)
  sensitivity[i] <- TP / (TP + FN)
  
  # Specificity = TN / (TN + FP)
  specificity[i] <- TN / (TN + FP)
}

# Combine results into a table
results <- data.frame(
  Class = classes,
  Sensitivity = round(sensitivity, 3),
  Specificity = round(specificity, 3)
)

print(results)

# Class Sensitivity Specificity
# Class Sensitivity Specificity
# 1 Low.risk       0.937       0.810
# 2    Alert       0.375       0.929
# 3 Epidemic       0.841       0.928
# ----------------------------
# ROC Curves (One-vs-All)
# ----------------------------
###########################
# ROC Curves (One-vs-All)
#########################
pred_probs_test <- predict(rf_cv_class, newdata = test_set, type = "prob")

# Low risk
actual_low <- ifelse(test_set$Rate_cat == "Low.risk", 1, 0)
roc_low <- roc(response = actual_low, predictor = pred_probs_test[,"Low.risk"])

# Alert
actual_alert <- ifelse(test_set$Rate_cat == "Alert", 1, 0)
roc_alert <- roc(response = actual_alert, predictor = pred_probs_test[,"Alert"])

# Epidemic
actual_epi <- ifelse(test_set$Rate_cat == "Epidemic", 1, 0)
roc_epi <- roc(response = actual_epi, predictor = pred_probs_test[,"Epidemic"])

# Plot ROC
library(ggplot2)

library(ggplot2)

# Convert each ROC curve into a data frame
roc_low_df <- data.frame(
  fpr = 1 - roc_low$specificities,
  tpr = roc_low$sensitivities,
  model = "Low risk"
)

roc_alert_df <- data.frame(
  fpr = 1 - roc_alert$specificities,
  tpr = roc_alert$sensitivities,
  model = "Alert"
)

roc_epi_df <- data.frame(
  fpr = 1 - roc_epi$specificities,
  tpr = roc_epi$sensitivities,
  model = "Epidemic"
)

# Combine into one data frame
roc_df <- rbind(roc_low_df, roc_alert_df, roc_epi_df)

# Plot with custom colors
#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/RF_roc.jpg", width = 800, height = 600)
ggplot(roc_df, aes(x = fpr, y = tpr, color = model)) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +   # force square box
  scale_color_manual(values = c("Low risk" = "green",
                                "Alert" = "orange",
                                "Epidemic" = "red")) +
  labs(x = "False Positive Rate", y = "True Positive Rate",
       color = "Class") +  # legend title
  theme_minimal(base_size = 16) +  # increases base text size for axis labels
  theme(
    axis.title.x = element_text(size = 18),  # x-axis label
    axis.title.y = element_text(size = 18),  # y-axis label
    axis.text = element_text(size = 14),                    # tick labels
    legend.title = element_text(size = 16, face = "bold"),  # legend title
    legend.text = element_text(size = 14),                  # legend items
    panel.grid.minor = element_blank()
  ) +
  # Add AUC text with larger font
  annotate("text", x = 0.65, y = 0.25, label = paste0("AUC=", round(auc(roc_low),3)),
           color = "green", size = 6, fontface = "bold") +
  annotate("text", x = 0.65, y = 0.18, label = paste0("AUC=", round(auc(roc_alert),3)),
           color = "orange", size = 6, fontface = "bold") +
  annotate("text", x = 0.65, y = 0.11, label = paste0("AUC=", round(auc(roc_epi),3)),
           color = "red", size = 6, fontface = "bold")
#dev.off()
ggsave("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/RF_roc.pdf", width = 7, height = 5)
# AUCs
cat("AUC Low risk:", auc(roc_low), "\n")
cat("AUC Alert:", auc(roc_alert), "\n")
cat("AUC Epidemic:", auc(roc_epi), "\n")

#AUC Low risk: 0.9621599
#AUC Alert: 0.8593382
#AUC Epidemic: 0.9662514

##################################
# F1-Score for Multi-class
##################################
library(MLmetrics)

# Convert factors to numeric labels if needed
actual <- test_set$Rate_cat
predicted <- pred_class_test

# Compute F1-score for each class
f1_scores <- sapply(levels(actual), function(cls){
  F1_Score(y_true = actual, y_pred = predicted, positive = cls)
})

# Weighted F1-score
class_counts <- table(actual)
weighted_f1 <- sum(f1_scores * class_counts / sum(class_counts))

# Print results
cat("F1-score per class:\n")
print(f1_scores)
cat("Weighted F1-score:", round(weighted_f1,3), "\n")

#Weighted F1-score: 0.797

saveRDS(rf_cv_class, file = "C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/RF_model.rds")
##############Logistics regression
# ----------------------------
# Train/Test Split (80/20)
# ----------------------------
# set.seed(123)
# train_index <- createDataPartition(train_data$Rate_cat, p = 0.8, list = FALSE)
# train_set <- train_data[train_index, ]
# test_set  <- train_data[-train_index, ]
# 
# # ----------------------------
# # Train Multinomial Logistic Regression
# # ----------------------------
# set.seed(123)
# # Use caret with method = "multinom"
# ctrl <- trainControl(
#   method = "cv",
#   number = 10,
#   verboseIter = TRUE,
#   classProbs = TRUE,
#   savePredictions = TRUE
# )
# 
# logit_model <- train(
#   Rate_cat ~ .,
#   data = train_set,
#   method = "multinom",
#   trControl = ctrl,
#   trace = FALSE
# )
# 
# # ----------------------------
# # Variable Importance
# # ----------------------------
# var_imp <- varImp(logit_model)
# plot(var_imp, main="Variable Importance (Multinomial Logistic Regression)")
# 
# # ----------------------------
# # Predictions & Confusion Matrix
# # ----------------------------
# pred_class_test <- predict(logit_model, newdata = test_set)
# confusion_test <- table(Predicted = pred_class_test, Actual = test_set$Rate_cat)
# accuracy_test <- sum(diag(confusion_test)) / sum(confusion_test)
# cat("Test Accuracy:", accuracy_test, "\n")
# 
# # Confusion Matrix Heatmap
# conf_mat <- as.data.frame(confusion_test)
# colnames(conf_mat) <- c("Predicted", "Actual", "Freq")
# 
# ggplot(conf_mat, aes(x = Actual, y = Predicted, fill = Freq)) +
#   geom_tile(color = "white") +
#   geom_text(aes(label = Freq), color = "black", size = 5) +
#   scale_fill_gradient(low = "white", high = "steelblue") +
#   theme_minimal() +
#   labs(title = "Confusion Matrix Heatmap", fill = "Count")
# 
# # ----------------------------
# # ROC Curves (One-vs-All)
# # ----------------------------
# pred_probs_test <- predict(logit_model, newdata = test_set, type = "prob")
# 
# # Low risk
# actual_low <- ifelse(test_set$Rate_cat == "Low.risk", 1, 0)
# roc_low <- roc(response = actual_low, predictor = pred_probs_test[,"Low.risk"])
# 
# # Alert
# actual_alert <- ifelse(test_set$Rate_cat == "Alert", 1, 0)
# roc_alert <- roc(response = actual_alert, predictor = pred_probs_test[,"Alert"])
# 
# # Epidemic
# actual_epi <- ifelse(test_set$Rate_cat == "Epidemic", 1, 0)
# roc_epi <- roc(response = actual_epi, predictor = pred_probs_test[,"Epidemic"])
# 
# # Plot ROC
# plot(roc_low, col="blue", lwd=2, main="One-vs-All ROC Curves (Logistic Regression)")
# plot(roc_alert, col="orange", lwd=2, add=TRUE)
# plot(roc_epi, col="red", lwd=2, add=TRUE)
# legend("bottomright", legend=c("Low risk", "Alert", "Epidemic"),
#        col=c("blue", "orange", "red"), lwd=2)
# 
# # AUCs
# cat("AUC Low risk:", auc(roc_low), "\n")
# cat("AUC Alert:", auc(roc_alert), "\n")
# cat("AUC Epidemic:", auc(roc_epi), "\n")
# 
# # Extract final model from caret
# final_model <- logit_model$finalModel
# 
# # Coefficients
# coef(final_model)
# 
# # Odds ratios
# exp(coef(final_model))
# 
# ###############Model selection
# library(MASS)
# install.packages("nnet")
# library(nnet)
# full_model <- multinom(Rate_cat ~ .-RH2M_1-PM10-WS10M_1, data = train_set)
# 
# step_model <- step(full_model, direction = "both", trace = TRUE)
# 
# summary(step_model)
# 
# exp(coef(step_model))

#################Boosting
##################################
# Ensure outcome is factor
##################################
# train_set$Rate_cat <- as.factor(train_set$Rate_cat)
# test_set$Rate_cat  <- as.factor(test_set$Rate_cat)
# 
# train_set <- na.omit(train_set)
# test_set  <- na.omit(test_set)

##################################
# Load libraries
##################################
library(caret)
library(gbm)
library(ggplot2)
library(pROC)

##################################
# Set up 10-fold CV
##################################
ctrl <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE,
  classProbs = TRUE,        
  savePredictions = "final"
)

##################################
# Train Gradient Boosting Model
##################################
set.seed(123)
boost_cv_class <- train(
  Rate_cat ~ .,
  data = train_set,
  method = "gbm",
  trControl = ctrl,
  tuneLength = 5,   # tries different n.trees, interaction.depth, shrinkage
  metric = "Accuracy",
  verbose = FALSE
)
# ----------------------------
# Variable Importance
# ----------------------------
var_imp <- varImp(boost_cv_class)
plot(var_imp, main="Variable Importance (Boosting)")

# WVAL, Ozone, PS QV2M
# ----------------------------
# Predictions & Confusion Matrix
# ----------------------------
pred_class_test <- predict(boost_cv_class, newdata = test_set)
confusion_test <- table(Predicted = pred_class_test, Actual = test_set$Rate_cat)
accuracy_test <- sum(diag(confusion_test)) / sum(confusion_test)
cat("Test Accuracy:", accuracy_test, "\n")

#Test Accuracy: 0.7904762
# Confusion Matrix Heatmap
conf_mat <- as.data.frame(confusion_test)
colnames(conf_mat) <- c("Predicted", "Actual", "Freq")
# Confusion Matrix Heatmap
#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Boost_confusion.jpg", width = 800, height = 600)

library(ggplot2)

ggplot(conf_mat, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "",
    fill = "Count",
    x = "Actual",
    y = "Predicted"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 18),   # x-axis label
    axis.title.y = element_text(size = 18),   # y-axis label
    axis.text.x = element_text(size = 14, face = "bold"),    # x-axis tick labels ("Low Alert", "Epidemic")
    axis.text.y = element_text(size = 14, face = "bold"),    # y-axis tick labels
    legend.title = element_text(size = 16, face = "bold"),   # legend title
    legend.text = element_text(size = 14)                    # legend tick labels
  )
#dev.off()

ggsave("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Boost_confusion.pdf", width = 7, height = 5)
# Get the unique classes
classes <- unique(c(conf_mat$Actual, conf_mat$Predicted))

# Initialize vectors to store results
sensitivity <- numeric(length(classes))
specificity <- numeric(length(classes))

# Loop over each class
for (i in seq_along(classes)) {
  class <- classes[i]
  
  # True positives (TP)
  TP <- conf_mat$Freq[conf_mat$Actual == class & conf_mat$Predicted == class]
  if(length(TP) == 0) TP <- 0
  
  # False negatives (FN)
  FN <- sum(conf_mat$Freq[conf_mat$Actual == class & conf_mat$Predicted != class])
  
  # False positives (FP)
  FP <- sum(conf_mat$Freq[conf_mat$Actual != class & conf_mat$Predicted == class])
  
  # True negatives (TN)
  TN <- sum(conf_mat$Freq[conf_mat$Actual != class & conf_mat$Predicted != class])
  
  # Sensitivity = TP / (TP + FN)
  sensitivity[i] <- TP / (TP + FN)
  
  # Specificity = TN / (TN + FP)
  specificity[i] <- TN / (TN + FP)
}

# Combine results into a table
results <- data.frame(
  Class = classes,
  Sensitivity = round(sensitivity, 3),
  Specificity = round(specificity, 3)
)

print(results)

# Class Sensitivity Specificity
# 1 Low.risk       0.913       0.798
# 2    Alert       0.425       0.912
# 3 Epidemic       0.773       0.928
##################################
# ROC Curves (One-vs-All)
##################################
pred_probs_test <- predict(boost_cv_class, newdata = test_set, type = "prob")

# Low risk
actual_low <- ifelse(test_set$Rate_cat == "Low.risk", 1, 0)
roc_low <- roc(response = actual_low, predictor = pred_probs_test[,"Low.risk"])

# Alert
actual_alert <- ifelse(test_set$Rate_cat == "Alert", 1, 0)
roc_alert <- roc(response = actual_alert, predictor = pred_probs_test[,"Alert"])

# Epidemic
actual_epi <- ifelse(test_set$Rate_cat == "Epidemic", 1, 0)
roc_epi <- roc(response = actual_epi, predictor = pred_probs_test[,"Epidemic"])

# Plot ROC
library(ggplot2)

# Convert each ROC curve into a data frame
roc_low_df <- data.frame(
  fpr = 1 - roc_low$specificities,
  tpr = roc_low$sensitivities,
  model = "Low risk"
)

roc_alert_df <- data.frame(
  fpr = 1 - roc_alert$specificities,
  tpr = roc_alert$sensitivities,
  model = "Alert"
)

roc_epi_df <- data.frame(
  fpr = 1 - roc_epi$specificities,
  tpr = roc_epi$sensitivities,
  model = "Epidemic"
)

# Combine into one data frame
roc_df <- rbind(roc_low_df, roc_alert_df, roc_epi_df)

# Plot with custom colors


#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Boost_roc.jpg", width = 800, height = 600)
library(ggplot2)

ggplot(roc_df, aes(x = fpr, y = tpr, color = model)) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +   # force square box
  scale_color_manual(values = c("Low risk" = "green",
                                "Alert" = "orange",
                                "Epidemic" = "red")) +
  labs(x = "False Positive Rate", y = "True Positive Rate",
       color = "Class") +  # legend title
  theme_minimal(base_size = 16) +  # increases base text size for axis labels
  theme(
    axis.title.x = element_text(size = 18),  # x-axis label
    axis.title.y = element_text(size = 18),  # y-axis label
    axis.text = element_text(size = 14),                    # tick labels
    legend.title = element_text(size = 16, face = "bold"),  # legend title
    legend.text = element_text(size = 14),                  # legend items
    panel.grid.minor = element_blank()
  ) +
  # Add AUC text with larger font
  annotate("text", x = 0.65, y = 0.25, label = paste0("AUC=", round(auc(roc_low),3)),
           color = "green", size = 6, fontface = "bold") +
  annotate("text", x = 0.65, y = 0.18, label = paste0("AUC=", round(auc(roc_alert),3)),
           color = "orange", size = 6, fontface = "bold") +
  annotate("text", x = 0.65, y = 0.11, label = paste0("AUC=", round(auc(roc_epi),3)),
           color = "red", size = 6, fontface = "bold")
#dev.off()
ggsave("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Boost_roc.pdf", width = 7, height = 5)

# AUCs
cat("AUC Low risk:", auc(roc_low), "\n")
cat("AUC Alert:", auc(roc_alert), "\n")
cat("AUC Epidemic:", auc(roc_epi), "\n")

#AUC Low risk: 0.9489796
#AUC Alert: 0.845
#AUC Epidemic: 0.9531763
##################################
# F1-Score for Multi-class
##################################
library(MLmetrics)

# Convert factors to numeric labels if needed
actual <- test_set$Rate_cat
predicted <- pred_class_test

# Compute F1-score for each class
f1_scores <- sapply(levels(actual), function(cls){
  F1_Score(y_true = actual, y_pred = predicted, positive = cls)
})

# Weighted F1-score
class_counts <- table(actual)
weighted_f1 <- sum(f1_scores * class_counts / sum(class_counts))

# Print results
cat("F1-score per class:\n")
print(f1_scores)
# Low.risk     Alert  Epidemic 
# 0.8914729 0.4722222 0.7555556 

cat("Weighted F1-score:", round(weighted_f1,3), "\n")

#Weighted F1-score: 0.783

saveRDS(boost_cv_class, file = "C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Boost_model.rds")