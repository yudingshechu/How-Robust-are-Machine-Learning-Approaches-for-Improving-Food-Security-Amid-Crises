library(haven)
library(tidyr)
library('ggplot2')
library(dplyr)
library(terra)
library(stringr)
library(tidyverse)
library(ggpubr)
library(ggridges)
library(viridis)
library(reshape2)
library(PRROC)
library(Metrics)
library(plotROC)
library(ggrepel)
# Function to compute metrics for each threshold and cases 
compute_metrics <- function(Testing_Prob, Y_testData, tick) {
  thresholds <- seq(0, 1, by = tick)
  all_metrics <- data.frame()
  prob <- Testing_Prob
  true <- Y_testData
  for (i in thresholds) {
    metrics <- sapply(i, function(threshold) {
      pred <- ifelse(prob >= threshold, 1, 0)
      TP <- sum(pred == 1 & true == 1)
      FP <- sum(pred == 1 & true == 0)
      FN <- sum(pred == 0 & true == 1)
      TN <- sum(pred == 0 & true == 0)
      
      FPR <- ifelse(FP + TN == 0, 0, FP / (FP + TN))
      precision <- ifelse(TP + FP == 0, 0, TP / (TP + FP))
      recall <- ifelse(TP + FN == 0, 0, TP / (TP + FN))
      accuracy <- (TP + TN) / length(true)
      f1 <- ifelse(precision + recall == 0, 0, 2 * (precision * recall) / (precision + recall))
      
      c(Accuracy = accuracy, Recall = recall, Precision = precision, F1 = f1, FPR = FPR)
    })
    
    metrics_df <- as.data.frame(t(metrics))
    metrics_df$Threshold <- i
    all_metrics <- rbind(all_metrics, metrics_df)
  }
  
  all_metrics
}

################################################################################
read_bootstrap_data <- function(single_case, alg, region, resampemethod){
  # resampemethod has to be '', or '_ada' or '_smote' or '_smoteenn' or '_smotetom'
  # alg has to be LR, XGB, RF
  dfy <- read.csv(paste("bld/", single_case, "/output_df_", alg, "_", region, resampemethod, ".csv", sep = ''))
  return(dfy)
}

recall_FPR_compare <- function(df_before, df_during, Threshold_list) {
  colnames(df_before)[1:5] <- paste(colnames(df_before)[1:5], 'before', sep = '_')
  output <- merge(df_before, df_during, by = 'Threshold', all.x = TRUE)
  return(output)
}

summ_data_manage <- function(df, region){
  df$model <- sapply(strsplit(df$method, "_"), `[`, 1)
  df$resample <- sapply(strsplit(df$method, "_"), function(x) ifelse(length(x) > 1, x[2], "balanced weight"))
  Resampling <- df$resample
  Resampling <- gsub('noW', 'original', Resampling)
  Resampling <- gsub('ada', 'ADASYN', Resampling)
  Resampling <- gsub('smoteenn', 'SMOTE ENN', Resampling)
  Resampling <- gsub('smotetom', 'SMOTE Tomek', Resampling)
  Resampling <- gsub('smote', 'SMOTE', Resampling)
  df['Resampling'] <- paste(df$model, Resampling, sep = ' ')
  df['Region'] <- region
  return(df)
}
################################################################################
# district
region <- 'district'
band_plot_df_LR <- read_bootstrap_data('single_case', 'LR', region, '')
band_plot_df_LR_noW <- read_bootstrap_data('single_case', 'LR', region, '_noW')
band_plot_df_LR_ada <- read_bootstrap_data('single_case', 'LR', region, '_ada')
band_plot_df_LR_smote <- read_bootstrap_data('single_case', 'LR', region, '_smote')
band_plot_df_LR_smoteenn <- read_bootstrap_data('single_case', 'LR', region, '_smoteenn')
band_plot_df_LR_smotetom <- read_bootstrap_data('single_case', 'LR', region, '_smotetom')
band_plot_df_LR_before <- read_bootstrap_data('single_case_before', 'LR', region, '_before')
band_plot_df_LR_before_noW <- read_bootstrap_data('single_case_before', 'LR', region, '_before_noW')
band_plot_df_LR_ada_before <- read_bootstrap_data('single_case_before', 'LR', region, '_before_ada')
band_plot_df_LR_smote_before <- read_bootstrap_data('single_case_before', 'LR', region, '_before_smote')
band_plot_df_LR_smoteenn_before <- read_bootstrap_data('single_case_before', 'LR', region, '_before_smoteenn')
band_plot_df_LR_smotetom_before <- read_bootstrap_data('single_case_before', 'LR', region, '_before_smotetom')
band_plot_df_RF <- read_bootstrap_data('single_case', 'RF', region, '')
band_plot_df_RF_noW <- read_bootstrap_data('single_case', 'RF', region, '_noW')
band_plot_df_RF_ada <- read_bootstrap_data('single_case', 'RF', region, '_ada')
band_plot_df_RF_smote <- read_bootstrap_data('single_case', 'RF', region, '_smote')
band_plot_df_RF_smoteenn <- read_bootstrap_data('single_case', 'RF', region, '_smoteenn')
band_plot_df_RF_smotetom <- read_bootstrap_data('single_case', 'RF', region, '_smotetom')
band_plot_df_RF_before <- read_bootstrap_data('single_case_before', 'RF', region, '')
band_plot_df_RF_before_noW <- read_bootstrap_data('single_case_before', 'RF', region, '_noW')
band_plot_df_RF_ada_before <- read_bootstrap_data('single_case_before', 'RF', region, '_ada')
band_plot_df_RF_smote_before <- read_bootstrap_data('single_case_before', 'RF', region, '_smote')
band_plot_df_RF_smoteenn_before <- read_bootstrap_data('single_case_before', 'RF', region, '_smoteenn')
band_plot_df_RF_smotetom_before <- read_bootstrap_data('single_case_before', 'RF', region, '_smotetom')
band_plot_df_XGB <- read_bootstrap_data('single_case', 'XGB', region, '')
band_plot_df_XGB_noW <- read_bootstrap_data('single_case', 'XGB', region, '_noW')
band_plot_df_XGB_ada <- read_bootstrap_data('single_case', 'XGB', region, '_ada')
band_plot_df_XGB_smote <- read_bootstrap_data('single_case', 'XGB', region, '_smote')
band_plot_df_XGB_smoteenn <- read_bootstrap_data('single_case', 'XGB', region, '_smoteenn')
band_plot_df_XGB_smotetom <- read_bootstrap_data('single_case', 'XGB', region, '_smotetom')
band_plot_df_XGB_before <- read_bootstrap_data('single_case_before', 'XGB', region, '')
band_plot_df_XGB_before_noW <- read_bootstrap_data('single_case_before', 'XGB', region, '_noW')
band_plot_df_XGB_ada_before <- read_bootstrap_data('single_case_before', 'XGB', region, '_ada')
band_plot_df_XGB_smote_before <- read_bootstrap_data('single_case_before', 'XGB', region, '_smote')
band_plot_df_XGB_smoteenn_before <- read_bootstrap_data('single_case_before', 'XGB', region, '_smoteenn')
band_plot_df_XGB_smotetom_before <- read_bootstrap_data('single_case_before', 'XGB', region, '_smotetom')
tiks <- 0.001
all_metrics_LR <- compute_metrics(band_plot_df_LR[,1], band_plot_df_LR[,2], tiks)
all_metrics_LR_noW <- compute_metrics(band_plot_df_LR_noW[,1], band_plot_df_LR_noW[,2], tiks)
all_metrics_LR_ada <- compute_metrics(band_plot_df_LR_ada[,1], band_plot_df_LR_ada[,2], tiks)
all_metrics_LR_smote <- compute_metrics(band_plot_df_LR_smote[,1], band_plot_df_LR_smote[,2], tiks)
all_metrics_LR_smoteenn <- compute_metrics(band_plot_df_LR_smoteenn[,1], band_plot_df_LR_smoteenn[,2], tiks)
all_metrics_LR_smotetom <- compute_metrics(band_plot_df_LR_smotetom[,1], band_plot_df_LR_smotetom[,2], tiks)
all_metrics_LR_before <- compute_metrics(band_plot_df_LR_before[,1], band_plot_df_LR_before[,2], tiks)
all_metrics_LR_before_noW <- compute_metrics(band_plot_df_LR_before_noW[,1], band_plot_df_LR_before_noW[,2], tiks)
all_metrics_LR_ada_before <- compute_metrics(band_plot_df_LR_ada_before[,1], band_plot_df_LR_ada_before[,2], tiks)
all_metrics_LR_smote_before <- compute_metrics(band_plot_df_LR_smote_before[,1], band_plot_df_LR_smote_before[,2], tiks)
all_metrics_LR_smoteenn_before <- compute_metrics(band_plot_df_LR_smoteenn_before[,1], band_plot_df_LR_smoteenn_before[,2], tiks)
all_metrics_LR_smotetom_before <- compute_metrics(band_plot_df_LR_smotetom_before[,1], band_plot_df_LR_smotetom_before[,2], tiks)

all_metrics_RF <- compute_metrics(band_plot_df_RF[,1], band_plot_df_RF[,2], tiks)
all_metrics_RF_noW <- compute_metrics(band_plot_df_RF_noW[,1], band_plot_df_RF_noW[,2], tiks)
all_metrics_RF_ada <- compute_metrics(band_plot_df_RF_ada[,1], band_plot_df_RF_ada[,2], tiks)
all_metrics_RF_smote <- compute_metrics(band_plot_df_RF_smote[,1], band_plot_df_RF_smote[,2], tiks)
all_metrics_RF_smoteenn <- compute_metrics(band_plot_df_RF_smoteenn[,1], band_plot_df_RF_smoteenn[,2], tiks)
all_metrics_RF_smotetom <- compute_metrics(band_plot_df_RF_smotetom[,1], band_plot_df_RF_smotetom[,2], tiks)
all_metrics_RF_before <- compute_metrics(band_plot_df_RF_before[,1], band_plot_df_RF_before[,2], tiks)
all_metrics_RF_before_noW <- compute_metrics(band_plot_df_RF_before_noW[,1], band_plot_df_RF_before_noW[,2], tiks)
all_metrics_RF_ada_before <- compute_metrics(band_plot_df_RF_ada_before[,1], band_plot_df_RF_ada_before[,2], tiks)
all_metrics_RF_smote_before <- compute_metrics(band_plot_df_RF_smote_before[,1], band_plot_df_RF_smote_before[,2], tiks)
all_metrics_RF_smoteenn_before <- compute_metrics(band_plot_df_RF_smoteenn_before[,1], band_plot_df_RF_smoteenn_before[,2], tiks)
all_metrics_RF_smotetom_before <- compute_metrics(band_plot_df_RF_smotetom_before[,1], band_plot_df_RF_smotetom_before[,2], tiks)
all_metrics_XGB <- compute_metrics(band_plot_df_XGB[,1], band_plot_df_XGB[,2], tiks)
all_metrics_XGB_noW <- compute_metrics(band_plot_df_XGB_noW[,1], band_plot_df_XGB_noW[,2], tiks)
all_metrics_XGB_ada <- compute_metrics(band_plot_df_XGB_ada[,1], band_plot_df_XGB_ada[,2], tiks)
all_metrics_XGB_smote <- compute_metrics(band_plot_df_XGB_smote[,1], band_plot_df_XGB_smote[,2], tiks)
all_metrics_XGB_smoteenn <- compute_metrics(band_plot_df_XGB_smoteenn[,1], band_plot_df_XGB_smoteenn[,2], tiks)
all_metrics_XGB_smotetom <- compute_metrics(band_plot_df_XGB_smotetom[,1], band_plot_df_XGB_smotetom[,2], tiks)
all_metrics_XGB_before <- compute_metrics(band_plot_df_XGB_before[,1], band_plot_df_XGB_before[,2], tiks)
all_metrics_XGB_before_noW <- compute_metrics(band_plot_df_XGB_before_noW[,1], band_plot_df_XGB_before_noW[,2], tiks)
all_metrics_XGB_ada_before <- compute_metrics(band_plot_df_XGB_ada_before[,1], band_plot_df_XGB_ada_before[,2], tiks)
all_metrics_XGB_smote_before <- compute_metrics(band_plot_df_XGB_smote_before[,1], band_plot_df_XGB_smote_before[,2], tiks)
all_metrics_XGB_smoteenn_before <- compute_metrics(band_plot_df_XGB_smoteenn_before[,1], band_plot_df_XGB_smoteenn_before[,2], tiks)
all_metrics_XGB_smotetom_before <- compute_metrics(band_plot_df_XGB_smotetom_before[,1], band_plot_df_XGB_smotetom_before[,2], tiks)
results_list <- list()
recall_lt <- 1:100
# Compute and store metrics for LR with various techniques
results_list[["LR"]] <- recall_FPR_compare(all_metrics_LR_before, all_metrics_LR, recall_lt)
results_list[["LR_noW"]] <- recall_FPR_compare(all_metrics_LR_before_noW, all_metrics_LR_noW, recall_lt)
results_list[["LR_ada"]] <- recall_FPR_compare(all_metrics_LR_ada_before, all_metrics_LR_ada, recall_lt)
results_list[["LR_smote"]] <- recall_FPR_compare(all_metrics_LR_smote_before, all_metrics_LR_smote, recall_lt)
results_list[["LR_smoteenn"]] <- recall_FPR_compare(all_metrics_LR_smoteenn_before, all_metrics_LR_smoteenn, recall_lt)
results_list[["LR_smotetom"]] <- recall_FPR_compare(all_metrics_LR_smotetom_before, all_metrics_LR_smotetom, recall_lt)
# Similarly, compute for RF
results_list[["RF"]] <- recall_FPR_compare(all_metrics_RF_before, all_metrics_RF, recall_lt)
results_list[["RF_noW"]] <- recall_FPR_compare(all_metrics_RF_before_noW, all_metrics_RF_noW, recall_lt)
results_list[["RF_ada"]] <- recall_FPR_compare(all_metrics_RF_ada_before, all_metrics_RF_ada, recall_lt)
results_list[["RF_smote"]] <- recall_FPR_compare(all_metrics_RF_smote_before, all_metrics_RF_smote, recall_lt)
results_list[["RF_smoteenn"]] <- recall_FPR_compare(all_metrics_RF_smoteenn_before, all_metrics_RF_smoteenn, recall_lt)
results_list[["RF_smotetom"]] <- recall_FPR_compare(all_metrics_RF_smotetom_before, all_metrics_RF_smotetom, recall_lt)
# Compute for XGB
results_list[["XGB"]] <- recall_FPR_compare(all_metrics_XGB_before, all_metrics_XGB, recall_lt)
results_list[["XGB_noW"]] <- recall_FPR_compare(all_metrics_XGB_before_noW, all_metrics_XGB_noW, recall_lt)
results_list[["XGB_ada"]] <- recall_FPR_compare(all_metrics_XGB_ada_before, all_metrics_XGB_ada, recall_lt)
results_list[["XGB_smote"]] <- recall_FPR_compare(all_metrics_XGB_smote_before, all_metrics_XGB_smote, recall_lt)
results_list[["XGB_smoteenn"]] <- recall_FPR_compare(all_metrics_XGB_smoteenn_before, all_metrics_XGB_smoteenn, recall_lt)
results_list[["XGB_smotetom"]] <- recall_FPR_compare(all_metrics_XGB_smotetom_before, all_metrics_XGB_smotetom, recall_lt)
# Convert list to a data frame and add a 'method' column
results_df <- do.call(rbind, lapply(names(results_list), function(name) {
  df <- results_list[[name]]
  df$method <- name
  return(df)
}))
# Reset row names if needed
row.names(results_df) <- NULL
unique(results_df$method)
# Split 'method' into 'method_initial' and 'method_suffix'
results_df$model <- sapply(strsplit(results_df$method, "_"), `[`, 1)
results_df$resample <- sapply(strsplit(results_df$method, "_"), function(x) ifelse(length(x) > 1, x[2], "balanced weight"))
Resampling <- results_df$resample
Resampling <- gsub('noW', 'original', Resampling)
Resampling <- gsub('ada', 'ADASYN', Resampling)
Resampling <- gsub('smoteenn', 'SMOTE ENN', Resampling)
Resampling <- gsub('smotetom', 'SMOTE Tomek', Resampling)
Resampling <- gsub('smote', 'SMOTE', Resampling)
Resampling <- gsub('smote', 'SMOTE', Resampling)
results_df['Resampling'] <- paste(results_df$model, Resampling, sep = ' ')
results_df_plt <- results_df
results_df_plt_district <- results_df_plt
##########################################################################################
##########################################################################################
# county
region <- 'county'
band_plot_df_LR <- read_bootstrap_data('single_case', 'LR', region, '')
band_plot_df_LR_noW <- read_bootstrap_data('single_case', 'LR', region, '_noW')
band_plot_df_LR_ada <- read_bootstrap_data('single_case', 'LR', region, '_ada')
band_plot_df_LR_smote <- read_bootstrap_data('single_case', 'LR', region, '_smote')
band_plot_df_LR_smoteenn <- read_bootstrap_data('single_case', 'LR', region, '_smoteenn')
band_plot_df_LR_smotetom <- read_bootstrap_data('single_case', 'LR', region, '_smotetom')
band_plot_df_LR_before <- read_bootstrap_data('single_case_before', 'LR', region, '_before')
band_plot_df_LR_before_noW <- read_bootstrap_data('single_case_before', 'LR', region, '_before_noW')
band_plot_df_LR_ada_before <- read_bootstrap_data('single_case_before', 'LR', region, '_before_ada')
band_plot_df_LR_smote_before <- read_bootstrap_data('single_case_before', 'LR', region, '_before_smote')
band_plot_df_LR_smoteenn_before <- read_bootstrap_data('single_case_before', 'LR', region, '_before_smoteenn')
band_plot_df_LR_smotetom_before <- read_bootstrap_data('single_case_before', 'LR', region, '_before_smotetom')
band_plot_df_RF <- read_bootstrap_data('single_case', 'RF', region, '')
band_plot_df_RF_noW <- read_bootstrap_data('single_case', 'RF', region, '_noW')
band_plot_df_RF_ada <- read_bootstrap_data('single_case', 'RF', region, '_ada')
band_plot_df_RF_smote <- read_bootstrap_data('single_case', 'RF', region, '_smote')
band_plot_df_RF_smoteenn <- read_bootstrap_data('single_case', 'RF', region, '_smoteenn')
band_plot_df_RF_smotetom <- read_bootstrap_data('single_case', 'RF', region, '_smotetom')
band_plot_df_RF_before <- read_bootstrap_data('single_case_before', 'RF', region, '')
band_plot_df_RF_before_noW <- read_bootstrap_data('single_case_before', 'RF', region, '_before_noW')
band_plot_df_RF_ada_before <- read_bootstrap_data('single_case_before', 'RF', region, '_ada')
band_plot_df_RF_smote_before <- read_bootstrap_data('single_case_before', 'RF', region, '_smote')
band_plot_df_RF_smoteenn_before <- read_bootstrap_data('single_case_before', 'RF', region, '_smoteenn')
band_plot_df_RF_smotetom_before <- read_bootstrap_data('single_case_before', 'RF', region, '_smotetom')
band_plot_df_XGB <- read_bootstrap_data('single_case', 'XGB', region, '')
band_plot_df_XGB_noW <- read_bootstrap_data('single_case', 'XGB', region, '_noW')
band_plot_df_XGB_ada <- read_bootstrap_data('single_case', 'XGB', region, '_ada')
band_plot_df_XGB_smote <- read_bootstrap_data('single_case', 'XGB', region, '_smote')
band_plot_df_XGB_smoteenn <- read_bootstrap_data('single_case', 'XGB', region, '_smoteenn')
band_plot_df_XGB_smotetom <- read_bootstrap_data('single_case', 'XGB', region, '_smotetom')
band_plot_df_XGB_before <- read_bootstrap_data('single_case_before', 'XGB', region, '')
band_plot_df_XGB_before_noW <- read_bootstrap_data('single_case_before', 'XGB', region, '_before_noW')
band_plot_df_XGB_ada_before <- read_bootstrap_data('single_case_before', 'XGB', region, '_ada')
band_plot_df_XGB_smote_before <- read_bootstrap_data('single_case_before', 'XGB', region, '_smote')
band_plot_df_XGB_smoteenn_before <- read_bootstrap_data('single_case_before', 'XGB', region, '_smoteenn')
band_plot_df_XGB_smotetom_before <- read_bootstrap_data('single_case_before', 'XGB', region, '_smotetom')

all_metrics_LR <- compute_metrics(band_plot_df_LR[,1], band_plot_df_LR[,2], tiks)
all_metrics_LR_noW <- compute_metrics(band_plot_df_LR_noW[,1], band_plot_df_LR_noW[,2], tiks)
all_metrics_LR_ada <- compute_metrics(band_plot_df_LR_ada[,1], band_plot_df_LR_ada[,2], tiks)
all_metrics_LR_smote <- compute_metrics(band_plot_df_LR_smote[,1], band_plot_df_LR_smote[,2], tiks)
all_metrics_LR_smoteenn <- compute_metrics(band_plot_df_LR_smoteenn[,1], band_plot_df_LR_smoteenn[,2], tiks)
all_metrics_LR_smotetom <- compute_metrics(band_plot_df_LR_smotetom[,1], band_plot_df_LR_smotetom[,2], tiks)
all_metrics_LR_before <- compute_metrics(band_plot_df_LR_before[,1], band_plot_df_LR_before[,2], tiks)
all_metrics_LR_before_noW <- compute_metrics(band_plot_df_LR_before_noW[,1], band_plot_df_LR_before_noW[,2], tiks)
all_metrics_LR_ada_before <- compute_metrics(band_plot_df_LR_ada_before[,1], band_plot_df_LR_ada_before[,2], tiks)
all_metrics_LR_smote_before <- compute_metrics(band_plot_df_LR_smote_before[,1], band_plot_df_LR_smote_before[,2], tiks)
all_metrics_LR_smoteenn_before <- compute_metrics(band_plot_df_LR_smoteenn_before[,1], band_plot_df_LR_smoteenn_before[,2], tiks)
all_metrics_LR_smotetom_before <- compute_metrics(band_plot_df_LR_smotetom_before[,1], band_plot_df_LR_smotetom_before[,2], tiks)

all_metrics_RF <- compute_metrics(band_plot_df_RF[,1], band_plot_df_RF[,2], tiks)
all_metrics_RF_noW <- compute_metrics(band_plot_df_RF_noW[,1], band_plot_df_RF_noW[,2], tiks)
all_metrics_RF_ada <- compute_metrics(band_plot_df_RF_ada[,1], band_plot_df_RF_ada[,2], tiks)
all_metrics_RF_smote <- compute_metrics(band_plot_df_RF_smote[,1], band_plot_df_RF_smote[,2], tiks)
all_metrics_RF_smoteenn <- compute_metrics(band_plot_df_RF_smoteenn[,1], band_plot_df_RF_smoteenn[,2], tiks)
all_metrics_RF_smotetom <- compute_metrics(band_plot_df_RF_smotetom[,1], band_plot_df_RF_smotetom[,2], tiks)
all_metrics_RF_before <- compute_metrics(band_plot_df_RF_before[,1], band_plot_df_RF_before[,2], tiks)
all_metrics_RF_before_noW <- compute_metrics(band_plot_df_RF_before_noW[,1], band_plot_df_RF_before_noW[,2], tiks)
all_metrics_RF_ada_before <- compute_metrics(band_plot_df_RF_ada_before[,1], band_plot_df_RF_ada_before[,2], tiks)
all_metrics_RF_smote_before <- compute_metrics(band_plot_df_RF_smote_before[,1], band_plot_df_RF_smote_before[,2], tiks)
all_metrics_RF_smoteenn_before <- compute_metrics(band_plot_df_RF_smoteenn_before[,1], band_plot_df_RF_smoteenn_before[,2], tiks)
all_metrics_RF_smotetom_before <- compute_metrics(band_plot_df_RF_smotetom_before[,1], band_plot_df_RF_smotetom_before[,2], tiks)
all_metrics_XGB <- compute_metrics(band_plot_df_XGB[,1], band_plot_df_XGB[,2], tiks)
all_metrics_XGB_noW <- compute_metrics(band_plot_df_XGB_noW[,1], band_plot_df_XGB_noW[,2], tiks)
all_metrics_XGB_ada <- compute_metrics(band_plot_df_XGB_ada[,1], band_plot_df_XGB_ada[,2], tiks)
all_metrics_XGB_smote <- compute_metrics(band_plot_df_XGB_smote[,1], band_plot_df_XGB_smote[,2], tiks)
all_metrics_XGB_smoteenn <- compute_metrics(band_plot_df_XGB_smoteenn[,1], band_plot_df_XGB_smoteenn[,2], tiks)
all_metrics_XGB_smotetom <- compute_metrics(band_plot_df_XGB_smotetom[,1], band_plot_df_XGB_smotetom[,2], tiks)
all_metrics_XGB_before <- compute_metrics(band_plot_df_XGB_before[,1], band_plot_df_XGB_before[,2], tiks)
all_metrics_XGB_before_noW <- compute_metrics(band_plot_df_XGB_before_noW[,1], band_plot_df_XGB_before_noW[,2], tiks)
all_metrics_XGB_ada_before <- compute_metrics(band_plot_df_XGB_ada_before[,1], band_plot_df_XGB_ada_before[,2], tiks)
all_metrics_XGB_smote_before <- compute_metrics(band_plot_df_XGB_smote_before[,1], band_plot_df_XGB_smote_before[,2], tiks)
all_metrics_XGB_smoteenn_before <- compute_metrics(band_plot_df_XGB_smoteenn_before[,1], band_plot_df_XGB_smoteenn_before[,2], tiks)
all_metrics_XGB_smotetom_before <- compute_metrics(band_plot_df_XGB_smotetom_before[,1], band_plot_df_XGB_smotetom_before[,2], tiks)
results_list <- list()
recall_lt <- 1:100
# Compute and store metrics for LR with various techniques
results_list[["LR"]] <- recall_FPR_compare(all_metrics_LR_before, all_metrics_LR, recall_lt)
results_list[["LR_noW"]] <- recall_FPR_compare(all_metrics_LR_before_noW, all_metrics_LR_noW, recall_lt)
results_list[["LR_ada"]] <- recall_FPR_compare(all_metrics_LR_ada_before, all_metrics_LR_ada, recall_lt)
results_list[["LR_smote"]] <- recall_FPR_compare(all_metrics_LR_smote_before, all_metrics_LR_smote, recall_lt)
results_list[["LR_smoteenn"]] <- recall_FPR_compare(all_metrics_LR_smoteenn_before, all_metrics_LR_smoteenn, recall_lt)
results_list[["LR_smotetom"]] <- recall_FPR_compare(all_metrics_LR_smotetom_before, all_metrics_LR_smotetom, recall_lt)
# Similarly, compute for RF
results_list[["RF"]] <- recall_FPR_compare(all_metrics_RF_before, all_metrics_RF, recall_lt)
results_list[["RF_noW"]] <- recall_FPR_compare(all_metrics_RF_before_noW, all_metrics_RF_noW, recall_lt)
results_list[["RF_ada"]] <- recall_FPR_compare(all_metrics_RF_ada_before, all_metrics_RF_ada, recall_lt)
results_list[["RF_smote"]] <- recall_FPR_compare(all_metrics_RF_smote_before, all_metrics_RF_smote, recall_lt)
results_list[["RF_smoteenn"]] <- recall_FPR_compare(all_metrics_RF_smoteenn_before, all_metrics_RF_smoteenn, recall_lt)
results_list[["RF_smotetom"]] <- recall_FPR_compare(all_metrics_RF_smotetom_before, all_metrics_RF_smotetom, recall_lt)
# Compute for XGB
results_list[["XGB"]] <- recall_FPR_compare(all_metrics_XGB_before, all_metrics_XGB, recall_lt)
results_list[["XGB_noW"]] <- recall_FPR_compare(all_metrics_XGB_before_noW, all_metrics_XGB_noW, recall_lt)
results_list[["XGB_ada"]] <- recall_FPR_compare(all_metrics_XGB_ada_before, all_metrics_XGB_ada, recall_lt)
results_list[["XGB_smote"]] <- recall_FPR_compare(all_metrics_XGB_smote_before, all_metrics_XGB_smote, recall_lt)
results_list[["XGB_smoteenn"]] <- recall_FPR_compare(all_metrics_XGB_smoteenn_before, all_metrics_XGB_smoteenn, recall_lt)
results_list[["XGB_smotetom"]] <- recall_FPR_compare(all_metrics_XGB_smotetom_before, all_metrics_XGB_smotetom, recall_lt)
# Convert list to a data frame and add a 'method' column
results_df <- do.call(rbind, lapply(names(results_list), function(name) {
  df <- results_list[[name]]
  df$method <- name
  return(df)
}))
# Reset row names if needed
row.names(results_df) <- NULL
unique(results_df$method)
# Split 'method' into 'method_initial' and 'method_suffix'
results_df$model <- sapply(strsplit(results_df$method, "_"), `[`, 1)
results_df$resample <- sapply(strsplit(results_df$method, "_"), function(x) ifelse(length(x) > 1, x[2], "balanced weight"))
Resampling <- results_df$resample
Resampling <- gsub('noW', 'original', Resampling)
Resampling <- gsub('ada', 'ADASYN', Resampling)
Resampling <- gsub('smoteenn', 'SMOTE ENN', Resampling)
Resampling <- gsub('smotetom', 'SMOTE Tomek', Resampling)
Resampling <- gsub('smote', 'SMOTE', Resampling)
Resampling <- gsub('smote', 'SMOTE', Resampling)
results_df['Resampling'] <- paste(results_df$model, Resampling, sep = ' ')
results_df_plt <- results_df
results_df_plt_county <- results_df_plt
##########################################################################################
##########################################################################################
# subcounty
region <- 'subcounty'
band_plot_df_LR <- read_bootstrap_data('single_case', 'LR', region, '')
band_plot_df_LR_noW <- read_bootstrap_data('single_case', 'LR', region, '_noW')
band_plot_df_LR_ada <- read_bootstrap_data('single_case', 'LR', region, '_ada')
band_plot_df_LR_smote <- read_bootstrap_data('single_case', 'LR', region, '_smote')
band_plot_df_LR_smoteenn <- read_bootstrap_data('single_case', 'LR', region, '_smoteenn')
band_plot_df_LR_smotetom <- read_bootstrap_data('single_case', 'LR', region, '_smotetom')
band_plot_df_LR_before <- read_bootstrap_data('single_case_before', 'LR', region, '_before')
band_plot_df_LR_before_noW <- read_bootstrap_data('single_case_before', 'LR', region, '_before_noW')
band_plot_df_LR_ada_before <- read_bootstrap_data('single_case_before', 'LR', region, '_before_ada')
band_plot_df_LR_smote_before <- read_bootstrap_data('single_case_before', 'LR', region, '_before_smote')
band_plot_df_LR_smoteenn_before <- read_bootstrap_data('single_case_before', 'LR', region, '_before_smoteenn')
band_plot_df_LR_smotetom_before <- read_bootstrap_data('single_case_before', 'LR', region, '_before_smotetom')
band_plot_df_RF <- read_bootstrap_data('single_case', 'RF', region, '')
band_plot_df_RF_noW <- read_bootstrap_data('single_case', 'RF', region, '_noW')
band_plot_df_RF_ada <- read_bootstrap_data('single_case', 'RF', region, '_ada')
band_plot_df_RF_smote <- read_bootstrap_data('single_case', 'RF', region, '_smote')
band_plot_df_RF_smoteenn <- read_bootstrap_data('single_case', 'RF', region, '_smoteenn')
band_plot_df_RF_smotetom <- read_bootstrap_data('single_case', 'RF', region, '_smotetom')
band_plot_df_RF_before <- read_bootstrap_data('single_case_before', 'RF', region, '')
band_plot_df_RF_before_noW <- read_bootstrap_data('single_case_before', 'RF', region, '_before_noW')
band_plot_df_RF_ada_before <- read_bootstrap_data('single_case_before', 'RF', region, '_ada')
band_plot_df_RF_smote_before <- read_bootstrap_data('single_case_before', 'RF', region, '_smote')
band_plot_df_RF_smoteenn_before <- read_bootstrap_data('single_case_before', 'RF', region, '_smoteenn')
band_plot_df_RF_smotetom_before <- read_bootstrap_data('single_case_before', 'RF', region, '_smotetom')
band_plot_df_XGB <- read_bootstrap_data('single_case', 'XGB', region, '')
band_plot_df_XGB_noW <- read_bootstrap_data('single_case', 'XGB', region, '_noW')
band_plot_df_XGB_ada <- read_bootstrap_data('single_case', 'XGB', region, '_ada')
band_plot_df_XGB_smote <- read_bootstrap_data('single_case', 'XGB', region, '_smote')
band_plot_df_XGB_smoteenn <- read_bootstrap_data('single_case', 'XGB', region, '_smoteenn')
band_plot_df_XGB_smotetom <- read_bootstrap_data('single_case', 'XGB', region, '_smotetom')
band_plot_df_XGB_before <- read_bootstrap_data('single_case_before', 'XGB', region, '')
band_plot_df_XGB_before_noW <- read_bootstrap_data('single_case_before', 'XGB', region, '_before_noW')
band_plot_df_XGB_ada_before <- read_bootstrap_data('single_case_before', 'XGB', region, '_ada')
band_plot_df_XGB_smote_before <- read_bootstrap_data('single_case_before', 'XGB', region, '_smote')
band_plot_df_XGB_smoteenn_before <- read_bootstrap_data('single_case_before', 'XGB', region, '_smoteenn')
band_plot_df_XGB_smotetom_before <- read_bootstrap_data('single_case_before', 'XGB', region, '_smotetom')

all_metrics_LR <- compute_metrics(band_plot_df_LR[,1], band_plot_df_LR[,2], tiks)
all_metrics_LR_noW <- compute_metrics(band_plot_df_LR_noW[,1], band_plot_df_LR_noW[,2], tiks)
all_metrics_LR_ada <- compute_metrics(band_plot_df_LR_ada[,1], band_plot_df_LR_ada[,2], tiks)
all_metrics_LR_smote <- compute_metrics(band_plot_df_LR_smote[,1], band_plot_df_LR_smote[,2], tiks)
all_metrics_LR_smoteenn <- compute_metrics(band_plot_df_LR_smoteenn[,1], band_plot_df_LR_smoteenn[,2], tiks)
all_metrics_LR_smotetom <- compute_metrics(band_plot_df_LR_smotetom[,1], band_plot_df_LR_smotetom[,2], tiks)
all_metrics_LR_before <- compute_metrics(band_plot_df_LR_before[,1], band_plot_df_LR_before[,2], tiks)
all_metrics_LR_before_noW <- compute_metrics(band_plot_df_LR_before_noW[,1], band_plot_df_LR_before_noW[,2], tiks)
all_metrics_LR_ada_before <- compute_metrics(band_plot_df_LR_ada_before[,1], band_plot_df_LR_ada_before[,2], tiks)
all_metrics_LR_smote_before <- compute_metrics(band_plot_df_LR_smote_before[,1], band_plot_df_LR_smote_before[,2], tiks)
all_metrics_LR_smoteenn_before <- compute_metrics(band_plot_df_LR_smoteenn_before[,1], band_plot_df_LR_smoteenn_before[,2], tiks)
all_metrics_LR_smotetom_before <- compute_metrics(band_plot_df_LR_smotetom_before[,1], band_plot_df_LR_smotetom_before[,2], tiks)

all_metrics_RF <- compute_metrics(band_plot_df_RF[,1], band_plot_df_RF[,2], tiks)
all_metrics_RF_noW <- compute_metrics(band_plot_df_RF_noW[,1], band_plot_df_RF_noW[,2], tiks)
all_metrics_RF_ada <- compute_metrics(band_plot_df_RF_ada[,1], band_plot_df_RF_ada[,2], tiks)
all_metrics_RF_smote <- compute_metrics(band_plot_df_RF_smote[,1], band_plot_df_RF_smote[,2], tiks)
all_metrics_RF_smoteenn <- compute_metrics(band_plot_df_RF_smoteenn[,1], band_plot_df_RF_smoteenn[,2], tiks)
all_metrics_RF_smotetom <- compute_metrics(band_plot_df_RF_smotetom[,1], band_plot_df_RF_smotetom[,2], tiks)
all_metrics_RF_before <- compute_metrics(band_plot_df_RF_before[,1], band_plot_df_RF_before[,2], tiks)
all_metrics_RF_before_noW <- compute_metrics(band_plot_df_RF_before_noW[,1], band_plot_df_RF_before_noW[,2], tiks)
all_metrics_RF_ada_before <- compute_metrics(band_plot_df_RF_ada_before[,1], band_plot_df_RF_ada_before[,2], tiks)
all_metrics_RF_smote_before <- compute_metrics(band_plot_df_RF_smote_before[,1], band_plot_df_RF_smote_before[,2], tiks)
all_metrics_RF_smoteenn_before <- compute_metrics(band_plot_df_RF_smoteenn_before[,1], band_plot_df_RF_smoteenn_before[,2], tiks)
all_metrics_RF_smotetom_before <- compute_metrics(band_plot_df_RF_smotetom_before[,1], band_plot_df_RF_smotetom_before[,2], tiks)
all_metrics_XGB <- compute_metrics(band_plot_df_XGB[,1], band_plot_df_XGB[,2], tiks)
all_metrics_XGB_noW <- compute_metrics(band_plot_df_XGB_noW[,1], band_plot_df_XGB_noW[,2], tiks)
all_metrics_XGB_ada <- compute_metrics(band_plot_df_XGB_ada[,1], band_plot_df_XGB_ada[,2], tiks)
all_metrics_XGB_smote <- compute_metrics(band_plot_df_XGB_smote[,1], band_plot_df_XGB_smote[,2], tiks)
all_metrics_XGB_smoteenn <- compute_metrics(band_plot_df_XGB_smoteenn[,1], band_plot_df_XGB_smoteenn[,2], tiks)
all_metrics_XGB_smotetom <- compute_metrics(band_plot_df_XGB_smotetom[,1], band_plot_df_XGB_smotetom[,2], tiks)
all_metrics_XGB_before <- compute_metrics(band_plot_df_XGB_before[,1], band_plot_df_XGB_before[,2], tiks)
all_metrics_XGB_before_noW <- compute_metrics(band_plot_df_XGB_before_noW[,1], band_plot_df_XGB_before_noW[,2], tiks)
all_metrics_XGB_ada_before <- compute_metrics(band_plot_df_XGB_ada_before[,1], band_plot_df_XGB_ada_before[,2], tiks)
all_metrics_XGB_smote_before <- compute_metrics(band_plot_df_XGB_smote_before[,1], band_plot_df_XGB_smote_before[,2], tiks)
all_metrics_XGB_smoteenn_before <- compute_metrics(band_plot_df_XGB_smoteenn_before[,1], band_plot_df_XGB_smoteenn_before[,2], tiks)
all_metrics_XGB_smotetom_before <- compute_metrics(band_plot_df_XGB_smotetom_before[,1], band_plot_df_XGB_smotetom_before[,2], tiks)

results_list <- list()
recall_lt <- 1:100
# Compute and store metrics for LR with various techniques
results_list[["LR"]] <- recall_FPR_compare(all_metrics_LR_before, all_metrics_LR, recall_lt)
results_list[["LR_noW"]] <- recall_FPR_compare(all_metrics_LR_before_noW, all_metrics_LR_noW, recall_lt)
results_list[["LR_ada"]] <- recall_FPR_compare(all_metrics_LR_ada_before, all_metrics_LR_ada, recall_lt)
results_list[["LR_smote"]] <- recall_FPR_compare(all_metrics_LR_smote_before, all_metrics_LR_smote, recall_lt)
results_list[["LR_smoteenn"]] <- recall_FPR_compare(all_metrics_LR_smoteenn_before, all_metrics_LR_smoteenn, recall_lt)
results_list[["LR_smotetom"]] <- recall_FPR_compare(all_metrics_LR_smotetom_before, all_metrics_LR_smotetom, recall_lt)
# Similarly, compute for RF
results_list[["RF"]] <- recall_FPR_compare(all_metrics_RF_before, all_metrics_RF, recall_lt)
results_list[["RF_noW"]] <- recall_FPR_compare(all_metrics_RF_before_noW, all_metrics_RF_noW, recall_lt)
results_list[["RF_ada"]] <- recall_FPR_compare(all_metrics_RF_ada_before, all_metrics_RF_ada, recall_lt)
results_list[["RF_smote"]] <- recall_FPR_compare(all_metrics_RF_smote_before, all_metrics_RF_smote, recall_lt)
results_list[["RF_smoteenn"]] <- recall_FPR_compare(all_metrics_RF_smoteenn_before, all_metrics_RF_smoteenn, recall_lt)
results_list[["RF_smotetom"]] <- recall_FPR_compare(all_metrics_RF_smotetom_before, all_metrics_RF_smotetom, recall_lt)
# Compute for XGB
results_list[["XGB"]] <- recall_FPR_compare(all_metrics_XGB_before, all_metrics_XGB, recall_lt)
results_list[["XGB_noW"]] <- recall_FPR_compare(all_metrics_XGB_before_noW, all_metrics_XGB_noW, recall_lt)
results_list[["XGB_ada"]] <- recall_FPR_compare(all_metrics_XGB_ada_before, all_metrics_XGB_ada, recall_lt)
results_list[["XGB_smote"]] <- recall_FPR_compare(all_metrics_XGB_smote_before, all_metrics_XGB_smote, recall_lt)
results_list[["XGB_smoteenn"]] <- recall_FPR_compare(all_metrics_XGB_smoteenn_before, all_metrics_XGB_smoteenn, recall_lt)
results_list[["XGB_smotetom"]] <- recall_FPR_compare(all_metrics_XGB_smotetom_before, all_metrics_XGB_smotetom, recall_lt)
# Convert list to a data frame and add a 'method' column
results_df <- do.call(rbind, lapply(names(results_list), function(name) {
  df <- results_list[[name]]
  df$method <- name
  return(df)
}))
# Reset row names if needed
row.names(results_df) <- NULL
unique(results_df$method)
# Split 'method' into 'method_initial' and 'method_suffix'
results_df$model <- sapply(strsplit(results_df$method, "_"), `[`, 1)
results_df$resample <- sapply(strsplit(results_df$method, "_"), function(x) ifelse(length(x) > 1, x[2], "balanced weight"))
Resampling <- results_df$resample
Resampling <- gsub('noW', 'original', Resampling)
Resampling <- gsub('ada', 'ADASYN', Resampling)
Resampling <- gsub('smoteenn', 'SMOTE ENN', Resampling)
Resampling <- gsub('smotetom', 'SMOTE Tomek', Resampling)
Resampling <- gsub('smote', 'SMOTE', Resampling)
Resampling <- gsub('smote', 'SMOTE', Resampling)
results_df['Resampling'] <- paste(results_df$model, Resampling, sep = ' ')
results_df_plt <- results_df
results_df_plt_subcounty <- results_df_plt
###############################################################
############################

r4_colors2 <- c("LR" = "#7D0112",
                "RF" = "#1F968BFF", 
                "XGB" = "#440154FF" )
colnames(results_df_plt_district)[13] <- 'Model'
colnames(results_df_plt_county)[13] <- 'Model'
colnames(results_df_plt_subcounty)[13] <- 'Model'


colnames(results_df_plt_district)
# here change is COVID_before - COVID_during 
results_df_plt_district['Recall_change'] <- results_df_plt_district$Recall_before - results_df_plt_district$Recall
results_df_plt_district['Accuracy_change'] <- results_df_plt_district$Accuracy_before - results_df_plt_district$Accuracy
results_df_plt_district['FPR_change'] <- results_df_plt_district$FPR_before - results_df_plt_district$FPR
results_df_plt_county['Recall_change'] <- results_df_plt_county$Recall_before - results_df_plt_county$Recall
results_df_plt_county['Accuracy_change'] <- results_df_plt_county$Accuracy_before - results_df_plt_county$Accuracy
results_df_plt_county['FPR_change'] <- results_df_plt_county$FPR_before - results_df_plt_county$FPR
results_df_plt_subcounty['Recall_change'] <- results_df_plt_subcounty$Recall_before - results_df_plt_subcounty$Recall
results_df_plt_subcounty['Accuracy_change'] <- results_df_plt_subcounty$Accuracy_before - results_df_plt_subcounty$Accuracy
results_df_plt_subcounty['FPR_change'] <- results_df_plt_subcounty$FPR_before - results_df_plt_subcounty$FPR

results_df_plt_district['Precision_change'] <- results_df_plt_district$Precision_before - results_df_plt_district$Precision
results_df_plt_county['Precision_change'] <- results_df_plt_county$Precision_before - results_df_plt_county$Precision
results_df_plt_subcounty['Precision_change'] <- results_df_plt_subcounty$Precision_before - results_df_plt_subcounty$Precision
###########################################################################################
###########################################################################################
# given recall rate, compare the FPR
region_df <-results_df_plt_district

results_df_plt_district_before <- region_df[region_df$Recall_before >= 0.5 &
                                              region_df$Recall_before <= 0.7, ]
results_df_plt_district_during <- region_df[region_df$Recall >= 0.5 &
                                              region_df$Recall <= 0.7, ]

summary_metrics_before <- results_df_plt_district_before %>%
  group_by(Resampling, Model) %>%
  summarise(
    abs_Recall_change = mean(abs(Recall_change)), 
    abs_Accuracy_change = mean(abs(Accuracy_change)), 
    abs_FPR_change = mean(abs(FPR_change)), 
    abs_Precision_change = mean(abs(Precision_change)), 
    Recall_change = mean(Recall_change), 
    Accuracy_change = mean(Accuracy_change), 
    FPR_change = mean(FPR_change), 
    Precision_change = mean(Precision_change),     
    mean_Recall_before = mean(Recall_before), 
    mean_Accuracy_before = mean(Accuracy_before), 
    mean_FPR_before = mean(FPR_before), 
    mean_Precision_before = mean(Precision_before), 
    mean_Recall = mean(Recall), 
    mean_Accuracy = mean(Accuracy), 
    mean_FPR = mean(FPR), 
    mean_Precision = mean(Precision), 
    .groups = "drop"
  )

summary_metrics_during <- results_df_plt_district_during %>%
  group_by(Resampling, Model) %>%
  summarise(
    abs_Recall_change = mean(abs(Recall_change)), 
    abs_Accuracy_change = mean(abs(Accuracy_change)), 
    abs_FPR_change = mean(abs(FPR_change)), 
    abs_Precision_change = mean(abs(Precision_change)), 
    Recall_change = mean(Recall_change), 
    Accuracy_change = mean(Accuracy_change), 
    FPR_change = mean(FPR_change), 
    Precision_change = mean(Precision_change),     
    mean_Recall_before = mean(Recall_before), 
    mean_Accuracy_before = mean(Accuracy_before), 
    mean_FPR_before = mean(FPR_before), 
    mean_Precision_before = mean(Precision_before), 
    mean_Recall = mean(Recall), 
    mean_Accuracy = mean(Accuracy), 
    mean_FPR = mean(FPR), 
    mean_Precision = mean(Precision), 
    .groups = "drop"
  )

summary_metrics_before['period'] <- 'before'
summary_metrics_during['period'] <- 'during'

summary_metrics <- summary_metrics_during[, c(1, 2, 15:18)]
summary_metrics['mean_FPR_before'] <- summary_metrics_before$mean_FPR_before
summary_metrics['mean_Accuracy_before'] <- summary_metrics_before$mean_Accuracy_before
summary_metrics['mean_Recall_before'] <- summary_metrics_before$mean_Recall_before
summary_metrics['mean_Precision_before'] <- summary_metrics_before$mean_Precision_before
summary_metrics['abs_FPR'] <- abs(summary_metrics$mean_FPR - summary_metrics$mean_FPR_before)
summary_metrics['abs_Recall'] <- abs(summary_metrics$mean_Recall - summary_metrics$mean_Recall_before)
summary_metrics['abs_Accuracy'] <- abs(summary_metrics$mean_Accuracy - summary_metrics$mean_Accuracy_before)
summary_metrics['abs_Precision'] <- abs(summary_metrics$mean_Precision - summary_metrics$mean_Precision_before)
summary_metrics['Recall'] <- 'low'
summary_metrics_low <- summary_metrics

results_df_plt_district_before <- region_df[region_df$Recall_before >= 0.7 &
                                              region_df$Recall_before <= 0.9, ]
results_df_plt_district_during <- region_df[region_df$Recall >= 0.7 &
                                              region_df$Recall <= 0.9, ]

summary_metrics_before <- results_df_plt_district_before %>%
  group_by(Resampling, Model) %>%
  summarise(
    abs_Recall_change = mean(abs(Recall_change)), 
    abs_Accuracy_change = mean(abs(Accuracy_change)), 
    abs_FPR_change = mean(abs(FPR_change)), 
    abs_Precision_change = mean(abs(Precision_change)), 
    Recall_change = mean(Recall_change), 
    Accuracy_change = mean(Accuracy_change), 
    FPR_change = mean(FPR_change), 
    Precision_change = mean(Precision_change),     
    mean_Recall_before = mean(Recall_before), 
    mean_Accuracy_before = mean(Accuracy_before), 
    mean_FPR_before = mean(FPR_before), 
    mean_Precision_before = mean(Precision_before), 
    mean_Recall = mean(Recall), 
    mean_Accuracy = mean(Accuracy), 
    mean_FPR = mean(FPR), 
    mean_Precision = mean(Precision), 
    .groups = "drop"
  )

summary_metrics_during <- results_df_plt_district_during %>%
  group_by(Resampling, Model) %>%
  summarise(
    abs_Recall_change = mean(abs(Recall_change)), 
    abs_Accuracy_change = mean(abs(Accuracy_change)), 
    abs_FPR_change = mean(abs(FPR_change)), 
    abs_Precision_change = mean(abs(Precision_change)), 
    Recall_change = mean(Recall_change), 
    Accuracy_change = mean(Accuracy_change), 
    FPR_change = mean(FPR_change), 
    Precision_change = mean(Precision_change),     
    mean_Recall_before = mean(Recall_before), 
    mean_Accuracy_before = mean(Accuracy_before), 
    mean_FPR_before = mean(FPR_before), 
    mean_Precision_before = mean(Precision_before), 
    mean_Recall = mean(Recall), 
    mean_Accuracy = mean(Accuracy), 
    mean_FPR = mean(FPR), 
    mean_Precision = mean(Precision), 
    .groups = "drop"
  )

summary_metrics_before['period'] <- 'before'
summary_metrics_during['period'] <- 'during'

summary_metrics <- summary_metrics_during[, c(1, 2, 15:18)]
summary_metrics['mean_FPR_before'] <- summary_metrics_before$mean_FPR_before
summary_metrics['mean_Accuracy_before'] <- summary_metrics_before$mean_Accuracy_before
summary_metrics['mean_Recall_before'] <- summary_metrics_before$mean_Recall_before
summary_metrics['mean_Precision_before'] <- summary_metrics_before$mean_Precision_before
summary_metrics['abs_FPR'] <- abs(summary_metrics$mean_FPR - summary_metrics$mean_FPR_before)
summary_metrics['abs_Recall'] <- abs(summary_metrics$mean_Recall - summary_metrics$mean_Recall_before)
summary_metrics['abs_Accuracy'] <- abs(summary_metrics$mean_Accuracy - summary_metrics$mean_Accuracy_before)
summary_metrics['abs_Precision'] <- abs(summary_metrics$mean_Precision - summary_metrics$mean_Precision_before)
summary_metrics['Recall'] <- 'high'
summary_metrics_high <- summary_metrics


summary_metrics_total <- rbind(summary_metrics_low, summary_metrics_high)

p1 <- ggplot(summary_metrics_total, aes(x = abs_FPR, y = mean_FPR, color = Model, shape = Recall)) +
  geom_point(size = 2) +
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Resampling), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() +
  labs(
    x = "absolute difference in FPR",
    y = "FPR during COVID"
  ) + xlim(0, 0.3) + 
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'red')
p1
ggsave('bld/figures/FPR_given_total_recall.jpg', p1, width = 7,
       height = 5, dpi = 500)

p1 <- ggplot(summary_metrics_high, aes(x = abs_FPR, y = mean_FPR, color = Model, shape = Model)) +
  geom_point(size = 2) +
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Resampling), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() +
  labs(
    x = "robustness: absolute change in FPR",
    y = "performance: FPR during COVID"
  ) 
p1
ggsave('bld/figures/FPR_given_0709_recall.jpg', p1, width = 7,
       height = 5, dpi = 500)

p1 <- ggplot(summary_metrics_low, aes(x = abs_FPR, y = mean_FPR, color = Model, shape = Model)) +
  geom_point(size = 2) +
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Resampling), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() +
  labs(
    x = "robustness: absolute change in FPR",
    y = "performance: FPR during COVID"
  ) 
p1
ggsave('bld/figures/FPR_given_0507_recall.jpg', p1, width = 7,
       height = 5, dpi = 500)
################################################################################################
results_df_plt_district_before <- region_df[region_df$Recall_before >= 0.6 &
                                              region_df$Recall_before <= 0.75, ]
results_df_plt_district_during <- region_df[region_df$Recall >= 0.6 &
                                              region_df$Recall <= 0.75, ]

summary_metrics_before <- results_df_plt_district_before %>%
  group_by(Resampling, Model) %>%
  summarise(
    abs_Recall_change = mean(abs(Recall_change)), 
    abs_Accuracy_change = mean(abs(Accuracy_change)), 
    abs_FPR_change = mean(abs(FPR_change)), 
    abs_Precision_change = mean(abs(Precision_change)), 
    Recall_change = mean(Recall_change), 
    Accuracy_change = mean(Accuracy_change), 
    FPR_change = mean(FPR_change), 
    Precision_change = mean(Precision_change),     
    mean_Recall_before = mean(Recall_before), 
    mean_Accuracy_before = mean(Accuracy_before), 
    mean_FPR_before = mean(FPR_before), 
    mean_Precision_before = mean(Precision_before), 
    mean_Recall = mean(Recall), 
    mean_Accuracy = mean(Accuracy), 
    mean_FPR = mean(FPR), 
    mean_Precision = mean(Precision), 
    .groups = "drop"
  )

summary_metrics_during <- results_df_plt_district_during %>%
  group_by(Resampling, Model) %>%
  summarise(
    abs_Recall_change = mean(abs(Recall_change)), 
    abs_Accuracy_change = mean(abs(Accuracy_change)), 
    abs_FPR_change = mean(abs(FPR_change)), 
    abs_Precision_change = mean(abs(Precision_change)), 
    Recall_change = mean(Recall_change), 
    Accuracy_change = mean(Accuracy_change), 
    FPR_change = mean(FPR_change), 
    Precision_change = mean(Precision_change),     
    mean_Recall_before = mean(Recall_before), 
    mean_Accuracy_before = mean(Accuracy_before), 
    mean_FPR_before = mean(FPR_before), 
    mean_Precision_before = mean(Precision_before), 
    mean_Recall = mean(Recall), 
    mean_Accuracy = mean(Accuracy), 
    mean_FPR = mean(FPR), 
    mean_Precision = mean(Precision), 
    .groups = "drop"
  )

summary_metrics_before['period'] <- 'before'
summary_metrics_during['period'] <- 'during'

summary_metrics <- summary_metrics_during[, c(1, 2, 15:18)]
summary_metrics['mean_FPR_before'] <- summary_metrics_before$mean_FPR_before
summary_metrics['mean_Accuracy_before'] <- summary_metrics_before$mean_Accuracy_before
summary_metrics['mean_Recall_before'] <- summary_metrics_before$mean_Recall_before
summary_metrics['mean_Precision_before'] <- summary_metrics_before$mean_Precision_before
summary_metrics['abs_FPR'] <- abs(summary_metrics$mean_FPR - summary_metrics$mean_FPR_before)
summary_metrics['abs_Recall'] <- abs(summary_metrics$mean_Recall - summary_metrics$mean_Recall_before)
summary_metrics['abs_Accuracy'] <- abs(summary_metrics$mean_Accuracy - summary_metrics$mean_Accuracy_before)
summary_metrics['abs_Precision'] <- abs(summary_metrics$mean_Precision - summary_metrics$mean_Precision_before)
summary_metrics['Recall'] <- 'low'
summary_metrics_low <- summary_metrics

results_df_plt_district_before <- region_df[region_df$Recall_before >= 0.75 &
                                              region_df$Recall_before <= 0.95, ]
results_df_plt_district_during <- region_df[region_df$Recall >= 0.75 &
                                              region_df$Recall <= 0.95, ]

summary_metrics_before <- results_df_plt_district_before %>%
  group_by(Resampling, Model) %>%
  summarise(
    abs_Recall_change = mean(abs(Recall_change)), 
    abs_Accuracy_change = mean(abs(Accuracy_change)), 
    abs_FPR_change = mean(abs(FPR_change)), 
    abs_Precision_change = mean(abs(Precision_change)), 
    Recall_change = mean(Recall_change), 
    Accuracy_change = mean(Accuracy_change), 
    FPR_change = mean(FPR_change), 
    Precision_change = mean(Precision_change),     
    mean_Recall_before = mean(Recall_before), 
    mean_Accuracy_before = mean(Accuracy_before), 
    mean_FPR_before = mean(FPR_before), 
    mean_Precision_before = mean(Precision_before), 
    mean_Recall = mean(Recall), 
    mean_Accuracy = mean(Accuracy), 
    mean_FPR = mean(FPR), 
    mean_Precision = mean(Precision), 
    .groups = "drop"
  )

summary_metrics_during <- results_df_plt_district_during %>%
  group_by(Resampling, Model) %>%
  summarise(
    abs_Recall_change = mean(abs(Recall_change)), 
    abs_Accuracy_change = mean(abs(Accuracy_change)), 
    abs_FPR_change = mean(abs(FPR_change)), 
    abs_Precision_change = mean(abs(Precision_change)), 
    Recall_change = mean(Recall_change), 
    Accuracy_change = mean(Accuracy_change), 
    FPR_change = mean(FPR_change), 
    Precision_change = mean(Precision_change),     
    mean_Recall_before = mean(Recall_before), 
    mean_Accuracy_before = mean(Accuracy_before), 
    mean_FPR_before = mean(FPR_before), 
    mean_Precision_before = mean(Precision_before), 
    mean_Recall = mean(Recall), 
    mean_Accuracy = mean(Accuracy), 
    mean_FPR = mean(FPR), 
    mean_Precision = mean(Precision), 
    .groups = "drop"
  )

summary_metrics_before['period'] <- 'before'
summary_metrics_during['period'] <- 'during'

summary_metrics <- summary_metrics_during[, c(1, 2, 15:18)]
summary_metrics['mean_FPR_before'] <- summary_metrics_before$mean_FPR_before
summary_metrics['mean_Accuracy_before'] <- summary_metrics_before$mean_Accuracy_before
summary_metrics['mean_Recall_before'] <- summary_metrics_before$mean_Recall_before
summary_metrics['mean_Precision_before'] <- summary_metrics_before$mean_Precision_before
summary_metrics['abs_FPR'] <- abs(summary_metrics$mean_FPR - summary_metrics$mean_FPR_before)
summary_metrics['abs_Recall'] <- abs(summary_metrics$mean_Recall - summary_metrics$mean_Recall_before)
summary_metrics['abs_Accuracy'] <- abs(summary_metrics$mean_Accuracy - summary_metrics$mean_Accuracy_before)
summary_metrics['abs_Precision'] <- abs(summary_metrics$mean_Precision - summary_metrics$mean_Precision_before)
summary_metrics['Recall'] <- 'high'
summary_metrics_high <- summary_metrics


summary_metrics_total <- rbind(summary_metrics_low, summary_metrics_high)

p1 <- ggplot(summary_metrics_total, aes(x = abs_FPR, y = mean_FPR, color = Model, shape = Recall)) +
  geom_point(size = 2) +
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Resampling), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() +
  labs(
    x = "absolute difference in FPR",
    y = "FPR during COVID"
  ) + xlim(0, 0.3) + 
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'red')
p1
ggsave('bld/figures/FPR_given_total95_recall.jpg', p1, width = 7,
       height = 5, dpi = 500)

p1 <- ggplot(summary_metrics_high, aes(x = abs_FPR, y = mean_FPR, color = Model, shape = Model)) +
  geom_point(size = 2) +
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Resampling), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() +
  labs(
    x = "robustness: absolute change in FPR",
    y = "performance: FPR during COVID"
  )  
p1
ggsave('bld/figures/FPR_given_7595_recall.jpg', p1, width = 7,
       height = 5, dpi = 500)

p1 <- ggplot(summary_metrics_low, aes(x = abs_FPR, y = mean_FPR, color = Model, shape = Model)) +
  geom_point(size = 2) +
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Resampling), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() +
  labs(
    x = "robustness: absolute change in FPR",
    y = "performance: FPR during COVID"
  )  
p1
ggsave('bld/figures/FPR_given_0675_recall.jpg', p1, width = 7,
       height = 5, dpi = 500)

################################################################################################
unique(region_df$Recall_before)
unique(region_df$Recall)
#######################################
select_summarize <- function(df){
  summary_metrics <- df %>%
    group_by(Resampling, Model) %>%
    summarise(
      abs_Recall_change = mean(abs(Recall_change)), 
      abs_Accuracy_change = mean(abs(Accuracy_change)), 
      abs_FPR_change = mean(abs(FPR_change)), 
      abs_Precision_change = mean(abs(Precision_change)), 
      mean_Recall_before = mean(Recall_before), 
      mean_Accuracy_before = mean(Accuracy_before), 
      mean_FPR_before = mean(FPR_before), 
      mean_Precision_before = mean(Precision_before), 
      mean_Recall = mean(Recall), 
      mean_Accuracy = mean(Accuracy), 
      mean_FPR = mean(FPR), 
      mean_Precision = mean(Precision), 
      max_Recall = max(Recall), 
      min_Recall = min(Recall), 
      median_Recall = median(Recall), 
      max_Accuracy = max(Accuracy), 
      min_Accuracy = min(Accuracy), 
      median_Accuracy = median(Accuracy), 
      max_FPR = max(FPR), 
      min_FPR = min(FPR), 
      median_FPR = median(FPR), 
      max_Precision = max(Precision), 
      min_Precision = min(Precision), 
      median_Precision = median(Precision), 
      median_Recall_before = median(Recall_before), 
      max_Accuracy_before = max(Accuracy_before), 
      min_Accuracy_before = min(Accuracy_before), 
      median_Accuracy_before = median(Accuracy_before), 
      max_FPR_before = max(FPR_before), 
      min_FPR_before = min(FPR_before), 
      median_FPR_before = median(FPR_before), 
      max_Precision_before = max(Precision_before), 
      min_Precision_before = min(Precision_before), 
      median_Precision_before = median(Precision_before), 
      length_Threshold = n(),   
      min_Threshold = min(Threshold), 
      max_Threshold = max(Threshold), 
      .groups = "drop"
    )
  return(summary_metrics)
}
####################################################################################
region_df <- results_df_plt_district
Rl <- 0.5
Rh <- 0.6
Fh <- 1
results_df_plt_district_0 <- region_df[region_df$Recall_before >= Rl & 
                                         region_df$Recall_before <= Rh& 
                                         region_df$FPR_before <= Fh, ]
dim(results_df_plt_district_0)
summary_metrics_0 <- select_summarize(results_df_plt_district_0)

summary_metrics_1 <- summary_metrics_0[summary_metrics_0$min_Recall >= Rl, ]
summary_metrics_1['Recall range'] <- '50% - 60%'
summary_metrics_1_low <- summary_metrics_1

Rl <- 0.6
Rh <- 0.7
Fh <- 1
results_df_plt_district_0 <- region_df[region_df$Recall_before >= Rl & 
                                         region_df$Recall_before <= Rh& 
                                         region_df$FPR_before <= Fh, ]
dim(results_df_plt_district_0)
summary_metrics_0 <- select_summarize(results_df_plt_district_0)

summary_metrics_1 <- summary_metrics_0[summary_metrics_0$min_Recall >= Rl, ]
summary_metrics_1['Recall range'] <- '60% - 70%'
summary_metrics_1_low2 <- summary_metrics_1

Rl <- 0.7
Rh <- 0.8
Fh <- 1
results_df_plt_district_0 <- region_df[region_df$Recall_before >= Rl & 
                                         region_df$Recall_before <= Rh& 
                                         region_df$FPR_before <= Fh, ]
dim(results_df_plt_district_0)
summary_metrics_0 <- select_summarize(results_df_plt_district_0)

summary_metrics_1 <- summary_metrics_0[summary_metrics_0$min_Recall >= Rl, ]
summary_metrics_1['Recall range'] <- '70% - 80%'
summary_metrics_1_mid <- summary_metrics_1

Rl <- 0.8
Rh <- 0.9
Fh <- 1
results_df_plt_district_0 <- region_df[region_df$Recall_before >= Rl & 
                                         region_df$Recall_before <= Rh& 
                                         region_df$FPR_before <= Fh, ]
dim(results_df_plt_district_0)
summary_metrics_0 <- select_summarize(results_df_plt_district_0)

summary_metrics_1 <- summary_metrics_0[summary_metrics_0$min_Recall >= Rl, ]
summary_metrics_1['Recall range'] <- '80% - 90%'
summary_metrics_1_high <- summary_metrics_1
summary_metrics <- rbind(summary_metrics_1_low, summary_metrics_1_low2, 
                         summary_metrics_1_mid, summary_metrics_1_high)
summary_metrics['threshold_plot'] <- summary_metrics$length_Threshold/1000
p0 <- ggplot(summary_metrics, aes(x =threshold_plot , y = abs_FPR_change, color = Model, shape = `Recall range`)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Resampling), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() + labs(
    x = "length of decision threshold",
    y = "absolute difference in FPR"
  ) 
p0
ggsave('bld/figures/Feasiblity_ThresholdvsFPR.jpg', p0, width = 10,
       height = 6, dpi = 800)
p0 <- ggplot(summary_metrics, aes(x = abs_FPR_change, y = mean_FPR, color = Model, shape = `Recall range`)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Resampling), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() + labs(
    x = "robustness: absolute change in FPR",
    y = "performance: FPR during COVID"
  ) 
p0
ggsave('bld/figures/Feasiblity_Recall607080.jpg', p0, width = 10,
       height = 6, dpi = 800)

summary_metrics['threshold_diff'] <- summary_metrics$max_Threshold - summary_metrics$min_Threshold
p0 <- ggplot(summary_metrics, aes(x = min_Threshold, y = threshold_diff, color = Model, shape = `Recall range`)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Resampling), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() + labs(
    x = "lower bound of Threshold",
    y = "length of Threshold"
  )
p0
ggsave('bld/figures/Feasiblity_Threshold607080.jpg', p0, width = 10,
       height = 6, dpi = 800)
