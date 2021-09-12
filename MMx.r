install.packages("stringr")  ##for all##
install.packages("Corr")
library(dplyr)
library(stringr)
library(DataCombine)
library(data.table)
library(reshape)
library(reshape2)
install.packages("readxl")
library(readxl)
Model_data <- read.csv("C:Model_data.csv")

Model_data  <- Model_data[order(Model_data$CUST_ID, Model_data$YearMonth),]  #ordered based on customer_id, YearMonth

#Removing highly correlated variables

Correlated_Data <- Model_data[,c("N_Calls", "N_Meals", "N_Samples" , "N_FieldSpeakerProgram", "GRP", 
        "Impressions_PaidSearch", "Clicks_PaidSearch", "Impressions_BannerAd", "Clicks_BannerAd", 
          "Total_TRx")]

#View(Correlated_Data)

cor_matrix <- cor(Correlated_Data, method = c("pearson") )

#columns <- apply(cor_matrix[-1], 1, function(x) names(which(x >0 && x!=1)))
#Model_data <- cor_matrix[,!apply(cor_matrix,2,function(x) any(x > 0.60))]
#Model_data <- FindCorr(cor_matrix, cutoff = .60, verbose = FALSE)

#Dropping High correlated values and Keeping one

Model_data$Clicks_PaidSearch <- NULL
Model_data$Impressions_BannerAd <- NULL
Model_data$Clicks_BannerAd <- NULL

#lags For Sales and Promotional Channels #Keep only impressions, paid search, drop rest

#TRx_Lags
Model_data <-slide(Model_data,Var = "Total_TRx", GroupVar = "CUST_ID", NewVar = "Total_TRx_Lag1", slideBy = -1)
Model_data <-slide(Model_data,Var = "Total_TRx", GroupVar = "CUST_ID", NewVar = "Total_TRx_Lag2", slideBy = -2)
Model_data <-slide(Model_data,Var = "Total_TRx", GroupVar = "CUST_ID", NewVar = "Total_TRx_Lag3", slideBy = -3)

#Calls_Lags
Model_data <-slide(Model_data,Var = "N_Calls", GroupVar = "CUST_ID", NewVar = "N_Calls_Lag1", slideBy = -1)
Model_data <-slide(Model_data,Var = "N_Calls", GroupVar = "CUST_ID", NewVar = "N_Calls_Lag2", slideBy = -2)
Model_data <-slide(Model_data,Var = "N_Calls", GroupVar = "CUST_ID", NewVar = "N_Calls_Lag3", slideBy = -3)

#Meals_Lags
Model_data <-slide(Model_data,Var = "N_Meals", GroupVar = "CUST_ID", NewVar = "N_Meals_Lag1", slideBy = -1)
Model_data <-slide(Model_data,Var = "N_Meals", GroupVar = "CUST_ID", NewVar = "N_Meals_Lag2", slideBy = -2)
Model_data <-slide(Model_data,Var = "N_Meals", GroupVar = "CUST_ID", NewVar = "N_Meals_Lag3", slideBy = -3)

#Samples_Lags
Model_data <-slide(Model_data,Var = "N_Samples", GroupVar = "CUST_ID", NewVar = "N_Samples_Lag1", slideBy = -1)
Model_data <-slide(Model_data,Var = "N_Samples", GroupVar = "CUST_ID", NewVar = "N_Samples_Lag2", slideBy = -2)
Model_data <-slide(Model_data,Var = "N_Samples", GroupVar = "CUST_ID", NewVar = "N_Samples_Lag3", slideBy = -3)

#SpeakerPrograms_Lags
Model_data <-slide(Model_data,Var = "N_FieldSpeakerProgram", GroupVar = "CUST_ID", NewVar = "N_SpeakerProg_Lag1", slideBy = -1)
Model_data <-slide(Model_data,Var = "N_FieldSpeakerProgram", GroupVar = "CUST_ID", NewVar = "N_SpeakerProg_Lag2", slideBy = -2)
Model_data <-slide(Model_data,Var = "N_FieldSpeakerProgram", GroupVar = "CUST_ID", NewVar = "N_SpeakerProg_Lag3", slideBy = -3)

#GRP_Lags
Model_data <-slide(Model_data,Var = "GRP", GroupVar = "CUST_ID", NewVar = "GRP_Lag1", slideBy = -1)
Model_data <-slide(Model_data,Var = "GRP", GroupVar = "CUST_ID", NewVar = "GRP_Lag2", slideBy = -2)
Model_data <-slide(Model_data,Var = "GRP", GroupVar = "CUST_ID", NewVar = "GRP_Lag3", slideBy = -3)

#ImpressionPaidSearch_Lags
Model_data <-slide(Model_data,Var = "Impressions_PaidSearch", GroupVar = "CUST_ID", NewVar = "Impressions_PS_Lag1", slideBy = -1)
Model_data <-slide(Model_data,Var = "Impressions_PaidSearch", GroupVar = "CUST_ID", NewVar = "Impressions_PS_Lag2", slideBy = -2)
Model_data <-slide(Model_data,Var = "Impressions_PaidSearch", GroupVar = "CUST_ID", NewVar = "Impressions_PS_Lag3", slideBy = -3)

#NA removing
Model_data[is.na(Model_data)] <- 0

#Filtering time period
Model_data <- Model_data[which(Model_data$YearMonth %in% 201704:201803),]

#Curvature Values
#By mean values For speaker Programs in console
#sum(Model_data$N_FieldSpeakerProgram)/length(Model_data$N_FieldSpeakerProgram)
#0.04587817 * 4
#seq(0.04587817, 0.1835127, by = (0.1835127-0.04587817)/5 )

Segments <- unique(Model_data$Segment_Final)

Curvature_Calls <- c(0.5,1,1.5, 2)
Curvature_Meals <- c(0.5,1,1.5, 2)
Curvature_Samples <- c(0.5,1,1.5, 2)
Curvature_Impressions_PaidSearch <- c(0.0155, 0.0181, 0.0207, 0.0233)
Curvature_GRP <- c(0.0071, 0.0082, 0.0094, 0.0106)
#Curvature_SpeakerPrograms <- c(0.0458, 0.0734, 0.1009, 0.1284) #linear 1 or 0


model_results <- data.frame()
#Iterations
Model_No <- 1
for(o in 1: length(Curvature_GRP))
{
  for(n in 1: length(Curvature_Impressions_PaidSearch))
  {
    for(l in 1: length(Curvature_Meals))
    {
      for(k in 1: length(Curvature_Samples))
      {
        for(j in 1: length(Curvature_Calls))
        {
          for(i in 1: length(Segments))
          {
            a <- setDT(Model_data[Model_data$Segment_Final == Segments[i],])
            
            #Transformation, Adstock part = lambda i.e. 0.5,0.25,0.125
            a$Calls_t <- 1- exp(-Curvature_Calls[j]* (a$N_Calls + 0.5* a$N_Calls_Lag1 + 0.25* a$N_Calls_Lag2 + 0.125* a$N_Calls_Lag3))
            a$Meals_t <- 1- exp(-Curvature_Meals[l]* (a$N_Meals + 0.5* a$N_Meals_Lag1 + 0.25* a$N_Meals_Lag2 + 0.125* a$N_Meals_Lag3))
            a$Samples_t <- 1- exp(-Curvature_Samples[k]* (a$N_Samples + 0.5* a$N_Samples_Lag1 + 0.25* a$N_Samples_Lag2 + 0.125* a$N_Samples_Lag3))
            a$GRP_t <- 1- exp(-Curvature_GRP[o]* (a$GRP + 0.5* a$GRP_Lag1 + 0.25* a$GRP_Lag2 + 0.125* a$GRP_Lag3))
            a$Impressions_PaidSearch_t <- 1- exp(-Curvature_Impressions_PaidSearch[n]* (a$Impressions_PaidSearch + 0.5* a$Impressions_PS_Lag1 + 0.25* a$Impressions_PS_Lag2 + 0.125* a$Impressions_PS_Lag3))
            
            a <- a[order(a$CUST_ID, a$YearMonth),]
            #View(a)
            
            #Linear Regression
            fit <- lm(Total_TRx ~ 1 + Total_TRx_Lag1 + Total_TRx_Lag2 + Total_TRx_Lag3 + Calls_t + Samples_t + Meals_t + GRP_t 
                      + Impressions_PaidSearch_t + N_FieldSpeakerProgram, data = a)
            
            summary_coef <- summary(fit)$coefficients[,c(1,4),drop = F]      
            coeff_name <- as.data.frame(summary_coef)
            coeff_name_temp <- coeff_name 
            coeff_name <- coeff_name_temp
            rm(coeff_name_temp)
            coeff_name <- data.frame(rn = row.names(coeff_name), coeff_name, stringsAsFactors = FALSE)
            row.names(coeff_name) <- NULL
            
            cf <- data.frame(Segments[i], coeff_name, summary(fit)$r.squared, sqrt(mean((a$Total_TRx-fitted(fit))^2)), sum(a$Total_TRx)/sum(fitted(fit))) 
            cf$Model_No <- Model_No
            
            #C-values
            cf$c_Curvature_GRP <- Curvature_GRP[o]
            cf$Curvature_Impressions_PaidSearch <- Curvature_Impressions_PaidSearch[n]
            cf$Curvature_Meals <- Curvature_Meals[l]
            cf$Curvature_Samples <- Curvature_Samples[k]
            cf$Curvature_Calls <- Curvature_Calls[j]
            
            if(Model_No == 1){
              
              model_results <- cf
            }else{
              model_results <- rbind(model_results,cf)
              
            }
            print(paste0("Running model : ",Model_No))
            
            Model_No <- Model_No + 1
           
            
             
          }
        }
      }
    }
  }
  
}
#View(a)

#Summary_coeff
#intercept
#c values for all --- LEFT

#Best Model
coeff<- model_results 
colnames(coeff)[c(1,5)] <- c("Segments","r_square")
colnames(coeff)[c(2)] <- c("Variables")
coefficients_df <- dcast(coeff,c_Curvature_GRP + Curvature_Calls +  Curvature_Impressions_PaidSearch + Curvature_Samples +  Curvature_Meals +  Segments + r_square + Model_No ~ Variables , value.var="Estimate", fun.aggregate=sum)

setDT(coefficients_df)[,SSE_Rank:=rank(r_square ,ties.method="first"),by =  list(Segments)]
coefficients_df <- coefficients_df[SSE_Rank ==1,]
