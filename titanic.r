#instalando sparklyr
library(dplyr)    # Data Refinery/wrangling
library(ggplot2)  # Data Visualization
library(tidyverse)
#pacotes importantes
#important libraries
#install.packages("caret")
library(e1071)
library(caret)
library(corrplot)
#grafico de corr
#correlation plot
#down bellow

#install.packages("sparklyr")
packageVersion("sparklyr")
library(sparklyr)
#spark_install("2.3")
spark_available_versions()
#spark_install(version ="3.1")
spark_installed_versions()
#dataset do star wars
data()

df <- as.data.frame(Titanic)                                 

sc <- spark_connect(master = "local", version = "3.1")

titan <- copy_to(sc, df)

titan <- titan %>% 
  mutate(Survivor = ifelse(Survived=="No",0,1))


titan
count(titan)
#####################################3

table_av1 <- titan %>% group_by(Age,Survived) %>%
  summarise(sum_freq = sum(Freq,na.rm=T)) %>% collect()

table_av <- titan %>% group_by(Class,Sex) %>%
  summarise( sum_freq = sum(Freq,na.rm=T)) %>% collect()


table_av2 <- titan %>% group_by(Class,Survived) %>%
  summarise(sum_freq = sum(Freq,na.rm=T)) %>% collect()

View(table_av)

##################################################################


partition <- titan %>% 
  mutate(Survivor = ifelse(Survived=="No",0,1)) %>%
  select(Survivor, Class, Sex, Age, Freq) %>%
  sdf_partition(train = 0.70, test = 0.3, seed = 8585)

# Create table references
train_tbl <- partition$train
test_tbl <- partition$test

# Model survival as a function of several predictors
ml_formula <- formula(Survivor ~ Sex + Age)
# Train a logistic regression model
(ml_log <- ml_logistic_regression(train_tbl, ml_formula))

test <- test_tbl %>% collect()
# Create a function for scoring
pred <- ml_predict(ml_log, test_tbl) %>% collect()

library(caret)
confusionMatrix(factor(pred$prediction),factor(test[["Survivor"]]))

spark_disconnect(sc)

# https://github.com/rstudio/sparkDemos/blob/master/dev/cloudera/spark_ml_classification_titanic.Rmd
