#instalando sparklyr
library(dplyr)    # Data Refinery/wrangling
library(ggplot2)  # Data Visualization
library(tidyverse)
#pacotes importantes
#important libraries
#install.packages("caret")
library(caret)
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

titan
count(titan)
#####################################3

##########################################################################
table_av1 <- titan %>% group_by(Age,Survived) %>%
  summarise(sum_freq = sum(Freq,na.rm=T)) %>% collect()

#Faixa
ggplot(table_av1, aes(table_av1$Age, table_av1$sum_freq)) +
  geom_col(aes(fill=table_av1$Survived),position = "dodge")+theme_minimal()+theme(legend.position = "right")+labs(x="Faixa-etária",y="Quantidade")

####################################################3
table_av <- titan %>% group_by(Class,Sex) %>%
  summarise( sum_freq = sum(Freq,na.rm=T)) %>% collect()

#classe
ggplot(table_av, aes(table_av$Sex, table_av$sum_freq)) +
  geom_col(aes(fill=table_av$Class),position = "dodge")+theme_minimal()+theme(legend.position = "right")+labs(x="Sexo",y="Quantidade",fill="Classe")

table_av2 <- titan %>% group_by(Class,Survived) %>%
  summarise(sum_freq = sum(Freq,na.rm=T)) %>% collect()

#classe
ggplot(table_av2, aes(table_av2$Class, table_av$sum_freq)) +
  geom_col(aes(fill=table_av2$Survived),position = "dodge")+theme_minimal()+theme(legend.position = "right")+labs(x="Sexo",y="Quantidade",fill="Sobreviveu?")


##################################################################
#bonus
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
# Creat
pred <- ml_predict(ml_log, test_tbl) %>% collect()

library(caret)
confusionMatrix(factor(pred$prediction),factor(test[["Survivor"]]))

spark_disconnect(sc)

# https://github.com/rstudio/sparkDemos/blob/master/dev/cloudera/spark_ml_classification_titanic.Rmd
