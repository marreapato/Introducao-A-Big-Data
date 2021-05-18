#instalando sparklyr
library(dplyr)    # Data Refinery/wrangling
library(ggplot2)  # Data Visualization
library(tidyverse)
#pacotes importantes
#important libraries
#install.packages("corrplot")
library(e1071)
library(caret)
library(corrplot)
#grafico de corr
#correlation plot
#down bellow

#caso deseje ver a correlacao com a pedigree
#In case you need to see the correlation with the pedigree function

#tirando a pedigree function
#taking out the pedigree function

#install.packages("sparklyr")
packageVersion("sparklyr")
library(sparklyr)
#spark_install("2.3")
spark_available_versions()
#spark_install(version ="3.1")
spark_installed_versions()
#dataset do star wars
data()
df <- txhousing                                 

sc <- spark_connect(master = "local", version = "3.1")

starw <- copy_to(sc, df)


starw
count(starw)

#####################################3

table_av1 <- starw %>% group_by(year) %>%
  summarise(count = n(), sum_sales = sum(sales,na.rm=T)) %>% collect()

table_av <- starw %>% group_by(city,year) %>%
  summarise(count = n(), sum_sales = sum(sales,na.rm=T)) %>% collect()


table_av2 <- starw %>% group_by(city,year) %>%
  summarise(vol = mean(volume),list=mean(listings), sum_sales = sum(sales,na.rm=T)) %>% collect()

#separating data between test and training
#separando os dados entre treinamento e teste
cor(table_av2$vol,table_av2$list)
corrplot(cor(table_av2[,-1]),method="number")
indice<-createDataPartition(starw[["sales"]],list=FALSE,p=0.70)
treino<-diabetes[indice,]
teste <-diabetes[-indice,]

table_av$species[table_av$species!=" NA"]
table_av <- table_av[-29,]


View(table_av)

#altura media
ggplot(table_av, aes(species, average_height)) +
  geom_col(aes(fill=species))+coord_flip()+theme_minimal()+theme(legend.position = "none")

#massa media
ggplot(na.omit(table_av), aes(species, average_mass)) +
  geom_col(aes(fill=species))+coord_flip()+theme_minimal()+theme(legend.position = "none")

View(table_av[,-2])

spark_disconnect(sc)
