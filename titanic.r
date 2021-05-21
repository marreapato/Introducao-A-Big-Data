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

starw <- copy_to(sc, df)

spark_disconnect(sc)

starw
count(starw)
#####################################3

table_av1 <- starw %>% group_by(Age,Survived) %>%
  summarise(sum_freq = sum(Freq,na.rm=T)) %>% collect()

table_av <- starw %>% group_by(Class,Sex) %>%
  summarise( sum_freq = sum(Freq,na.rm=T)) %>% collect()


table_av2 <- starw %>% group_by(Class,Survived) %>%
  summarise(sum_freq = sum(Freq,na.rm=T)) %>% collect()

View(table_av)

#altura media
ggplot(table_av, aes(table_av$city, table_av$sum_sales)) +
  geom_col(aes(fill=city))+coord_flip()+theme_minimal()+theme(legend.position = "none")

#massa media
ggplot(na.omit(table_av), aes(species, average_mass)) +
  geom_col(aes(fill=species))+coord_flip()+theme_minimal()+theme(legend.position = "none")

View(table_av[,-2])
##################################################################


