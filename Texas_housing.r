#instalando sparklyr
library(dplyr)    # Data Refinery/wrangling
library(ggplot2)  # Data Visualization

#install.packages("sparklyr")
packageVersion("sparklyr")
library(sparklyr)
#spark_install("2.3")
spark_available_versions()
#spark_install(version ="3.1")
spark_installed_versions()

sc <- spark_connect(master = "local", version = "3.1")

#dataset do star wars
data()
df <- txhousing                                 

starw <- copy_to(sc, df)

starw
count(starw)

#####################################3
#Alturas e Pesos
#processamnto spark e coletando para o R
table_av <- starw %>% group_by(species) %>%
  summarise(count = n(), average_height = mean(height,na.rm=T),
            average_mass = mean(mass,na.rm=T)) %>% collect()

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
