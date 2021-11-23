#habitats N2000 présents sur Chamrousse

### Librairies -------------------------------------

library(sf)
library(data.table)
library(dplyr)
library(ggplot2)

### Constantes -------------------------------------

# Espace de travail
wd <- "C:/Users/perle.charlot/Documents/PhD/DATA/"

# Chemin  fichier spatial
chm_emprise <- paste0(wd,"/Variables_spatiales_Belledonne/Milieux/Natura_2000/Telechargement_1637231485_1556/1e973b74-eabb-4077-992a-775d7af34593_1637231485_4873/n_hab_dominants_n2000_s_r84.shp")

# Chemin csv liste tous habitats N2000
chm_csv <- paste0(wd,"/Variables_spatiales_Belledonne/Milieux/Natura_2000/INPN_ListedeshabitatspourNatura2000.csv")

### Programme -------------------------------------

# Espace de travail
setwd(wd)

# Chargement fichier spatial
emprise <- read_sf(chm_emprise)
emprise$area <- st_area(emprise)

# Conversion en table
dt_N2000 <- data.table(emprise)

str(dt_N2000)
# champ cdn2000 : code habitat Natura 2000



# Visu répartition
summary_N2000_Belledonne <- dt_N2000 %>%
  group_by(cdn2000) %>%
  summarise(nb_poly_par_hab = n(),
            sum_surf_par_hab = sum(area),
            median_recouvr_par_hab = median(rechab),
            libellé= unique(lbn2000))

copy <-summary_N2000_Belledonne 

summary_N2000_Belledonne$libellé[summary_N2000_Belledonne$libellé == "Non concerné"] <- NA
summary_N2000_Belledonne$pourcent_surf <- as.numeric(summary_N2000_Belledonne$sum_surf_par_hab) / as.numeric(sum(summary_N2000_Belledonne$sum_surf_par_hab))

summary_N2000_Belledonne_plot <- summary_N2000_Belledonne %>%
  ggplot(aes(x="", y=as.numeric(sum_surf_par_hab), fill=libellé)) +
  geom_bar(stat="identity", width=1,color="white") +
  coord_polar("y", start=0)+
  scale_fill_hue(iwanthue(length(summary_N2000_Belledonne$cdn2000)),na.value = "black")+
  labs(y="Superficie",title="Surface par habitat Natura 2000 - Belledonne")

summary_N2000_Belledonne %>%
  ggplot(aes(x="", y=as.numeric(sum_surf_par_hab), fill=libellé)) +
  geom_bar(stat="identity", width=1,color="white")



summary_N2000_Belledonne_eunis <- dt_N2000 %>%
  group_by(cdeunis) %>%
  summarise(nb_poly_par_hab = n(),
            sum_surf_par_hab = sum(area),
            median_recouvr_par_hab = median(rechab),
            libellé= unique(lbeunis))

summary_N2000_Belledonne_eunis %>%
  ggplot( aes(x="", y=as.numeric(sum_surf_par_hab), fill=libellé)) +
  geom_bar(stat="identity", width=1,color="white") +
  coord_polar("y", start=0)+
  scale_fill_hue(iwanthue(length(summary_N2000_Belledonne_eunis$cdeunis)),na.value = "black")+
  labs(y="Superficie",title="Surface par habitat EUNIS - Belledonne")

summary_N2000_Belledonne_eunis %>%
  ggplot(aes(x="", y=as.numeric(sum_surf_par_hab), fill=libellé)) +
  geom_col(stat="identity", width=1,color="black")+
  guides(fill = "none")

summary_N2000_Belledonne_eunis %>%
  ggplot(aes(x=cdeunis, y=as.numeric(sum_surf_par_hab), fill=cdeunis)) +
  geom_col(stat="identity", width=1,color="black")+
  guides(fill = "none")+
  theme(axis.text.x = element_text(angle = 90))


geom_histogram()

library(hues)
mapalette <- iwanthue(length(summary_N2000_Belledonne$cdn2000))
plot(mapalette)

# # Lecture liste tous hab N2000
# tableN2000 <- fread(chm_csv)
# # Extraction habitats présents
# tableN2000[tableN2000$Code %in% unique(dt_N2000$cdn2000),]
