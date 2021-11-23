### Titre -------------------------------------
# Nom : Extraction de dalles de MNT correspondant à une emprise
# Auteure : Perle Charlot
# Date de création : 10-11-2021
# Dates de modification : 15-11-2021

### Librairies -------------------------------------

library(sf)
library(raster)
library(purrr)
library(rgdal)

### Fonctions -------------------------------------

### Constantes -------------------------------------

# Espace de travail
setwd("C:/Users/perle.charlot/Documents/PhD/DATA")

# Chemin de l'emprise de l'étude (ici, znieff type 2 Belledonne)
chm_emprise <- paste0(getwd(),"/Variables_spatiales_Belledonne/Milieux/directives_UE/directive_habitats/znieff2_belledonne.gpkg")

# Chemin des dalles dont il faut extraire l'information

### Programme -------------------------------------

# Chargement de l'emprise d'étude
emprise <- read_sf(chm_emprise)

# chargement des dalles de MNT
chm_dalles_mnt <- paste0(getwd(),"/Variables_spatiales_Belledonne/Milieux/IGN/RGEALTI_2-0_5M_ASC_LAMB93-IGN69_D038_2020-11-13/RGEALTI/3_SUPPLEMENTS_LIVRAISON_2021-10-00009/RGEALTI_MNT_5M_ASC_LAMB93_IGN69_D038/dalles.shp")
dalles <- read_sf(chm_dalles_mnt)

# Intersection de l'emprise et des dalles
dalle_emp <- st_intersection(emprise,dalles)

chm_asc <- paste0(getwd(),"/Variables_spatiales_Belledonne/Milieux/IGN/RGEALTI_2-0_5M_ASC_LAMB93-IGN69_D038_2020-11-13/RGEALTI/1_DONNEES_LIVRAISON_2021-10-00009/RGEALTI_MNT_5M_ASC_LAMB93_IGN69_D038/")

liste_rast_mnt <- as.list(paste0(chm_asc,dalle_emp$NOM_DALLE,".asc"))

To <- do.call(raster::merge,map(liste_rast_mnt,raster))
To
plot(To)
writeRaster(To,paste0(getwd(),"/Variables_spatiales_Belledonne/Milieux/IGN/mnt_belledonne.tif"))


