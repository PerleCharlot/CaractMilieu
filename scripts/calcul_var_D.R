### Titre -------------------------------------
# Nom : Calcul des variables de la dimension DYNAMIQUE
# Auteure : Perle Charlot
# Date de création : 01-03-2022
# Dates de modification : 

### Librairies -------------------------------------

library(data.table)


library(raster)
library(sf)
library(fasterize)
library(rgdal)

### Fonctions -------------------------------------

### Constantes -------------------------------------

# Espace de travail
wd <- getwd()
# Dossier des inputs (dans le git)
input_path <- paste0(wd,"/input/")
# Dossier des outputs (dans le git)
output_path <- paste0(wd,"/output/")
# Projection Lambert 93 (EPSG : 2154)
EPSG_2154 =  "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs "

#### Données spatiales ####

#### Tables ####

### Programme -------------------------------------

#### Régime de perturbation ####

## VAR : présence couloir d’avalanche (/âge depuis dernière perturbation) ##

#### Stade de succession ####

## VAR : vitesse greening ##


