### Titre -------------------------------------
# Nom : Calcul des variables de la dimension INFRASTRUCTURE
# Auteure : Perle Charlot
# Date de création : 08-01-2022
# Dates de modification : 31-03-2022

### Librairies -------------------------------------
library(raster)
library(sf)
library(fasterize)
library(data.table)
library(rgdal)
library(tidyverse)
library(exactextractr)

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
# Dossier des variables spatiales & chemins des fichiers
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"
# Vars sp
chemin_mnt <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_25m_belledonne_cale.tif")
# Habitats expertisés
path_raster_habitat <- paste0(output_path,"/var_intermediaire/habitat_raster_25m.tif")
path_bati_vect <- paste0(output_path,"/var_intermediaire/infrastructures.gpkg")


#### Tables ####
path_table_NoPs <- paste0(output_path,"/tables/table_NoPs_climat.csv")
# Table correspondance habitat et degré d'artificialisation (0 à 3, pas à très artificialisé)
corresp_hbt_artif_path <- paste0(dos_var_sp,"/Milieux/IGN/habitat_artificialisation.csv")
corresp_hbt_code_path <-paste0(output_path,"/tables/correspondance_habitat_raster.csv")

### Programme -------------------------------------

#### Niveau d'artificialisation du sol ####

## VAR : % d'infrastructure au sol ##
vect_bati <- st_read(path_bati_vect)
vect_bati$infra <- 1
vect_bati <- vect_bati %>% select(infra)
grille <- raster(chemin_mnt)
datavals <- do.call(rbind,exact_extract(grille, vect_bati,
                                        include_xy = TRUE,progress = FALSE))
datavals$value[!is.na(datavals$value)] <- 1
datavals$surf <- datavals$value * datavals$coverage_fraction * 100
xyz <- as.data.frame(cbind(x = datavals$x, y = datavals$y, z = datavals$surf))
xyz$z[is.na(xyz$z)] = 0
pourcent_bati <- rasterFromXYZ(xyz,crs=crs(grille))
pourcent_bati[is.na(pourcent_bati)] = 0
plot(pourcent_bati, colNA='black')
writeRaster(pourcent_bati, paste0(output_path,"/var_I/pourcentage_infrastructures.tif"))

## VAR : degré aménagement du sol ##
corresp_habitat_artif <- fread(corresp_hbt_artif_path)
# Chargement table correspondance habitat <-> code
corresp_habitat_code <- fread(corresp_hbt_code_path,drop="V1")
corresp_code_artif <- merge(corresp_habitat_artif,corresp_habitat_code, by=c("lbhab","cdhab"))
# Chargement raster hbt
rast_habitat <- raster(path_raster_habitat)
habitat_artif_rast <- subs(rast_habitat, corresp_code_artif, by="code",which="degre_artif")
plot(habitat_artif_rast,colNA="black")

# Sauvegarde du vecteur et du raster
st_write(habitat_artif_vect)
writeRaster(habitat_artif_rast, paste0(output_path,"/var_I/degre_artif.tif"))
