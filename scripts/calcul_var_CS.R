### Titre -------------------------------------
# Nom : Calcul des variables de la dimension CONTEXTE SPATIAL
# Auteure : Perle Charlot
# Date de création : 01-03-2022
# Dates de modification : 

### Librairies -------------------------------------

library(data.table)
library(sf)
library(fasterize)
library(terra)


library(rgdal)

### Fonctions -------------------------------------

DistanceA <- function(vect_obj,
                      champ_rasterisation = NULL, # champ qui doit comporter des 0 et des 1
                      champ_commun = NULL,
                      table_correspondance=NULL,
                      chemin_raster_ref,
                      chemin_output){
  # #TEST
  # vect_obj = vecteur_habitat
  # champ_rasterisation = "habitat_forestier" 
  # champ_commun = "cdhab"
  # chemin_table_correspondance = tbl_hbt_path
  # chemin_raster_ref = chemin_mnt
  # chemin_output = paste0(output_path,"/var_CS/distance_foret.tif")
  
  
  if(!is.null(champ_rasterisation)){champ_rasterisation <- as.character(champ_rasterisation)}
  if(!is.null(champ_commun)){champ_commun <- as.character(champ_commun)}
  
  vect_obj <- st_as_sf(vect_obj)
  
  if(!is.null(table_correspondance)){
    # Lecture table de correspondance
    tbl_cor <- as.data.frame(table_correspondance)
    # Fusion pour avoir champ d'intérêt
    vect_obj <- merge(vect_obj,tbl_cor,champ_commun)
  }
  raster_ref <- raster(chemin_raster_ref)
  if(is.null(champ_rasterisation)){
    vect_obj_rast <- fasterize(vect_obj,raster_ref)
  } else {vect_obj_rast <- fasterize(vect_obj,raster_ref, champ_rasterisation)}
  
  # Passer en NA ce qui est égal à 0
  vect_obj_rast[vect_obj_rast == 0] <- NA
  dist_vect_obj_rast <- terra::distance(vect_obj_rast)
  writeRaster(dist_vect_obj_rast, chemin_output, overwrite=TRUE)
  return(dist_vect_obj_rast)
}


### Constantes -------------------------------------

# Espace de travail
wd <- getwd()
# Dossier des inputs (dans le git)
input_path <- paste0(wd,"/input/")
# Dossier des outputs (dans le git)
output_path <- paste0(wd,"/output/")
# Projection Lambert 93 (EPSG : 2154)
EPSG_2154 =  "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs "

resolution_etude = 25

#### Données spatiales ####

# Dossier des variables spatiales & chemins des fichiers
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"
# MNT 25m CALé sur le grille de REFERENCE
chemin_mnt <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_25m_belledonne_cale.tif")
path_pente <- paste0(output_path,"/var_topo/pente_25m.tif")

# Surface en eaux libres et tronçons rivières IGN
path_vecteur_eaux_libres <- paste0(output_path,"/var_IGN/eaux_libres.gpkg")
# Bâti IGN (bâtiments, infrastructures de transport et transport par cable)
path_vecteur_infrastructure <- paste0(output_path,"/var_IGN/infrastructures.gpkg")

# Habitats expertisés
path_vecteur_habitat <- paste0(dos_var_sp,"/Milieux/Natura_2000/n_hab_dominants_n2000_s_r84_jointure.gpkg")
path_vecteur_habitat_simplifié <- paste0(dos_var_sp,"/Milieux/Natura_2000/polygones_habitat_simplifié.gpkg")
path_raster_habitat <- paste0(output_path,"/habitat_raster_25m.tif")

#### Tables ####

# Table correspondance habitat et autres infos
tbl_hbt_path <- paste0(dos_var_sp,"/Milieux/Natura_2000/table_habitats.csv")

### Programme -------------------------------------

#### Similarité d'attributs du pixel avec ceux qui l’entourent ####

## VAR : % de même habitat adjacent dans un rayon de 1km/500m/250m/100m ##
## VAR : taille du patch d’habitat ##

#### Capacité d’atteinte ce pixel par un usager ####

## VAR : temps d’accès au pixel ##
# (rugosité X distance aux chemins/point haut remontées mécaniques X distance point d’entrée)

#### Viewshed/ information visuelle ####

## VAR : note esthétique moyenne perçue des habitats visibles ##
## VAR : sommets visibles depuis le pixel ##
## VAR : présence d’eaux libres / eaux libres visibles ##

#### Empreinte des infrastructures ####

## VAR : distance aux infrastructures ##
vecteur_infra <- st_read(path_vecteur_infrastructure)
dist_infra <- DistanceA(vect_obj= vecteur_infra,
                        chemin_raster_ref= chemin_mnt,
                        chemin_output = paste0(output_path,"/var_CS/distance_infrastructure.tif"))
plot(dist_infra)
plot(vecteur_infra,add=TRUE)


#### Proximité d'un couvert forestier ####

## VAR : distance à la forêt ##
# /!!!\ effets de bords, il faudrait utiliser la couche forêt de l'IGN pour avoir hors zone natura 2000
vecteur_habitat <- st_read(path_vecteur_habitat_simplifié)
names(vecteur_habitat)
tbl_hbt <- fread(tbl_hbt_path)
names(tbl_hbt)

dist_foret <- DistanceA(vecteur_habitat,
                "habitat_forestier" ,  
                "cdhab",
                tbl_hbt,
          chemin_mnt,
          paste0(output_path,"/var_CS/distance_foret.tif"))
plot(dist_foret,colNA="black")

# coût de la pente
pente <- raster(path_pente)
plot(pente)
plot(vecteur_habitat[,1],col="transparent", add=TRUE)

plot(tr)


library(gdistance)
tr <- transition( 1/pente, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
costDistance(tr,vecteur_habitat)

# TODO :
# - dans construction_var_inter sauvegarder vecteur d'habitat forestier
# - enlever champ de commun et table dans fonction DistanceA
# - calculer costDistance avec vecteur habitat forestier
# - mieux ranger les fonctions : créer des scripts par dimension, faire passer calc var intermédiaires, etc
