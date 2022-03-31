### Titre -------------------------------------
# Nom : Calcul des variables de la dimension CONTEXTE SPATIAL
# Auteure : Perle Charlot
# Date de création : 01-03-2022
# Dates de modification : 31-03-2022

### Librairies -------------------------------------

library(data.table)
library(sf)
library(fasterize)
library(terra)
library(rgrass7)
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
# vecteur emrprise carrée
path_emprise <- paste0(dos_var_sp ,"/limites_etude/emprise.gpkg")
path_N2000 <- paste0(dos_var_sp ,"/limites_etude/cembraie_N2000_limites.gpkg")
# MNT 25m CALé sur le grille de REFERENCE
chemin_mnt <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_25m_belledonne_cale.tif")
chemin_mnt2 <- paste0(dos_var_sp ,"/Milieux/IGN/MNT_25m_50km_cale.tif")
path_pente <- paste0(output_path,"/var_CA/pente_25m.tif")

# Surface en eaux libres et tronçons rivières IGN
path_vecteur_eaux_libres <- paste0(output_path,"/var_intermediaire/eaux_libres.gpkg")
# Bâti IGN (bâtiments, infrastructures de transport et transport par cable)
path_vecteur_infrastructure <- paste0(output_path,"/var_intermediaire/infrastructures.gpkg")

# Habitats expertisés
path_raster_habitat <- paste0(output_path,"/var_intermediaire/habitat_raster_25m.tif")

# forêt (via IGN, qui semble au final plus précis que la couche d'habitats expertisés...)
path_foret_IGN <- paste0(dos_var_sp ,"/Milieux/IGN/foret_vecteur.gpkg")
path_foret_rast <- paste0(output_path ,"/var_intermediaire/foret_raster_IGN.TIF")

# Occupation du sol 2020
path_OCS <- paste0(dos_var_sp, "/Milieux/occupation_sol/OCS_2020_emprise.tif")

#### Tables ####

# Table correspondance habitat et autres infos
tbl_hbt_path <- paste0(dos_var_sp,"/Milieux/Natura_2000/table_habitats.csv")
# Table correspondance entre habitat et note esthétique
tbl_esth_path <- paste0(wd,"/input/tables/table_OCS.csv")
# Table correspondance entre habitat et vitesse de déplacement
tbl_vitesse_path <- paste0(wd,"/input/tables/table_.csv")

### Programme -------------------------------------

#### Similarité d'attributs du pixel avec ceux qui l’entourent ####

rast_habitat <- raster(path_rast_habitat)

## VAR : % d'habitat similaire dans un rayon de 1km/500m/250m/100m ##
liste_rayons = c(100,250,500,1000) #en mètre

for(rayon in liste_rayons){
  rayon_mw = rayon
  # Consruction moving windows de 100, 250, 500 et 1000m
  f <- terra::focalMat(rast_habitat, rayon_mw, "circle") 
  f[f > 0] <- 1
  # Récupération des codes d'habitats
  classes <- sort(as.numeric(unique(values(r_cale))))
  
  if(!dir.exists(paste0(output_path,"/var_CS/autre/couches_habitats_identiques_",rayon_mw,"m/"))){
    dir.create(paste0(output_path,"/var_CS/autre/couches_habitats_identiques_",rayon_mw,"m/"))}
  # Boucle de calcul de la proportion de même habitat dans un rayon de 500m, par habitat
  for(i in classes){
    # # TEST
    #i = 2
    print(i)
    pclass <- function(x, y=c(i)) {
      return( length(which(x %in% y)) / length(x) )}
    ft <- terra::focal(r_cale,w=f, fun=pclass)
    ft <- mask(ft,r_cale)
    ft[r_cale != i] <- NA
    writeRaster(ft,
                filename=paste0(output_path,"/var_CS/autre/couches_habitats_identiques_",rayon_mw,"m/",i,".tif"))
  }
  # Rassembler les couches d'habitats similaires
  x <- list.files(paste0(output_path,"/var_CS/autre/couches_habitats_identiques_",rayon_mw,"m/"),".tif", full.names=TRUE)
  raster_habitat_voisinage <- Reduce(merge, lapply(x, raster))
  writeRaster(raster_habitat_voisinage, 
              filename=paste0(output_path,"/var_CS/habitat_similaire_",rayon_mw,"m.TIF"))
  
}

## VAR : taille du patch d’habitat ##
# superficie, en m², du patch d'habitat auquel appartient le pixel
# calcul réalisé sur la donnée raster pour parer les NA centraux trous rivière
rast_habitat_vect <- rasterToPolygons(rast_habitat, dissolve = TRUE)
rast_habitat_vect <- st_as_sf(rast_habitat_vect)
rast_habitat_vect <- st_cast(rast_habitat_vect, "POLYGON")
rast_habitat_vect$superficie <- st_area(rast_habitat_vect)
raster_taille_patch_habitat <- fasterize(rast_habitat_vect,rast_habitat,
                                          field='superficie')
writeRaster(raster_taille_patch_habitat, 
            filename=paste0(output_path,"/var_CS/taille_patch_habitat_m2.TIF"))

#### Capacité d’atteinte ce pixel par un usager ####

## VAR : temps d’accès au pixel ##
# (rugosité X distance aux chemins/point haut remontées mécaniques X distance point d’entrée)

rast_habitat <- raster(path_rast_habitat)
table_vitesse_habitat <- fread(tbl_vitesse_path)

# 1 - surface de rugosité

# TODO :
# lire : Weiss et al 2018, A global map of travel time
# afin de créer une table habitat <-> vitesse potentielle de déplacement

# 2 - matrice de transition
# gdistance :: transition
# 3 - accumulations coûts


#### Viewshed/ information visuelle ####

## VAR : note esthétique moyenne perçue des habitats visibles ##
## VAR : sommets visibles depuis le pixel ##

## VAR : distance à l'eau ##
eau_vect <- st_read(path_vecteur_eaux_libres)
vect_obj_rast <- fasterize(eau_vect,raster(chemin_mnt))
dist_vect_obj_rast <- terra::distance(vect_obj_rast)
plot(dist_vect_obj_rast, colNA='black')
writeRaster(dist_vect_obj_rast, paste0(output_path,"/var_CS/distance_eau.tif"))


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

# Petite bidouille pour tester avec GRASS (sous Qgis)
vecteur_foret <- st_read(path_foret_IGN)
# retirer les forêts ouvertes + landes + fomration herbacée
unique(vecteur_foret$TFV)
# Champs rencontrés : Id_BIOTOPE|ID_BIOTOPE|*biotope|ID_NAT|ID_BIO|id_nat
vecteur_foret_nett <- vecteur_foret[-grep(pattern = 'Lande|Formation herbacée|Forêt ouverte*',
                                     ignore.case=TRUE,
                                     x=vecteur_foret$TFV),]
foret_rast <- fasterize(vecteur_foret_nett ,raster(chemin_mnt))
#foret_rast[foret_rast==0] <- NA
plot(foret_rast,colNA="black")
writeRaster(foret_rast, paste0(output_path,"forest_raster_IGN.tif"))

# raster friction = 1 partout
r <- raster(chemin_mnt)
r[] <- 1
plot(r)
writeRaster(r, paste0(output_path,"frictione_1.TIF"))

# TODO :
# - dans construction_var_inter sauvegarder vecteur d'habitat forestier
# - enlever champ de commun et table dans fonction DistanceA
# - calculer costDistance avec vecteur habitat forestier
# - pour éviyter effet de bord, prendre foret plus large que N2000
# - mieux ranger les fonctions : créer des scripts par dimension, faire passer calc var intermédiaires, etc
