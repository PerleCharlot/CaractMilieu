### Titre -------------------------------------
# Nom : Calcul variables Contexte Spatiael (CS) > Similarité
# Auteure : Perle Charlot
# Date de création : 10-02-2022
# Dates de modification : 

### Librairies -------------------------------------

library(sf)
library(dplyr)
library(raster)
library(data.table)
library(tictoc)
library(fasterize)
library(terra)

### Fonctions -------------------------------------

### Constantes -------------------------------------

# Espace de travail
wd <- getwd()
# Dossier des inputs
input_path <- paste0(wd,"/input/")
output_path <- paste0(wd,"/output/")

# Dossier des variables spatiales
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"

# Chemin du MNT 25m CALé sur le grille de REFERENCE
chemin_mnt_5 <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_belledonne.tif")

# Chemin vecteur habitat
path_vecteur_habitat <- paste0(dos_var_sp,"/Milieux/Natura_2000/n_hab_dominants_n2000_s_r84_jointure.gpkg")

# Chemin d'un raster calé
path_raster_ref <- paste0(dos_var_sp,"/Milieux/IGN/mnt_25m_belledonne_cale.TIF")

# Projection Lambert 93 (EPSG : 2154)
EPSG_2154 =  "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs "

### Programme -------------------------------------

## Mise en forme de la donnée habitat ####

vecteur_habitat <- st_read(path_vecteur_habitat)
# table correspondance habitat (lbhab) <-> code
correspondance_habitat <- vecteur_habitat %>% as.data.frame %>%
  group_by(lbhab, cdhab) %>%
  summarise()
correspondance_habitat <- cbind(correspondance_habitat,code=1:dim(correspondance_habitat)[1])
write.csv(correspondance_habitat,paste0(output_path,"/correspondance_habitat_raster.csv"))
vecteur_habitat <- merge(vecteur_habitat, correspondance_habitat, by="cdhab")

# Fusionner par attribut d'habitat et passer de multipolygones à polygones simples
multiP <- vecteur_habitat %>% 
  group_by(code) %>%
  summarise()
multiP <- st_cast(multiP, "MULTIPOLYGON")
singleP <- st_cast(multiP, "POLYGON")

vecteur_habitat2 <- merge(singleP , correspondance_habitat, by="code")
st_write(vecteur_habitat2,paste0(output_path,"/polygones_simplifié_habitat.gpkg"))

# Rastériser le vecteur
# Méthode : rasteriser à 5 m puis resample("ngb") à 25m 
# Cela permet de virer les NA au centre

mnt <- raster(chemin_mnt_5)
raster_habitat <- fasterize(vecteur_habitat,mnt,field='code')
raster_ref <- raster(path_raster_ref)
r_cale <- resample(raster_habitat, raster_ref, method="ngb")

# ! pb des linéaires (lbhab: Routes, pistes pastorales et forestières, parkings, zones terrassées)
# (code = 60)
# qui font >25m de largeur -> disparaissent lors rastérisation
# On souhaite forcer le pas sur ces linéaires
vecteur_habitat_lineaire <- vecteur_habitat[vecteur_habitat$code == 60,]
lin <- vecteur_habitat_lineaire[c(1:6,9,12:14,16,19:21,23,24),] #sélection manuelle ...
lin_buf <- st_buffer(lin,10)
raster_habitat_lineaire <- fasterize(lin_buf,raster_ref,field='code',fun="any")
#writeRaster(raster_habitat_lineaire, paste0(output_path,"/lineaire_test.tif"),overwrite=TRUE)
# combiner les 2 rasters
r_cale[raster_habitat_lineaire==60] <- 1
writeRaster(r_cale, paste0(output_path,"/habitat_raster_25m.tif"),overwrite=TRUE)



## Habitat similaire adjacent ####

# Consruction moving windows de 100, 250, 500 et 1000m
rayon_mw = 500 #en mètre
# rayon_mw = 250 #en mètre
# rayon_mw = 100 #en mètre
# rayon_mw = 1000 #en mètre
f <- terra::focalMat(r_cale, rayon_mw, "circle") 
f[f > 0] <- 1
# Récupération des codes d'habitats
classes <- sort(as.numeric(unique(values(r_cale))))

if(!dir.exists(paste0(output_path,"/var_habitat/couches_habitats_identiques_",rayon_mw,"m/"))){
  dir.create(paste0(output_path,"/var_habitat/couches_habitats_identiques_",rayon_mw,"m/"))}
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
              filename=paste0(output_path,"/var_habitat/couches_habitats_identiques_",rayon_mw,"m/",i,".tif"))
  }
# Rassembler les couches d'habitats similaires
x <- list.files(paste0(output_path,"/var_habitat/couches_habitats_identiques_",rayon_mw,"m/"),".tif", full.names=TRUE)
raster_habitat_voisinage <- Reduce(merge, lapply(x, raster))
writeRaster(raster_habitat_voisinage, 
            filename=paste0(output_path,"/var_habitat/habitat_similaire_",rayon_mw,"m.TIF"))

## Taille du patch d’habitat ####
# superficie, en m², du patch d'habitat auquel appartient le pixel
# calcul réalisé sur la donnée vecteur/raster pour comparaison

# Grille raster (pixels dont on a besoin)
grille <- raster(paste0(output_path,"/habitat_raster_25m.tif"))

# raster -> polygone (éliminer les NA centraux)
grille_v <- rasterToPolygons(grille, dissolve = TRUE)
grille_v <- st_as_sf(grille_v)
grille_v <- st_cast(grille_v, "POLYGON")
grille_v$superficie <- st_area(grille_v)
raster_taille_patch_habitat2 <- fasterize(grille_v,grille,field='superficie')
writeRaster(raster_taille_patch_habitat2, 
            filename=paste0(output_path,"/var_habitat/taille_patch_habitat_m2.TIF"))


