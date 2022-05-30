### Titre -------------------------------------
# Nom : Constructions des variables intermédiaires
# Auteure : Perle Charlot
# Date de création : 01-03-2022
# Dates de modification : 

### Librairies -------------------------------------

library(data.table)
library(dplyr)
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

#### Données spatiales ####

# Dossier des variables spatiales & chemins des fichiers
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"
# MNT 25m CALé sur le grille de REFERENCE
chemin_mnt <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_25m_belledonne_cale.tif")
chemin_mnt_emprise <- paste0(dos_var_sp ,"/Milieux/IGN/MNT_25m_50km_cale.tif")

# Chemin du MNT 25m CALé sur le grille de REFERENCE
chemin_mnt_5 <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_belledonne.tif")
# Surface en eaux libres et tronçons rivières IGN
surface_eau_path <- paste0(dos_var_sp,"/Milieux/IGN/surface_vecteur.gpkg")
tronçon_eau_path <- paste0(dos_var_sp,"/Milieux/IGN/tronçon_vecteur.gpkg")
# Forêts IGN
foret_path <- paste0(dos_var_sp,"/Milieux/IGN/tronçon_vecteur.gpkg")
# Bâti IGN (bâtiments, infrastructures de transport et transport par cable)
bati_path <- paste0(dos_var_sp,"/Milieux/IGN/batiment_emprise.gpkg")
transp_path <- paste0(dos_var_sp,"/Milieux/IGN/equipement_de_transport_emprise.gpkg")
cable_path <- paste0(dos_var_sp,"/Milieux/IGN/transport_par_cable_emprise.gpkg")
# Habitats expertisés
path_vecteur_habitat <- paste0(dos_var_sp,"/Milieux/Natura_2000/n_hab_dominants_n2000_s_r84_jointure.gpkg")
#path_vecteur_habitat_simplifié <- paste0(dos_var_sp,"/Milieux/Natura_2000/polygones_habitat_simplifié.gpkg")
path_raster_habitat <- paste0(output_path,"/habitat_raster_25m.tif")

# Carte Habitat OCS
path_OCS <- paste0(dos_var_sp,"/Milieux/occupation_sol/OCS_2020_emprise.tif")

# Chemin PPDIR
PDIPR_path <- paste0(dos_var_sp,"/Usages/randonnée/CD38_2017_PDIPR/PPDIR_emprise.gpkg")
sentier_path <- paste0(dos_var_sp,"/Milieux/IGN/troncon_route_transport_BD_TOPO_2019.gpkg")


#### Tables ####

### Programme -------------------------------------

# Création des dossiers de sauvegarde
if(!dir.exists(paste0(output_path,"/var_intermediaire"))){
  dir.create(paste0(output_path,"/var_intermediaire"))
}

#### COUCHE VECTORIELLE HABITATS N2000 ####
## Mise en forme de la donnée habitat
vecteur_habitat <- st_read(path_vecteur_habitat)
# table correspondance habitat (lbhab) <-> code
correspondance_habitat <- vecteur_habitat %>% as.data.frame %>%
  group_by(lbhab, cdhab) %>%
  summarise()
correspondance_habitat <- cbind(correspondance_habitat,code=1:dim(correspondance_habitat)[1])
write.csv(correspondance_habitat,paste0(output_path,"/tables/correspondance_habitat_raster.csv"))
vecteur_habitat <- merge(vecteur_habitat, correspondance_habitat, by="cdhab")

# Fusionner par attribut d'habitat et passer de multipolygones à polygones simples
multiP <- vecteur_habitat %>% 
  group_by(code) %>%
  summarise()
multiP <- st_cast(multiP, "MULTIPOLYGON")
singleP <- st_cast(multiP, "POLYGON")

vecteur_habitat2 <- merge(singleP , correspondance_habitat, by="code")
st_write(vecteur_habitat2,paste0(output_path,"/var_intermediaire/polygones_simplifié_habitat.gpkg"))

#### COUCHE RASTER HABITATS N2000 ####
# Méthode : rasteriser à 5 m puis resample("ngb") à 25m 
# Cela permet de virer les NA au centre
vecteur_habitat <- st_read(paste0(output_path,"/var_intermediaire/polygones_simplifié_habitat.gpkg"))
mnt <- raster(chemin_mnt_5)
raster_habitat <- fasterize(vecteur_habitat,mnt,field='code')
raster_ref <- raster(chemin_mnt)
r_cale <- resample(raster_habitat, raster_ref, method="ngb")

# ! pb des linéaires (lbhab: Routes, pistes pastorales et forestières, parkings, zones terrassées)
# (code = 60)
# qui font >25m de largeur -> disparaissent lors rastérisation
# On souhaite forcer le pas sur ces linéaires
vecteur_habitat_lineaire <- vecteur_habitat[vecteur_habitat$code == 60,]
lin <- vecteur_habitat_lineaire[c(1:6,9,12:14,16,19:21,23,24),] #sélection manuelle ...
lin_buf <- st_buffer(lin,10)
raster_habitat_lineaire <- fasterize(lin_buf,raster_ref,field='code',fun="any")
# combiner les 2 rasters
r_cale[raster_habitat_lineaire==60] <- 1
plot(r_cale, colNA='black')
writeRaster(r_cale, paste0(output_path,"/var_intermediaire/habitat_raster_25m.tif"),overwrite=TRUE)

#### COUCHE VECTORIELLE INFRASTRUCTURES ####

# Assemblage des différentes couches d'infrastructures
# Charger les vecteurs infrastructures (batiment, linéaire cable, transport)
bati_vect <- readOGR(bati_path)
cable_line <- readOGR(cable_path)
transp_vect <- readOGR(transp_path)
# Transformer de ligne à polygone + singlepolygon
cable_line_buf <- buffer(cable_line,10)
multiP <- st_cast(st_as_sf(cable_line_buf), "MULTIPOLYGON")
singleP <- st_cast(multiP, "POLYGON")
cable_line_buf <- as_Spatial(singleP)
# !! après examen sous QGIS, le refuge de la Pra disparait lors de la rastérisation
bati_vect_buf <- buffer(bati_vect, 5)
multiP <- st_cast(st_as_sf(bati_vect_buf), "MULTIPOLYGON")
singleP <- st_cast(multiP, "POLYGON")
bati_vect_buf <- as_Spatial(singleP)
# Regrouper toutes les infrastructures
infrastructures_vect <- raster::bind(bati_vect_buf,transp_vect,cable_line_buf)
st_write(st_as_sf(infrastructures_vect), paste0(output_path,"/var_intermediaire/infrastructures.gpkg"),append=F)

#### COUCHE RASTER EAUX LIBRES ####

# Chargement des vecteurs de tronçons et surface en eau (issu BD TOPO IGN)
vect_surf_eau <- st_read(surface_eau_path)
vect_surf_eau <- st_cast(vect_surf_eau, "POLYGON")

vect_tron_eau <- st_read(tronçon_eau_path)
vect_tron_eau <- buffer(as_Spatial(vect_tron_eau), 5)
vect_tron_eau <- st_cast(st_as_sf(vect_tron_eau), "POLYGON")
# Assembkage des vecteurs et sauvegarde
vect_eaux_libres <- raster::bind(as_Spatial(vect_surf_eau) ,as_Spatial(vect_tron_eau))
plot(vect_eaux_libres)
st_write(st_as_sf(vect_eaux_libres), paste0(output_path,"/var_intermediaire/eaux_libres.gpkg"),append=F)

# Rasteriser les vecteurs
rast_ref <- raster(chemin_mnt)
rast_surf_eau <- fasterize(vect_surf_eau,rast_ref)
rast_tron_eau <- rasterize(as(vect_tron_eau,"Spatial"),rast_ref, field=1)
# Assemblage des rasters
rast_eau <- merge(rast_surf_eau,rast_tron_eau)
names(rast_eau) <- "eaux_libres"
plot(rast_eau, colNA="black")
# Sauvegarde fichier raster
writeRaster(rast_eau, file= paste0(output_path,"/var_CS/eaux_libres.TIF"))

#### COUCHE RASTER CHEMINS ####

# Chargement des vecteurs de tronçons et surface en eau (issu BD TOPO IGN)
vect_sentier <- st_read(sentier_path)
vect_sentier <- st_zm(vect_sentier)
vect_sentier <- buffer(as_Spatial(vect_sentier), 10) 
vect_sentier <- st_cast(st_as_sf(vect_sentier), "POLYGON")
rast_ref <- raster(chemin_mnt)
rast_sentier <- fasterize(vect_sentier,rast_ref)
plot(rast_sentier, colNA="black")
writeRaster(rast_sentier, paste0(output_path,"/var_intermediaire/raster_sentier.tif"), overwrite=TRUE)

#### COUCHE POINTS D'ENTREES ####
vect_transport <- st_read(transp_path)
vect_parking <- vect_transport[vect_transport$NATURE == "Parking",]
point_parking <- st_centroid(vect_parking )
st_write(point_parking, paste0(output_path,"/var_intermediaire/points_parkings.gpkg"))
# les parkings sont-ils autant valides en été et en hiver ?

# inclusion des remontées mécaniques :
#  - point haut du télécabine de la Croix (ouvert weekends de juin + juillet + août + weeknds de septembre)
#     --> départ randonnées pédestre + VTT en été
#     point d'entrée en hiver pour ski de rando (et alpin)
#  - point haut télésiège Bachat-Bouloud : VTT + randonnée pédestre (ouvert juillet - aout)

trspt_cable <- st_read(cable_path)
ind <- grepl(c("Télésiège de Bachat Bouloud|Télécabine de la Croix"),trspt_cable$TOPONYME)
trspt_cable_pt_entree <- trspt_cable[ind,]

pt_BchBo <- st_cast(trspt_cable_pt_entree[1,], "POINT")
co_BchBO <- as.data.frame(st_coordinates(pt_BchBo))
pt_ht_BchBo <- pt_BchBo[which(co_BchBO$Z == max(co_BchBO$Z)),]

pt_Croix <- st_cast(trspt_cable_pt_entree[2,], "POINT")
co_Croix <- as.data.frame(st_coordinates(pt_Croix))
pt_ht_Croix <- pt_Croix[which(co_Croix$Z == max(co_Croix$Z)),]

# est-il pertinent de ne consider que ces 2 remontées comme points d'entrée en hiver ???

# Regrouper les points hauts des remontées mécaniques + les centroides des parkings
pts_entree <- rbind(pt_ht_BchBo[,2], # 1 point
      pt_ht_Croix[,2], # 1 point
      point_parking[,2]) # 19 points
pts_entree <- st_zm(pts_entree)
# plot(pts_entree)
st_write(pts_entree, paste0(output_path,"/var_intermediaire/pts_entree_été.gpkg"))

# library(ggplot2)
# ggplot() +
#   geom_sf(data = trspt_cable_pt_entree) +
#   geom_sf(data = pt_ht_Croix, color = 'green') + #
#   geom_sf(data = pt_ht_BchBo, color = 'red') +
#   coord_sf(datum = NULL)

#### COUCHE RASTER HABITATS OCS ####
rast_OCS <- raster(path_OCS) # à 10m, en 23 classes de land cover
# ré-échantillonnage pour mettre bonne résolution (25m) et caler la grille
rast_MNT_emprise <- raster(chemin_mnt_emprise)
rast_OCS_resample <- resample(rast_OCS, rast_MNT_emprise, method="ngb") # méthode ngb car on veut garder des entiers pour les classes de land cover
writeRaster(rast_OCS_resample, paste0(output_path,"/var_intermediaire/habitats_OCS.tif"))

