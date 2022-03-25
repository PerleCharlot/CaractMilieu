### Titre -------------------------------------
# Nom : Constructions des tables
# Auteure : Perle Charlot
# Date de création : 01-03-2022
# Dates de modification : 

### Librairies -------------------------------------

library(data.table)
library(sf)
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

resolution_etude = 25

#### Données spatiales ####

# Dossier des variables spatiales & chemins des fichiers
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"
# MNT 25m CALé sur le grille de REFERENCE
chemin_mnt <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_25m_belledonne_cale.tif")
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
path_raster_habitat <- paste0(output_path,"/habitat_raster_25m.tif")

#### Tables ####

# Table correspondance habitat et degré d'artificialisation (0 à 3, pas à très artificialisé)
tbl_hbt_path <- paste0(dos_var_sp,"/Milieux/Natura_2000/table_habitats.csv")
path_table_hbt <- paste0(dos_var_sp,"/Milieux/Natura_2000/table_cdhab.csv")
path_table_corresp_hbt <- paste0(output_path,"/tables/correspondance_habitat_raster.csv")

### Programme -------------------------------------


#### Table liant habitat <-> degré artificialisation <-> forêt <-> code raster habitat ####
vecteur_habitat <- st_read(path_vecteur_habitat)
correspondance_habitat <- vecteur_habitat %>% as.data.frame %>%
  group_by(lbhab, cdhab) %>%
  summarise()
correspondance_habitat <- cbind(correspondance_habitat,code_raster=1:dim(correspondance_habitat)[1])

write.csv(correspondance_habitat,paste0(output_path,"/correspondance_habitat_raster.csv"))
vecteur_habitat <- merge(vecteur_habitat, correspondance_habitat, by="cdhab")


tbl_hbt <- fread(tbl_hbt_path)
tbl_correspo_code_rast <- fread(path_table_corresp_hbt)
tbl_hbt <- merge(tbl_hbt, tbl_correspo_code_rast, by = c("cdhab","lbhab"))
names(tbl_hbt)[5] <- "code_raster"
write.csv2(tbl_hbt, paste0(dos_var_sp,"/Milieux/Natura_2000/table_habitats.csv"),row.names = FALSE)

#### Table liant habitat <-> attributs PV <-> code raster habitat ####
table_cdhab <- fread(path_table_hbt)
table_corresp_hbt <- fread(path_table_corresp_hbt)
table_hbt <- merge(table_cdhab, table_corresp_hbt,by=c("cdhab","lbhab"))
write.csv(table_hbt,paste0(output_path,"/tables/table_hbt_PV.csv"))
