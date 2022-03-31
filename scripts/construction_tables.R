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

# Fonction qui sort le Numero_of_Points d'un df ayant en en colonne alti, orientation et pente reclassées selon les catégories SAFRAN CROCUS
extraction_NoP <- function(dtX){ #dtX doit avoir ses colonnes dans le bon ordre
  numP <- dt_sf[cat_raster_alti %in% dtX[,1] & 
                  cat_raster_aspect %in% dtX[,2] &
                  cat_raster_slope %in% dtX[,3] &
                  massif_num %in% dtX[,4]]$Number_of_points
  return(numP)
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
# Numéro de massif étudié (ici, Belledonne = 8; cf table_massifs_METEOFRANCE dans dossier input)
num_massif_etudie <- 8

#### Données spatiales ####

# Dossier des variables spatiales & chemins des fichiers
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"
# MNT 25m CALé sur le grille de REFERENCE
chemin_mnt <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_25m_belledonne_cale.tif")
expo_path <- paste0(output_path,"/var_CA/pas_utilisé/exposition_25m.TIF")
pente_path <- paste0(output_path,"/var_CA/pente_25m.TIF")
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
# Chemin table classes SAFRAN CROCUS
classSAFCRO_path<- paste0(input_path,"/safran_classes_complet.csv")

### Programme -------------------------------------

#### Table liant habitat (nom CBNA) <-> code raster habitat (1 à 60) ####
# cf script "construction_var_inter"

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





#### Table donnant les NoPs de site d'étude ####
# Chargement de la table des classes de SAFRAN CROCUS
safran_classes_complet <- fread(classSAFCRO_path, dec=",")
# Transformation des couches MNT, pente, expo en classes
mnt25m <- raster(chemin_mnt)
# rast_crocus_alti <- cut(mnt25m, breaks = c(0, seq(150, 4800, by = 300)) , include.lowest = TRUE)
reclassALTI <- data.frame(from= c(0,seq(150,4800,by=300)),
                          to= c(seq(150,5100,by=300)),
                          becomes= c(1:length(c(seq(150,5100,by=300)))))
rast_crocus_alti <- reclassify(mnt25m,reclassALTI, include.lowest=TRUE)
# Exposition
expo25m <- raster(expo_path)
reclassASPECT <- data.frame(from= c(-1,0,seq(22.5,360,by=45)),
                            to= c(-1,seq(22.5,400,by=45)),
                            becomes= c(1:9,2))
rast_crocus_aspect_1 <- reclassify(expo25m,reclassASPECT, include.lowest=TRUE)
# Pente
pente25m <- raster(pente_path)
reclassPENTE <- data.frame(from= c(0,10,30),
                           to= c(10,30,90),
                           becomes= c(1:3))
rast_crocus_slope <- reclassify(pente25m,reclassPENTE, include.lowest=TRUE)
rast_crocus_aspect <- mask(x = rast_crocus_aspect_1 , mask = rast_crocus_slope , 
                           maskvalue = 1, updatevalue=1)
# Ajout d'une couche massif
crocus_massif <- rast_crocus_alti
vals <- rep(num_massif_etudie,ncell(crocus_massif))
crocus_massif <- setValues(crocus_massif,vals)
rast_crocus_massif <- mask(crocus_massif, rast_crocus_alti)
# Rassembler rasters en une stack
crocus_stack <- stack(rast_crocus_alti, rast_crocus_aspect, 
                      rast_crocus_slope, rast_crocus_massif)
names(crocus_stack) <- c("alti","aspect","slope","massif")
plot(crocus_stack, colNA='black')
writeRaster(crocus_stack, file = paste0(input_path,"/stack.TIF"), 
            bylayer=TRUE,overwrite=TRUE,suffixe=names(crocus_stack))
# Extraire les valeurs de la stack pour traiter sur une DT
df_crocus_stack <- data.table(as.data.frame(crocus_stack)) 
df_crocus_stack <- cbind(df_crocus_stack, raster::coordinates(rast_crocus_alti))
df_crocus_stack <- df_crocus_stack[complete.cases(df_crocus_stack),]
# Transformer en DT
dt_sf <- as.data.table(safran_classes_complet)
DT_crocus_stack <- as.data.table(df_crocus_stack)
# Ajout d'une colonne index
DT_crocus_stack[, ..I := .I]
# Trouver le NoP correspondant à la combinaison alti + pente + orientation + massif
tic()
num_points = DT_crocus_stack[, extraction_NoP(.SD), by = ..I]
toc() # 1313.58 sec elapsed ~ 20-25 min
# Reconstruire un raster à partir de NoP
dfXYZ <- merge(DT_crocus_stack,num_points, all=TRUE)

write.csv(data.frame(NoPs = unique(dfXYZ$V1)),
          paste0(output_path,"/var_intermediaire/liste_NoP.csv"))

raster_NoP <- rasterFromXYZ(cbind(dfXYZ$x, dfXYZ$y, dfXYZ$V1))
crs(raster_NoP) <- EPSG_2154;names(raster_NoP) <- "Number_of_points"
plot(raster_NoP, colNA='black')
writeRaster(raster_NoP, file = paste0(input_path,"/raster_NoP.TIF"),overwrite=TRUE)
Belledonne <- safran_classes_complet %>% subset(massif_num == 8)
write.csv(Belledonne,paste0(input_path,"/combinaison_Belledonne.csv"))

# Sauvegarder le fichier de correspondance
df_combi_all <- merge(num_points,safran_classes_complet, by.x="V1",by.y="Number_of_points")
combinaisons_zone_etude <- merge(data.frame("Number_of_points"=unique(num_points$V1)),
                                 safran_classes_complet, by="Number_of_points")
write.csv(df_combi_all,file=paste0(input_path,"/df_pixels_safran.csv"))
write.csv(combinaisons_zone_etude,file=paste0(input_path,"/combinaisons_safran.csv"))



