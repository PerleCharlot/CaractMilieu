### Titre -------------------------------------
# Nom : Prepare conversion table - Préparation données SAFRAN CROCUS
# Auteure : Isabelle Boulangeat
# Modification : Perle Charlot
# Date de création : ?
# Dates de modification : 08-01-2022

### Librairies -------------------------------------

library(tidync)
library(ncmeta)
library(tidyr)
library(raster)
library(data.table)
library(tictoc)

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
# Dossier des inputs
input_path <- paste0(wd,"/input/")
output_path <- paste0(wd,"/output/")

# Dossier des variables spatiales
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"

# Chemin des données SAFRAN (sur le serveur Infogeo, besoin connexion VPN)
chemin_fichiersncdf <- "D:/Meteo_France/SAFRAN_montagne-CROCUS_2020/alp_allslopes"

# Année pour laquelle on veut extraire la modélisation 
#/!\ une année commence le 080106 ?? 1er août ? ou 1er juin ??)
y = 2017 #(max 2017)

# Chemin du fichier nc de l'année correspondante
pro_file = paste0(path_data_allslopes, "/pro/PRO_",as.character(y),"080106_",as.character(y+1),"080106.nc")
meteo_file = paste0(path_data_allslopes, "/meteo/FORCING_",as.character(y),"080106_",as.character(y+1),"080106.nc")

# Numéro de massif étudié (ici, Belledonne = 8; cf table_massifs_METEOFRANCE dans dossier input)
num_massif_etudie <- 8

# Chemin raster MNT, exposition, pente de la zone étudiée
MNT_path <- paste0(dos_var_sp,"/Milieux/IGN/mnt_25m_belledonne_cale.TIF")
expo_path <- paste0(output_path,"var_topo/pas_utilisé/exposition_25m.TIF")
pente_path <- paste0(output_path,"var_topo/pente_25m.TIF")

# MNT_path <- paste0(dos_var_sp,"/Milieux/IGN/mnt_25m_belledonne.TIF")
# expo_path <- paste0(dos_var_sp,"/Milieux/variables_calculees_mnt/exposition_25m.TIF")
# pente_path <- paste0(dos_var_sp,"/Milieux/variables_calculees_mnt/pente_25m.TIF")

# Chemin table classes SAFRAN CROCUS
classSAFCRO_path<- paste0(input_path,"/safran_classes_complet.csv")

# Projection Lambert 93 (EPSG : 2154)
EPSG_2154 =  "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs "

### Programme -------------------------------------

## 1 - Création d'un raster ayant le numéro de combinaison des classes ####

# Chargement de la table des classes de SAFRAN CROCUS
safran_classes_complet <- fread(classSAFCRO_path, dec=",")

# Transformation des couches MNT, pente, expo en classes
mnt25m <- raster(MNT_path)
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
# rast_crocus_slope <- cut(pente25m, breaks = c(0, 10, 30, 90) , include.lowest = TRUE)

# Application masque pente faible [0-10°] (<=> rast_crocus_slope = 1 ) => expo = -1
# rast_crocus_aspect <- mask(x = rast_crocus_aspect_1 , mask = pente25m , 
#                                   maskvalue = 0, updatevalue=-1)
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
raster_NoP <- rasterFromXYZ(cbind(dfXYZ$x, dfXYZ$y, dfXYZ$V1))
crs(raster_NoP) <- EPSG_2154;names(raster_NoP) <- "Number_of_points"
plot(raster_NoP, colNA='black')

# Sauvegarder le raster
writeRaster(raster_NoP, file = paste0(input_path,"/raster_NoP.TIF"),overwrite=TRUE)

Belledonne <- safran_classes_complet %>% subset(massif_num == 8)
write.csv(Belledonne,paste0(input_path,"/combinaison_Belledonne.csv"))

# Sauvegarder le fichier de correspondance
df_combi_all <- merge(num_points,safran_classes_complet, by.x="V1",by.y="Number_of_points")
combinaisons_zone_etude <- merge(data.frame("Number_of_points"=unique(num_points$V1)),
      safran_classes_complet, by="Number_of_points")
write.csv(df_combi_all,file=paste0(input_path,"/df_pixels_safran.csv"))
write.csv(combinaisons_zone_etude,file=paste0(input_path,"/combinaisons_safran.csv"))

## 2 - Calcul des variables climatiques ####

# Test
safran_classes_complet <- fread(classSAFCRO_path, dec=",")
var_to_use_pro = c("RN_ISBA", "DSN_T_ISBA") 
# Net Radiation; total snow depth
# pas de temps = journalier
var_to_use_meteo = c("Tair","Rainf","NEB", "Wind","Wind_DIR") 
# temperature air, rainfall rate, nebulosity, wind speed, wind direction
# pas de temps = horaire
jours_ete_to_use = c("2017-06-17","2017-09-14") # ete (17 juin - 14 septembre = jours 320-44)
jours_hiver_to_use = c("2017-01-01","2017-03-31") # hiver (1 janvier - 31 mars = 90 jours = jours 153-242)
combinaisons_zone_etude <- fread(paste0(input_path,"/combinaisons_safran.csv"),drop="V1")
liste_combinaisons_to_use = combinaisons_zone_etude$Number_of_points
years_to_use = c(1991:2021) #moyenne sur 30 nas


source(paste0(wd,"/scripts/extract_meteo_fct_maj.R"))

extraction_meteo = extract_meteo(path_data_allslopes = chemin_fichiersncdf, 
                                            years = years_to_use, 
                                            variables = var_to_use,
                                            safran_classes_table = safran_classes_complet, 
                                            liste_combinaisons = liste_combinaisons_to_use, 
                                            jours_ete = jours_ete_to_use, 
                                            jours_hiver = jours_hiver_to_use)
meteo = extraction_meteo
# Sauvegarder la sortie
write.csv2(meteo, paste0(wd,"/output/resultats_modele_meteo_160pts.csv"), row.names = TRUE)


# Mettre en forme (bonnes unités, remplacer Inf par valeur)
library(weathermetrics)
hist(kelvin.to.celsius(meteo$Tmoy_hiver, round = 2))
kelvin.to.celsius(meteo$Tmoy_ete, round = 2)
hist(kelvin.to.celsius(meteo$Tquant25_hiver, round = 2))
kelvin.to.celsius(meteo$Tquant25_ete, round = 2)
kelvin.to.celsius(meteo$Tquant75_hiver, round = 2)
kelvin.to.celsius(meteo$Tquant75_ete, round = 2)
hist(meteo$Enneigmoy_hiver) #en m
hist(meteo$Enneigmoy_ete) #en m

hist(meteo$duree_neige_continue) #pourquoi ça monte à plus que 90 jours ??
hist(meteo$date_deneig) # c'est le n-ième jour ou ça représnte n jours ??
# sachant que 1 = 01/08

hist(meteo$nb_jr_gel_moins5_air_periodeveg)
hist(meteo$nb_jr_gel_0_sol_hiver)
hist(meteo$nb_jr_gel_0_sol_ete)
hist(meteo$nb_jr_precip_hiver)
hist(meteo$nb_jr_precip_ete)


# Replacer les Inf par 365 ?? pourquoi faire ça ?
# date déneigement infinie = le couvert neigeux est présent toute l'année
# durée enneigement infinie = le couvert neigeux est présent toute l'année
str(meteo$date_deneig[161])

any()

meteo[apply(meteo,2,is.infinite)] <- 365

meteo$date_deneig[is.infinite(meteo$date_deneig)] <- 365



# Extraire les valeurs de la stack pour traiter sur une DT
df_NoP <- data.table(as.data.frame(raster_NoP)) 
df_NoP <- cbind(df_NoP, raster::coordinates(raster_NoP))
df_NoP <- df_NoP[complete.cases(df_NoP),]


df_NoP_meteo <- merge(df_NoP, meteo, by.x="Number_of_points", by.y="NoP")
str(df_NoP_meteo)

names(df_NoP_meteo)

# Construire les rasters des var météo


# fonction pour construire un raster à partir d'un df
rastMeteo <- function(df, champ_x = "x", champ_y="y", champ_z){
  
  #TEST
  # df <- df_NoP_meteo
  # # champ_x <- "x"
  # # champ_y <- "y"
  # champ_z <- "Tmoy_hiver"
  
  df=as.data.frame(df)
  
  r_var <- rasterFromXYZ(cbind(df[,names(df) %in% champ_x],
                      df[,names(df) %in% champ_y],
                      df[,names(df) %in% champ_z]))
  
  crs(r_var) <- EPSG_2154;names(r_var) <- champ_z
  
  # Sauvegarder le raster
  writeRaster(r_var, file = paste0(output_path,"/var_meteo/",champ_z,".TIF"),overwrite=TRUE,progress="text")
}


liste_var_meteo <- names(df_NoP_meteo)[!names(df_NoP_meteo) %in% c("Number_of_points","x","y")]


for(i in 1:length(liste_var_meteo)){
  
  nom_z <- liste_var_meteo[i]
  print(nom_z)

  rastMeteo(df = df_NoP_meteo, champ_z=nom_z)
}



src$variable$name
src$source
src$axis
src$dimension
src$variable


str(src)
print(src)

