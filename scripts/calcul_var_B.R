### Titre -------------------------------------
# Nom : Calcul des variables de la dimension BIOMASSE
# Auteure : Perle Charlot
# Date de création : 25-03-2022
# Dates de modification : 30-05-2022

### Librairies -------------------------------------

library(data.table)
library(raster)
library(dplyr)
library(SPEI)

### Fonctions --------------------------------------

# Calcul de l'ETP, en mm, par l'équation de Penman. Par défaut, taille = "short"
calculETP <- function(i, taille = "short"){
  
  # # TEST
  # i = 1
  
  NoP = table_meteo$V1[i]
  taille = as.character(taille)
  
  # Tester sur une seule combinaison NoP
  sample_table_meteo <- as.data.frame(table_meteo[i,])
  # remettre en forme les données
  tmin = unlist(sample_table_meteo[,grep("tmin",names(sample_table_meteo))])
  tmax = unlist(sample_table_meteo[,grep("tmax",names(sample_table_meteo))])
  U2 = unlist(sample_table_meteo[,grep("windmean",names(sample_table_meteo))])
  Rs = unlist(sample_table_meteo[,grep("raymean",names(sample_table_meteo))])
  
  data_test <- data.frame('mois'=c(1:12),
                          'tmin'=tmin,'tmax'=tmax,'Rs'=Rs,'U2'=U2)
  
  lat_i = NoP_safran[NoP_safran$Number_of_points == NoP,]$latitude
  z_i = NoP_safran[NoP_safran$Number_of_points == NoP,]$alti_av 
  
  ETP <- penman(Tmin = data_test$tmin, #OK
                Tmax = data_test$tmax ,  #OK
                Rs =   data_test$Rs, #MJ.m-2.d-1 (par jour) incoming solar radiation !! OPTIONNEL
                U2 =  data_test$U2, #vitesse moyenne du vent mensuel, m.s-1 (mètre par seconde)
                lat= lat_i , # OK en degrés
                z = z_i,
                crop = taille,
  )
  
  
  # Ajouter NoP sur la ligne
  df_ETP <- as.data.frame(t(ETP))
  names(df_ETP) = month.name
  df_ETP$NoP = NoP
  
  l_ETP <- as.list(df_ETP)
  
  return(l_ETP)
  
}

# Fonction qui retourne l'ETP moyen pour une liste de mois de l'année donnée
sum_ETP <- function(liste_mois, fct){ #fct, character pour nommer "sum" ou "mean"
  fct = as.character(fct)
  index_col_var <- which(names(ETP_all) %in% liste_mois)
  df_var <- as.data.frame(ETP_all)[,index_col_var]
  df_var <- as.data.frame(apply(df_var,2,as.numeric))
  fct_ETP <- apply(df_var,1,sum) #sum des ETP mois par mois
  df_var_2 <- data.frame(fct_ETP)
  names(df_var_2) = paste0(fct,'ETP_',substitute(liste_mois))
  df_var_2$NoP = as.numeric(ETP_all$NoP)
  return(df_var_2)
}

# Fonction qui calcul le cumul de précipitations (en mm) sur une saison (donnée par une liste de mois)
sum_precip <- function(liste_mois, fct){
  
  # # TEST
  # liste_mois = été
  # fct = "sum"
  
  fct = as.character(fct)
  
  index_col_var <- which(names(df_precip) %in% paste0("prsum_",liste_mois))
  df_var <- as.data.frame(df_precip)[,index_col_var]
  df_var <- as.data.frame(apply(df_var,2,as.numeric))
  fct_precip <- apply(df_var,1,sum)
  df_var_2 <- data.frame(fct_precip)
  names(df_var_2) = paste0(fct,'precip_',substitute(liste_mois))
  df_var_2$NoP = as.numeric(ETP_all$NoP)
  return(df_var_2)
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

#### Données spatiales ####
# Dossier des variables spatiales & chemins des fichiers
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"
# chemin du dossier contenant les différentes couches spatiales iées aux habitats
path_poly_hab <- paste0(dos_var_sp ,"/Milieux/Natura_2000/n_hab_dominants_n2000_s_r84_jointure.gpkg")
path_rast_hab <- paste0(output_path,"/habitat_raster_25m.TIF")
# Dossier contenant les NDVI
path_dos_ndvi <- paste0(dos_var_sp,"/Milieux/NDVI/")

# Emprise carré autour N2000
path_emprise <- paste0(dos_var_sp,"/limites_etude/emprise.gpkg")
chemin_mnt <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_25m_belledonne_cale.tif")

#### Tables ####
path_table_hbt_PV <- paste0(output_path,"/tables/table_hbt_PV.csv")
path_table_data_meteo <- paste0(output_path,"/tables/table_NoPs_climat.csv")
path_NoP_safran <- paste0(input_path,"/combinaisons_safran.csv")

### Programme -------------------------------------

#### Quantité ####

## VAR : GDD ##
# calculé dans script "calcul_var_CA"  dans ** conditions climatiques ***

## VAR : NDVI ##
liste_ndvi <- list.files(path_dos_ndvi,".tif$",full.names = TRUE)
liste_ndvi_été <- liste_ndvi[grep("_ete", liste_ndvi)]
liste_ndvi_hiv <- liste_ndvi[grep("_hiv", liste_ndvi)]

stack_ndvi_hiv <- stack(liste_ndvi_hiv)
stack_ndvi_ete <- stack(liste_ndvi_été)

# Moyenne sur les X années
rast_ndvi_mean_été <- calc(stack_ndvi_ete, mean)
rast_ndvi_mean_hiv <- calc(stack_ndvi_hiv, mean)
plot(rast_ndvi_mean_hiv, colNA='black')

# recaler sur raster de ref
rast_ref <- raster(chemin_mnt)
rast_ndvi_mean_été <- projectRaster(rast_ndvi_mean_été,rast_ref)
rast_ndvi_mean_été_cale <- resample(rast_ndvi_mean_été, rast_ref)
rast_ndvi_mean_hiv <- projectRaster(rast_ndvi_mean_hiv,rast_ref)
rast_ndvi_mean_hiv_cale <- resample(rast_ndvi_mean_hiv, rast_ref)

# crop à l'emprise
emprise <- st_read(path_emprise)
rast_ndvi_mean_été_cale_crop <- raster::crop(rast_ndvi_mean_été_cale,emprise)
rast_ndvi_mean_hiv_cale_crop <- raster::crop(rast_ndvi_mean_hiv_cale,emprise)

par(mfrow=c(1,2))
plot(rast_ndvi_mean_été_cale_crop, main="NDVI été")
plot(rast_ndvi_mean_hiv_cale_crop, main ="NDVI hiver")

writeRaster(rast_ndvi_mean_été_cale_crop, paste0(output_path,"/var_B/NDVI_été.tif"))
writeRaster(rast_ndvi_mean_hiv_cale_crop, paste0(output_path,"/var_B/NDVI_hiv.tif"))

## VAR : Bois sur pied ##

## VAR : Abondance feullage ##
rast_hab <- raster(path_rast_hab)
table_hbt <- fread(path_table_hbt_PV)

# passer de catégoriel (néant, rare, moyen, abaondant) à ordinal (0,1,2,3)
table_hbt  <- table_hbt %>% 
  mutate(abondance_feuillage_été = recode(feuillage_dispo_été,
                                                             `néant`=0,
                                                             `rare`=1,
                                                             `moyen`=2,
                                                             `abondant`=3
                                                             ),
                     abondance_feuillage_hiv = recode(feuillage_dispo_hiv,
                                                             `néant`=0,
                                                             `rare`=1,
                                                             `moyen`=2,
                                                             `abondant`=3)
                     )
# créer les rasters
rast_feuillage_ete <- raster::subs(rast_hab,table_hbt,by='code',which='abondance_feuillage_été')
rast_feuillage_hiv <- raster::subs(rast_hab,table_hbt,by='code',which='abondance_feuillage_hiv')
writeRaster(rast_feuillage_ete, paste0(output_path,"/var_B/abondance_feuillage_été.tif"))
writeRaster(rast_feuillage_hiv, paste0(output_path,"/var_B/abondance_feuillage_hiv.tif"))

#### Qualité ####

## VAR : Digestibilité ##

#### Productivité ####

## VAR : P-ETP ##

# Lecture table contenant toutes les variables météo nécessaires pour calcul ETP
table_meteo <- fread(path_table_data_meteo)
# Lecture table contenant combinaisons safran
NoP_safran <- fread(path_NoP_safran,drop="V1")
# calcul ETP pour tous les NoP
ETP_all <- lapply(1:dim(table_meteo)[1], function(x) calculETP(x))
ETP_all <- as.data.frame(do.call(rbind, ETP_all))

ETP_all_tall <- lapply(1:dim(table_meteo)[1], function(x) calculETP(x,"tall"))
ETP_all_tall <- as.data.frame(do.call(rbind, ETP_all_tall))

# Définition des mois correspondant aux saisons étudiées (été et d'hiver)
été = c("June","July","August","September") 
hiver = c("December","January","February","March")
# Calculs des ETP saisonniers
ETP_été <- sum_ETP(été)
ETP_hiver <- sum_ETP(hiver)
df_meanETP <- merge(ETP_été, ETP_hiver, by ="NoP")

# TODO : demander à Claire + Isa son avis sur le fait de mettre tall (0.5m) ou short (0.12m)
#         demander avis sur valeurs ETP

# TODO : sortir un vecteur de précipitation avec les précipitations en mm, pour chaque mois

# les valeurs de précipitations me semblent assez faibles, à vérifier si le cumul est bien effectué 
# + si c'est bien en mm

# calcul cumul précipitations par saison
df_meteo = as.data.frame(table_meteo)
df_precip = df_meteo[,grep("prsum",names(df_meteo))]

été = c("06","07","08","09") 
hiver = c("12","01","02","03")

sum_precip(été, "sum")
sum_precip(hiver, "sum")

# Calcul P-ETP

