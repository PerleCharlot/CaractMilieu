### Titre -------------------------------------
# Nom : Calcul des variables de la dimension DYNAMIQUE
# Auteure : Perle Charlot
# Date de création : 01-03-2022
# Dates de modification : 05-07-2022

### Librairies -------------------------------------

library(data.table)
library(raster)

# library(sf)
# library(fasterize)
# library(rgdal)

### Fonctions -------------------------------------

CreateRastClim <- function(mois,
                          liste_variables = liste_var){
  # # TESt
  # liste_variables = liste_var
  # mois = "05"
  
  refMois <- data.frame(NomMois = c("avril","mai","juin","juillet","aout","septembre"),
                        ChiffreMois = c("04","05","06","07","08","09"))
  
  index_col_var <- which(names(table_NoPs) %in% paste0(liste_variables,"_",mois))
  df_var <- as.data.frame(table_NoPs)[,index_col_var]
  
  df_var$NoPs = table_NoPs$NoPs
  
  # Création et remplissage des rasters climatiques
  rast_NoPs <- raster(paste0(input_path,"/raster_NoP.tif"))
  names(rast_NoPs) = "NoPs"
  
  rast_climato <- subs(rast_NoPs, df_var, by="NoPs", which=1:(dim(df_var)[2]-1))
  writeRaster(rast_climato,
              paste0(output_path,"/var_CA/par_periode/",refMois$NomMois[refMois$ChiffreMois == mois],"/", 
                     liste_var,".tif"),
              bylayer=TRUE,overwrite=TRUE)
  
  return(df_var)
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
# chemin du dossier contenant les différentes couches shape du CLPA
path_doss_CLPA <- paste0(dos_var_sp,"Milieux/CLPA_Alp_L93_20191213_PCharlot/")

# CLPA_Alp_linpi_L93_20191213 # photo-interprétation, linéaire
# CLPA_Alp_lint_L93_20191213 # témoignage, linéaire
# CLPA_Alp_zonpi_L93_20191213 # photo-interprétation, polygone
# CLPA_Alp_zont_L93_20191213 # témoignage, polygone

# Chemin raster NoP SAFRAN
path_raster_NoP <- paste0(input_path,"/raster_NoP.tif")

# Emprise carré autour N2000
path_emprise <- paste0(dos_var_sp,"/limites_etude/emprise.gpkg")

chemin_mnt <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_25m_belledonne_cale.tif")

#### Tables ####
path_table_NoPs_1959_1988 <- paste0(output_path,"/tables/table_NoPs_climat_1959_1988.csv")
path_table_NoPs_1989_2018 <- paste0(output_path,"/tables/table_NoPs_climat_1989_2018.csv")


### Programme -------------------------------------

#### Régime de perturbation ####

## VAR : présence couloir d’avalanche (/âge depuis dernière perturbation) ##
liste_couche_CLPA <- list.files(path_doss_CLPA, "*.shp$", full.names=TRUE)
# do.call(readOGR, as.list(liste_couche_CLPA))
CLPA1 <- readOGR(liste_couche_CLPA[[1]])
CLPA2 <- readOGR(liste_couche_CLPA[[2]])
CLPA3 <- readOGR(liste_couche_CLPA[[3]])
CLPA4 <- readOGR(liste_couche_CLPA[[4]])

# Transformer de ligne à polygone + singlepolygon
CLPA1_buf <- buffer(CLPA1,10)
CLPA2_buf <- buffer(CLPA2,10)

# Regrouper toutes les infrastructures
CLPA <- raster::bind(CLPA1_buf,CLPA2_buf,CLPA3,CLPA4)

# rasteriser les couches
CLPA_raster <- fasterize(st_as_sf(CLPA),raster(chemin_mnt), background = 0)
plot(CLPA_raster, colNA="black")
writeRaster(CLPA_raster, paste0(output_path,"/var_D/presence_avalanche.TIF"),overwrite=TRUE)
st_write(st_as_sf(CLPA), paste0(output_path,"/var_intermediaire/couches_CLPA_regroupe.gpkg"))

#### Stade de succession ####

## VAR : vitesse greening ##

#### Changement climatique ####

## VAR : hausse température ##
tbl_old <- fread(path_table_NoPs_1959_1988)
names(tbl_old) = paste0(names(tbl_old),'_old')
names(tbl_old)[1] = "NoPs"
tbl_now <- fread(path_table_NoPs_1989_2018)
names(tbl_now)[1] = "NoPs"

tbl_CC <- as.data.frame(merge(tbl_old, tbl_now, by="NoPs"))

# Affreux cette boucle mais j'ai pas le courage de coder ça plus proprement ...  
mois = paste0("0",seq(4,9,1))
for(i in mois){
  #test
  #i = "04"
  
  old = grep(paste0("^tmean_",i,"_old$"),names(tbl_CC))
  now = grep(paste0("^tmean_",i,"$"),names(tbl_CC)) 
  
  tbl_CC$dif_tmean = tbl_CC[,now] - tbl_CC[, old]
  
  names(tbl_CC)[dim(tbl_CC)[2]] = paste0("dif_tmean_",i)
}

# Visualisation pour exploration des résultats
library(ggplot2)
library(patchwork)
p4 <- ggplot(tbl_CC,aes(y=dif_tmean_04,x=NoPs))+
         geom_point()+ geom_hline(yintercept=0, col='red')+
  labs(y="différence de température moyenne journalière (°C)", title="AVRIL")
p5 <-ggplot(tbl_CC,aes(y=dif_tmean_05,x=NoPs))+
  geom_point()+ geom_hline(yintercept=0, col='red')+
  labs(y="différence de température moyenne journalière (°C)", title="MAI")
p6 <-ggplot(tbl_CC,aes(y=dif_tmean_06,x=NoPs))+
  geom_point()+ geom_hline(yintercept=0, col='red')+
  labs(y="différence de température moyenne journalière (°C)", title="JUIN")
p7 <-ggplot(tbl_CC,aes(y=dif_tmean_07,x=NoPs))+
  geom_point()+ geom_hline(yintercept=0, col='red')+
  labs(y="différence de température moyenne journalière (°C)", title="JUILLET")
p8 <-ggplot(tbl_CC,aes(y=dif_tmean_08,x=NoPs))+
  geom_point()+ geom_hline(yintercept=0, col='red')+
  labs(y="différence de température moyenne journalière (°C)", title="AOUT")
p9 <-ggplot(tbl_CC,aes(y=dif_tmean_09,x=NoPs))+
  geom_point()+ geom_hline(yintercept=0, col='red')+
  labs(y="différence de température moyenne journalière (°C)", title="SEPTEMBRE")
(p4 + p5 + p6) / (p7 + p8 +p9)

# Création et remplissage des rasters climatiques
rast_NoPs <- raster(path_raster_NoP)
names(rast_NoPs) = "NoPs"
# Par périodes
rast_diffT_mois <- subs(rast_NoPs, tbl_CC, 
                       by="NoPs", 
                       which=grep("dif_tmean",names(tbl_CC)))
plot(rast_diffT_mois)

cuts=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2) #set breaks
pal <- colorRampPalette(c("blue","red"))

plot(rast_diffT_mois, breaks=cuts, col = pal(9))

writeRaster(rast_diffT_mois, bylayer = TRUE,suffix = names(rast_diffT_mois),
            paste0(output_path,"/var_D/diffT_.tif"),overwrite=TRUE)
       