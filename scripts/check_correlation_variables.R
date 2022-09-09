### Titre -------------------------------------
# Nom : Corrélation entre variables climatiques
# Auteure : Perle Charlot
# Date de création : 09-08-2022
# Dates de modification : 08-08-2022

### Librairies -------------------------------------

library(raster)
library(sf)
library(data.table)
library(corrplot)
library(purrr)

### Fonctions -------------------------------------

# Transforme stack d'usages en dataframe, pour un mois donné
CheckCorr <- function(dimension, mois=c("mai","juin","juillet","aout","septembre")){
  # # TEST
  # mois = liste.mois[1]
  
  for(m in mois){
    list_usages = list.files(paste0(output_path,"/stack_dim/",dimension,"/",m),recursive=TRUE, ".tif$", full.names=TRUE)
    usages = stack(list_usages)
    usages_masked <- raster::mask(usages, limiteN2000)
    df_usages_masked = as.data.frame(as.data.table(usages_masked[]))
    df_usages_masked = na.omit(df_usages_masked)
    
    cor <- cor(df_usages_masked, method = "spearman")
    
    #"square", "ellipse", "number", "shade", "color", "pie""circle"
    if(!dir.exists(paste0(output_path,"/correlation"))){
      dir.create(paste0(output_path,"/correlation"),recursive=TRUE)}
    png(file=paste0(output_path,"/correlation/",dimension,"_mat_corr_spearman_",m,".png"),
        width=1000, height=1000)
    corrplot(cor, method = "number", type="lower", title = m,tl.srt=60,mar=c(0,1,1,0))
    dev.off()
  }
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
# Limite Zone Natura 2000
limiteN2000 <- paste0(dos_var_sp, "/limites_etude/cembraie_N2000_limites.gpkg")

#### Autre ####
# Liste mois étudiés
liste.mois = c("mai","juin","juillet","aout","septembre")
# Liste dimensions
liste.dim =  c("CA","B","PV","CS","D","I")

### Programme -------------------------------------

# Restreindre corrélation à la zone d'étude
limiteN2000 <- st_read(limiteN2000)

# Sortir les matrices de corrélation, par mois, pour la dimension CA
lapply(liste.dim, CheckCorr)


