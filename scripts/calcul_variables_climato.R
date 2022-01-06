### Titre -------------------------------------
# Nom : Prepare conversion table - Préparation données SAFRAN CROCUS
# Auteure : Isabelle Boulangeat
# Modification : Perle Charlot
# Date de création : ?
# Dates de modification : 06-01-2022

### Librairies -------------------------------------

library(tidync)
library(ncmeta)
library(tidyr)
library(raster)
library(data.table)

### Fonctions -------------------------------------

### Constantes -------------------------------------

# Espace de travail
wd <- getwd()
# Dossier des inputs
input_path <- paste0(wd,"/input/")
# Dossier des variables spatiales
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"

# Chemin des données SAFRAN (sur le serveur Infogeo, besoin connexion VPN)
path_data_allslopes = "//infogeo//infogeo/Meteo_France/SAFRAN_montagne-CROCUS_2020/alp_allslopes"
# path_data_allslopes = "/Volumes/infogeo/Meteo_France/SAFRAN_montagne-CROCUS_2019/alp_allslopes"
# path_data_allslopes = "/Volumes/ISA-RESEARCH/alp_allslopes"

# Année pour laquelle on veut extraire la modélisation 
#/!\ une année commence le 080106 ?? 1er août ? ou 1er juin ??)
y = 2017 #(max 2017)

# Chemin du fichier nc de l'année correspondante
pro_file = paste0(path_data_allslopes, "/reanalysis/pro/PRO_",as.character(y),"080106_",as.character(y+1),"080106.nc")

# Numéro de massif étudié (ici, Belledonne = 8; cf table_massifs_METEOFRANCE dans dossier input)
num_massif_etudie <- 8

# Chemin raster MNT, exposition, pente de la zone étudiée
MNT_path <- paste0(dos_var_sp,"/Milieux/IGN/mnt_25m_belledonne.TIF")
expo_path <- paste0(dos_var_sp,"/Milieux/variables_calculees_mnt/exposition_25m.TIF")
pente_path <- paste0(dos_var_sp,"/Milieux/variables_calculees_mnt/pente_25m.TIF")

# Chemin table classes SAFRAN CROCUS
classSAFCRO_path<- paste0(input_path,"/safran_classes_complet.csv")

### Programme -------------------------------------

# Chargement de la table des classes de SAFRAN CROCUS
safran_classes_complet <- fread(classSAFCRO_path)

# Transformation des couches MNT, pente, expo en classes
mnt25m <- raster(MNT_path)
crocus_alti <- cut(mnt25m, breaks = c(0, seq(150, 5000, by = 300)) , include.lowest = TRUE)

expo25m <- raster(expo_path)
crocus_aspect <- cut(expo25m, breaks = c(-1,0,seq(22.5, 400, by = 45)) , include.lowest = TRUE)

pente25m <- raster(pente_path)
crocus_slope <- cut(pente25m, breaks = c(-1, 0, 36.4, 85) , include.lowest = FALSE)
#cut(crocus_slope_alps, breaks = c(0, 10, 30, 90) , include.lowest = TRUE)

# Ajout d'une couche massif
crocus_massif <- crocus_alti
vals <- rep(num_massif_etudie,ncell(crocus_massif))
crocus_massif <- setValues(crocus_massif,vals)

# Rassembler rasters en une stack
crocus_stack <- stack(crocus_alti, crocus_aspect, crocus_slope, crocus_massif)
names(crocus_stack) <- c("alti","aspect","slope","massif")
plot(crocus_stack)

sf4$NoP_stack = unlist(apply(sf4[,c("cat_raster_alti", "cat_raster_slope", "cat_raster_aspect", "massif_num")], 1, function(x){paste0(x[1:4], collapse="")}))
write.table(sf4, file = "/Volumes/ISA-RESEARCH/_DATA/fra_SAFRAN_CROCCUS/safran_classes_complet.csv", sep=";", row.names = FALSE, dec=",")


df_crocus_stack <- data.table(as.data.frame(crocus_stack)) # Finished in 00:09:24 elapsed (00:06:46 cpu)
df_crocus_stack <- df_crocus_stack[complete.cases(df_crocus_stack),]

# x c'est le rasterµ
# sf4 c'est la table ref safran_classes_complet
names(safran_classes_complet)






test <- safran_classes_complet[which(((safran_classes_complet$cat_raster_alti == df_crocus_stack$alti & 
              safran_classes_complet$cat_raster_aspect == df_crocus_stack$aspect) & 
             safran_classes_complet$cat_raster_slope==df_crocus_stack$slope) & 
            safran_classes_complet$massif_num==df_crocus_stack$massif),
    "Number_of_points"]


# Fonction correspondance
corresp <- function(x, na.rm){
  if(sum(is.na(x))==0){
  num_point = safran_classes_complet[which(((safran_classes_complet$cat_raster_alti == x[1] & safran_classes_complet$cat_raster_aspect == x[3]) & safran_classes_complet$cat_raster_slope==x[2]) & safran_classes_complet$massif_num==x[4]), "Number_of_points"]
  }else num_point=NA
  if(length(num_point)==0) num_point=as.numeric(paste0(c(9999, x), collapse = ""))
  return(num_point)
}


test2 <- stackApply(crocus_stack,1:4,fun = corresp)

library(doParallel)
beginCluster(n=4, type="SOCK")
cl = getCluster()
clusterExport(cl, "safran_classes_complet", envir=environment())
NoP = clusterR(crocus_stack, stackApply, args = list(indices=rep(1, 4), fun = corresp, na.rm=FALSE))
endCluster()


writeRaster(NoP, file = "/Volumes/ISA-RESEARCH/_DATA/fra_SAFRAN_CROCCUS/safran_raster_NoP.img", format = "HFA", overwrite=TRUE)

# collapse <- function(x, na.rm){as.numeric(paste0(x, collapse = ""))}
#
# library(doParallel)
# beginCluster(n=4, type="SOCK")
# cl = getCluster()
# clusterExport(cl, "sf4", envir=environment())
# NoP_stack = clusterR(crocus_stack, stackApply, args = list(indices=rep(1, 4), fun = collapse, na.rm=FALSE))
# endCluster()

# NoP_stack <- stackApply(crocus_stack, indices=rep(1, 4),fun = collapse, na.rm=TRUE)

# index = which(NoP[]==9999)
# values = extract(crocus_stack, index)

# writeRaster(NoP_stack, file = "/Volumes/ISA-RESEARCH/_DATA/fra_SAFRAN_CROCCUS/safran_raster_NoP_stack.img", format = "HFA", overwrite=TRUE)
