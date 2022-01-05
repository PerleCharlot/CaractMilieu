### Titre -------------------------------------
# Nom : Création des variables issues du MNT
# Auteure : Perle Charlot
# Date de création : 17-11-2021
# Dates de modification : -2021

### Librairies -------------------------------------

library(data.table)
library(raster)


library(purrr)
library(rgdal)

### Fonctions -------------------------------------

### Constantes -------------------------------------

# Espace de travail
wd <- "C:/Users/perle.charlot/Documents/PhD/DATA/"

# Chemin du MNT 25m (si pas encore créé, celui à 5m)
if(file.exists(paste0(wd,"/Variables_spatiales_Belledonne/Milieux/IGN/mnt_25m_belledonne.tif"))){
  chemin_mnt <- paste0(wd,"/Variables_spatiales_Belledonne/Milieux/IGN/mnt_25m_belledonne.tif")
} else {chemin_mnt <- paste0(wd,"/Variables_spatiales_Belledonne/Milieux/IGN/mnt_belledonne.tif")}

# Dossier où l'on souhaite sauvegarder les variables calculées
dossier_sauv <- paste0(wd,"/Variables_spatiales_Belledonne/Milieux/variables_calculees_mnt/")

# Projection Lambert 93 (EPSG : 2154)
EPSG_2154 =  "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs "


### Programme -------------------------------------

# Espace de travail
setwd(wd)
# Espace sauvegarde des variables calculées
if(!dir.exists(dossier_sauv)){dir.create(dossier_sauv)}

# chargement MNT
MNT <- raster(chemin_mnt)
resMNT <- res(MNT)

# Passer le MNT à 25m de resolution
if(resMNT[1] != 25){
  facteur_aggreg <- 25/resMNT[1]
  crs(MNT) <- EPSG_2154
  MNT25m <- aggregate(MNT, fact=facteur_aggreg,filename=paste0(wd,"/Variables_spatiales_Belledonne/Milieux/IGN/mnt_25m_belledonne.tif"))
} else {MNT25m <- MNT}

# Calcul de la pente ----
pente25m <- terrain(MNT25m, opt="slope",unit="degrees",filename=paste0(dossier_sauv,"/pente_25m.tif"))

# Calcul d l'exposition ----
# Pour éviter discontinuité, transfo en northing et easting
exposition25m <- terrain(MNT25m, opt="aspect",unit="radians")
# /!\ sin(x) et cos(x) où x en radian
northing25m <- calc(exposition25m, fun = cos, filename= paste0(dossier_sauv,"/northing_25m.tif")) 
easting25m <- calc(exposition25m, sin,filename= paste0(dossier_sauv,"/easting_25m.tif"))
# Sauvegarde de l'exposition en degrés
exposition25m_deg <- terrain(MNT25m, opt="aspect",unit="degrees",filename=paste0(dossier_sauv,"/exposition_25m.tif"))

# Calcul du TWI ----
source("C:/Users/perle.charlot/Documents/R/win-library/dynatopmodel_1.2.1.tar/dynatopmodel_1.2.1/dynatopmodel/R/upslope_area.R")
TWI25m <- upslope.area(MNT25m, atb = TRUE)$atb
writeRaster(TWI25m, filename=paste0(dossier_sauv,"/TWI_25m.tif"))



library(spatialEco)

test_tpi100m <- tpi(MNT25m,scale=5)
test_tpi1000m <- tpi(MNT25m,scale=51)

plot(test_tpi1000m)

# Calcul du TPI 
TPI_100m <- terrain(MNT25m, opt="TPI",filename=paste0(dossier_sauv,"/TPI_25m.tif"))


#https://rpubs.com/ials3/dem1
library(rasterVis)
plot3D(MNT25m)

library(colorspace)
myTheme <- rasterTheme(region=sequential_hcl(10, power=2.2))
levelplot(MNT25m, par.settings = myTheme, contour = TRUE)
levelplot(MNT25m, par.settings = RdBuTheme, contour = TRUE)

# Calcul landform

# Depuis https://rpubs.com/ials3/dem1 
# TPI for different neighborhood size:
# first step: define customized function
tpiw <- function(x, w) {
  m <- matrix(1/(w^2-1), nc=w, nr=w)
  m[ceiling(0.5 * length(m))] <- 0
  f <- focal(x, m)
  x - f
}
# tpi300m (25m)
window300 <- round(300/25)+1
window2000 <- round(2000/25)+1
tpi300 <- tpiw(MNT25m, w=window300)
tpi2000 <- tpiw(MNT25m, w=window2000)

plot(tpi300)
plot(stack(tpi300,tpi2000))

hist(tpi5[])

TPI_raster <- tpi300
# Get the standard deviation of the TPI
SD <- sd(TPI_raster[],na.rm=T)

# Make landform classes
#Morphologic class De Reu et al. 2013;  Weiss (2001)
landform <- reclassify(TPI_raster, matrix(c(-Inf, -SD, 1,
                                      -SD, -SD/2, 2,
                                      -SD/2, 0, 3,
                                      0, SD/2, 4,
                                      SD/2, SD, 5,
                                      SD, Inf, 6),
                                    ncol = 3, byrow = T),
                       right = T)

# Turn it into categorical raster
landform <- as.factor(landform) 
rat <- levels(landform)[[1]]
rat[["landform"]] <- c('Valley', 'Lower Slope', 
                       'Flat Area','Middle Slope', 
                       'Upper Slope', 'Ridge')
levels(landform) <- rat 
# Plot the classification
x11(12,12)
library(RColorBrewer)
levelplot(landform, col.regions = rev(brewer.pal(6,'RdYlBu')),
          labels = rat$landcover,
          main = "Landform Classification",
          colorkey=list(labels=list(at=1:6, labels=rat[["landform"]])))
writeRaster(landform,paste0(dossier_sauv,"/landform_25m.tif"))


stack_var_topo <- stack(MNT25m,pente25m,TPI25m,exposition25m_deg,northing25m,easting25m,TWI)
plot(stack_var_topo)
as.data.table()






library(rgrass7)
library(rgdal)

