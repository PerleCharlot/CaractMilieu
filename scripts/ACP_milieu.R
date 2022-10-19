### Titre -------------------------------------
# Nom : ACP du milieu 
# Auteure : Perle Charlot
# Date de création : 04-06-2022
# Dates de modification : 18-10-2022

### Librairies -------------------------------------

library(raster)
library(data.table)
library(FactoMineR)
library(factoextra)
library(patchwork)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sjmisc)

### Fonctions -------------------------------------

# Fonction qui calcule une FAMD, pour une dimension, pour une saison
makeFAMD <- function(table_donnees, saison, dimension, palette_couleur){
  # # TEST
  # table_donnees = dt_stack
  # saison = i
  # dimension = "CA"
  # palette_couleur = mypalette

  corresp = data.frame(numero_saison=c("05","06","07","08","09"),
                       periode = c("mai",'juin','juillet','aout','septembre'))
  num_saison = corresp$numero_saison[which(corresp$periode == saison)]
  corresp_col = data.frame(dim_name = c("CA","B","PV","CS","D","I","toutes","ACP_FAMD"),
                       colour_dim = c("dodgerblue","darkgoldenrod1","darkgreen",
                                      "brown","blueviolet","darkgray","antiquewhite","ivory3"))
  dim_col = corresp_col$colour_dim[which(corresp_col$dim_name == dimension)]
  
  # Pour dimension = ACP_FAMD
  if(dimension == "ACP_FAMD"){
    corresp_axes = data.frame(axe_FAMd =c(paste0("axe1_B_",saison),paste0("axe2_B_",saison) ,paste0("axe3_B_",saison),
                                          paste0("axe1_CA_",saison), paste0("axe2_CA_",saison), paste0("axe3_CA_",saison),
                                          paste0("axe1_CS_",saison), paste0("axe2_CS_",saison) ,paste0("axe3_CS_",saison),
                                          paste0("axe1_D_",saison) ,  paste0("axe2_D_",saison),
                                          paste0("axe1_I_",saison),  paste0("axe2_I_",saison), paste0("axe3_I_",saison),
                                          paste0("axe1_PV_",saison), paste0("axe2_PV_",saison), paste0("axe3_PV_",saison)),
                              dim_name=c(rep("B",3),rep("CA",3),rep("CS",3),rep("D",2),rep("I",3),rep("PV",3))
    )
    a = merge(corresp_axes, corresp_col, by="dim_name")
    palette_couleur <- setNames(a$colour_dim, 
                          a$axe_FAMd)
  }

  # Retirer x et y 
  tbl_data = subset(table_donnees,select=-c(x,y))
  
  # Recoder les variables factorielles (pour ne pas qu'il y ait de doublons)
  col_quali <- unlist(lapply(tbl_data, is.factor))
  index_col_quali = which(col_quali == TRUE)
  for(index_colonne in index_col_quali){
    # # TEST
    # index_colonne = index_col_quali[1]
    cat(paste0("\nVariable ",names(tbl_data)[index_colonne], " en traitement."))
    # recodage
    recodage = paste0(levels(tbl_data[, index_colonne]),"=",
                      names(tbl_data)[index_colonne],"_",levels(tbl_data[, index_colonne]),sep=";",collapse="")
    # mutate avec var recodée + retirer la colonne initiale
    tbl_data  = tbl_data %>%
      mutate(X = rec(tbl_data[, index_colonne], rec =  recodage))
    # changer name col var recodée
    names(tbl_data)[which(names(tbl_data)== "X")] <- paste0(names(tbl_data)[index_colonne],"_rec")
      }
  # retirer les colonnes initiales
  tbl_data  = tbl_data %>%
    select(-index_col_quali)

  new_names = substr(names(tbl_data)[grep("_rec",names(tbl_data))], 1,nchar(names(tbl_data)[grep("_rec",names(tbl_data))])-4)
  names(tbl_data)[grep("_rec",names(tbl_data))] <- new_names

  t = try(FAMD(tbl_data , graph = FALSE))
  if(inherits(t, "try-error")) {
    # PCA si seulement quanti
    res.famd <- PCA(tbl_data , graph = FALSE)
    type = "PCA"
  } else{  # FAMD si miste quali/quanti
    res.famd <- FAMD(tbl_data , graph = FALSE)
    type = "FAMD"
    }
  
  # % variance expliquée par axe
  liste_variance_expl = round(res.famd$eig[,2],1)
  if(length(liste_variance_expl)>2){
    graph_var_expl <- fviz_eig(res.famd,
                               choice=c("variance"),
                               geom=c("bar"),
                               xlab="Axe",
                               ncp=length(liste_variance_expl),
                               barfill= dim_col,
                               ylim=c(0,100),
                               addlabels=F,main=' ',
                               font.x=c(24,"plain","black"),
                               font.y=c(28,"plain","black"),
                               font.tickslab = c(24,"plain","black")
    )  + geom_text(size = 12,label = liste_variance_expl[1:length(liste_variance_expl)]) + annotate(geom="text", x=length(liste_variance_expl)/2, y=80, size = 12,
                   label=paste0("Somme variance expliquée\npar 3 premiers axes : ",sum(liste_variance_expl[1:3]),"%"),
                   color="black")
  } else {
    graph_var_expl <- fviz_eig(res.famd,
                               choice=c("variance"),
                               geom=c("bar"),
                               xlab="Axe",
                               ncp=2,
                               barfill= dim_col,
                               ylim=c(0,100),
                               addlabels=F,main=' ',
                               font.x=c(24,"plain","black"),
                               font.y=c(28,"plain","black"),
                               font.tickslab = c(24,"plain","black")
    )  + geom_text(size = 15,label = liste_variance_expl[1:2])+ annotate(geom="text", x=2.5, y=80, size = 10,
                   label=paste0("Somme variance expliquée\npar 3 premiers axes : ",sum(liste_variance_expl[1:2]),"%"),
                   color="black")
  }

  # Graphique des variables
  options(ggrepel.max.overlaps = Inf)
  graph_var <- fviz_famd_var(res.famd, "var", col.var = "cos2",geom=c("arrow","text"),
                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                             font.x=c(24,"plain","black"),
                             font.y=c(24,"plain","black"),
                             font.tickslab = c(24,"plain","black"),
                             labelsize=8,
                             repel = TRUE)
  if(type == "PCA"){
    graph_var_quanti_23 <- fviz_famd_var(res.famd, "var", col.var = "cos2",
                                         geom=c("arrow","text"),
                                         axes = c(2, 3),
                                         font.x=c(24,"plain","black"),
                                         font.y=c(24,"plain","black"),
                                         font.tickslab = c(24,"plain","black"),
                                         labelsize=8,
                                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                         repel = TRUE)
    p8b = graph_var_quanti_23
    png(file=paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/plot_rmd/graph_var_quanti_axes2_3_",dimension,"_",saison,".png"), 
        width=1000, height=800)
    print(p8b)
    dev.off()
    
  }
  
  if(type == "FAMD"){
    
    t = try(fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
                          geom=c("arrow","text"),
                          font.x=c(24,"plain","black"),
                          font.y=c(24,"plain","black"),
                          font.tickslab = c(24,"plain","black"),
                          labelsize=8,
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          repel = TRUE))
    if(inherits(t, "try-error")) {
      # trop peu de vars quanti
    } else{
      graph_var_quanti_12 <- fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
                                           geom=c("arrow","text"),
                                           font.x=c(24,"plain","black"),
                                           font.y=c(24,"plain","black"),
                                           font.tickslab = c(24,"plain","black"),
                                           labelsize=8,
                                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                           repel = TRUE)
      graph_var_quanti_23 <- fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
                                           geom=c("arrow","text"),
                                           axes = c(2, 3),
                                           font.x=c(24,"plain","black"),
                                           font.y=c(24,"plain","black"),
                                           font.tickslab = c(24,"plain","black"),
                                           labelsize=8,
                                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                           repel = TRUE)
      
      p8a = graph_var_quanti_12
      p8b = graph_var_quanti_23
      png(file=paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/plot_rmd/graph_var_quanti_axes1_2_",dimension,"_",saison,".png"), 
          width=1000, height=800)
      print(p8a)
      dev.off()
      png(file=paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/plot_rmd/graph_var_quanti_axes2_3_",dimension,"_",saison,".png"), 
          width=1000, height=800)
      print(p8b)
      dev.off()
    }
    
    t = try(fviz_famd_var(res.famd, "quali.var", col.var = "cos2",
                          geom=c("arrow","text"),
                          font.x=c(24,"plain","black"),
                          font.y=c(24,"plain","black"),
                          font.tickslab = c(24,"plain","black"),
                          labelsize=8,
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          repel = TRUE))
    if(inherits(t, "try-error")) {
      # trop peu de vars quali
    } else {
      graph_var_quali_12 <- fviz_famd_var(res.famd, "quali.var", col.var = "cos2",
                                          geom=c("arrow","text"),
                                          font.x=c(24,"plain","black"),
                                          font.y=c(24,"plain","black"),
                                          font.tickslab = c(24,"plain","black"),
                                          labelsize=8,
                                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                          repel = TRUE)
      if(length(liste_variance_expl)>2){      
        graph_var_quali_23 <- fviz_famd_var(res.famd, "quali.var", 
                                            col.var = "cos2",
                                            geom=c("arrow","text"),
                                            axes = c(2, 3),
                                            font.x=c(24,"plain","black"),
                                            font.y=c(24,"plain","black"),
                                            font.tickslab = c(24,"plain","black"),
                                            labelsize=8,
                                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                            repel = TRUE)
        p9b = graph_var_quali_23
        png(file=paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/plot_rmd/graph_var_quali_axes2_3_",dimension,"_",saison,".png"), 
            width=1000, height=800)
        print(p9b)
        dev.off()
        }

      p9a = graph_var_quali_12

      png(file=paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/plot_rmd/graph_var_quali_axes1_2_",dimension,"_",saison,".png"), 
          width=1000, height=800)
      print(p9a)
      dev.off()

      
    }
  }
  
  # Colorer les barres par dimension
  graph_contrib_var_axe1 <- fviz_contrib(res.famd, "var", 
               axes = 1,
               title='',
               fill = "name", 
               color = "black",
               font.y=c(24,"plain","black"),
               font.tickslab = c(28,"plain","black"))+ 
    scale_fill_manual(values = palette_couleur)+
    theme(legend.position = "none")
  graph_contrib_var_axe2 <- fviz_contrib(res.famd, "var", 
                                         axes = 2,
                                         title='',
                                         fill = "name", 
                                         color = "black",
                                         font.y=c(24,"plain","black"),
                                         font.tickslab = c(28,"plain","black"))+ 
    scale_fill_manual(values = palette_couleur)+
    theme(legend.position = "none")
  if(length(liste_variance_expl)>2){
    graph_contrib_var_axe3 <- fviz_contrib(res.famd, "var", 
                                           axes = 3,
                                           title='',
                                           fill = "name", 
                                           color = "black",
                                           font.y=c(24,"plain","black"),
                                           font.tickslab = c(28,"plain","black"))+ 
      scale_fill_manual(values = palette_couleur)+
      theme(legend.position = "none")
  }

  # Graphique des individus
  t2 = try(fviz_famd_ind(res.famd,  alpha.ind = 0.05,
                         geom=c("point"),
                         repel = FALSE), silent=T)
  if(inherits(t2, "try-error")) {
    graph_ind <- fviz_pca_ind(res.famd,  alpha.ind = 0.05,
                              title=' ',
                              geom=c("point"),
                              font.x=c(28,"plain","black"),
                              font.y=c(28,"plain","black"),
                              font.tickslab = c(28,"plain","black"),
                              repel = FALSE)
  } else{  
    graph_ind <- fviz_famd_ind(res.famd,  alpha.ind = 0.05,
                               title=' ',
                               font.x=c(28,"plain","black"),
                               font.y=c(28,"plain","black"),
                               font.tickslab = c(28,"plain","black"),
                               geom=c("point"),
                               repel = FALSE)}
  
  # Création et Sauvegarde des graphiques
  if(!dir.exists(paste0(output_path,"/ACP/",dimension,"/",num_saison,saison))){
    dir.create(paste0(output_path,"/ACP/",dimension,"/",num_saison,saison),recursive = T)
    }
  p1 = plot(graph_var_expl) 
  p2 = plot(graph_contrib_var_axe1) 
  p3 = plot(graph_contrib_var_axe2)
  p4 = p1 / (p2 | p3)
  png(file=paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/contrib_",dimension,"_",saison,".png"), 
      width=1400, height=800)
  print(p4)
  dev.off()
  
  p5 = graph_var
  p6 = graph_ind
  p7 = p5 | p6
  png(file=paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/graph_",dimension,"_",saison,".png"), 
      width=1400, height=800)
  print(p7)
  dev.off()
  
  # Pour rmd, sauvegarde plots seuls
  if(!dir.exists(paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/plot_rmd"))){
    dir.create(paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/plot_rmd"))}
    
  png(file=paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/plot_rmd/graph_var_",dimension,"_",saison,".png"), 
      width=1000, height=800)
  print(p5)
  dev.off()
  
  png(file=paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/plot_rmd/graph_ind_",dimension,"_",saison,".png"), 
      width=1000, height=800)
  print(p6)
  dev.off()
  
  png(file=paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/plot_rmd/contrib_axe1_",dimension,"_",saison,".png"), 
      width=1000, height=800)
  print(graph_contrib_var_axe1)
  dev.off()
  png(file=paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/plot_rmd/contrib_axe2_",dimension,"_",saison,".png"), 
      width=1000, height=800)
  print(graph_contrib_var_axe2)
  dev.off()
  if(length(liste_variance_expl)>2){
    png(file=paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/plot_rmd/contrib_axe3_",dimension,"_",saison,".png"), 
        width=1000, height=800)
    print(graph_contrib_var_axe3)
    dev.off()
  }
  
  png(file=paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/plot_rmd/eboulis_variance_",dimension,"_",saison,".png"), 
      width=1000, height=800)
  print(graph_var_expl)
  dev.off()
  
  # valeurs des axes, par pixels
  # FAMD_tbl <-as.data.frame(res.famd$svd$U)
  FAMD_tbl <- as.data.frame(res.famd$ind$coord)
  names(FAMD_tbl) = paste0("axe",seq(1:length(colnames(FAMD_tbl))))
  table_all <- cbind(table_donnees,FAMD_tbl)
  write.csv(table_all, paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/tblFAMD_",dimension,"_",saison,".csv"))
  
  # Création des rasters des axes 1, 2 et 3 de la FAMD
  rast_axe1 <- rasterFromXYZ(cbind(table_all$x, table_all$y, table_all$axe1), crs=EPSG_2154)
  names(rast_axe1) = paste0("axe1_",dimension,"_",saison)
  rast_axe2 <- rasterFromXYZ(cbind(table_all$x, table_all$y, table_all$axe2), crs=EPSG_2154)
  names(rast_axe2) = paste0("axe2_",dimension,"_",saison)
  
  ref = raster(chemin_mnt)
  ExtCRS <- function(raster_to_check, raster_ref = ref){
    # #TEST
    # raster_to_check = rast_axe1
    # raster_ref=ref
    
    ext.to.check <- extent(raster_to_check)
    sameCRS <- compareCRS(raster_to_check,EPSG_2154)
    sameExtent <- (ext.to.check == extent(raster_ref))
    if(any(!sameCRS,!sameExtent)) {
      raster_to_check <- projectRaster(raster_to_check, raster_ref)
      cat("\nRaster", names(raster_to_check),"a été modifié et sauvegardé.")
    }
    return(raster_to_check)
  }
  rast_axe1 = ExtCRS(rast_axe1)
  rast_axe2 = ExtCRS(rast_axe2)
  writeRaster(rast_axe1, paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/axe1_",dimension,"_",saison,".tif"), overwrite=T)
  writeRaster(rast_axe2, paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/axe2_",dimension,"_",saison,".tif"), overwrite=T)
  if(length(liste_variance_expl)>2){
    rast_axe3 <- rasterFromXYZ(cbind(table_all$x, table_all$y, table_all$axe3), crs=EPSG_2154)
    names(rast_axe3) = paste0("axe3_",dimension,"_",saison)
    rast_axe3 = ExtCRS(rast_axe3)
    writeRaster(rast_axe3, paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/axe3_",dimension,"_",saison,".tif"), overwrite=T)
    # sauvegarde brick pour visualiser en multiband sur QGIS
    writeRaster(stack(rast_axe1,rast_axe2,rast_axe3), paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/stack_",dimension,"_",saison,".tif"), overwrite=T)
  }else{# sauvegarde brick pour visualiser en multiband sur QGIS
    writeRaster(stack(rast_axe1,rast_axe2), paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/stack_",dimension,"_",saison,".tif"), overwrite=T)}
  }

# fonction qui fait tout ... 
fct_FAMD <- function(dimension,
                     # arg_ACP,
                     periode=c("mai",'juin','juillet','aout','septembre'),
                     palette_couleur=mypalette){
  
  # # # TEST
  # dimension = liste.dim[1]
  # periode = c("mai",'juin','juillet','aout','septembre')
  # palette_couleur = mypalette
  # # # TEST
  # i = periode[1]
  
  # Fonctionnement par période
  for(i in periode){
     
    # # Pour dimension CA, stack avec toutes les variables ou avec 10 vars clim synthétisées en 2 axes
    # if(dimension == "CA"){
    #   
    #   cat("\n L'analyse factorielle sur la dimension CA est faite ",arg_ACP,
    #       " axes ACP des variables climatiques.")
    #   
    #   liste_stack = list.files(paste0(path_dos_stack,dimension,"/",i),".tif", full.names = T)
    # 
    #   if(arg_ACP == "sans"){
    #     stack_ACP = liste_stack[grepl("sansACP",liste_stack)]
    #   }
    #   if(arg_ACP == "avec"){
    #     stack_ACP = liste_stack[!grepl("sansACP",liste_stack)]
    #   }
    #   stack_dim <- stack(stack_ACP)
    # } else {stack_dim <- stack(list.files(paste0(path_dos_stack,dimension,"/",i),".tif", full.names = T))}
    
    stack_dim <- stack(list.files(paste0(path_dos_stack,dimension,"/",i),".tif", 
                                  full.names = T))
    
    # Transformation en table
    dt_stack <- as.data.frame(data.table(as.data.frame(stack_dim)))
    dt_stack <- cbind(dt_stack,coordinates(stack_dim))
    # Retirer les NA (quand calculé sur N2000 et pas emrpise carrée) : 211 200 pixels --> 50 320
    dt_stack <- dt_stack[complete.cases(dt_stack),]     # 211 200 pixels --> 98 967
    # Ré-écrire correctement le nom des variables (si jamais du superflu traine)
    
    # Si au moins un nom de variable de la stack n'est pas trouvé dans la liste des variables
    noms_variables = names(dt_stack)[! names(dt_stack) %in% c("x","y")]
    if(any(!noms_variables %in% table_variables$Nom)){
      noms_bug = noms_variables[!noms_variables %in% table_variables$Nom]
      cat(paste0("Bug(s) sur le(s) nom(s) : \n- ",paste(noms_bug, collapse ="\n- ")))
      # A la mano
      if(any(grepl("temps_acces", noms_bug))){
        names(dt_stack)[grepl("temps_acces", names(dt_stack))] = "temps_acces"
      }
      if(any(grepl("abondance_feuillage", noms_bug))){
        names(dt_stack)[grepl("abondance_feuillage", names(dt_stack))] = "abondance_feuillage"
      }
      if(any(grepl("NDVI", noms_bug))){
        names(dt_stack)[grepl("NDVI", names(dt_stack))] = "NDVI"
      }
      if(any(grepl("P_ETP", noms_bug))){
        names(dt_stack)[grepl("P_ETP", names(dt_stack))] = "P_ETP"
      }
      if(any(grepl("ht_physio_max", noms_bug))){
        names(dt_stack)[grepl("ht_physio_max", names(dt_stack))] = "ht_physio_max"
      }
      if(any(grepl("diffT__dif_tmean", noms_bug))){
        names(dt_stack)[grepl("diffT", names(dt_stack))] = "diffT"
      }
    }

    # Vérifier la nature des variables (si qualitative, coder en facteur)
    extr_tb = table_variables[table_variables$Nom %in% names(dt_stack), ]
    liste_nom_var_quali = extr_tb$Nom[which(extr_tb$Nature == "qualitative")]
    dt_stack[liste_nom_var_quali] <- lapply(dt_stack[liste_nom_var_quali] , factor)
    cat(paste0("\nPréparation table des variables pour dimension ",dimension,
               " pour le mois de ",i," effectuée.\n"))

    makeFAMD(dt_stack,i, dimension, palette_couleur)
  }
  
  # # Chargement d'une stack d'une dimension pour une saison
  # dim_ete <- paste0(path_dos_stack,dimension,"_ete.tif")
  # dim_hiv <- paste0(path_dos_stack,dimension,"_hiv.tif")
  # # Création des stacks
  # stack_dim_ete <- stack(dim_ete)
  # #plot(stack_dim_ete)
  # stack_dim_hiv <- stack(dim_hiv)
  # # Transformation en table
  # dt_stack_ete <- as.data.frame(data.table(as.data.frame(stack_dim_ete)))
  # dt_stack_ete <- cbind(dt_stack_ete,coordinates(stack_dim_ete))
  # dt_stack_hiv <- as.data.frame(data.table(as.data.frame(stack_dim_hiv)))
  # dt_stack_hiv <- cbind(dt_stack_hiv,coordinates(stack_dim_hiv))
  # # Retirer les NA (quand calculé sur N2000 et pas emrpise carrée) : 211 200 pixels --> 50 320
  # dt_stack_ete <- dt_stack_ete[complete.cases(dt_stack_ete),]
  # dt_stack_hiv <- dt_stack_hiv[complete.cases(dt_stack_hiv),]
  # names(dt_stack_ete) <- sub("_ete","",names(dt_stack_ete))
  # names(dt_stack_hiv) <- sub("_hiv|_hiver","",names(dt_stack_hiv))
  # Vérifier la nature des variables (si qualitative, coder en facteur)
  # extr_tb = table_variables[table_variables$Nom %in% sub("_ete","",names(dt_stack_ete)), ]
  # liste_nom_var_quali = extr_tb$Nom[which(extr_tb$Nature == "qualitative")]
  # dt_stack_ete[liste_nom_var_quali] <- lapply(dt_stack_ete[liste_nom_var_quali] , factor)
  # dt_stack_hiv[liste_nom_var_quali] <- lapply(dt_stack_hiv[liste_nom_var_quali] , factor)
  # str(dt_stack_ete)
  # str(dt_stack_hiv)
  # makeFAMD(dt_stack_ete,"ete")
  # makeFAMD(dt_stack_hiv,"hiv")
}


# fonction FAMD sur toutes les variables en meme temps
fct_FAMD_all <- function(periode=c("mai",'juin','juillet','aout','septembre'), palette_couleur=mypalette){
  
  # # # TEST
  # periode = c("mai",'juin','juillet','aout','septembre')
  # i = periode[1]
  # palette_couleur = mypalette

  # Fonctionnement par période
  for(i in periode){
    
    dirs_mois = list.dirs(paste0(path_dos_stack))[grep(i, list.dirs(paste0(path_dos_stack)))] 
    files_mois = list.files(dirs_mois,".tif", full.names = T)
    
    # # Pour dimension CA, stack avec toutes les variables ou avec 10 vars clim synthétisées en 2 axes
    # if(dimension == "CA"){
    #   liste_stack = list.files(paste0(path_dos_stack,dimension,"/",i),".tif", full.names = T)
    #   #stack_ssACP = liste_stack[grepl("sansACP",liste_stack)]
    #   stack_ACP = liste_stack[!grepl("sansACP",liste_stack)]
    #   stack_dim <- stack(stack_ACP)
    # } 
    # # Utiliser la stack avec les 10 vars clims en 2 axes ACP
    # files_mois = files_mois[-grep("sansACP",files_mois)]
    
    # # Utiliser la stack avec les 10 vars clims
    # files_mois = files_mois[-grep(paste0("_CA_",i,".tif"),files_mois)]

    stack_mois <- stack(lapply(files_mois ,stack))
    # Transformation en table
    dt_stack <- as.data.frame(data.table(as.data.frame(stack_mois)))
    dt_stack <- cbind(dt_stack,raster::coordinates(stack_mois))
    # Retirer les NA (quand calculé sur N2000 et pas emrpise carrée) : 211 200 pixels --> 50 320
    dt_stack <- dt_stack[complete.cases(dt_stack),]     # 211 200 pixels --> 98 967
    # Ré-écrire correctement le nom des variables (si jamais du superflu traine)
    
    # Si au moins un nom de variable de la stack n'est pas trouvé dans la liste des variables
    noms_variables = names(dt_stack)[! names(dt_stack) %in% c("x","y")]
    if(any(!noms_variables %in% table_variables$Nom)){
      noms_bug = noms_variables[!noms_variables %in% table_variables$Nom]
      cat(paste0("Bug(s) sur le(s) nom(s) : \n- ",paste(noms_bug, collapse ="\n- ")))
      # A la mano
      if(any(grepl("temps_acces", noms_bug))){
        names(dt_stack)[grepl("temps_acces", names(dt_stack))] = "temps_acces"
      }
      if(any(grepl("abondance_feuillage", noms_bug))){
        names(dt_stack)[grepl("abondance_feuillage", names(dt_stack))] = "abondance_feuillage"
      }
      if(any(grepl("NDVI", noms_bug))){
        names(dt_stack)[grepl("NDVI", names(dt_stack))] = "NDVI"
      }
      if(any(grepl("P_ETP", noms_bug))){
        names(dt_stack)[grepl("P_ETP", names(dt_stack))] = "P_ETP"
      }
      if(any(grepl("ht_physio_max", noms_bug))){
        names(dt_stack)[grepl("ht_physio_max", names(dt_stack))] = "ht_physio_max"
      }
      if(any(grepl("diffT__dif_tmean", noms_bug))){
        names(dt_stack)[grepl("diffT", names(dt_stack))] = " diffT"
      }
    }
    
    # Vérifier la nature des variables (si qualitative, coder en facteur)
    extr_tb = table_variables[table_variables$Nom %in% names(dt_stack), ]
    liste_nom_var_quali = extr_tb$Nom[which(extr_tb$Nature == "qualitative")]
    dt_stack[liste_nom_var_quali] <- lapply(dt_stack[liste_nom_var_quali] , factor)
    cat(paste0("\nPréparation table des variables pour le mois de ",i," effectuée.\n"))
    #str(dt_stack)
    
    # # ajouter la dimension d'appartenance
    # merge(dt_stack, table_variables)
    
    makeFAMD(dt_stack,i, dimension = "toutes", palette_couleur)
    
  }
}

# Fonction qui vérifie que le raster ait le bon CRS et extent, et le modifie si besoin
AjustExtCRS <- function(path.raster.to.check, path.raster.ref=chemin_mnt){
  
  # # TEST
  # path.raster.to.check = r_hiv[2]
  # raster.ref = MNT
  
  raster.to.check <- raster(path.raster.to.check)
  raster.ref <- raster(path.raster.ref)
  
  ext.to.check <- extent(raster.to.check)
  bon.extent <- extent( raster.ref)
  
  sameCRS <- compareCRS(raster.to.check,EPSG_2154)
  sameExtent <- (ext.to.check == bon.extent)
  
  if(any(!sameCRS,!sameExtent)) {
    raster.to.check <- projectRaster(raster.to.check, raster.ref)
    writeRaster(raster.to.check, path.raster.to.check, overwrite=TRUE)
    cat("\nRaster ", names(raster.to.check)," a été modifié et sauvegarde.")
  }
  
}

# Fonction pour faire une ACP sur les axes des FAMD de chaque dimensions
ACP_axesFAMD <- function(mois, palette_couleur=mypalette){
  
  # # # TEST
  # mois = liste.mois[1]
  # palette_couleur = mypalette
  
  cat("\n Mois de ",mois," en cours.")
  
  liste_axes = list.files(paste0(output_path,"/ACP/"),".tif", recursive = T, full.names = T)
  # Virer les axes issues de la FAMD globale
  liste_axes = liste_axes[!grepl("toutes|ACP_FAMD|TEST",liste_axes)]
  # Garder le mois en cours
  liste_axes = liste_axes[grepl(mois,liste_axes)]
  
  # S'assurer de la conformité des variables
  lapply(liste_axes, AjustExtCRS)
  # Stack les axes
  stack_mois <- stack(liste_axes)
  
  # Transformation en table
  dt_stack <- as.data.frame(data.table(as.data.frame(stack_mois)))
  dt_stack <- cbind(dt_stack,raster::coordinates(stack_mois))
  # Retirer les NA (quand calculé sur N2000 et pas emprise carrée) : 211 200 pixels --> 50 320
  dt_stack <- dt_stack[complete.cases(dt_stack),]     # 211 200 pixels --> 98 967
    
  # # ajouter la dimension d'appartenance
  # merge(dt_stack, table_variables)
    
  makeFAMD(dt_stack, mois, dimension = "ACP_FAMD", palette_couleur)
  
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
path_dos_stack <- paste0(output_path,"/stack_dim/")
# Dossier des variables spatiales & chemins des fichiers
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"
# MNT 25m CALé sur le grille de REFERENCE
chemin_mnt <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_25m_belledonne_cale.tif")

#### Tables ####
path_table_variables <- paste0(input_path,"/liste_variables.csv")
path_table_points_multiusage <- "C:/Users/perle.charlot/Documents/PhD/DATA/R_git/CaractUsages/output/multiusage/table_points_multiusage.csv"
path_coords_usages <- "C:/Users/perle.charlot/Documents/PhD/DATA/R_git/CaractUsages/output/par_periode/"

#### Autre ####
# Liste dimensions
liste.dim=  c("CA","B","PV","CS","D","I")
# Table correspondance entre dimension et couleur à utiliser dans les graphs
corresp_col = data.frame(dim_name = c(liste.dim,"toutes"),
                         colour_dim = c("dodgerblue","darkgoldenrod1","darkgreen",
                                        "brown","blueviolet","darkgray","antiquewhite"))
# Liste mois étudiés
liste.mois = c("mai","juin","juillet","aout","septembre")

### Programme -------------------------------------
table_variables <- fread(path_table_variables)
col_dim = merge(table_variables, corresp_col,by.x="Dimension", by.y="dim_name")
mypalette <- setNames(col_dim$colour_dim, 
                      col_dim$Nom)

##### FAMD/ACP par dimension par mois ####
lapply(liste.dim, fct_FAMD)

# sapply(liste.dim, fct_FAMD,arg_ACP="sans")
#sapply(liste.dim, fct_FAMD,arg_ACP="avec")

# # Relancer FAMD sur une dimension spécifique
fct_FAMD("CS")

##### FAMD sur toutes les dimensions simultanément, par mois ####
fct_FAMD_all()

##### ACP sur axes FAMD des dimensions, par mois ####
lapply(liste.mois, ACP_axesFAMD)


##### t-SNE par dimension par saison ####
library(Rtsne)
# stack_matrix <- as.matrix(dt_stack[,1:21])
# Set a seed if you want reproducible results
set.seed(42)
tsne_out <- Rtsne(dt_stack[,1:21],partial_pca=T) # Run TSNE
# Show the objects in the 2D tsne representation
plot(tsne_out$Y, asp=0,alpha=0.1)
DATA = as.data.frame(tsne_out$Y)
ggplot(DATA, aes(x=V1,y=V2))+
  geom_point(alpha=0.05)

##### Niche de chaque usage ####

# NicheUsage <- function(dimension, 
#                        periode=c("mai",'juin','juillet','aout','septembre'),
#                        ACPclimat = "oui"){
#   # Test
#   dimension = liste.dim[6]
#   periode=c("mai",'juin','juillet','aout','septembre')
#   ACPclimat = "oui"
#   #i= periode[3]
#   
#   corresp = data.frame(numero_saison=c("05","06","07","08","09"),
#                        periode = c("mai",'juin','juillet','aout','septembre'))
#    
#   cat("\nDimension en cours : ", dimension)
#   
#   for(i in periode){
#     cat("\nMois en cours : ", i)
#     num_saison = corresp$numero_saison[which(corresp$periode == i)]
#     # Condition si utilisation d'une ACP sur les variables climatiques de la dimension CA
#     if(dimension == "CA"){
#       if(ACPclimat == "non"){
#         path_rastFAMD_mois <- list.files(paste0(output_path,"/ACP/",dimension,"/sans_ACP_clim/",num_saison,i),
#                                          recursive = T, full.names = T, pattern = ".tif")
#       }else{
#         path_rastFAMD_mois <- list.files(paste0(output_path,"/ACP/",dimension,"/avec_ACP_clim/",num_saison,i),
#                                          recursive = T, full.names = T, pattern = ".tif")}
#     }else{
#       path_rastFAMD_mois <- list.files(paste0(output_path,"/ACP/",dimension,"/",num_saison,i),
#                                        recursive = T, full.names = T, pattern = ".tif")
#     }
#     rast_FAMD_mois <- stack(path_rastFAMD_mois)
#     if(length(names(rast_FAMD_mois))>2){names(rast_FAMD_mois) <- c("axe1","axe2","axe3")
#     }else{names(rast_FAMD_mois) <- c("axe1","axe2")}
#     tbl_FAMD_mois <- as.data.frame(data.table(as.data.frame(rast_FAMD_mois)))
#     tbl_FAMD_mois <- cbind(tbl_FAMD_mois,coordinates(rast_FAMD_mois))
#     tbl_FAMD_mois <- tbl_FAMD_mois[complete.cases(tbl_FAMD_mois),]
#     path_coordUs_mois <- list.files(paste0(path_coords_usages,"/",i),recursive = T, full.names = T, pattern = "coord")
#     tbl_coordUs_mois <- fread(path_coordUs_mois,drop="V1")
#     corresp_nom_Us = data.frame(
#       nom_long = c("parade_TLY","nidification_TLY","paturage","couchade","randonnee_pedestre","VTT"),
#       nom_court = c("Lk",'Ni','Pa','Co','Ra','Vt'),
#       couleur = c("darkgreen","mediumseagreen","lightgoldenrod","lightsalmon","plum1","mediumpurple4"))
#     paletteUsages <- setNames(corresp_nom_Us$couleur, 
#                               corresp_nom_Us$nom_court)
#     index_fin = grep("x",names(tbl_coordUs_mois)) -1
#     tbl_coordUs_mois = tbl_coordUs_mois %>%
#       data.frame(.) %>%
#       rename_at(1:index_fin, ~ setNames(as.character(corresp_nom_Us$nom_court), corresp_nom_Us$nom_long)[.])
#     # Merge usages et axes FAMD
#     tbl_FAMD_Us <- merge(tbl_FAMD_mois, tbl_coordUs_mois, by=c("x","y"))
#     
#     # Mise en forme table
#     # pb quand pas de 3e axe FAMD
#     if(dimension=="D"){index_debut = grep("axe2",names(tbl_FAMD_Us))+1
#     } else{index_debut = grep("axe3",names(tbl_FAMD_Us))+1}
#     
#     # rajouter nom dimension avec axe
#     index_nom_axe = grep("axe",names(tbl_FAMD_Us))
#     names(tbl_FAMD_Us)[index_nom_axe] <- paste0(names(tbl_FAMD_Us)[index_nom_axe],"_",dimension)
#     
#     head(tbl_FAMD_Us)
#     # exporter table avec usages (0/1), coordonnées et axes FAMD 
#     # de la dim dimension + mois i
#     if(dimension == "CA"){
#       if(ACPclimat == "non"){
#         nom_path_output = paste0(output_path,"/ACP/",dimension,"/sans_ACP_clim/",
#                                  num_saison,i,"/axesFAMD_",dimension,"_usages_",i,".csv")} else{
#         nom_path_output = paste0(output_path,"/ACP/",dimension,"/avec_ACP_clim/",
#                                  num_saison,i,"/axesFAMD_",dimension,"_usages_",i,".csv")}
#       }else{
#       nom_path_output = paste0(output_path,"/ACP/",dimension,"/",
#              num_saison,i,"/axesFAMD_",dimension,"_usages_",i,".csv")
#     }
#     
#     write.csv(tbl_FAMD_Us, nom_path_output)
#     
#     # tbl_FAMD_Us2 = tbl_FAMD_Us  %>%
#     #   mutate(across(index_debut:Vt, ~case_when(. == 1 ~ cur_column()), .names = 'new_{col}')) %>%
#     #   unite(Usage, starts_with('new'), na.rm = TRUE, sep = '_')
#     # # Somme des usages
#     # tbl_FAMD_Us2 = tbl_FAMD_Us2 %>%
#     #   replace(is.na(.), 0) %>%
#     #   mutate(sum = rowSums(across(index_debut:Vt)))
#     #
#     # # Niche de toutes les combinaisons
#     # p0 = tbl_FAMD_Us2 %>% subset(sum > 0) %>%
#     #   ggplot(aes(axe1, axe2, col=as.factor(Usage))) +
#     #   geom_point(alpha=0.5)+
#     #   labs(x="FAMD_Axe1",y="FAMD_Axe2",col="Usage",
#     #        title= paste0("Niches d'usage - ",i," - ",dimension)) +
#     #   facet_wrap(~ reorder(Usage,sum))+
#     #   scale_color_manual(values = paletteUsages)+
#     #   theme(legend.position = "none")
# 
#     # tbl_FAMD_Us2 %>% subset(sum == 1) %>%
#     #   ggplot(aes(axe1, axe2, col=as.factor(Usage))) +
#     #   geom_point(alpha=0.5)+
#     #   labs(x="FAMD_Axe1",y="FAMD_Axe2",col="Usage",
#     #        title= paste0("Niches d'usage - ",i," - ",dimension)) +
#     #   facet_wrap(~ Usage)+
#     #   scale_color_manual(values = paletteUsages)
# 
# #
# #     # Niche d'un usage, avec toutes les combinaison contenant cet usage
# #     liste_usages_presents = names(tbl_FAMD_Us2)[names(tbl_FAMD_Us2) %in% corresp_nom_Us$nom_court]
# #     for(u in liste_usages_presents){
# #       cat("\nPlots de l'usage ",u)
# #
# #       # valeurs le long axes 1 et 2, des pixels contenant usage X,
# #       # facet_wrap() par la combinaison d'usages des pixels (X, X-Y, X-Y-Z, etc)
# #       # couleur = somme usages
# #       p1 = tbl_FAMD_Us2 %>% filter(grepl(u, Usage)) %>%
# #         ggplot(aes(axe1, axe2, col=as.factor(sum))) +
# #         geom_point(alpha=0.5)+
# #         labs(x="FAMD_Axe1",y="FAMD_Axe2",col="Somme\nd'usages",
# #              title= paste0("Niches d'usage ",u," - ",i," - ",dimension)) +
# #         facet_wrap(~ reorder(Usage,sum))
# #       # valeurs le long axes 1 et 2, des pixels contenant usage X,
# #       # facet_wrap() par somme d'usages des pixels (1 : usage seul, 2 usages, 3,usages, etc)
# #       # couleur = combinaison usages
# #       p2 = tbl_FAMD_Us2 %>% filter(grepl(u, Usage)) %>%
# #         ggplot(aes(axe1, axe2, col=Usage)) +
# #         geom_point(alpha=0.5)+
# #         labs(x="FAMD_Axe1",y="FAMD_Axe2",col="Usage",
# #              title= paste0("Niches d'usage - ",i," - ",dimension)) +
# #         facet_wrap(~sum)
# #       # Couleurs : usage seul VS usage couplé avec d'autres usages
# #       p3 = tbl_FAMD_Us2 %>% filter(grepl(u, Usage)) %>%
# #         ggplot(aes(axe1, axe2, col=ifelse(sum > 1, "plusieurs","seul"))) +
# #         geom_point(alpha=0.5)+
# #         labs(x="FAMD_Axe1",y="FAMD_Axe2",col="Usage",
# #              title= paste0("Niches d'usage - ",i," - ",dimension))
# #
# #       # Sauvegarder les plots
# #       if(!dir.exists(paste0(output_path,"/ACP/",dimension,"/",num_saison,i,"/plot_niche_usage/",u))){
# #         dir.create(paste0(output_path,"/ACP/",dimension,"/",num_saison,i,"/plot_niche_usage/",u),recursive=TRUE)}
# #
# #       SaveGraph <- function(plot, nom,
# #                             path_save){
# #         nom=as.character(nom)
# #         png(file=paste0(output_path,"/ACP/",dimension,"/",num_saison,i,"/plot_niche_usage/",u,"/graph_",nom,"_",dimension,"_",i,".png"),
# #             width=1000, height=800)
# #         print(plot)
# #         dev.off()
# #       }
# #       map2(list(p1,p2,p3),
# #            c("p1","p2","p3"),
# #            SaveGraph)
# #     }
# #
# #     # Niches de plusieurs usages
# #     # tous les usages seuls
# #     p4 = tbl_FAMD_Us2 %>% subset(sum == 1) %>%
# #       ggplot(aes(axe1, axe2, col=as.factor(Usage))) +
# #       geom_point(alpha=0.5)+
# #       labs(x="FAMD_Axe1",y="FAMD_Axe2",col="Usage",
# #            title= paste0("Niches d'usage - ",i," - ",dimension))+
# #       scale_color_manual(values = paletteUsages)
# #     if(dimension != "D"){
# #       p5 = tbl_FAMD_Us2 %>% subset(sum == 1) %>%
# #         ggplot(aes(axe1, axe3, col=as.factor(Usage))) +
# #         geom_point(alpha=0.5)+
# #         labs(x="FAMD_Axe1",y="FAMD_Axe3",col="Usage",
# #              title= paste0("Niches d'usage - ",i," - ",dimension))+
# #         scale_color_manual(values = paletteUsages)
# #       p6 = tbl_FAMD_Us2 %>% subset(sum == 1) %>%
# #         ggplot(aes(axe2, axe3, col=as.factor(Usage))) +
# #         geom_point(alpha=0.5)+
# #         labs(x="FAMD_Axe2",y="FAMD_Axe3",col="Usage",
# #              title= paste0("Niches d'usage - ",i," - ",dimension))+
# #         scale_color_manual(values = paletteUsages)
# #     }
# #
# #
# #     # Sauvegarder les plots
# #     u = "pls_us"
# #     if(!dir.exists(paste0(output_path,"/ACP/",dimension,"/",num_saison,i,"/plot_niche_usage/",u))){
# #       dir.create(paste0(output_path,"/ACP/",dimension,"/",num_saison,i,"/plot_niche_usage/",u),recursive=TRUE)}
# #     if(dimension != "D"){
# #     map2(list(p0,p4,p5,p6),
# #          c("p0","p4","p5","p6"),
# #          SaveGraph)
# #     } else{    map2(list(p0,p4),
# #                     c("p0","p4"),
# #                     SaveGraph)}
# #
#   }
# }
# 
# lapply(liste.dim, NicheUsage)


# TODO:
# - réfléchir à comment définir/visualier la niche d'usage (sum ==1 réduit la niche mais gagne en visibilité)
# - sortir graph, pour un usage (ex Pa) facet_wrap(~ Pa et tous les Pa_ dérivés)
# -> sort une niche d'usage, tout en décomposant les superpositions
# - enregistrer les graphs selon même principe FAMD



# tbl_FAMD_MU %>% subset(sumUsage > 0) %>%
#   ggplot(aes(axe1, axe2, col=as.factor(sumUsage))) +
#   geom_point(alpha=0.5)+
#   labs(col="Nombre d'usages\nen simultané",
#        x="FAMD_Axe1",y="FAMD_Axe2",
#        title= paste0("Multiusage en dimensions réduites - ",
#                      basename(dirname(path_table_analyse))))+
#   theme(legend.position = "none")

##### Niche du multiusage ####

# # Sur toutes les variables
# liste_tableFAMD_path = list.files(paste0(output_path,"/ACP/toutes/"), recursive = T, full.names = T,'.csv')
# #liste_tableFAMD <- lapply(liste_tableFAMD_path, fread)
# table_points_multiusage = fread(path_table_points_multiusage, drop="V1")
# tbl_pts_MU = table_points_multiusage %>% pivot_longer(cols=c("sumUsage_04avril",
#                                                 "sumUsage_05mai",
#                                                 "sumUsage_06juin",
#                                                 "sumUsage_07juillet",
#                                                 "sumUsage_08aout",  
#                                                 "sumUsage_09septembre"),
#                                          values_to = "sumUsage",
#                                          names_to = "mois")
# 
# function(path_table_analyse){
#   
#   # TEST
#   path_table_analyse = liste_tableFAMD_path[2]
#   
# 
#   tbl_FAMD <- as.data.frame(fread(path_table_analyse, drop='V1'))
#   tbl_pts_MU <- tbl_pts_MU %>% 
#     subset(mois == paste0("sumUsage_", basename(dirname(path_table_analyse))))
#   tbl_FAMD_MU <- merge(tbl_FAMD, tbl_pts_MU, by=c('x','y'))
# 
#   # Graphiques
#   p1 = tbl_FAMD_MU %>% subset(sumUsage > 0) %>%
#     ggplot(aes(axe1, axe2, col=as.factor(sumUsage))) +
#     geom_point(alpha=0.5)+
#     labs(col="Nombre d'usages\nen simultané",
#          x="FAMD_Axe1",y="FAMD_Axe2",
#          title= paste0("Multiusage en dimensions réduites - ",
#                        basename(dirname(path_table_analyse))))+
#     theme(legend.position = "none")
#   p2 = tbl_FAMD_MU %>% subset(sumUsage > 0) %>%
#     ggplot(aes(axe1, axe3, col=as.factor(sumUsage))) +
#     geom_point(alpha=0.5)+
#     labs(col="Nombre d'usages\nen simultané",
#          x="FAMD_Axe1",y="FAMD_Axe3",
#          title= paste0("Multiusage en dimensions réduites - ",
#                        basename(dirname(path_table_analyse))))
#   p3 = tbl_FAMD_MU %>% subset(sumUsage > 0) %>%
#     ggplot(aes(axe2, axe3, col=as.factor(sumUsage))) +
#     geom_point(alpha=0.5)+
#     labs(col="Nombre d'usages\nen simultané",
#          x="FAMD_Axe2",y="FAMD_Axe3",
#          title= paste0("Multiusage en dimensions réduites - ",
#                        basename(dirname(path_table_analyse))))
#   
#   p4 = p1 / p2 / p3
#   p4
#   
#   
#   p5 = tbl_FAMD_MU %>% subset(sumUsage > -1) %>%
#     ggplot(aes(axe1, axe2, col=as.factor(sumUsage))) +
#     geom_point(alpha=0.5)+
#     facet_wrap(~sumUsage)+
#     labs(col="Nombre d'usages\nen simultané",
#          x="FAMD_Axe1",y="FAMD_Axe2",
#          title= paste0("Multiusage en dimensions réduites - ",
#                        basename(dirname(path_table_analyse))))
#   
# 
#   
# }
# # TODO : à tester : avoir sur un graph les variables avec les flèches ACP +
# # en couleur les points des individus multiusage
# head(tbl_FAMD_MU)
# c(axe1,axe2,axe3,x,y,mois,sumUsage)
# res.famd <- PCA(subset(tbl_FAMD_MU,select=-c(axe1,axe2,axe3,x,y,mois,sumUsage)), 
#                  graph = FALSE, ncp = 3)
# fviz_pca_var(res.famd, col.var = "cos2",
#                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              select.var = list(cos2 = 0.5),
#                             repel = T)
# fviz_pca_var(res.famd, col.var = "cos2",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              select.var = list(contrib = 10),
#              repel = T)
# # TROP long ..
# fviz_pca_ind(res.famd, label="none", habillage=tbl_FAMD_MU$sumUsage,
#              addEllipses=TRUE, ellipse.level=0.95,
#              select.ind = list(cos = 5))
# 




##### Autre ####
# # Tips Natéo pour visualiser ACP
# library(Factoshiny)
# Factoshiny(dt_stack_ete)

# inspiration rpz d'un espèce écologique et individus sur ACP
# https://iboulangeat.github.io/pdfs/Boulangeat2012_EcolLet_published.pdf

# TODO : dudi.mix par rapport à FAMD ???
# https://www.rdocumentation.org/packages/ade4/versions/1.7-19/topics/dudi.mix
