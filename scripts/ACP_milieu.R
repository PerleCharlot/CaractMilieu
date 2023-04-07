### Titre -------------------------------------
# Nom : ACP du milieu
# Auteure : Perle Charlot
# Date de création : 04-06-2022
# Dates de modification : 16-02-2023

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
library(fastDummies)
### Fonctions -------------------------------------

# Permet de nettoyer les noms de colonnes (= variables) d'un dt
cleanVarName <- function(liste_nom_var_ref, dt_to_clean){
  # # TEST
  # liste_nom_var_ref = table_variables$Nom
  # dt_to_clean = test_dt
  
  noms_variables = names(dt_to_clean)[! names(dt_to_clean) %in% c("x","y")]
  if(any(!noms_variables %in% liste_nom_var_ref)){
    noms_bug = noms_variables[!noms_variables %in% liste_nom_var_ref]
    
    for(i in liste_nom_var_ref){
      if(any(grepl(i, noms_bug))){
        names(dt_to_clean)[grepl(i, names(dt_to_clean))] = i
      }}
    }
  return(dt_to_clean)
}

# Fonction qui calcule une ACP, pour une dimension, pour une saison
makePCA <- function(table_donnees, saison, dimension, palette_couleur, ponderation,predict = FALSE){
  # # # TEST
  # table_donnees = dt_stacks
  # saison = "summer"
  # dimension = dimension
  # palette_couleur = palette_couleur
  # ponderation = ponderation
  
  # table_donnees = dt_stacks
  # saison = "summer"
  # #dimension = dimension
  # #palette_couleur = palette_couleur
  # #ponderation = FALSE
  # predict = FALSE
  

  corresp = data.frame(numero_saison=c("05","06","07","08","09"),
                       periode = c("mai",'juin','juillet','aout','septembre'))
  num_saison = corresp$numero_saison[which(corresp$periode == saison)]
  corresp_col = data.frame(dim_name = c("CA","B","PV","CS","D","I",
                                        "ACP_sans_ponderation","ACP_avec_ponderation",
                                        "ACP_ACP"),
                       colour_dim = c("dodgerblue","darkgoldenrod1","darkgreen",
                                      "brown","blueviolet","darkgray",
                                      "antiquewhite","antiquewhite",
                                      "ivory3"))
  
  dim_col = corresp_col$colour_dim[which(corresp_col$dim_name == dimension)]

  
  if(ponderation){pond_att = "avec"}else{pond_att= "sans"}
  
  chemin_output_ACP = paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/",pond_att,"_ponderation/")
  chemin_output_ACP_plot_rmd = paste0(chemin_output_ACP,"/plot_rmd")
  
  # Création et Sauvegarde des graphiques
  if(predict){
    if(!dir.exists(paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"_fromPCAjune/plot_rmd")))
    { dir.create(paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"_fromPCAjune/plot_rmd"),recursive = T)}
  } else{
    if(!dir.exists(chemin_output_ACP_plot_rmd))
    { dir.create(chemin_output_ACP_plot_rmd,recursive = T)}
  }
  
  # Pour dimension = ACP_ACP (qui maintenant est une ACP d'ACP ...)
  if(dimension == "ACP_ACP"){
    n=3
    corresp_axes = data.frame(axe_AFDM =unlist(lapply(liste.dim,function(x)paste0("axe",1:n,"_",x))),
                              dim_name=unlist(lapply(liste.dim,function(x)rep(x,n)))
                                         )
    a = merge(corresp_axes, corresp_col, by="dim_name")
    palette_couleur <- setNames(a$colour_dim,
                          a$axe_AFDM)
  }

  # ### UTILITE DE CE CHUNK ?? pas vu pour dimension == "toutes"
  # # Recoder les variables factorielles (pour ne pas qu'il y ait de doublons)
  # col_quali <- unlist(lapply(tbl_data, is.factor))
  # index_col_quali = which(col_quali == TRUE)
  # for(index_colonne in index_col_quali){
  #   # # TEST
  #   # index_colonne = index_col_quali[1]
  #   cat(paste0("\nVariable ",names(tbl_data)[index_colonne], " en traitement."))
  #   # recodage
  #   recodage = paste0(levels(tbl_data[, index_colonne]),"=",
  #                     names(tbl_data)[index_colonne],"_",levels(tbl_data[, index_colonne]),sep=";",collapse="")
  #   # mutate avec var recodée + retirer la colonne initiale
  #   tbl_data  = tbl_data %>%
  #     mutate(X = rec(tbl_data[, index_colonne], rec =  recodage))
  #   # changer name col var recodée
  #   names(tbl_data)[which(names(tbl_data)== "X")] <- paste0(names(tbl_data)[index_colonne],"_rec")
  #     }
  # # retirer les colonnes initiales
  # tbl_data  = tbl_data %>%
  #   select(-index_col_quali)
  # new_names = substr(names(tbl_data)[grep("_rec",names(tbl_data))], 1,nchar(names(tbl_data)[grep("_rec",names(tbl_data))])-4)
  # names(tbl_data)[grep("_rec",names(tbl_data))] <- new_names
  # ### UTILITE DE CE CHUNK ?? pas vu pour dimension == "toutes"
  
  tbl_data = table_donnees
  
  # Identifier index vars à rendre booléennes
  extr_tb = table_variables[table_variables$Nom %in% names(tbl_data), ]
  names_var_quali =  extr_tb$Nom[which(extr_tb$Nature == "qualitative")] 
  
  # Check for dummies variables
  t = try(dummy_cols(tbl_data, remove_selected_columns=T), silent = TRUE)
  if(inherits(t, "try-error")) {
    tbl_data2 <- tbl_data
    df_w = table_variables[table_variables$Nom %in% names(tbl_data2)]
  } else {   
    # Transformer factoriel (ordi et quali) en boolean (0/1)
    tbl_data2 <- dummy_cols(tbl_data, 
                            select_columns=names_var_quali,
                            remove_selected_columns=T)
    
    # retirer _ dans noms dummies
    f <- function(x){
      a <- unlist(strsplit(x,"_"))
      if(length(a)>2){
        new.name <- paste0(paste0(a[1:2],collapse="_"),
                           a[3],collapse="") 
      }else{
        new.name <- paste0(a,collapse="") 
      }
      return(new.name)
    }
    index_dummies <- grep(pattern=paste0(names_var_quali,collapse = "|"), names(tbl_data2))
    names(tbl_data2)[index_dummies] <- unlist(lapply(names(tbl_data2)[index_dummies],f))

    # Matrice de pondération des variables dans ACP par dimension
    df_dum <- table_variable_dummies[table_variable_dummies$Nom %in% names(tbl_data2)]
    df_quanti <- table_variables[table_variables$Nom %in% names(tbl_data2)]
    df_w <- rbind(df_quanti, df_dum, fill=T)
  }
  
  # Dans tous les cas, les variables dummies sont pondérées
  # Mais on peut ensuite pondérer les variables par dimension 
  # (afin de donner un poids égale à chaque dimension)
  
  if(ponderation){
    cat("Analyse factorielle en composantes principales - avec pondération par dimension. \n")
    df_w$weight = (1/df_w$b) * (1/df_w$n) * (1/df_w$D)
  } else {
    cat("Analyse factorielle en composantes principales - sans pondération par dimension. \n")
    if(dimension == "ACP_sans_ponderation"){
      df_w$weight  =(1/df_w$b)* (1/length(tbl_data))
    } else{df_w$weight = (1/df_w$b)* (1/df_w$n) # fonctionne pour ACP sur une dimension
    }
  }

  #Ordonner les poids
  W = df_w$weight[match(names(tbl_data2), df_w$Nom)]
  #sum(na.omit(W))
  W[is.na(W)] <- 0
  
  # # TODO : il faut que cette condition puisse s'appliquer ou non
  # # Dans ACP_ACP, les poids sont nulles -> les passer à 1
  # W[] <- 1
  
  # Plutot que les retirer, les considérer en vars supplémentaires
  idx_vars_quali_sup = grep("^month$", names(tbl_data2))
  if(length(idx_vars_quali_sup) > 0){
    idx_vars_quali_sup = grep("^month$", names(tbl_data2))
  } else{idx_vars_quali_sup = NULL}
  idx_vars_quanti_sup = c(grep("^x$", names(tbl_data2)),grep("^y$", names(tbl_data2)))
  
  # PCA
  if(predict){
    load(paste0(output_path,"/ACP/",dimension,"/06juin/PCA.rdata"))
    cat("ACP de juin chargée.\n")
    tbl_data3 <- tbl_data2
    a = predict.PCA(object = res.pca, newdata =  tbl_data3)
    PCA_tbl <- as.data.frame(a$coord)
    names(PCA_tbl) = paste0("axe",seq(1:length(colnames(PCA_tbl))))
    table_all <- cbind(table_donnees,PCA_tbl)
    write.csv(table_all, paste0(output_path,"/ACP/",dimension,"/",num_saison,saison,"/tblPCA_",dimension,"_",saison,".csv"))
  } else{
    tbl_data3 <- tbl_data2 %>% dplyr::select(-idx_vars_quali_sup,-idx_vars_quanti_sup)
    W = W[-c(idx_vars_quali_sup,idx_vars_quanti_sup)]
    
    res.pca <- PCA(tbl_data3,
                   ncp=7,
                   col.w = W, # avec ou sans pondération (par dimension)
                   graph = FALSE,
                   scale.unit = TRUE, # data are scaled to unit variance
                   #quali.sup =  idx_vars_quali_sup, 
                   #quanti.sup = idx_vars_quanti_sup
    ) 
    cat("ACP calculée.\n")
    # mettre des variables supplémentaires fait énormément baisser
    # la variance expliquée 
  

  # % variance expliquée par axe
  liste_variance_expl = round(res.pca$eig[,2],1) # quand il y a peu de var
  n_ncp = length(liste_variance_expl)
  if(n_ncp > 10){
    n_ncp = 7
  }
  if(n_ncp > 3){
    graph_var_expl <- fviz_eig(res.pca,
                               choice=c("variance"),
                               geom=c("bar"),
                               xlab="Axe",
                               ncp=n_ncp,
                               barfill= dim_col,
                               ylim=c(0,100),
                               addlabels=F,main=' ',
                               font.x=c(24,"plain","black"),
                               font.y=c(28,"plain","black"),
                               font.tickslab = c(24,"plain","black")) +
      annotate(geom="text", 
                x=n_ncp/2, y=80, size = 12,
                label=paste0("Somme variance expliquée\npar 3 premiers axes : ",sum(liste_variance_expl[1:3]),"%"),
                color="black"
                )    + geom_text(size = 12,
                   label = liste_variance_expl[1:n_ncp]) 
    
  } else {
    graph_var_expl <- fviz_eig(res.pca,
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
    )  + geom_text(size = 15,label = liste_variance_expl[1:2])+ annotate(geom="text", x=1.5, y=80, size = 10,
                   label=paste0("Somme variance expliquée\npar 2 premiers axes : ",sum(liste_variance_expl[1:2]),"%"),
                   color="black")
  }

  # Graphique des variables
  options(ggrepel.max.overlaps = Inf)
  # graph_var <- fviz_famd_var(res.famd, "var", col.var = "cos2",geom=c("arrow","text"),
  #                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  #                            font.x=c(24,"plain","black"),
  #                            font.y=c(24,"plain","black"),
  #                            font.tickslab = c(24,"plain","black"),
  #                            labelsize=8,
  #                            repel = TRUE)
  
  # Si on a plus de 10 variables dans l'ACP, on ne montre que les 12 qui contribuent le +
  # Si on en a moins que 10, on en mettre le nombre total
  nb_axes_grap = ifelse(n_ncp == 7,12,n_ncp)
  titre_graph = ifelse(nb_axes_grap == 12,"Cercle de corrélation (12 variables contribuant le plus)","Cercle de corrélation" )
  graph_var_1_2 <- fviz_pca_var(res.pca, 
                            title=titre_graph,
                            axes=c(1,2),
               col.var = "cos2",
               geom=c("arrow","text"),
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               font.x=c(24,"plain","black"),
               font.y=c(24,"plain","black"),
               font.tickslab = c(24,"plain","black"),
               labelsize=8,
               select.var = list(contrib= nb_axes_grap),
               habillage="none",
               repel = TRUE)
  if(n_ncp > 3){
    graph_var_2_3 <- fviz_pca_var(res.pca, 
                                  title=titre_graph,
                                  axes=c(2,3),
                                  col.var = "cos2",
                                  geom=c("arrow","text"),
                                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                  font.x=c(24,"plain","black"),
                                  font.y=c(24,"plain","black"),
                                  font.tickslab = c(24,"plain","black"),
                                  labelsize=8,
                                  select.var = list(contrib= nb_axes_grap),
                                  habillage="none",
                                  repel = TRUE)
    
    png(file=paste0(chemin_output_ACP_plot_rmd,"/cercle_correlation_axes2_3_",dimension,"_",saison,".png"),
        width=1000, height=800)
    print(graph_var_2_3)
    dev.off()
  }
  # Colorer les barres par dimension
  nb_vars_grap = ifelse(n_ncp == 7,15,n_ncp)
  titre_graph_eboul = ifelse(nb_vars_grap == 15,"Eboulis des valeurs propres (15 variables contribuant le plus)",
                       "Eboulis des valeurs propres " )
  graph_contrib_var_axe1 <- fviz_contrib(res.pca, "var",
               axes = 1,
               title=paste0(titre_graph_eboul," - Axe 1"),
               top=nb_vars_grap,
               fill = "name",
               color = "black",
               font.y=c(24,"plain","black"),
               font.tickslab = c(28,"plain","black"))+
    scale_fill_manual(values = palette_couleur)+
    theme(legend.position = "none")
  
  if(n_ncp > 3){
    graph_contrib_var_axe2 <- fviz_contrib(res.pca,
                                           choice= "var",
                                           axes = 2,
                                           title=paste0(titre_graph_eboul," - Axe 2"),
                                           top=nb_vars_grap,
                                           fill = "name",
                                           color = "black",
                                           font.y=c(24,"plain","black"),
                                           font.tickslab = c(28,"plain","black"))+
      scale_fill_manual(values = palette_couleur)+
      theme(legend.position = "none")
    
    graph_contrib_var_axe3 <- fviz_contrib(res.pca, "var",
                                           axes = 3,
                                           title=paste0(titre_graph_eboul," - Axe 3"),
                                           top=nb_vars_grap,
                                           fill = "name",
                                           color = "black",
                                           font.y=c(24,"plain","black"),
                                           font.tickslab = c(28,"plain","black"))+
      scale_fill_manual(values = palette_couleur)+
      theme(legend.position = "none")

  }

  # Graphique des individus
  graph_ind <- fviz_pca_ind(res.pca,  alpha.ind = 0.05,
                              title=' ',
                              geom=c("point"),
                              font.x=c(28,"plain","black"),
                              font.y=c(28,"plain","black"),
                              font.tickslab = c(28,"plain","black"),
                            # habillage = ifelse(saison == "summer",c(tbl_data2$month),NULL),
                            #habillage = ifelse(length(idx_vars_quali_sup) == 0, "none",idx_vars_quali_sup),
                              repel = FALSE)

  # Sauvegarde des graphiques en format images
  p1 = graph_var_expl
  p2 = graph_contrib_var_axe1

  
  png(file=paste0(chemin_output_ACP_plot_rmd,"/eboulis_variance_",dimension,"_",saison,".png"),
      width=1000, height=800)
  plot(p1)
  dev.off()
  png(file=paste0(chemin_output_ACP_plot_rmd,"/contrib_axe1_",dimension,"_",saison,".png"),
      width=1000, height=800)
  plot(p2)
  dev.off()
  
  if(n_ncp>3){
    p3 = graph_contrib_var_axe2
    png(file=paste0(chemin_output_ACP_plot_rmd,"/contrib_axe2_",dimension,"_",saison,".png"),
        width=1000, height=800)
    plot(p3)
    dev.off()
    
    p3bis = graph_contrib_var_axe3
    png(file=paste0(chemin_output_ACP_plot_rmd,"/contrib_axe3_",dimension,"_",saison,".png"),
        width=1000, height=800)
    plot(p3bis)
    dev.off()
  }

  t = try(p1 / (p2 | p3), silent = TRUE)
  if(inherits(t, "try-error")) {
    p4 = p1 / (p2)
  } else { p4 = p1 / (p2 | p3)}
  
  png(file=paste0(chemin_output_ACP,"/contrib_",dimension,"_",saison,".png"),
      width=1400, height=800)
  plot(p4)
  dev.off()
  
  p5 = graph_var_1_2
  p6 = graph_ind
  p7 = p5 | p6
  
  png(file=paste0(chemin_output_ACP_plot_rmd,"/cercle_correlation_axe1_2_",dimension,"_",saison,".png"),
      width=1000, height=800)
  plot(p5)
  dev.off()
  cat("graph cercle corrélation axes 1 et 2 calculé.\n")
  png(file=paste0(chemin_output_ACP_plot_rmd,"/graph_individus_",dimension,"_",saison,".png"),
      width=1000, height=800)
  plot(p6)
  dev.off()
  png(file=paste0(chemin_output_ACP,"/graph_",dimension,"_",saison,".png"),
      width=1400, height=800)
  plot(p7)
  dev.off()

  # valeurs des axes, par pixels
  # FAMD_tbl <-as.data.frame(res.famd$svd$U)
  PCA_tbl <- as.data.frame(res.pca$ind$coord)
  names(PCA_tbl) = paste0("axe",seq(1:length(colnames(PCA_tbl))))
  table_all <- cbind(table_donnees,PCA_tbl)
  write.csv(table_all, paste0(chemin_output_ACP,"/tblPCA_",dimension,"_",saison,".csv"))

  # Composantes principales en rasters
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
    }
    return(raster_to_check)
  }
  createRasterPCA <- function(number, table){
    # #TEST
    # number = 1
    # table = table_all
    
    rast_axe <- rasterFromXYZ(cbind(table[,grep("^x$", names(table))], 
                                      table[grep("^y$", names(table))], 
                                      table[grep(paste0("axe",number,"$"), names(table))]), 
                                crs=EPSG_2154)
    names(rast_axe) = paste0("axe",number,"_",dimension)
    rast_axe = ExtCRS(rast_axe)
    writeRaster(rast_axe, 
                  paste0(chemin_output_ACP,"/",names(rast_axe),".tif"), 
                  overwrite=T)
  }
  if(n_ncp>3){end = 7}else{end=1}
  
  if(any(grepl(dimension,liste.dim))){
    end=2 # ACP sur une dimension, on ne garde au max que 2 axes
  }
  cat(paste0("normalement devrait y avoir ",end," composantes en raster."))
  lapply(1:end, function(x) createRasterPCA(number=x, table=table_all))

  # Sauvegarde transfo ACP pour l'appliquer sur d'autres mois
  save(res.pca, file = paste0(chemin_output_ACP,"/PCA.rdata"))
  }
  }

# fonction qui lance PCA sur une dimension
fct_PCA <- function(dimension, # "CA" "B" "PV" "D" "I" "CS" 
                     palette_couleur=mypalette,
                    ponderation){ # TRUE/FALSE

  # # # TEST
  # dimension = liste.dim[1]
  # palette_couleur = mypalette
  # ponderation = FALSE
  
  liste_path_stack_dim = list.files(paste0(path_dos_stack,dimension),".tif$",recursive = T,
             full.names = T)
  liste_stack_dim = lapply(liste_path_stack_dim, stack)
  coords = coordinates(liste_stack_dim[[1]])
  liste_dt_stack_dim = lapply(liste_stack_dim, function(x)as.data.frame(data.table(as.data.frame(x))))
  liste_dt_stack_dim = lapply(liste_dt_stack_dim, function(x) cbind(coords, x))
  # S'assurer qu'il y ait les bons noms de colonnes
  liste_dt_stack_dim = lapply(liste_dt_stack_dim, function(x) cleanVarName(liste_nom_var_ref = table_variables$Nom, 
                                                                     dt_to_clean = x))
  dt_stacks =  do.call(rbind, liste_dt_stack_dim)
  # Retirer les NA (quand calculé sur N2000 et pas emrpise carrée)
  dt_stacks  <- dt_stacks [complete.cases(dt_stacks),]
  # Vérifier la nature des variables (si qualitative, coder en facteur)
  extr_tb = table_variables[table_variables$Nom %in% names(dt_stacks), ]
  liste_nom_var_quali = extr_tb$Nom[which(extr_tb$Nature == "qualitative")]
  dt_stacks[liste_nom_var_quali] <- lapply(dt_stacks[liste_nom_var_quali] , factor)
  cat(paste0("\nPréparation table des variables pour dimension ",dimension,
               " effectuée.\n"))
  makePCA(table_donnees = dt_stacks,
          saison = "summer", 
          dimension = dimension, 
          palette_couleur = palette_couleur,
          ponderation = ponderation)

}


# # fonction qui lance PCA sur toutes les variables en meme temps
# fct_PCA_all <- function(periode=c("mai",'juin','juillet','aout','septembre'),
#                          palette_couleur=mypalette, ponderation,
#                         predict = FALSE){
# 
#   # # # TEST
#   # periode = 'juillet'
#   # i = periode[1]
#   # palette_couleur = mypalette
#   # ponderation ="no" # "yes" ou "no"
#   # predict= TRUE
# 
#   # Fonctionnement par période
#   for(i in periode){
# 
#     dirs_mois = list.dirs(paste0(path_dos_stack))[grep(i, list.dirs(paste0(path_dos_stack)))]
#     files_mois = list.files(dirs_mois,".tif", full.names = T)
# 
#     # # Pour dimension CA, stack avec toutes les variables ou avec 10 vars clim synthétisées en 2 axes
#     # if(dimension == "CA"){
#     #   liste_stack = list.files(paste0(path_dos_stack,dimension,"/",i),".tif", full.names = T)
#     #   #stack_ssACP = liste_stack[grepl("sansACP",liste_stack)]
#     #   stack_ACP = liste_stack[!grepl("sansACP",liste_stack)]
#     #   stack_dim <- stack(stack_ACP)
#     # }
#     # # Utiliser la stack avec les 10 vars clims en 2 axes ACP
#     # files_mois = files_mois[-grep("sansACP",files_mois)]
# 
#     # # Utiliser la stack avec les 10 vars clims
#     # files_mois = files_mois[-grep(paste0("_CA_",i,".tif"),files_mois)]
# 
#     stack_mois <- stack(lapply(files_mois ,stack))
#     # Transformation en table
#     dt_stack <- as.data.frame(data.table(as.data.frame(stack_mois)))
#     dt_stack <- cbind(dt_stack,raster::coordinates(stack_mois))
#     # Retirer les NA (quand calculé sur N2000 et pas emrpise carrée) : 211 200 pixels --> 50 320
#     dt_stack <- dt_stack[complete.cases(dt_stack),]     # 211 200 pixels --> 98 967
#     # Ré-écrire correctement le nom des variables (si jamais du superflu traine)
#     dt_stack <- cleanVarName(liste_nom_var_ref = table_variables$Nom, 
#                              dt_to_clean = dt_stack)
#     # Vérifier la nature des variables (si qualitative, coder en facteur)
#     extr_tb = table_variables[table_variables$Nom %in% names(dt_stack), ]
#     liste_nom_var_quali = extr_tb$Nom[which(extr_tb$Nature == "qualitative")]
#     dt_stack[liste_nom_var_quali] <- lapply(dt_stack[liste_nom_var_quali] , factor)
#     cat(paste0("\nPréparation table des variables pour le mois de ",i," effectuée.\n"))
#     # # ajouter la dimension d'appartenance
#     if(ponderation == "no"){
#       dimension = "ACP_sans_ponderation"
#     } else{dimension = "ACP_avec_ponderation"}
#     # Faire tourner l'ACP
#     makePCA(table_donnees =  dt_stack,
#             saison = i, 
#             dimension = dimension, 
#             palette_couleur = palette_couleur, 
#             ponderation = ponderation,
#             predict = predict)
#   }
# }

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
ACP_axesPCA <- function(palette_couleur=mypalette){

  # # # TEST
  # mois = liste.mois[1]
  # palette_couleur = mypalette

  #cat("\n Mois de ",mois," en cours.")

  liste_axes = list.files(paste0(output_path,"/ACP/",liste.dim,"/summer/avec_ponderation/"),
                          ".tif$|.TIF$", recursive = T, full.names = T)
  
  # # Virer les axes issues de la FAMD globale
  # liste_axes = liste_axes[!grepl("toutes|ACP_ACP|TEST",liste_axes)]
  # # Garder le mois en cours
  # liste_axes = liste_axes[grepl(mois,liste_axes)]

  # S'assurer de la conformité des variables
  lapply(liste_axes, AjustExtCRS)
  # Stack les axes
  stack_axesACPs <- stack(liste_axes)
  plot(stack_axesACPs, colNA="black")

  # # Après inspection, inutile conserver deux axes de D 
  # testD = stack_axesACPs$axe1_D -  stack_axesACPs$axe2_D
  # plot(testD, colNA= "black")

  stack_axesACPs <- dropLayer(stack_axesACPs, grep("axe2_D",names(stack_axesACPs)))
  
  # Transformation en table
  dt_stack <- as.data.frame(data.table(as.data.frame(stack_axesACPs)))
  dt_stack <- cbind(dt_stack,raster::coordinates(stack_axesACPs[[1]]))
  # Retirer les NA (quand calculé sur N2000 et pas emprise carrée) : 211 200 pixels --> 50 320
  dt_stack <- dt_stack[complete.cases(dt_stack),]     # 211 200 pixels --> 98 967

  # # ajouter la dimension d'appartenance
  # merge(dt_stack, table_variables)

  makePCA(table_donnees = dt_stack,
          saison = "summer",
          dimension = "ACP_ACP",
          palette_couleur = palette_couleur,
          ponderation = FALSE)

}

# fonction qui transforme les vars envs selon une ACP choisie
applyPCAtransformation <- function(mois, # "mai" "juin" etc
                                   ponderation, # TRUE FALSE
                                   mode # dimensions ou ACP_ACP ou globale
                                   ){
  
  # # TEST
  # mois = "mai"
  # ponderation = FALSE # TRUE FALSE
  # mode = "dimensions" # "dimensions" "ACP_ACP" "globale"
  
  pond <- ifelse(ponderation,"avec","sans")
  
  if(mode == "dimensions"){
    # Dossier où seront stockées les prédictions
    chemin_stock <- paste0(output_path,"/ACP/",liste.dim,"/summer/",pond,"_ponderation/")
    # Dossiers où récuperer les rasters sur lesquels il faut appliquer la transformation (ACP)
    dirs_mois <- list.dirs(paste0(path_dos_stack))[grep(mois, list.dirs(paste0(path_dos_stack)))]
    # # Chemin ACP à utiliser
    # liste_PCA = paste0(chemin_stock,"/PCA.rdata")
    cat("Applications d'ACPs sur les dimensions B, CA, PV, I, CS et D, une par une. \n")
  }
  if(mode == "globale"){
    nom_modalite = paste0("ACP_",pond,"_ponderation")
    # Dossier où seront stockées les prédictions
    chemin_stock <- paste0(output_path,"/ACP/",nom_modalite,"/summer/")
    # Dossiers où récuperer les rasters sur lesquels il faut appliquer la transformation (ACP)
    dirs_mois = list.dirs(paste0(path_dos_stack))[grep(mois, list.dirs(paste0(path_dos_stack)))]
    # # Chemin ACP à utiliser
    # liste_PCA = paste0(chemin_stock, "/PCA.rdata")
    cat("Application d'ACP sur les dimensions B, CA, PV, I, CS et D. \n")
  }
  # TODO : à tester
  if(mode == "ACP_ACP"){
    nom_modalite = mode
    # Dossier où seront stockées les prédictions
    chemin_stock <- paste0(output_path,"/ACP/",nom_modalite,"/summer/",pond,"_ponderation/")
    # Dossiers où récuperer les rasters sur lesquels il faut appliquer la transformation (ACP)
    dirs_mois<- paste0(output_path,"/ACP/",liste.dim,"/summer/",pond,"_ponderation/pred_month/",mois)
    # # Chemin ACP à utiliser
    # liste_PCA = paste0(chemin_stock,"/PCA.rdata")
    cat("Application d'ACP sur les ACPs des dimensions B, CA, PV, I, CS et D. \n")
  }

  # Chemin ACP à utiliser
  liste_PCA <- paste0(chemin_stock,"/PCA.rdata")
  
  if(!any(dir.exists(paste0(chemin_stock,"/pred_month/",mois,"/")))){
    lapply(paste0(chemin_stock,"/pred_month/",mois,"/"), function(x) dir.create(x,recursive=T))
    }

  StackandPred <- function(dossier_input,nom_dim=FALSE){
    # # TEST
    # dossier_input = dirs_mois[1]
    # nom_dim = TRUE
    
    files_mois = list.files(dossier_input,".tif$", full.names = T)
    stack_mois <- stack(lapply(files_mois ,stack))
    # Transformation en table
    dt_stack <- as.data.frame(data.table(as.data.frame(stack_mois)))
    dt_stack <- cbind(raster::coordinates(stack_mois),dt_stack)
    # Retirer les NA (quand calculé sur N2000 et pas emrpise carrée) 
    dt_stack <- dt_stack[complete.cases(dt_stack),] 
    # Ré-écrire correctement le nom des variables (si jamais du superflu traine)
    dt_stack <- cleanVarName(liste_nom_var_ref = table_variables$Nom, 
                             dt_to_clean = dt_stack)
    # Vérifier la nature des variables (si qualitative, coder en facteur)
    extr_tb = table_variables[table_variables$Nom %in% names(dt_stack), ]
    liste_nom_var_quali = extr_tb$Nom[which(extr_tb$Nature == "qualitative")]
    dt_stack[liste_nom_var_quali] <- lapply(dt_stack[liste_nom_var_quali] , factor)
    cat(paste0("\nPréparation table des variables pour le mois de ",mois," effectuée.\n"))
    
    stock <- dt_stack
    
    # Check for dummies variables
    t = try(dummy_cols(dt_stack, remove_selected_columns=T), silent = TRUE)
    if(inherits(t, "try-error")) {
      dt_stack <- dt_stack
      #df_w = table_variables[table_variables$Nom %in% names(dt_stack)]
    } else {   
      # Transformer factoriel (ordi et quali) en boolean (0/1)
      dt_stack <- dummy_cols(dt_stack, 
                             select_columns=liste_nom_var_quali,
                             remove_selected_columns=T)
      # retirer _ dans noms dummies
      f <- function(x){
        a <- unlist(strsplit(x,"_"))
        if(length(a)>2){
          new.name <- paste0(paste0(a[1:2],collapse="_"),
                             a[3],collapse="") 
        }else{
          new.name <- paste0(a,collapse="") 
        }
        return(new.name)
      }
      index_dummies <- grep(pattern=paste0(liste_nom_var_quali,collapse = "|"), names(dt_stack))
      names(dt_stack)[index_dummies] <- unlist(lapply(names(dt_stack)[index_dummies],f))
      
      # Matrice de pondération des variables dans ACP par dimension
      df_dum = table_variable_dummies[table_variable_dummies$Nom %in% names(dt_stack)]
      df_quanti = table_variables[table_variables$Nom %in% names(dt_stack)]
      df_w = rbind(df_quanti, df_dum, fill=T)
    }
    # Retirer les coordonnées x et y
    idx_vars_quanti_sup = c(grep("^x$", names(dt_stack)),grep("^y$", names(dt_stack)))
    dt_stack2 <- dt_stack %>% select(-idx_vars_quanti_sup)
    
    #nettoyer noms variables
    if(grepl(mois,names(dt_stack2[1]))){
      m = nchar(mois) + 1
      names(dt_stack2) = substr(names(dt_stack2), 1, nchar(names(dt_stack2))-m)
    }
    
    # Appliquer un poids ?? ou déjà pris en compte dans transfo d'ACP ?
    
    if(nom_dim){
      m = nchar(mois) + 2
      DIM = substr(dossier_input, nchar(dossier_input)-m, nchar(dossier_input)-m+1)
      DIM = gsub("/","",DIM)
      
      chemin_PCA = liste_PCA[grep(paste0("/",DIM,"/"), liste_PCA)]
      chemin_stock_ici = chemin_stock[grep(paste0("/",DIM,"/"), chemin_stock)]
      modalite = DIM
    }else{
      chemin_PCA = liste_PCA
      chemin_stock_ici = chemin_stock
      modalite = nom_modalite}
    
    # Load PCA
    load(chemin_PCA)
    # appliquer transfo ACP
    dt_predPCA <- predict.PCA(object = res.pca, newdata =  dt_stack2)
    # # vérifier les noms des variables
    # names(res.pca$call$X)
    # names(dt_stack2)
    
    # valeurs des axes, par pixels
    PCA_tbl <- as.data.frame(dt_predPCA$coord)
    names(PCA_tbl) = paste0("axe",seq(1:length(colnames(PCA_tbl))))
    table_all <- cbind(PCA_tbl,dt_stack)
    write.csv(table_all, paste0(chemin_stock_ici,"/pred_month/",mois,"/PCA_values_",mois,".csv"))
    
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
      }
      return(raster_to_check)
    }
    createRasterPCA <- function(number, table, mois){
      # #TEST
      # number = 1
      # table = table_all
      cat(modalite,"\n")
      rast_axe <- rasterFromXYZ(cbind(table[,grep("^x$", names(table))], 
                                      table[grep("^y$", names(table))], 
                                      table[grep(paste0("axe",number,"$"), names(table))]), 
                                crs=EPSG_2154)
      
      names(rast_axe) = paste0("axe",number,"_",modalite,"_",mois)
      rast_axe = ExtCRS(rast_axe)
      writeRaster(rast_axe, 
                  paste0(chemin_stock_ici,"/pred_month/",mois,"/",names(rast_axe),".tif"),
                  overwrite=T)
    }
    
    n_rast = length(list.files(chemin_stock_ici,".tif$"))
    if(n_rast>7){n_rast=7}
    lapply(1:n_rast, function(x) createRasterPCA(number=x, table=table_all, mois = mois))
  }
  
  # a l'air de fonctionner
  if(mode == "dimensions"){
    lapply(as.list(dirs_mois),function(x) StackandPred(dossier_input = x,
                                                       nom_dim = TRUE))
  } else{StackandPred(dirs_mois)} # if mode == "globale" ou "ACP_ACP"
  
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
path_table_variables_dummies <- paste0(input_path,"/liste_variables_dummies.csv")
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
table_variable_dummies <- fread(path_table_variables_dummies, dec=",")
col_dim = merge(rbind(table_variables, table_variable_dummies,fill=T), 
                corresp_col,by.x="Dimension", by.y="dim_name"                )
mypalette <- setNames(col_dim$colour_dim, 
                      col_dim$Nom)

##### ACP globale (tous les mois) : toutes les dimensions  ####
dt_stack <- fread(paste0(output_path, "/stack_dim_global/data_env_5months.csv"),drop="V1")
dt_stack <- as.data.frame(dt_stack[complete.cases(dt_stack),]) #1 056 000 obs -> 249 690 obs
# Vérifier la nature des variables (si qualitative, coder en facteur)
extr_tb = table_variables[table_variables$Nom %in% names(dt_stack), ]
liste_nom_var_quali = extr_tb$Nom[which(extr_tb$Nature == "qualitative")]
dt_stack[liste_nom_var_quali] <- lapply(dt_stack[liste_nom_var_quali] , factor)
str(dt_stack)

# Ici, j'ai agrégé toutes les valeurs à travers les mois
makePCA(table_donnees = dt_stack,
        saison="summer", 
        dimension="ACP_sans_ponderation",
        palette_couleur = mypalette, 
        ponderation = "no")

makePCA(table_donnees = dt_stack,
        saison="summer", 
        dimension="ACP_avec_ponderation",
        palette_couleur = mypalette, 
        ponderation = "yes")

# sortir df pour chaque mois des valeurs des variables dans les axes de l'ACP
# afin de pouvoir faire tourner modèle distribution linéire dessous

# fonctionne
lapply(liste.mois, function(x) applyPCAtransformation(mois = x,
                                                      ponderation= FALSE,
                                                      mode = "globale"))
# fonctionne
lapply(liste.mois, function(x) applyPCAtransformation(mois = x,
                                                      ponderation= TRUE,
                                                      mode = "globale"))

# une ACP sur 1 mois
# puis la projeter sur les autres mois
# --> ne fonctionn pas

##### ACP globale (tous les mois) par dimension ####
lapply(liste.dim, function(x) fct_PCA(dimension = x,
                                      palette_couleur=mypalette,
                                      ponderation = FALSE)
)
# application transformation ACP sur chaque mois
lapply(liste.mois, function(x) applyPCAtransformation(mois = x,
                                                      ponderation= FALSE,
                                                      mode = "dimensions"))

##### ACP d'ACPs des dimensions ####

ACP_axesPCA(palette_couleur=mypalette)

# fonctionne
lapply(liste.mois, function(x) applyPCAtransformation(mois = x,
                                                      ponderation= FALSE,
                                                      mode = "ACP_ACP"))

# ##### ACP par dimension par mois ####
# lapply(liste.dim, function(x) fct_PCA(x,
#                                  periode=c("mai",'juin','juillet','aout','septembre'),
#                                  palette_couleur=mypalette,
#                                  ponderation = "no")
#        )
# # sapply(liste.dim, fct_FAMD,arg_ACP="sans")
# #sapply(liste.dim, fct_FAMD,arg_ACP="avec")
# 
# # # # Relancer FAMD sur une dimension spécifique
# # fct_PCA(dimension = "CA")

# ##### FAMD sur toutes les dimensions simultanément, par mois ####
# fct_PCA_all(ponderation="yes")
# fct_PCA_all(ponderation="no")


# ##### t-SNE par dimension par saison ####
# library(Rtsne)
# # stack_matrix <- as.matrix(dt_stack[,1:21])
# # Set a seed if you want reproducible results
# set.seed(42)
# tsne_out <- Rtsne(dt_stack[,1:21],partial_pca=T) # Run TSNE
# # Show the objects in the 2D tsne representation
# plot(tsne_out$Y, asp=0,alpha=0.1)
# DATA = as.data.frame(tsne_out$Y)
# ggplot(DATA, aes(x=V1,y=V2))+
#   geom_point(alpha=0.05)

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




##### Autre ####
# # Tips Natéo pour visualiser ACP
# library(Factoshiny)
# Factoshiny(dt_stack_ete)

# inspiration rpz d'un espèce écologique et individus sur ACP
# https://iboulangeat.github.io/pdfs/Boulangeat2012_EcolLet_published.pdf

# TODO : dudi.mix par rapport à FAMD ???
# https://www.rdocumentation.org/packages/ade4/versions/1.7-19/topics/dudi.mix
