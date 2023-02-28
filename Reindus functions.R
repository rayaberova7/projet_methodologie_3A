# Fonction de récupération d'un vecteur d'émissions de Figaro pour une année donnée et au format (tri) d'une matrice de référence.
RecupVectEmiFigaro <- function(annee,vectREF)  # Voir ligne 956
{
  if(annee>=2010 & annee<=2019){
 # VarMerge<-""  #### A Finir a partir exemple --> pour Contenus et pour diff_SPA
  Path_FIG22emi<-"X:/HAB-Empreinte-carbone/MRIO/04_Bases_emissions_GES/FIGARO 22/CO2/"
  emi_fig22 <- fread(paste0(Path_FIG22emi,"CO2Footprint_",annee,"_20220404.csv"))
  Vect_emi<-emi_fig22[,sum(obs_value),by=c("ref_area","industry")] 
  tabpass_geo <- fread("TabPass_geo.csv") # On doit recoder geo de 2 digit a 3 digit...
  tabpass_geo<-unique(tabpass_geo[,c("V2","V5")])
  Vect_emi2<-tabpass_geo[Vect_emi,on=.(V5=ref_area),  nomatch=0]
  Vect_emi2[industry=="L68",industry:="L"]
  Vect_emi2$PR<-paste0(Vect_emi2$V2,"_",Vect_emi2$industry)
  ReorderVect_emi<-Vect_emi2[vectREF,on=.(PR=PR),nomatch=0] # Attention la variable vectREF est générée dans la fonction Contenu
  ReorderVect_emi<-ReorderVect_emi[,c("PR","V1")]
  setnames(ReorderVect_emi,"V1","value")
  ReorderVect_emi<-setDF(ReorderVect_emi)
  ReorderVect_emi<-GetRownamesFromFirstCol(ReorderVect_emi)
  ReorderVect_emi<-as.matrix(ReorderVect_emi)
  }else{
  stop("ERREUR : Année en dehors du scope de Figaro (2010-2019)")
}
  return (ReorderVect_emi)
}

# SPA : Fonction Diff_spa de differentiel de path entre 2 jeux de MRIO
# Type MRIO := ECOLE ou FIGARO (filtres adaptés à chacun)
# Au préalable il faut avoir calculé les resultats sous forme de liste du type ListMRIO1
# TargetCountry est le pays sur lequel on va focaliser en augmentant les path passant par ce pays
# TypContenu := VA,Emi 
# OptForeign : option pour ne selectionner que des path transitant au moins une fois par un pays différent de TargetCountry
Diff_spa <- function(ListMRIO1,ListMRIO2,TypMRIO,TargetCountry="FRA",TypContenu="VA",OptForeign=FALSE,NbPath=10000)
{ 
  Layer_0to4_1<-Decomp_Layers(ListMRIO1,TargetCountry=TargetCountry,TypContenu=TypContenu,TypMRIO=TypMRIO)
  Layer_0to4_2<-Decomp_Layers(ListMRIO2,TargetCountry=TargetCountry,TypContenu=TypContenu,TypMRIO=TypMRIO)
  
  # On cherche donc à comparer le SPA_FIGARO_Layer_0to4 initial avec le Layer_0to4 nouvellement varianté.
  Layer_0to4rank_1 <- Layer_0to4_1[,ranking:=rank(-spa_value)]  # var Ranking
  Layer_0to4rank_2 <- Layer_0to4_2[,ranking:=rank(-spa_value)]  # var Ranking
  
  Variante_Compar<-Layer_0to4rank_2[Layer_0to4rank_1,on=.(spa_name=spa_name),  nomatch=0]
  
  Variante_Compar<-Variante_Compar[,diffRanking:=ranking-i.ranking][,diffValue:=spa_value-i.spa_value][order(-abs(diffValue))][1:NbPath,]
  
  if(OptForeign==TRUE){
    # Sélection des path avec des branches étrangères (donc pas que du domestique)
    if(TypMRIO=="ECOLE"){
      List_GEO<-c("UE_OTHERS","FRA","CHN","USA","ROW")
    }else{
      List_GEO<-c("ABW","AFG","AGO","AIA","ALA","ALB","AND","ARE","ARG","ARM","ASM","ATA","ATF","ATG","AUS","AUT","AZE","BDI","BEL","BEN","BFA","BGD","BGR","BHR","BHS","BIH","BLM","BLR","BLZ","BMU","BOL","BRA","BRB","BRN","BTN","BVT","BWA","CAF","CAN","CCK","CHE","CHL","CHN","CIV","CMR","COD","COG","COK","COL","COM","CPV","CRI","CUB","CXR","CYM","CYP","CZE","DEU","DJI","DMA","DNK","DOM","DZA","ECU","EGY","ERI","ESH","ESP","EST","ETH","FIN","FJI","FLK","FRA","FRO","FSM","GAB","GBR","GEO","GGY","GHA","GIB","GIN","GLP","GMB","GNB","GNQ","GRC","GRD","GRL","GTM","GUF","GUM","GUY","HKG","HMD","HND","HRV","HTI","HUN","IDN","IMN","IND","IOT","IRL","IRN","IRQ","ISL","ISR","ITA","JAM","JEY","JOR","JPN","KAZ","KEN","KGZ","KHM","KIR","KNA","KOR","KWT","LAO","LBN","LBR","LBY","LCA","LIE","LKA","LSO","LTU","LUX","LVA","MAC","MAF","MAR","MCO","MDA","MDG","MDV","MEX","MHL","MKD","MLI","MLT","MMR","MNE","MNG","MNP","MOZ","MRT","MSR","MTQ","MUS","MWI","MYS","MYT","NAM","NCL","NER","NFK","NGA","NIC","NIU","NLD","NOR","NPL","NRU","NZL","OMN","PAK","PAN","PCN","PER","PHL","PLW","PNG","POL","PRI","PRK","PRT","PRY","PSE","PYF","QAT","REU","ROU","RUS","RWA","SAU","SDN","SEN","SGP","SGS","SHN","SJM","SLB","SLE","SLV","SMR","SOM","SPM","SRB","STP","SUR","SVK","SVN","SWE","SWZ","SYC","SYR","TCA","TCD","TGO","THA","TJK","TKL","TKM","TLS","TON","TTO","TUN","TUR","TUV","TWN","TZA","UGA","UKR","UMI","URY","USA","UZB","VAT","VCT","VEN","VGB","VIR","VNM","VUT","WLF","WSM","YEM","ZAF","ZMB","ZWE","ROW","UE_OTHER")
    }
    List_GEO_horsTarget<-List_GEO[!(List_GEO %in% TargetCountry)]
    # Selection des path passant au moins une fois par un pays hors FRA
    Variante_foreign<-Variante_Compar[order(-abs(diffValue))][1:NbPath,][Reduce(`|`, Map(`%like%`, list(spa_name), List_GEO_horsTarget))] # On selectionne (sans perte de generalite) parmi les 10.000 path les plus forts pour limiter le temps d'execution
    Variante_Compar<-Variante_foreign
  }
  
  return(Variante_Compar)  
}

# SPA : Formule de décomposition des layers // TypContenu := VA,Emi 
Decomp_Layers <- function(MRIO,TargetCountry,TypContenu,TypMRIO)
{ 
  
  # Seuils pour les layers (hors Ecole où il n'y a pas de seuil)
  if(TypMRIO=="FIGARO"){
    Seuil_L2_33<-40000
    Seuil_L2_67<-80000
    Seuil_L3_33<-500
    Seuil_L3_67<-1000
    Seuil_L4_33<-333
    Seuil_L4_67<-667
  }
  if(TypMRIO=="FIGARO_Halfrestrict"){
    Seuil_L2_33<-20000
    Seuil_L2_67<-40000
    Seuil_L3_33<-250
    Seuil_L3_67<-500
    Seuil_L4_33<-167
    Seuil_L4_67<-333
  }
    if(TypMRIO=="FIGARO_Semirestrict"){
    Seuil_L2_33<-10000
    Seuil_L2_67<-20000
    Seuil_L3_33<-120
    Seuil_L3_67<-240
    Seuil_L4_33<-100
    Seuil_L4_67<-200
  }
  if(TypMRIO=="FIGARO_restrict"){
    Seuil_L2_33<-4000
    Seuil_L2_67<-8000
    Seuil_L3_33<-50
    Seuil_L3_67<-100
    Seuil_L4_33<-33
    Seuil_L4_67<-67
  }
  
  # Recuperation des composantes du MRIO pour réaliser les calculs
  A_dt<-MRIO[["A"]]
  A_dt[value<0,]<-0
  A_dt<-AjoutPRBR(A_dt)
  if(TypContenu=="VA"){
    f_dt<-MRIO[["VA"]] # Init : On prends la VA/Prod comme stressor
  }
  if(TypContenu=="Emi"){
    ############################# A FINIR ET VERIF / ATTENTION A LA SUITE // AJOUTER DATE EN PARAMETRE
    f_dt<-RecupVectEmiFigaro(annee,"A_tab")  # Init : On prends la Emi/Prod comme stressor
  }
  
  Prod_dt<-MRIO[["PROD"]] 
  f_dt$value<-f_dt$value/Prod_dt$value
  f_dt<-GereInfNA(f_dt) 
  y_dt<-MRIO[["DF_TOT"]]
  f_dt<-AjoutPRBR(f_dt)
  y_dt<-AjoutPRBR(y_dt)
  A_dt<-A_dt[,c("PR","BR","value")]
  f_dt<-f_dt[,c("BR","value")]
  setnames(f_dt,"BR","PR")
  y_dt<-y_dt[,c("PR","value")]
  setnames(y_dt,"PR","BR")
  
  if(TypMRIO=="ECOLE"){
    ### Calcul des layers
    # Layer 0 : FIy
    Layer_0<-y_dt[f_dt,on=.(BR=PR),  nomatch=0]
    setnames(Layer_0,"value","y_i")
    setnames(Layer_0,"i.value","f_i")
    setnames(Layer_0,"BR","name_i")
    Layer_0<-Layer_0[,c("name_i","f_i","y_i")]  
    
    # Layer 1 : FAy
    Layer_1<-y_dt[A_dt,on=.(BR=BR),  nomatch=0]
    Layer_1<-Layer_1[f_dt,on=.(PR=PR),  nomatch=0]
    setnames(Layer_1,"value","y_j")
    setnames(Layer_1,"i.value.1","f_i")
    setnames(Layer_1,"i.value","a_ij")
    setnames(Layer_1,"BR","name_j")
    setnames(Layer_1,"PR","name_i")
    Layer_1<-Layer_1[,c("name_i","name_j","f_i","a_ij","y_j")]
    
    # Layer 2 : FA^2y
    A1<-A_dt
    A2<-A_dt
    Acartprod <- A2[A1, on=.(BR=PR),  allow.cartesian=TRUE]
    setnames(Acartprod,"value","a_ij")
    setnames(Acartprod,"i.value","a_jk")
    setnames(Acartprod,"PR","name_i")
    setnames(Acartprod,"BR","name_j")
    setnames(Acartprod,"i.BR","name_k")
    Acartprod<-Acartprod[,c("name_i","name_j","name_k","a_ij","a_jk")]
    Layer_2<-y_dt[Acartprod,on=.(BR=name_k),  nomatch=0]
    Layer_2<-Layer_2[f_dt,on=.(name_i=PR),  nomatch=0]
    setnames(Layer_2,"value","y_k")
    setnames(Layer_2,"i.value","f_i")
    setnames(Layer_2,"BR","name_k")
    Layer_2<-Layer_2[,c("name_i","name_j","name_k","f_i","a_ij","a_jk","y_k")]
    
    # Layer 3 : FA^3y
    A3cartprod <- Acartprod[A2, on=.(name_k=PR),  allow.cartesian=TRUE]
    setnames(A3cartprod,"BR","name_l")
    setnames(A3cartprod,"value","a_kl")
    A3cartprod<-A3cartprod[,c("name_i","name_j","name_k","name_l","a_ij","a_jk","a_kl")]
    Layer_3<-y_dt[A3cartprod,on=.(BR=name_l),  nomatch=0]
    Layer_3<-Layer_3[f_dt,on=.(name_i=PR),  nomatch=0]
    setnames(Layer_3,"value","y_l")
    setnames(Layer_3,"i.value","f_i")
    setnames(Layer_3,"BR","name_l")
    Layer_3<-Layer_3[,c("name_i","name_j","name_k","name_l","f_i","a_ij","a_jk","a_kl","y_l")]
    
    # Layer 4 : FA^4y
    A4cartprod <- A3cartprod[A2, on=.(name_l=PR),  allow.cartesian=TRUE]
    setnames(A4cartprod,"BR","name_m")
    setnames(A4cartprod,"value","a_lm")
    A4cartprod<-A4cartprod[,c("name_i","name_j","name_k","name_l","name_m","a_ij","a_jk","a_kl","a_lm")]
    Layer_4<-y_dt[A4cartprod,on=.(BR=name_m),  nomatch=0]
    Layer_4<-Layer_4[f_dt,on=.(name_i=PR),  nomatch=0]
    setnames(Layer_4,"value","y_m")
    setnames(Layer_4,"i.value","f_i")
    setnames(Layer_4,"BR","name_m")
    Layer_4<-Layer_4[,c("name_i","name_j","name_k","name_l","name_m","f_i","a_ij","a_jk","a_kl","a_lm","y_m")]
    
    
  }else{
    
    
    ### Calcul des layers
    # Layer 0 : FIy
    Layer_0<-y_dt[f_dt,on=.(BR=PR),  nomatch=0]
    setnames(Layer_0,"value","y_i")
    setnames(Layer_0,"i.value","f_i")
    setnames(Layer_0,"BR","name_i")
    Layer_0<-Layer_0[,c("name_i","f_i","y_i")]  
    
    # Layer 1 : FAy
    Layer_1<-y_dt[A_dt,on=.(BR=BR),  nomatch=0]
    Layer_1<-Layer_1[f_dt,on=.(PR=PR),  nomatch=0]
    setnames(Layer_1,"value","y_j")
    setnames(Layer_1,"i.value.1","f_i")
    setnames(Layer_1,"i.value","a_ij")
    setnames(Layer_1,"BR","name_j")
    setnames(Layer_1,"PR","name_i")
    Layer_1<-Layer_1[,c("name_i","name_j","f_i","a_ij","y_j")]
    
    # Layer 2 : FA^2y
    Select_A_dt<-A_dt[PR %like% TargetCountry | BR %like% TargetCountry,][order(-value)][1:Seuil_L2_33,]
    SelectSeuil_A_dt<-A_dt[value>mean(A_dt$value),][order(-value)][1:Seuil_L2_67,]
    SelectAll<-rbind(Select_A_dt,SelectSeuil_A_dt)
    SelectAll<-unique(SelectAll)
    
    A1<-SelectAll
    A2<-SelectAll
    Acartprod <- A2[A1, on=.(BR=PR),  allow.cartesian=TRUE]
    setnames(Acartprod,"value","a_ij")
    setnames(Acartprod,"i.value","a_jk")
    setnames(Acartprod,"PR","name_i")
    setnames(Acartprod,"BR","name_j")
    setnames(Acartprod,"i.BR","name_k")
    Acartprod<-Acartprod[,c("name_i","name_j","name_k","a_ij","a_jk")]
    Layer_2<-y_dt[Acartprod,on=.(BR=name_k),  nomatch=0]
    Layer_2<-Layer_2[f_dt,on=.(name_i=PR),  nomatch=0]
    setnames(Layer_2,"value","y_k")
    setnames(Layer_2,"i.value","f_i")
    setnames(Layer_2,"BR","name_k")
    Layer_2<-Layer_2[,c("name_i","name_j","name_k","f_i","a_ij","a_jk","y_k")]
    
    # Layer 3 : FA^3y
    Select_A_dt<-A_dt[PR %like% TargetCountry | BR %like% TargetCountry,][order(-value)][1:Seuil_L3_33,]
    SelectSeuil_A_dt<-A_dt[value>mean(A_dt$value),][order(-value)][1:Seuil_L3_67,]
    SelectAll<-rbind(Select_A_dt,SelectSeuil_A_dt)
    SelectAll<-unique(SelectAll)
    A2<-SelectAll
    
    A3cartprod <- Acartprod[A2, on=.(name_k=PR),  allow.cartesian=TRUE]
    setnames(A3cartprod,"BR","name_l")
    setnames(A3cartprod,"value","a_kl")
    A3cartprod<-A3cartprod[,c("name_i","name_j","name_k","name_l","a_ij","a_jk","a_kl")]
    Layer_3<-y_dt[A3cartprod,on=.(BR=name_l),  nomatch=0]
    Layer_3<-Layer_3[f_dt,on=.(name_i=PR),  nomatch=0]
    setnames(Layer_3,"value","y_l")
    setnames(Layer_3,"i.value","f_i")
    setnames(Layer_3,"BR","name_l")
    Layer_3<-Layer_3[,c("name_i","name_j","name_k","name_l","f_i","a_ij","a_jk","a_kl","y_l")]
    
    # Layer 4 : FA^4y
    Select_A_dt<-A_dt[PR %like% TargetCountry | BR %like% TargetCountry,][order(-value)][1:Seuil_L4_33,]
    SelectSeuil_A_dt<-A_dt[value>mean(A_dt$value),][order(-value)][1:Seuil_L4_67,]
    SelectAll<-rbind(Select_A_dt,SelectSeuil_A_dt)
    SelectAll<-unique(SelectAll)
    A2<-SelectAll
    
    A4cartprod <- A3cartprod[A2, on=.(name_l=PR),  allow.cartesian=TRUE]
    setnames(A4cartprod,"BR","name_m")
    setnames(A4cartprod,"value","a_lm")
    A4cartprod<-A4cartprod[,c("name_i","name_j","name_k","name_l","name_m","a_ij","a_jk","a_kl","a_lm")]
    Layer_4<-y_dt[A4cartprod,on=.(BR=name_m),  nomatch=0]
    Layer_4<-Layer_4[f_dt,on=.(name_i=PR),  nomatch=0]
    setnames(Layer_4,"value","y_m")
    setnames(Layer_4,"i.value","f_i")
    setnames(Layer_4,"BR","name_m")
    Layer_4<-Layer_4[,c("name_i","name_j","name_k","name_l","name_m","f_i","a_ij","a_jk","a_kl","a_lm","y_m")]
  }
  
  
  
  # Normalisation et constitution d'une base unique
  Emi_tot<-sum(MRIO[["VA"]]$value)
  
  Layer_0n<-Layer_0[,Layer:="Layer_0"][,spa_name:=paste0(name_i)][,spa_value:=f_i*y_i][,spa_valuePct:=spa_value/Emi_tot][,c("Layer","spa_name","spa_value","spa_valuePct")]
  
  Layer_1n<-Layer_1[,Layer:="Layer_1"][,spa_name:=paste0(name_i,"~",name_j)][,spa_value:=f_i*a_ij*y_j][,spa_valuePct:=spa_value/Emi_tot][,c("Layer","spa_name","spa_value","spa_valuePct")]
  
  Layer_2n<-Layer_2[,Layer:="Layer_2"][,spa_name:=paste0(name_i,"~",name_j,"~",name_k)][,spa_value:=f_i*a_ij*a_jk*y_k][,spa_valuePct:=spa_value/Emi_tot][,c("Layer","spa_name","spa_value","spa_valuePct")]
  
  Layer_3n<-Layer_3[,Layer:="Layer_3"][,spa_name:=paste0(name_i,"~",name_j,"~",name_k,"~",name_l)][,spa_value:=f_i*a_ij*a_jk*a_kl*y_l][,spa_valuePct:=spa_value/Emi_tot][,c("Layer","spa_name","spa_value","spa_valuePct")]
  
  Layer_4n<-Layer_4[,Layer:="Layer_4"][,spa_name:=paste0(name_i,"~",name_j,"~",name_k,"~",name_l,"~",name_m)][,spa_value:=f_i*a_ij*a_jk*a_kl*a_lm*y_m][,spa_valuePct:=spa_value/Emi_tot][,c("Layer","spa_name","spa_value","spa_valuePct")]
  
  Layer_0to4<-rbind(Layer_0n,Layer_1n,Layer_2n,Layer_3n,Layer_4n)
  
  return(Layer_0to4)  
}


# Fonction boucle Linkage Backward et Forward
BoucleLinkageBwdFwd <- function(base_dt,period,OptMRIO=TRUE)
{ 
  for(k in period){
    if(k==period[[1]]){ # initialisation
      List_interm<-LinkageBwdFwd(base_dt,k,OptMRIO=OptMRIO)
      Base_dt1<-List_interm[["Indic_Bwd_byCountry"]][,year:=k]
      Base_dt2<-List_interm[["Indic_Fwd_byCountry"]][,year:=k]
      Base_dt3<-List_interm[["Indic_Bwd_tot"]][,year:=k]
      Base_dt4<-List_interm[["Indic_Fwd_tot"]][,year:=k]
      Base_dt5<-List_interm[["Indic_Bwd_Prod"]][,year:=k]
      Base_dt6<-List_interm[["Indic_Fwd_Prod"]][,year:=k]
    }else{
      List_interm<-LinkageBwdFwd(base_dt,k,OptMRIO=OptMRIO)
      dt1<-List_interm[["Indic_Bwd_byCountry"]][,year:=k]
      dt2<-List_interm[["Indic_Fwd_byCountry"]][,year:=k]
      dt3<-List_interm[["Indic_Bwd_tot"]][,year:=k]
      dt4<-List_interm[["Indic_Fwd_tot"]][,year:=k]
      dt5<-List_interm[["Indic_Bwd_Prod"]][,year:=k]
      dt6<-List_interm[["Indic_Fwd_Prod"]][,year:=k]
      
      Base_dt1<-rbind(Base_dt1,dt1)
      Base_dt2<-rbind(Base_dt2,dt2)
      Base_dt3<-rbind(Base_dt3,dt3)
      Base_dt4<-rbind(Base_dt4,dt4)
      Base_dt5<-rbind(Base_dt5,dt5)
      Base_dt6<-rbind(Base_dt6,dt6)    
      
      Base_Out<-list(Indic_Bwd_byCountry=Base_dt1,Indic_Fwd_byCountry=Base_dt2,Indic_Bwd_tot=Base_dt3,Indic_Fwd_tot=Base_dt4,Indic_Bwd_Prod=Base_dt5,Indic_Fwd_Prod=Base_dt6)
      print(paste0("year = ",k))
    }
  }
  return(Base_Out)  
}



# Fonction Linkage Backward et Forward
LinkageBwdFwd <- function(base_dt,annee,OptMRIO=TRUE)
{ 
  # On démare par calculer le matrices sources dont on a besoin pour les linkages
  if(OptMRIO==TRUE){
    inter_listTab<-CompoECOLEouA17(base_dt,typeCompo="OptFullOptionsBonus",date=annee)
  }else{
    inter_listTab<-CompoMRIO(base_dt,typeCompo="OptFullOptionsBonus",date=annee)
  }
  L_dt<-setDT(AddRownamesToFirstCol(inter_listTab[["L"]]))
  L_dt<-melt(L_dt)
  setnames(L_dt,"joint","PR")
  setnames(L_dt,"variable","BR")
  L_dt<-L_dt[PR==BR,value:=0] # On met la diagonale à 0 pour avoir un indicateur de total linkage
  L_dt<-SplitPRBR(L_dt)
  
  G_dt<-setDT(AddRownamesToFirstCol(inter_listTab[["InvBGhosh"]]))
  G_dt<-melt(G_dt)
  setnames(G_dt,"joint","PR")
  setnames(G_dt,"variable","BR")
  G_dt<-G_dt[PR==BR,value:=0] # On met la diagonale à 0 pour avoir un indicateur de total linkage
  G_dt<-SplitPRBR(G_dt)
  
  # Sommation : numérateur
  Bwd_num<-L_dt[, sum(value),by=c("Col_Country","Col_Indus")]
  Fwd_num<-G_dt[, sum(value),by=c("Lig_Country","Lig_Indus")]
  
  # Sommation : dénominateur
  Bwd_Tot_denom<-L_dt[, sum(value)]
  Fwd_Tot_denom<-G_dt[, sum(value)]
  Bwd_Prod_denom<-inter_listTab[["PROD"]]
  Fwd_Prod_denom<-inter_listTab[["PROD"]]
  setnames(Fwd_Prod_denom,"Lig_Country","Col_Country")
  setnames(Fwd_Prod_denom,"Lig_Indus","Col_Indus")
  Bwd_byCountry_denom<-L_dt[, sum(value),by="Col_Country"]
  Fwd_byCountry_denom<-G_dt[, sum(value),by="Lig_Country"]
  
  # Calcul des indicateurs normalisés (par les dénominateurs)
  Indic_Bwd_tot<-Bwd_num # Init
  Indic_Bwd_tot[,3]<-Bwd_num[,3]/Bwd_Tot_denom
  Indic_Fwd_tot<-Fwd_num # Init
  Indic_Fwd_tot[,3]<-Fwd_num[,3]/Fwd_Tot_denom
  
  setnames(Fwd_num,c("Lig_Country","Lig_Indus"),c("Col_Country","Col_Indus"))  
  Indic_Fwd_Prod<-Fwd_Prod_denom[Fwd_num,on=.(Col_Country,Col_Indus)] # Init
  Indic_Fwd_Prod<-Indic_Fwd_Prod[,value:=V1/value]
  Indic_Fwd_Prod<-Indic_Fwd_Prod[,c("Col_Country","Col_Indus","value")]
  setnames(Bwd_Prod_denom,"Col_Country","Lig_Country")
  setnames(Bwd_Prod_denom,"Col_Indus","Lig_Indus")
  setnames(Bwd_num,c("Col_Country","Col_Indus"),c("Lig_Country","Lig_Indus")) 
  Indic_Bwd_Prod<-Bwd_Prod_denom[Bwd_num,on=.(Lig_Country,Lig_Indus)] # Init
  Indic_Bwd_Prod<-Indic_Bwd_Prod[,value:=V1/value]
  Indic_Bwd_Prod<-Indic_Bwd_Prod[,c("Lig_Country","Lig_Indus","value")]
  
  setnames(Fwd_byCountry_denom,"Lig_Country","Col_Country")
  Indic_Fwd_byCountry<-Fwd_byCountry_denom[Fwd_num,on=.(Col_Country)] # Init
  Indic_Fwd_byCountry<-Indic_Fwd_byCountry[,value:=i.V1/V1]
  Indic_Fwd_byCountry<-Indic_Fwd_byCountry[,c("Col_Country","Col_Indus","value")]
  setnames(Bwd_byCountry_denom,"Col_Country","Lig_Country")
  Indic_Bwd_byCountry<-Bwd_byCountry_denom[Bwd_num,on=.(Lig_Country)] # Init
  Indic_Bwd_byCountry<-Indic_Bwd_byCountry[,value:=i.V1/V1]
  Indic_Bwd_byCountry<-Indic_Bwd_byCountry[,c("Lig_Country","Lig_Indus","value")]
  
  List_out<-list(Indic_Bwd_byCountry=Indic_Bwd_byCountry,Indic_Fwd_byCountry=Indic_Fwd_byCountry,Indic_Bwd_tot=Indic_Bwd_tot,Indic_Fwd_tot=Indic_Fwd_tot,Indic_Bwd_Prod=Indic_Bwd_Prod,Indic_Fwd_Prod=Indic_Fwd_Prod)
  
  return(List_out)  
}


# Fonction boucle GAO
BoucleGAO <- function(base_dt,period,OptMRIO=TRUE)
{ 
  for(k in period){
    if(k==period[[1]]){ # initialisation
      Base_Out<-GAO(base_dt,k,(k+1),OptMRIO=OptMRIO)[["base_dt"]]
    }else{
      Base_interm<-GAO(base_dt,k,(k+1),OptMRIO=OptMRIO)[["base_dt"]]
      Base_Out<-rbind(Base_Out,Base_interm)
      print(k)
    }
  }
  return(Base_Out)  
}

# Fonction de décomposition GAO (offshoring/reshoring/reoffshoring)
GAO <- function(base_dt,date0,date1,OptMRIO=TRUE)
{ 
  if(OptMRIO==TRUE){
    MRIO_T0<-CompoECOLEouA17(base_dt,typeCompo="OptFullOptionsBonus",date=date0)
    if(date1==2000){MRIO_T1<-CompoECOLEouA17(base_dt,typeCompo="OptFullOptionsBonus",date=date1,OptMRIO2000="LR_WIOD")}else{
      MRIO_T1<-CompoECOLEouA17(base_dt,typeCompo="OptFullOptionsBonus",date=date1)
    }
  }else{
    MRIO_T0<-CompoMRIO(base_dt,typeCompo="OptFullOptionsBonus",date=date0)
    MRIO_T1<-CompoMRIO(base_dt,typeCompo="OptFullOptionsBonus",date=date1)
  }

  # Recuperation des caractéristiques de format du mrio considéré
  List_GEO<-c("ABW","AFG","AGO","AIA","ALA","ALB","AND","ARE","ARG","ARM","ASM","ATA","ATF","ATG","AUS","AUT","AZE","BDI","BEL","BEN","BFA","BGD","BGR","BHR","BHS","BIH","BLM","BLR","BLZ","BMU","BOL","BRA","BRB","BRN","BTN","BVT","BWA","CAF","CAN","CCK","CHE","CHL","CHN","CIV","CMR","COD","COG","COK","COL","COM","CPV","CRI","CUB","CXR","CYM","CYP","CZE","DEU","DJI","DMA","DNK","DOM","DZA","ECU","EGY","ERI","ESH","ESP","EST","ETH","FIN","FJI","FLK","FRA","FRO","FSM","GAB","GBR","GEO","GGY","GHA","GIB","GIN","GLP","GMB","GNB","GNQ","GRC","GRD","GRL","GTM","GUF","GUM","GUY","HKG","HMD","HND","HRV","HTI","HUN","IDN","IMN","IND","IOT","IRL","IRN","IRQ","ISL","ISR","ITA","JAM","JEY","JOR","JPN","KAZ","KEN","KGZ","KHM","KIR","KNA","KOR","KWT","LAO","LBN","LBR","LBY","LCA","LIE","LKA","LSO","LTU","LUX","LVA","MAC","MAF","MAR","MCO","MDA","MDG","MDV","MEX","MHL","MKD","MLI","MLT","MMR","MNE","MNG","MNP","MOZ","MRT","MSR","MTQ","MUS","MWI","MYS","MYT","NAM","NCL","NER","NFK","NGA","NIC","NIU","NLD","NOR","NPL","NRU","NZL","OMN","PAK","PAN","PCN","PER","PHL","PLW","PNG","POL","PRI","PRK","PRT","PRY","PSE","PYF","QAT","REU","ROU","RUS","RWA","SAU","SDN","SEN","SGP","SGS","SHN","SJM","SLB","SLE","SLV","SMR","SOM","SPM","SRB","STP","SUR","SVK","SVN","SWE","SWZ","SYC","SYR","TCA","TCD","TGO","THA","TJK","TKL","TKM","TLS","TON","TTO","TUN","TUR","TUV","TWN","TZA","UGA","UKR","UMI","URY","USA","UZB","VAT","VCT","VEN","VGB","VIR","VNM","VUT","WLF","WSM","YEM","ZAF","ZMB","ZWE","ROW","UE_OTHERS")
  List_BR<-c("AGR_INDU","ENRJ","SERV_ABRIT","SERV_EXPO","A01","A02","A03","AtB","AZ","B","C","C1","C10-C12","C10T12","C13-C15","C13T15","C16","C17","C18","C19","C2","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C3","C30","C31_32","C31_C32","C33","C4","C5","D","D15t16","D17t19","D21t22","D23","D24","D25","D26","D27t28","D29","D30t33","D34t35","D35","DE","Dnec","E","E36","E37-E39","E37T39","F","FZ","G","G45","G46","G47","GZ","H","H49","H50","H51","H52","H53","HZ","I","I60t63","I64","IZ","J","J58","J59_60","J59_J60","J61","J62_63","J62_J63","JZ","K","K64","K65","K66","KZ","L","L68","LtQ","LZ","M69_70","M69_M70","M71","M72","M73","M74_75","M74_M75","MN","N","N77","N78","N79","N80T82","O84","OQ","P85","Q","Q86","Q87_88","R_S","R90T92","R93","RU","S94","S95","S96","T","U")
  L1<-MRIO_T0[["CI"]]$Lig_Country 
  L1<-L1[L1 %in% List_GEO]
  L1<-unique(L1)
  Nb_Countries<-length(L1)
  List_Countries<-as.list(sort(unlist(as.list(L1))))
  L2<-MRIO_T0[["CI"]]$Lig_Indus 
  L2<-L2[L2 %in% List_BR]
  L2<-unique(L2)
  Nb_Indus<-length(L2)
  List_Indus<-as.list(sort(unlist(as.list(L2))))
  Nb_IndCountries<-Nb_Countries*Nb_Indus
  print(Nb_IndCountries)
  
  # Calcul de (I-A)^-1-I    ( L - I )
  ImoinsAmoins1moinsI_T0<-as.matrix(MRIO_T0[["L"]])-diag(Nb_IndCountries)
  ImoinsAmoins1moinsI_T1<-as.matrix(MRIO_T1[["L"]])-diag(Nb_IndCountries)
  
  # Extraction de DF au format 1 colonne par pays (et total version diagonale) : passage en format matrix
  DF0<-MRIO_T0[["DF"]]
  DF0sum<-ReqSum(DF0,"Col_Indus",OptMRIO=OptMRIO)
  DF0sum_tab<-dcast(DF0sum,Lig_Country + Lig_Indus ~ Col_Country,value.var="value")
  DF_T0<-as.matrix(DF0sum_tab[,3:ncol(DF0sum_tab)])
  rownames(DF_T0)<-rownames(ImoinsAmoins1moinsI_T0)
  
  DF1<-MRIO_T1[["DF"]]
  DF1sum<-ReqSum(DF1,"Col_Indus",OptMRIO=OptMRIO)
  DF1sum_tab<-dcast(DF1sum,Lig_Country + Lig_Indus ~ Col_Country,value.var="value")
  DF_T1<-as.matrix(DF1sum_tab[,3:ncol(DF1sum_tab)])
  rownames(DF_T1)<-rownames(ImoinsAmoins1moinsI_T1)
  
  DFdiag_T0<-diag(as.vector(MRIO_T0[["DF_TOT"]]$value))
  DFdiag_T1<-diag(as.vector(MRIO_T1[["DF_TOT"]]$value))
  rownames(DFdiag_T0)<-rownames(DFdiag_T1)<-rownames(ImoinsAmoins1moinsI_T1)
  colnames(DFdiag_T0)<-colnames(DFdiag_T1)<-colnames(ImoinsAmoins1moinsI_T1)
  
  # Calcul de c0 et c1
  c0<-ImoinsAmoins1moinsI_T0 %*% DFdiag_T1
  c1<-ImoinsAmoins1moinsI_T1 %*% DFdiag_T1
  
  # Calcul (par agrégation) de c0star et c1star et de leur version matricielle par empilement
  c0star<-colSums(c0[endsWith(rownames(c0), List_Indus[[1]]),])
  for(k in 2:Nb_Indus){
    c0star<-rbind(c0star,colSums(c0[endsWith(rownames(c0), List_Indus[[k]]),]))
  }
  rownames(c0star)<-List_Indus
  
  c1star<-colSums(c1[endsWith(rownames(c1), List_Indus[[1]]),])
  for(k in 2:Nb_Indus){
    c1star<-rbind(c1star,colSums(c1[endsWith(rownames(c1), List_Indus[[k]]),]))
  }
  rownames(c1star)<-List_Indus
  
  c0star_mat<-c0star
  for(k in 2:Nb_Countries){
    c0star_mat<-rbind(c0star_mat,c0star)
  }
  c1star_mat<-c1star
  for(k in 2:Nb_Countries){
    c1star_mat<-rbind(c1star_mat,c1star)
  }
  
  # Calcul de R0p et R1p
  R0p<-c0/c0star_mat
  R1p<-c1/c1star_mat
  
  # [C0* x r1A] et [C0* x r1B]:  
  Interm_c0starR1p<-c0star_mat*R1p
  
  # r1=[C0* x r1A] -c0 : relocalisation sectorielle tirée par les inputs (variation liée à un changement de fournisseur )
  r1<-Interm_c0starR1p-c0
  r1<-as.data.frame(r1)
  rownames(r1)<-rownames(c0)
  
  # On passe à ff
  interm_ff0<-colSums(DF_T0[endsWith(rownames(DF_T0), List_Indus[[1]]),])
  for(k in 2:Nb_Indus){
    interm_ff0<-rbind(interm_ff0,colSums(DF_T0[endsWith(rownames(DF_T0), List_Indus[[k]]),]))
  }
  rownames(interm_ff0)<-List_Indus
  interm_ff0<-AddRownamesToFirstCol(interm_ff0)
  interm_ff0<-setDT(interm_ff0)
  interm_ff0<-melt(interm_ff0)
  ff0<-t(as.matrix(interm_ff0$value,drop=FALSE))
  ff0mat<-ff0
  for(k in 2:Nb_IndCountries){ff0mat<-rbind(ff0mat,ff0)}
  
  interm_ff1<-colSums(DF_T1[endsWith(rownames(DF_T1), List_Indus[[1]]),])
  for(k in 2:Nb_Indus){
    interm_ff1<-rbind(interm_ff1,colSums(DF_T1[endsWith(rownames(DF_T1), List_Indus[[k]]),]))
  }
  rownames(interm_ff1)<-List_Indus
  interm_ff1<-AddRownamesToFirstCol(interm_ff1)
  interm_ff1<-setDT(interm_ff1)
  interm_ff1<-melt(interm_ff1)
  ff1<-t(as.matrix(interm_ff1$value,drop=FALSE))
  ff1mat<-ff1
  for(k in 2:Nb_IndCountries){ff1mat<-rbind(ff1mat,ff1)}
  
  # Part de la conso finale fournies par chaque économie (cas 2 produits/ 2 pays)
  MatDiagTO<-matrix(0,nrow=Nb_IndCountries,ncol=Nb_IndCountries)  # Initialisation
  for(lig in 1:Nb_Countries){
    for(col in 1:Nb_Countries){
      MatDiagTO[(lig*Nb_Indus-Nb_Indus+1):(lig*Nb_Indus),(col*Nb_Indus-Nb_Indus+1):(col*Nb_Indus)]<-diag(as.vector(DF_T0[(lig*Nb_Indus-Nb_Indus+1):(lig*Nb_Indus),col]))
    }
  }
  MatDiagT1<-matrix(0,nrow=Nb_IndCountries,ncol=Nb_IndCountries)  # Initialisation
  for(lig in 1:Nb_Countries){
    for(col in 1:Nb_Countries){
      MatDiagT1[(lig*Nb_Indus-Nb_Indus+1):(lig*Nb_Indus),(col*Nb_Indus-Nb_Indus+1):(col*Nb_Indus)]<-diag(as.vector(DF_T1[(lig*Nb_Indus-Nb_Indus+1):(lig*Nb_Indus),col]))
    }
  }
  
  Part_T0<-MatDiagTO/ff0mat
  Part_T1<-MatDiagT1/ff1mat
  
  # r2=(Fc1-Fc0)*ff1   ( relocalisation sectorielle tirée par les produits finaux  )
  r2<-(Part_T1-Part_T0)*ff1mat
  r2<-as.data.frame(r2)
  rownames(r2)<-rownames(r2)<-rownames(ImoinsAmoins1moinsI_T1)
  colnames(r2)<-colnames(r2)<-colnames(ImoinsAmoins1moinsI_T1)
  
  # r3=((I-A0)^1-1)*r2*e  (  relocalisation sectorielle indirecte tirée par les produits finaux  )
  interm_r2<-rowSums(r2)
  for(k in 2:Nb_IndCountries){interm_r2<-rbind(interm_r2,rowSums(r2))}
  r3<-ImoinsAmoins1moinsI_T0*interm_r2
  r3<-as.data.frame(r3)
  rownames(r3)<-rownames(r3)<-rownames(ImoinsAmoins1moinsI_T1)
  colnames(r3)<-colnames(r3)<-colnames(ImoinsAmoins1moinsI_T1)
  
  # Transformation en DT et base de résultat
  r1_dt<-AddRownamesToFirstCol(r1)
  r1_dt<-setDT(r1_dt)
  r2_dt<-AddRownamesToFirstCol(r2)
  r2_dt<-setDT(r2_dt)
  r3_dt<-AddRownamesToFirstCol(r3)
  r3_dt<-setDT(r3_dt)
  r1_dt<-melt(r1_dt)
  r2_dt<-melt(r2_dt)
  r3_dt<-melt(r3_dt)
  r1_dt<-r1_dt[,gao_res:="r1"]
  r2_dt<-r2_dt[,gao_res:="r2"]
  r3_dt<-r3_dt[,gao_res:="r3"]
  base_dt<-rbind(r1_dt,r2_dt,r3_dt)
  setnames(base_dt,"joint","PR")
  setnames(base_dt,"variable","BR")
  base_dt<-base_dt[,year:=paste0(date0,"-",date1)]
  
  List_out<-list(r1=r1,r2=r2,r3=r3,base_dt=base_dt)
  return(List_out)  
}


# Fonction boucle pays et années sur les contenus en VA des exports
BouclePaysEtAnneesContVAdesExports <- function(DT,period,OptMRIO=TRUE)
{ 
  for(k in period){
    if(k==period[[1]]){ # initialisation
      Base_Out<-BouclePaysContVAdesExports(DT,k,OptMRIO=OptMRIO)
    }else{
    Base_interm<-BouclePaysContVAdesExports(DT,k,OptMRIO=OptMRIO)
    Base_Out<-rbind(Base_Out,Base_interm)
    print(k)
    }
  }
  return(Base_Out)  
}

# Fonction boucle pays sur les contenus en VA des exports
BouclePaysContVAdesExports <- function(DT,annee,OptMRIO=TRUE)
{ 
  List_Pays<-unique(DT[,"Lig_Country"])
  List_GEO<-c("ABW","AFG","AGO","AIA","ALA","ALB","AND","ARE","ARG","ARM","ASM","ATA","ATF","ATG","AUS","AUT","AZE","BDI","BEL","BEN","BFA","BGD","BGR","BHR","BHS","BIH","BLM","BLR","BLZ","BMU","BOL","BRA","BRB","BRN","BTN","BVT","BWA","CAF","CAN","CCK","CHE","CHL","CHN","CIV","CMR","COD","COG","COK","COL","COM","CPV","CRI","CUB","CXR","CYM","CYP","CZE","DEU","DJI","DMA","DNK","DOM","DZA","ECU","EGY","ERI","ESH","ESP","EST","ETH","FIN","FJI","FLK","FRA","FRO","FSM","GAB","GBR","GEO","GGY","GHA","GIB","GIN","GLP","GMB","GNB","GNQ","GRC","GRD","GRL","GTM","GUF","GUM","GUY","HKG","HMD","HND","HRV","HTI","HUN","IDN","IMN","IND","IOT","IRL","IRN","IRQ","ISL","ISR","ITA","JAM","JEY","JOR","JPN","KAZ","KEN","KGZ","KHM","KIR","KNA","KOR","KWT","LAO","LBN","LBR","LBY","LCA","LIE","LKA","LSO","LTU","LUX","LVA","MAC","MAF","MAR","MCO","MDA","MDG","MDV","MEX","MHL","MKD","MLI","MLT","MMR","MNE","MNG","MNP","MOZ","MRT","MSR","MTQ","MUS","MWI","MYS","MYT","NAM","NCL","NER","NFK","NGA","NIC","NIU","NLD","NOR","NPL","NRU","NZL","OMN","PAK","PAN","PCN","PER","PHL","PLW","PNG","POL","PRI","PRK","PRT","PRY","PSE","PYF","QAT","REU","ROU","RUS","RWA","SAU","SDN","SEN","SGP","SGS","SHN","SJM","SLB","SLE","SLV","SMR","SOM","SPM","SRB","STP","SUR","SVK","SVN","SWE","SWZ","SYC","SYR","TCA","TCD","TGO","THA","TJK","TKL","TKM","TLS","TON","TTO","TUN","TUR","TUV","TWN","TZA","UGA","UKR","UMI","URY","USA","UZB","VAT","VCT","VEN","VGB","VIR","VNM","VUT","WLF","WSM","YEM","ZAF","ZMB","ZWE","ROW","UE_OTHERS")
  List_Pays<-List_Pays[Lig_Country %in% List_GEO,]
  
  Base_Out<-ContVAdesExports(DT=DT,annee=annee,pays=as.character(List_Pays[1,1]),OptMRIO=OptMRIO) # initialisation
  
  for(k in 1:nrow(List_Pays)){
    Base_interm<-ContVAdesExports(DT=DT,annee=annee,pays=as.character(List_Pays[k,1]),OptMRIO=OptMRIO)  
    Base_Out<-rbind(Base_Out,Base_interm)
  }
  return(Base_Out)  
}

# Fonction de calcul du contenu en VA des eXports pour un pays donné et une année donnée
ContVAdesExports <- function(DT,annee,pays,OptMRIO=TRUE)  # OptMRIO si on est sur A17 ou ECOLE
{
  if(OptMRIO==TRUE){
    Base_depart<-CompoECOLEouA17(DT,"OptFullOptionsBonus",date=annee)
  }else{
    Base_depart<-CompoMRIO(DT,"OptFullOptionsBonus",date=annee)
  }
  
  # Cadre de départ pour y placer les composantes du calcul (même format que la DF)
  Cadre<-Base_depart[["DF"]]
  Cadre$value<-0
  Cadre<-ReqSum(Cadre,"Col_Indus",OptMRIO=OptMRIO) # on agrège les composantes de demande finale
  Cadre$Col_Indus<-"TOTDF"  # On requalifie la colonne Col_Indus
  Cadre_tab<-dcast(Cadre, Lig_Country + Lig_Indus ~ Col_Country, value.var = "value") 
  
  # Calcul des exportations du pays FRA
  BaseCIDF<-rbind(Base_depart[["CI"]],Base_depart[["DF"]])
  BaseSelect<-BaseCIDF[Lig_Country==pays & Col_Country!=pays,]
  BaseSelect<-ReqSum(BaseSelect,c("Col_Country","Col_Indus"),OptMRIO=OptMRIO) # on agrège les composantes de demande finale
  BaseDom<-BaseSelect
  BaseDom$Col_Country<-pays
  BaseDom$Col_Indus<-"TOTDF"
  
  # Calcul des (en négatif) des exports des autres pays vers FRA
  BaseSelect<-BaseCIDF[Lig_Country!=pays & Col_Country==pays,]
  BaseSelect<-ReqSum(BaseSelect,c("Col_Indus"),OptMRIO=OptMRIO) # on agrège les composantes de demande finale
  BaseSelect$value<--BaseSelect$value
  BaseXversDom<-BaseSelect
  BaseXversDom$Col_Country<-BaseXversDom$Lig_Country
  BaseXversDom$Col_Indus<-"TOTDF"
  
  # Rajout des composantes au cadre et agrégation pour intégrer les composantes au cadre
  BaseTot<-rbind(BaseDom,BaseXversDom)
  Cadre_tab<-dcast(BaseTot, Lig_Country + Lig_Indus ~ Col_Country, value.var = "value") 
  Cadre_tab[is.na(Cadre_tab),]<-0  # Pour le hors diagonal qui ressort a NA
  
  # Definition des matrices intervenant dans le produit matriciel
  stressor<-as.matrix(Base_depart[["VA"]]$value,drop=FALSE)
  Prod<-as.matrix(Base_depart[["PROD"]]$value,drop=FALSE)
  Taux_Stressor<-stressor/Prod
  Taux_Stressor<-as.vector(Taux_Stressor)
  part1<-diag(Taux_Stressor)
  part2<-as.matrix(Base_depart[["L"]])
  part3<-as.matrix(Cadre_tab[,3:ncol(Cadre_tab)])
  MatContVAExports<-CFPcalculationRCPP(part1,part2,part3)
  MatContVAExports<-as.data.frame(MatContVAExports)
  MatContVAExports<-GereInfNA(MatContVAExports)
  rownames(MatContVAExports)<-paste0(Base_depart[["DF_tab"]]$Lig_Country,"_",Base_depart[["DF_tab"]]$Lig_Indus)
  colnames(MatContVAExports)<-unique(Base_depart[["DF_tab"]]$Lig_Country)
  MatContVAExports<-AddRownamesToFirstCol(MatContVAExports)
  
  MatContVAExports<-setDT(MatContVAExports)
  ComposantesVAinX<-melt(MatContVAExports)  
  setnames(ComposantesVAinX,"joint","PR")
  setnames(ComposantesVAinX,"variable","BR")
  ComposantesVAinX<-SplitPRBR(ComposantesVAinX)
  ComposantesVAinX[,Col_Indus:=NULL]  # pas utile et a vide
  
  DVAiX<-ComposantesVAinX[Lig_Country==pays & Col_Country==pays,][,Col_Indus:="TOTAL"][,Compo:="DVAiX"]
  DVAiM<-ComposantesVAinX[Lig_Country==pays & Col_Country!=pays,][,Col_Indus:="TOTAL"][,Compo:="DVAiM"]
  FVAiX<-ComposantesVAinX[Lig_Country!=pays & Col_Country==pays,][,Col_Indus:="TOTAL"][,Compo:="FVAiX"]
  FVAiM<-ComposantesVAinX[Lig_Country!=pays & Col_Country!=pays,][,Col_Indus:="TOTAL"][,Compo:="FVAiM"]
  BVAiM<-ComposantesVAinX[Lig_Country!=pays & Col_Country!=pays & Lig_Country==Col_Country,][,Col_Indus:="TOTAL"][,Compo:="BVAiM"]
  
  # Ajout des données d'exportations et de VA  pour dénominateur des ratio
  BaseCIDF<-rbind(Base_depart[["CI"]],Base_depart[["DF"]])
  BaseExport<-BaseCIDF[Lig_Country==pays & Col_Country!=pays,]
  BaseExport<-ReqSum(BaseExport,c("Col_Country","Col_Indus"),OptMRIO=OptMRIO) 
  BaseExport<-BaseExport[,Col_Indus:="TOTAL"][,Col_Country:="TOTAL"][,Compo:="Export"][,CountryREF:=pays]   
  setnames(BaseExport,"year","yearREF" )
  BaseVA<-Base_depart[["VA"]][Col_Country==pays,][,Lig_Indus:="TOTAL"][,Lig_Country:="TOTAL"][,Compo:="VA"][,CountryREF:=pays]   
  if(OptMRIO==TRUE){ # On drop l'info sur le MRIO 
    BaseExport<-BaseExport[,MRIO:=NULL]
    BaseVA<-BaseVA[,MRIO:=NULL]
    }   
  setnames(BaseVA,"year","yearREF" )

  # Calcul des indicateurs clés
  DVAiM_PR<-DVAiM[,sum(value),by=Lig_Indus][,Col_Indus:="TOTAL"][,Col_Country:="TOTAL"][,Lig_Country:="TOTAL"][,Compo:="DVAiM_PR"]  
  setnames(DVAiM_PR,"V1","value")
  FVAiX_PR<-FVAiX[,sum(value),by=Lig_Country][,Col_Indus:="TOTAL"][,Lig_Indus:="TOTAL"][,Col_Country:="TOTAL"][,Compo:="FVAiX_PR"]
  setnames(FVAiX_PR,"V1","value")
  FVAiM_PR<-FVAiM[,sum(value),by=Lig_Indus][,Col_Indus:="TOTAL"][,Lig_Country:="TOTAL"][,Col_Country:="TOTAL"][,Compo:="FVAiM_PR"]
  setnames(FVAiM_PR,"V1","value")
  BVAiM_PR<-BVAiM[,sum(value),by=Lig_Indus][,Col_Indus:="TOTAL"][,Lig_Country:="TOTAL"][,Col_Country:="TOTAL"][,Compo:="BVAiM_PR"]
  setnames(BVAiM_PR,"V1","value")
  VAXnoguera<-DVAiX # initialise
  VAXnoguera$value<-as.data.frame(DVAiX$value+DVAiM_PR$value,drop=FALSE)
  VAXnoguera<-setDT(VAXnoguera)
  VAXnoguera<-VAXnoguera[,Col_Indus:="TOTAL"][,Compo:="VAXnoguera_PR"]
  
  
  DVAiX_tot<-DVAiX[,sum(value),by=Compo][,Col_Indus:="TOTAL"][,Col_Country:="TOTAL"][,Lig_Indus:="TOTAL"][,Lig_Country:="TOTAL"][,Compo:="DVAiX_tot"]  
  setnames(DVAiX_tot,"V1","value")
  DVAiM_tot<-DVAiM_PR[,sum(value),by=Compo][,Col_Indus:="TOTAL"][,Col_Country:="TOTAL"][,Lig_Indus:="TOTAL"][,Lig_Country:="TOTAL"][,Compo:="DVAiM_tot"]  
  setnames(DVAiM_tot,"V1","value")
  FVAiX_tot<-FVAiX_PR[,sum(value),by=Compo][,Col_Indus:="TOTAL"][,Col_Country:="TOTAL"][,Lig_Indus:="TOTAL"][,Lig_Country:="TOTAL"][,Compo:="FVAiX_tot"]  
  setnames(FVAiX_tot,"V1","value")
  FVAiM_tot<-FVAiM_PR[,sum(value),by=Compo][,Col_Indus:="TOTAL"][,Col_Country:="TOTAL"][,Lig_Indus:="TOTAL"][,Lig_Country:="TOTAL"][,Compo:="FVAiM_tot"]  
  setnames(FVAiM_tot,"V1","value")
  BVAiM_tot<-BVAiM_PR[,sum(value),by=Compo][,Col_Indus:="TOTAL"][,Col_Country:="TOTAL"][,Lig_Indus:="TOTAL"][,Lig_Country:="TOTAL"][,Compo:="BVAiM_tot"]  
  setnames(BVAiM_tot,"V1","value")
  VAXnoguera_tot<-VAXnoguera[,sum(value),by=Compo][,Col_Indus:="TOTAL"][,Col_Country:="TOTAL"][,Lig_Indus:="TOTAL"][,Lig_Country:="TOTAL"][,Compo:="VAXnoguera_tot"]  
  setnames(VAXnoguera_tot,"V1","value")
  
  Base_Out<-rbind(DVAiX,DVAiM,FVAiX,FVAiM,BVAiM,DVAiM_PR,FVAiX_PR,FVAiM_PR,BVAiM_PR,VAXnoguera,DVAiX_tot,DVAiM_tot,FVAiX_tot,FVAiM_tot,BVAiM_tot,VAXnoguera_tot)
  Base_Out$CountryREF<-pays
  Base_Out$yearREF<-annee
  Base_Out<-rbind(Base_Out,BaseExport,BaseVA) # Ajout des données d'export et de VA pour les dénominateurs d'indicateurs ratio
  return(Base_Out)
}  


# Fonction de requetage pour caluler une somme à partir d'une liste des dimensions à sommer
ReqSum_test <- function(DT,ListDimASommer,OptMRIO=TRUE,OptStruct=FALSE)  # OptMRIO si on est sur A17 ou ECOLE
{
  
    ListDepart<-as.list(colnames(DT))

  if(OptStruct==FALSE){
    ListHorsDimASommer<-ListDepart[!(ListDepart %in% ListDimASommer)]
    ListHorsDimASommer<-as.character(ListHorsDimASommer)
    setkeyv(DT,ListHorsDimASommer)
    DT_sum<-DT[, sum(value),by=ListHorsDimASommer]
    DT_sum[,value:=NULL]
    setnames(DT_sum,"V1","value")
    interm<-DT_sum
  }else{
    ListHorsDimASommer<-ListDepart[!(ListDepart %in% ListDimASommer)]
    ListHorsDimASommer<-as.character(ListHorsDimASommer)
    setkeyv(DT,ListHorsDimASommer)
    DT_sum<-DT[, sum(value),by=ListHorsDimASommer]
    DT_sum<-DT_sum[DT,on=ListHorsDimASommer]
    DT_sum[,"value"]<-DT_sum[,"value"]/DT_sum[,"V1"]
    DT_sum<-GereInfNA(DT_sum)
    DT_sum<-DT_sum[,V1:=NULL]
    interm<-DT_sum
  }
  return(interm)
}  

# Fonction de requetage pour caluler une somme à partir d'une liste des dimensions à sommer
ReqSum <- function(DT,ListDimASommer,OptMRIO=TRUE,OptStruct=FALSE)  # OptMRIO si on est sur A17 ou ECOLE
{
  if(OptMRIO==TRUE){
    ListDepart<-list("year","Lig_Indus","Lig_Country","Col_Indus","Col_Country","MRIO")
  }else{
    ListDepart<-list("year","Lig_Indus","Lig_Country","Col_Indus","Col_Country")
  }
  
  if(OptStruct==FALSE){
    ListHorsDimASommer<-ListDepart[!(ListDepart %in% ListDimASommer)]
    ListHorsDimASommer<-as.character(ListHorsDimASommer)
    setkeyv(DT,ListHorsDimASommer)
    DT_sum<-DT[, sum(value),by=ListHorsDimASommer]
    setnames(DT_sum,"V1","value")
    interm<-DT_sum
  }else{
    ListHorsDimASommer<-ListDepart[!(ListDepart %in% ListDimASommer)]
    ListHorsDimASommer<-as.character(ListHorsDimASommer)
    setkeyv(DT,ListHorsDimASommer)
    DT_sum<-DT[, sum(value),by=ListHorsDimASommer]
    DT_sum<-DT_sum[DT,on=ListHorsDimASommer]
    DT_sum[,"value"]<-DT_sum[,"value"]/DT_sum[,"V1"]
    DT_sum<-GereInfNA(DT_sum)
    DT_sum<-DT_sum[,V1:=NULL]
    interm<-DT_sum
  }
  return(interm)
}  

# Fonction boucle années sur les MADE-IN
BoucleAnneesMADEINs <- function(DT,period,OptMRIO=TRUE)
{ 
  for(k in period){
    if(k==period[[1]]){ # initialisation
      Base_Out<-MadeIn(DT,k,OptMRIO=OptMRIO)[["ratio_dt"]]
    }else{
      Base_interm<-MadeIn(DT,k,OptMRIO=OptMRIO)[["ratio_dt"]]
      Base_Out<-rbind(Base_Out,Base_interm)
      print(k)
    }
  }
  return(Base_Out)  
}

# Fonction de calcul du MADE-IN
MadeIn<- function(base_dt,annee,OptMRIO=TRUE)  
{
  if(OptMRIO==TRUE){
    List_Interm<-CompoECOLEouA17(base_dt,"OptFullOptions",date=annee)
  }else{
    List_Interm<-CompoMRIO(base_dt,"OptFullOptions",date=annee)
  }
  List_Contenus<-Contenus(List_Interm,typeContenu="VA",MethContenu="MatDF")  # typeContenu=VA;CO2;Emploi   MethContenu=MatDF;DiagDFtot
  
  interm<-List_Contenus[["MatEmpreinte_dt"]] 
  denom<-interm[,sum(value),by=.(Lig_Indus,Col_Country)]
  num<-interm[Col_Country==Lig_Country,]
  num<-num[,sum(value),by=.(Lig_Indus,Col_Country)]
  
  # Calcul du ratio Made-In ventilé par branches
  ratio_parBR<-denom[num,on=.(Lig_Indus,Col_Country)] 
  ratio_parBR[,"value"]<-ratio_parBR[,"i.V1"]/ratio_parBR[,"V1"]
  ratio_parBR<-GereInfNA(ratio_parBR)
  Ordre_Col<-c("Lig_Indus","Col_Country","value")
  ratio_parBR<-ratio_parBR[,..Ordre_Col]  # selection des colonnes qui nous intéressent
  ratio_parBR<-ratio_parBR[,year:=annee]
  
  # Calcul du ratio Made-In total par pays
  ratio_Tot<-denom[num,on=.(Lig_Indus,Col_Country)] 
  ratio_Tot<-ratio_Tot[,.(sum(V1),sum(i.V1)), by=c("Col_Country")] 
  ratio_Tot[,"value"]<-ratio_Tot[,"V2"]/ratio_Tot[,"V1"]
  ratio_Tot<-GereInfNA(ratio_Tot)
  Ordre_Col<-c("Col_Country","value")
  ratio_Tot<-ratio_Tot[,..Ordre_Col]  # selection des colonnes qui nous intéressent
  ratio_Tot<-ratio_Tot[,year:=annee][,Lig_Indus:="TOTAL"]
    
  ratio_dt<-rbind(ratio_Tot,ratio_parBR)
    
  # Ventilation par pays de provenance
  ratio_parBR_tab<-dcast(ratio_parBR, Lig_Indus ~ Col_Country, value.var = "value") 

  # Sorties
  interm<-list(ratio_Tot=ratio_Tot,ratio_parBR_tab=ratio_parBR_tab,ratio_parBR=ratio_parBR,ratio_dt=ratio_dt)
  return(interm)
}

# FIGARO : Fonction de calcul simultané contenu en émissions et contenus en VA
CalcContenusEmiVA<- function(annee){
  mrio_fig22 <- fread(paste0(Path_FIG22ixi,"matrix_eu-ic-io_ind-by-ind_",annee,".csv"))
  emi_fig22 <- fread(paste0(Path_FIG22emi,"CO2Footprint_",annee,"_20220404.csv"))
  Vect_emi<-emi_fig22[,sum(obs_value),by=c("ref_area","industry")]  
  # Ajout de la production par calcul
  mrio_fig22$Prod<-rowSums(mrio_fig22[,2:3175])
  # Ajout d'une variable de jointure pays * BR
  Vect_emi$rowLabels<-paste0(Vect_emi$ref_area,"_",Vect_emi$industry)
  # Ajout des émissions par jointure : Merge du MRIO et des émissions
  MergeMRIO<-mrio_fig22[Vect_emi, on=.(rowLabels)] # on=.(ref_area,industry)
  print(sum(MergeMRIO[which(substr(MergeMRIO$rowLabels,1,3)=='FR_'),"V1"]))
  # Transformation en DF
  Tab<-GetRownamesFromFirstCol(MergeMRIO)
  # Attribution des composantes du MRIO : 2944 ignes*col
  TEI<-Tab[1:2944,1:2944]
  TEI<-GereInfNA(TEI)
  Prod<-vectDF(Tab[1:2944,"Prod"])
  rownames(Prod)<-rownames(Tab[1:2944,])
  Emi<-vectDF(Tab[1:2944,"V1"])
  rownames(Emi)<-rownames(Tab[1:2944,])
  FD<-vectDF(rowSums(Tab[1:2944,2945:3174]))
  VApxBase<-GereInfNA(Prod-vectDF(colSums(TEI,na.rm=TRUE)))
  rownames(VApxBase)<-rownames(Tab[1:2944,]) 
  # Calcul de la matrice 'empreinte'
  part1<-rbind(as.matrix(diag(Emi$vect/Prod$vect)),as.matrix(diag(VApxBase$vect/Prod$vect)))
  part2_1<-diag(2944)
  part2_2<-as.matrix(TEI[1:2944,1:2944])
  part2_3<-as.matrix(diag(1/Prod$vect))
  part2_3<-GereInfNA(part2_3)
  part2_4<-Mult2_rcpp3(part2_2,part2_3)
  part2_4<-GereInfNA(part2_4)
  part_synth<-part2_1-part2_4
  part_synth<-GereInfNA(part_synth)
  part_synth<-as.matrix(part_synth)
  part2<-inversion_rcpp3(part_synth)
  test<-solve(part_synth)
  part3<-as.matrix(diag(FD$vect))
  # Calcul de la matrice croisant Pays*BR de provenance vers Pays*BR de destination
  MatEmpreinte<-CFPcalculationRCPP(part1,part2,part3)
  MatEmpreinte<-GereInfNA(MatEmpreinte)
  # Calcul de l'empreinte CO2 en approche 'consommation'
  Empreinte_Conso<-vectDF(colSums(MatEmpreinte[1:2944,1:2944]))
  Contenus_VA<-vectDF(colSums(MatEmpreinte[2945:5888,1:2944]))
  Contenus<-cbind(Empreinte_Conso,Contenus_VA)
  rownames(Contenus)<-rownames(Tab[1:2944,]) 
  Contenus_FRA<-Contenus[which(substr(rownames(Contenus),1,3)=='FR_'),]
  rownames(Contenus_FRA)<-rownames(Tab[which(substr(rownames(Contenus),1,3)=='FR_'),]) 
  colnames(Contenus)<-c("Empreinte_CO2","Contenus_VA")
  return(Contenus_FRA)
}

# Fonction de calcul des contenus (en VA, émissions CO2, emploi)
Contenus<- function(List_dt,typeContenu,MethContenu="MatDF",EmprPays="FRA")  
  # typeContenu=VA;CO2;Emploi   MethContenu=MatDF;DiagDFtot;DiagDFpays
{
  # Extraction de la liste des composantes du calcul, et conversion en matrix
  
  # Production
  Prod<-List_dt[["PROD"]]
  AnneeProd_pourJointureEmi<-Prod[1,year]
  Prod$PR<-paste0(Prod$Lig_Country,"_",Prod$Lig_Indus)
  Prod<-Prod[,c("PR","value")]
  setcolorder(Prod, c("PR", setdiff(names(Prod), "PR")))
  Prod<-setDF(Prod)
  Prod<-GetRownamesFromFirstCol(Prod)
  Prod<-as.matrix(Prod)
  
  # CI
  CI<-List_dt[["CI_tab"]]
  CI$PR<-paste0(CI$Lig_Country,"_",CI$Lig_Indus)
  CI<-CI[,c("year","Lig_Country","Lig_Indus"):=NULL]
  setcolorder(CI, c("PR", setdiff(names(CI), "PR")))
  CI<-setDF(CI)
  CI<-GetRownamesFromFirstCol(CI)
  CI<-as.matrix(CI)

  # A
  A<-List_dt[["A_tab"]]
  A$PR<-paste0(A$Lig_Country,"_",A$Lig_Indus)
  A<-A[,c("year","Lig_Country","Lig_Indus"):=NULL]
  setcolorder(A, c("PR", setdiff(names(A), "PR")))
  A<-setDF(A)
  A<-GetRownamesFromFirstCol(A)
  A<-as.matrix(A)
  
  # DF
  DF<-List_dt[["DF_tab"]]
  DF$PR<-paste0(DF$Lig_Country,"_",DF$Lig_Indus)
  DF<-DF[,c("year","Lig_Country","Lig_Indus"):=NULL]
  setcolorder(DF, c("PR", setdiff(names(DF), "PR")))
  DF<-setDF(DF)
  DF<-GetRownamesFromFirstCol(DF)
  DF<-as.matrix(DF) 
  
  # DF_TOT
  DF_TOT<-List_dt[["DF_TOT"]]
  DF_TOT$PR<-paste0(DF_TOT$Lig_Country,"_",DF_TOT$Lig_Indus)
  DF_TOT<-DF_TOT[,c("PR","value")]
  setcolorder(DF_TOT, c("PR", setdiff(names(DF_TOT), "PR")))
  DF_TOT<-setDF(DF_TOT)
  DF_TOT<-GetRownamesFromFirstCol(DF_TOT)
  DF_TOT<-as.matrix(DF_TOT)
  
  # DF_EmprPays
  DF_EmprPays<-List_dt[["DF"]][Col_Country==EmprPays,]
  DF_EmprPays<-DF_EmprPays[,sum(value),by=c("year","Lig_Indus","Lig_Country","Col_Country")]
  setnames(DF_EmprPays,"V1","value")
  DF_EmprPays<-dcast(DF_EmprPays,year+Lig_Country+Lig_Indus~Col_Country,value.var = "value")
  DF_EmprPays$PR<-paste0(DF_EmprPays$Lig_Country,"_",DF_EmprPays$Lig_Indus)
  DF_EmprPays<-DF_EmprPays[,c("year","Lig_Country","Lig_Indus"):=NULL]
  setcolorder(DF_EmprPays, c("PR", setdiff(names(DF_EmprPays), "PR")))
  DF_EmprPays<-setDF(DF_EmprPays)
  DF_EmprPays<-GetRownamesFromFirstCol(DF_EmprPays)
  DF_EmprPays<-as.matrix(DF_EmprPays) 
  
  # VA
  VA<-List_dt[["VA"]]
  VA$PR<-paste0(VA$Col_Country,"_",VA$Col_Indus)
  VA<-VA[,c("PR","value")]
  setcolorder(VA, c("PR", setdiff(names(VA), "PR")))
  VA<-setDF(VA)
  VA_pourJointureEmi<-VA
  VA<-GetRownamesFromFirstCol(VA)
  VA<-as.matrix(VA)
  
  # Taille des matrices
  nb_ligcol<-nrow(Prod)
  
  # Identification et récupération du stressor
  if(typeContenu=="VA"){
    stressor<-VA
  }
  if(typeContenu=="CO2"){
    stressor<-RecupVectEmiFigaro(annee=AnneeProd_pourJointureEmi,vectREF=VA_pourJointureEmi) # 9999 signifie qu'on recupère automatiquement l'année par le datatable de la production
  }
  if(typeContenu=="Emploi"){
    # On calcule dans un premier temps les contenus en VA, que l'on multiplie ensuite par les intensités en emplois (calculées par méthode SRIO et justes dispo pour FR et DE)
    # Voir dans le notebook Spatatable pour le détail de la cnstruction de cette fonction
    # Pour les années hors 2010-2018 on prends les ratio de 2010 ou de 2018
    if(AnneeProd_pourJointureEmi<2010){
      anneeREF<-2010
    }
    if(AnneeProd_pourJointureEmi>2018){
      anneeREF<-2018
    }
    if(AnneeProd_pourJointureEmi<=2018 & AnneeProd_pourJointureEmi>=2010){
      anneeREF<-AnneeProd_pourJointureEmi
    }
    # Calcul de l'empreinte VA de la FRA en mode consommation et non plus production comme avant
    Interm_Contenu<-Contenus(MRIO,typeContenu="VA",MethContenu="DiagDFpays",EmprPays="FRA")
    Effet_ApprocheConso<-Interm_Contenu[["Empreinte_Conso"]]
    Effet_ApprocheConso<-AddRownamesToFirstCol(Effet_ApprocheConso)
    Effet_ApprocheConso<-setDT(Effet_ApprocheConso)
    Multiplicateurs<-readRDS("Sorties/Serie_EmploiSurVA_Multi_2010-2018_ApprConso.rds")
    Multi<-Multiplicateurs[variable==anneeREF & TypData=="Multiplicateur",]
    Multi<-Multi[Country=="FR",Country:="FRA"][Country=="DE",Country:="DEU"]
    Multi<-Multi[,joint:=sub('.*CPA_', '', Multi$joint)]
    Multi<-Multi[,joint:=paste0(Multi$Country,"_",Multi$joint)]
    # Attribution des coefficients et calcul de l'emploi résultat de la variante
    Attrib<-Effet_ApprocheConso[Multi,on=.(joint=joint),  nomatch=0]
    Attrib<-Attrib[,deltaEmplois:=Attrib$vect*Attrib$value*1000][,c("joint","deltaEmplois")]
    Attrib<-Attrib[,deltaEmplois:=round(deltaEmplois,5)]
    Attrib_Conso<-Attrib[order(-abs(deltaEmplois))]
    
    #         Calcul en approche PRODUCTION
    Effet_ApprocheProd<-Interm_Contenu[["Empreinte_Production"]]
    Effet_ApprocheProd<-AddRownamesToFirstCol(Effet_ApprocheProd)
    Effet_ApprocheProd<-setDT(Effet_ApprocheProd)
    Multiplicateurs<-readRDS("Sorties/Serie_EmploiSurVA_Multi_2010-2018_ApprProd.rds")
    Multi<-Multiplicateurs[variable==anneeREF & TypData=="Multiplicateur",]
    Multi<-Multi[Country=="FR",Country:="FRA"][Country=="DE",Country:="DEU"]
    Multi<-Multi[,joint:=sub('.*CPA_', '', Multi$joint)]
    Multi<-Multi[,joint:=paste0(Multi$Country,"_",Multi$joint)]
    # Attribution des coefficients et calcul de l'emploi résultat de la variante
    Attrib<-Effet_ApprocheProd[Multi,on=.(joint=joint),  nomatch=0]
    #Attrib<-Attrib[,deltaEmplois:=round(Attrib$vect*Attrib$value*1000,2)][,c("joint","deltaEmplois")]
    Attrib<-Attrib[,deltaEmplois:=Attrib$vect*Attrib$value*1000][,c("joint","deltaEmplois")]
    Attrib<-Attrib[,deltaEmplois:=round(deltaEmplois,2)]
    Attrib_Prod<-Attrib[order(-abs(deltaEmplois))]
    ######################################### Sorties
    return(list(ContEmplois_Appr_Prod=Attrib_Prod,ContEmplois_Appr_Conso=Attrib_Conso))
    
  }
  
  # Composantes du calcul
  Taux_Stressor<-stressor/Prod
  Taux_Stressor<-as.vector(Taux_Stressor)
  part1<-diag(Taux_Stressor)
  part2_1<-diag(nb_ligcol)
  part_synth<-part2_1-A
  part_synth<-GereInfNA(part_synth)
  part_synth<-as.matrix(part_synth)
  part2<-inversion_rcpp3(part_synth)
  
  if(MethContenu=="MatDF"){
    part3<-as.matrix(DF)
  }
  if(MethContenu=="DiagDFtot"){
    part3<-as.matrix(diag(DF_TOT[,1]))
  }
  if(MethContenu=="DiagDFpays"){
    part3<-as.matrix(diag(DF_EmprPays[,1]))
  }
  
  #### Calcul
  #    Calcul de la matrice croisant Pays*BR de provenance vers Pays*BR de destination
  MatEmpreinte<-CFPcalculationRCPP(part1,part2,part3)
  if(MethContenu=="MatDF"){
    rownames(MatEmpreinte)<-rownames(CI)
    colnames(MatEmpreinte)<-colnames(DF)
  }
  if(MethContenu=="DiagDFtot" | MethContenu=="DiagDFpays"){
    rownames(MatEmpreinte)<-rownames(CI)
    colnames(MatEmpreinte)<-colnames(CI)
  }
  
  MatEmpreinte<-as.data.frame(MatEmpreinte)
  MatEmpreinte_out<-MatEmpreinte
  MatEmpreinte<-AddRownamesToFirstCol(MatEmpreinte)
  MatEmpreinte[,2:ncol(MatEmpreinte)]<-GereInfNA(MatEmpreinte[,2:ncol(MatEmpreinte)])
  
  MatEmpreinte_dt<-setDT(MatEmpreinte)
  MatEmpreinte_dt<-melt(MatEmpreinte_dt)
  setnames(MatEmpreinte_dt,"joint","PR")
  setnames(MatEmpreinte_dt,"variable","BR")
  MatEmpreinte_dt<-SplitPRBR(MatEmpreinte_dt)
  
  if(MethContenu=="DiagDFtot"| MethContenu=="DiagDFpays"){
    #    Calcul contenus en approche 'production'
    Empreinte_Production<-vectDF(rowSums(MatEmpreinte[,2:ncol(MatEmpreinte)]))
    rownames(Empreinte_Production)<-rownames(CI)
    
    #    Calcul contenus en approche 'consommation'
    Empreinte_Conso<-vectDF(colSums(MatEmpreinte[,2:ncol(MatEmpreinte)]))
    rownames(Empreinte_Conso)<-rownames(CI)
  }
  
  
  # Sorties 
  #MatEmpreinte<-CFPcalculationRCPP(part1,part2,part3)
  if(MethContenu=="MatDF"){
    interm<-list(MatEmpreinte=MatEmpreinte_out,MatEmpreinte_dt=MatEmpreinte_dt)
  }
  if(MethContenu=="DiagDFtot"| MethContenu=="DiagDFpays"){
    interm<-list(Empreinte_Conso=Empreinte_Conso,Empreinte_Production=Empreinte_Production,MatEmpreinte=MatEmpreinte_out,MatEmpreinte_dt=MatEmpreinte_dt)
  }
  
  return(interm)
}


# Fonction HEM (hypothetical extraction method / A la Dietzenbacher sur MRIO)
HEM<- function(dt_mrio,annee,extract_country,extract_indus,verboseCheck=FALSE,OptMRIO=TRUE,OptAmontAval="ALL")  
{
  #	On place à 0 la ligne et la colonne concernée par l'extraction, mais à la façon Dietzenbacher en compensant
  #    les importations des autres pays en ce produit par un switch sur les autres produits 
  #	Les calculs sont transversaux au TEI et FD : c'est tout le produit qui est concerné uniformément.
  
  Base_init<-dt_mrio[year==annee,]
  
  # Extraction des composantes nécessaires pour le calcul final de l'indicateur HEM
  if(OptMRIO==TRUE){
    Prod<-CompoECOLEouA17(Base_init,"PROD",date=annee)
  }else{
    Prod<-CompoMRIO(Base_init,"PROD",date=annee)
  }
  Prod<-Prod[,Col_Country:="TOTAL"][,Col_Indus:="PROD"]
  Prod<-AjoutPRBR(Prod)
  List_GEO<-c("ABW","AFG","AGO","AIA","ALA","ALB","AND","ARE","ARG","ARM","ASM","ATA","ATF","ATG","AUS","AUT","AZE","BDI","BEL","BEN","BFA","BGD","BGR","BHR","BHS","BIH","BLM","BLR","BLZ","BMU","BOL","BRA","BRB","BRN","BTN","BVT","BWA","CAF","CAN","CCK","CHE","CHL","CHN","CIV","CMR","COD","COG","COK","COL","COM","CPV","CRI","CUB","CXR","CYM","CYP","CZE","DEU","DJI","DMA","DNK","DOM","DZA","ECU","EGY","ERI","ESH","ESP","EST","ETH","FIN","FJI","FLK","FRA","FRO","FSM","GAB","GBR","GEO","GGY","GHA","GIB","GIN","GLP","GMB","GNB","GNQ","GRC","GRD","GRL","GTM","GUF","GUM","GUY","HKG","HMD","HND","HRV","HTI","HUN","IDN","IMN","IND","IOT","IRL","IRN","IRQ","ISL","ISR","ITA","JAM","JEY","JOR","JPN","KAZ","KEN","KGZ","KHM","KIR","KNA","KOR","KWT","LAO","LBN","LBR","LBY","LCA","LIE","LKA","LSO","LTU","LUX","LVA","MAC","MAF","MAR","MCO","MDA","MDG","MDV","MEX","MHL","MKD","MLI","MLT","MMR","MNE","MNG","MNP","MOZ","MRT","MSR","MTQ","MUS","MWI","MYS","MYT","NAM","NCL","NER","NFK","NGA","NIC","NIU","NLD","NOR","NPL","NRU","NZL","OMN","PAK","PAN","PCN","PER","PHL","PLW","PNG","POL","PRI","PRK","PRT","PRY","PSE","PYF","QAT","REU","ROU","RUS","RWA","SAU","SDN","SEN","SGP","SGS","SHN","SJM","SLB","SLE","SLV","SMR","SOM","SPM","SRB","STP","SUR","SVK","SVN","SWE","SWZ","SYC","SYR","TCA","TCD","TGO","THA","TJK","TKL","TKM","TLS","TON","TTO","TUN","TUR","TUV","TWN","TZA","UGA","UKR","UMI","URY","USA","UZB","VAT","VCT","VEN","VGB","VIR","VNM","VUT","WLF","WSM","YEM","ZAF","ZMB","ZWE","ROW","UE_OTHERS")
  Base_init<-Base_init[Lig_Country %in% List_GEO & Col_Country %in% List_GEO, ]
  
  #	Etapes du calcul HEM :
  
  #	1/ On extrait le Lig_Indus de Lig_Country concerné et on le sanctuarise. On 
  #        obtient un montant à ventiler pour chaque colonne du MRIO.
  Delta<-Base_init[Lig_Indus==extract_indus & Lig_Country==extract_country,]
  
  #	2/ On extrait les Lig_Indus de tous les pays hors le pays extrait en ligne et hors parties 
  #        domestiques (Lig_country==Col_Country)
  Lig_Ventil<-Base_init[Lig_Indus==extract_indus & Lig_Country!=extract_country,]
  
  #	3/ On calcule directmeent en requêtes le total puis la structure (on le fait directement par 
  #        requetage pour eviter toute adherence aux formats).
  if(OptMRIO==TRUE){
    Lig_Ventil_totaux<-Lig_Ventil[,sum(value),by=.(year,Lig_Indus,Col_Indus,Col_Country,MRIO)]
    Lig_Ventil_totaux<-Lig_Ventil_totaux[Lig_Ventil,on=.(year,Lig_Indus,Col_Indus,Col_Country,MRIO)]
  }else{
    Lig_Ventil_totaux<-Lig_Ventil[,sum(value),by=.(year,Lig_Indus,Col_Indus,Col_Country)]
    Lig_Ventil_totaux<-Lig_Ventil_totaux[Lig_Ventil,on=.(year,Lig_Indus,Col_Indus,Col_Country)]
  }
  Lig_Ventil_totaux[,"value"]<-Lig_Ventil_totaux[,"value"]/Lig_Ventil_totaux[,"V1"]
  
  if(OptMRIO==TRUE){Ordre_Col<-c("year","Lig_Country","Col_Country","Lig_Indus","Col_Indus","value","MRIO")
  }else{Ordre_Col<-c("year","Lig_Country","Col_Country","Lig_Indus","Col_Indus","value")}
  Lig_Ventil_totaux<-Lig_Ventil_totaux[,..Ordre_Col]  # reordonne les colonnes
  Lig_Ventil_totaux<-GereInfNA(Lig_Ventil_totaux)
  
  if(verboseCheck==TRUE){ # check : On verifie que la structure somme bien à 1
    print("On verifie que la structure somme bien à 1 :")
    if(OptMRIO==TRUE){ print(Lig_Ventil_totaux[,sum(value),by=.(year,Lig_Indus,Col_Indus,Col_Country,MRIO)]) 
    }else{ print(Lig_Ventil_totaux[,sum(value),by=.(year,Lig_Indus,Col_Indus,Col_Country)]) }
  }
  
  #	4/ Ensuite on ventile le montant du point 1 sur la structure du point 3. 
  #     ATTENTION : On ne les convertit pas en négatif contrairement à HRM. On obtient les montants à additionner aux données du MRIO.
  Lig_Ventil_delta<-Delta[Lig_Ventil_totaux,on=.(Lig_Indus,Col_Country,Col_Indus)]
  Lig_Ventil_delta<-Lig_Ventil_delta[,value_delta:=value*i.value]
  if(OptMRIO==TRUE){ Ordre_Col<-c("year","i.Lig_Country","Col_Country","Lig_Indus","Col_Indus","value_delta","MRIO")
  }else{ Ordre_Col<-c("year","i.Lig_Country","Col_Country","Lig_Indus","Col_Indus","value_delta") }
  Lig_Ventil_delta<-Lig_Ventil_delta[,..Ordre_Col]  # selection des colonnes qui nous intéressent
  setnames(Lig_Ventil_delta, "value_delta", "value")  # On renomme value pour le cbind a venir
  setnames(Lig_Ventil_delta, "i.Lig_Country", "Lig_Country")  # On renomme value pour le cbind a venir
  
  #	5/ On ajoute (rbind) la base initiale avec la base du point 1 (ce qui est à ajouter) et la base du 
  #        point 4 (ce qui est à retrancher)
  #     On crée dans un premier temps la base des delta et on contrôle que les ajouts sont biens 
  #     compensés par les retraits
  Delta<-Delta[,type_correc:="SeraA0"]
  Lig_Ventil_delta<-Lig_Ventil_delta[,type_correc:="Ajout"]
  base_correc_HEM<-Lig_Ventil_delta
  
  #     On ajoute la base initiale et son statut
  Base_init<-Base_init[,type_correc:="Initial"]
  Base_HEM_detail<-rbind(Base_init,base_correc_HEM)
  
  
  #	6/ On somme suivant l'ensemble des dimensions du MRIO hormis la variable de statut : ça fait donc 
  #       automatiquement les sommes des montants du point 1 et ça retranche la ventilation du point 4.
  if(OptMRIO==TRUE){  Base_HEM<-Base_HEM_detail[,sum(value),by=.(year,Lig_Country, Col_Country,Lig_Indus,Col_Indus,MRIO)]
  }else{ Base_HEM<-Base_HEM_detail[,sum(value),by=.(year,Lig_Country, Col_Country,Lig_Indus,Col_Indus)]}
  setnames(Base_HEM, "V1", "value")
  
  # 6bis/ in fine on place la ligne et la colonne extraite à 0 (ce qui conserve la nullité de la somme 
  #      des corrections par colonnes)  
  # Une option permet de ne faire qu'en amont ou en aval
  if( OptAmontAval=="ALL"){
    Base_HEM[Lig_Indus==extract_indus & Lig_Country==extract_country,"value"]<-0
    Base_HEM[Col_Indus==extract_indus & Col_Country==extract_country,"value"]<-0
  }
  if( OptAmontAval=="Amont"){ # on ne place que la colonne extraite à 0
    Base_HEM[Col_Indus==extract_indus & Col_Country==extract_country,"value"]<-0
  }
  if( OptAmontAval=="Aval"){ # on ne place que la ligne extraite à 0
    Base_HEM[Lig_Indus==extract_indus & Lig_Country==extract_country,"value"]<-0
  }
  
  #	7/ Sortie sous forme de tableau, avec éventuel contrôle (on fait la différence entre le MRIO initial et 
  #      le final, et on calcule le colSums pour vérifier que c'est bien nul.
  Base_init_tab<-Base_init
  Base_init_tab$PR<-paste0(Base_init_tab$Lig_Country,"_",Base_init_tab$Lig_Indus)
  Base_init_tab$BR<-paste0(Base_init_tab$Col_Country,"_",Base_init_tab$Col_Indus)
  Base_init_tab<-dcast(Base_init_tab, PR ~ BR, value.var = "value") 
  Base_HEM$PR<-paste0(Base_HEM$Lig_Country,"_",Base_HEM$Lig_Indus)
  Base_HEM$BR<-paste0(Base_HEM$Col_Country,"_",Base_HEM$Col_Indus)
  HEM_out_tab<-dcast(Base_HEM, PR ~ BR, value.var = "value") 
  
  Diff_HEM_Init<-Base_init_tab
  nb_col<-ncol(Diff_HEM_Init)
  Diff_HEM_Init<-HEM_out_tab[,2:nb_col]-Base_init_tab[,2:nb_col]
  
  if(verboseCheck==TRUE){ # Check : On calcule le colSums des corrections pour vérifier que c'est bien nul
    print("On calcule le colSums des corrections pour vérifier que c'est bien à 0 :")
    print(colSums(Diff_HEM_Init))
  }
  
  Base_HEM<-rbind(Base_HEM,Prod)
  
  if(OptMRIO==TRUE){
    CompoCalcHEM<-CompoECOLEouA17(Base_HEM,"OptFullOptionsBonus",date=annee)
  }else{
    CompoCalcHEM<-CompoMRIO(Base_HEM,"OptFullOptionsBonus",date=annee)
  }
  
  # Calcul indicateur HEM
  Mat_L<-as.matrix(CompoCalcHEM[["L"]])
  Mat_DF<-as.matrix(CompoCalcHEM[["DF_TOT"]]$value)
  Res_HEM_BR<-Mult2_rcpp3(Mat_L,Mat_DF)
  Res_HEM_BR<-as.data.frame(Res_HEM_BR)
  colnames(Res_HEM_BR)[1]<-"value"
  rownames(Res_HEM_BR)<-as.list(Base_init_tab$PR)
  Res_HEM_ratio<-sum(Res_HEM_BR)/sum(Prod$value)
  
  Return_List<-list(Indic_HEM=Res_HEM_ratio,Indic_HEMparBR=Res_HEM_BR,HEM_Tab=HEM_out_tab,Diff=Diff_HEM_Init,Init_Tab=Base_init_tab,HEM_Base=Base_HEM)
  return(Return_List)
}


# Fonction HRM (hypothetical repatriation method)
HRM<- function(dt_mrio,repat_country,repat_indus,repat_pct,verboseCheck=FALSE,OptMRIO=TRUE)  
{
  #	On implémente un choc de repat_pct % sur le produit repat_indus pour le pays repat_country : 
  #      rappatriement de repat_pct % des importations de ce pays en ce produit.
  #	Les calculs sont transversaux au TEI et FD : c'est tout le produit qui est concerné uniformément.
  
  Base_init<-dt_mrio
  
  #	Etapes du calcul :
  
  #	1/ On extrait le Lig_Indus de Lig_Country concerné et on calcul repat_pct % de cette ligne. On 
  #        obtient un montant pour chaque colonne du MRIO.
  Delta1<-Base_init[Lig_Indus==repat_indus & Lig_Country!=repat_country & Col_Country==repat_country,]
  if(OptMRIO==TRUE){Delta1<-Delta1[,repat_pct*sum(value),by=.(year,Lig_Indus,Col_Country,Col_Indus,MRIO)]
  }else{Delta1<-Delta1[,repat_pct*sum(value),by=.(year,Lig_Indus,Col_Country,Col_Indus)]}
  Delta1<-Delta1[,Lig_Country:=repat_country]
  setnames(Delta1, "V1", "value")  
  if(OptMRIO==TRUE){Ordre_Col<-c("year","Lig_Country","Col_Country","Lig_Indus","Col_Indus","value","MRIO")
  }else{Ordre_Col<-c("year","Lig_Country","Col_Country","Lig_Indus","Col_Indus","value")}
  Delta1<-Delta1[,..Ordre_Col]  # reordonne les colonnes
  
  Delta2<-Base_init[Lig_Indus==repat_indus & Lig_Country==repat_country & Col_Country!=repat_country,]
  Delta2[,"value"]<-repat_pct*Delta2[,"value"] 
  
  Delta<-rbind(Delta1,Delta2)
  
  #	2/ On extrait les Lig_Indus de tous les pays hors le pays choqué en ligne et hors parties 
  #        domestiques (Lig_country==Col_Country)
  Lig_Ventil<-Base_init[Lig_Indus==repat_indus & Lig_Country!=repat_country & Lig_Country!=Col_Country,]
  
  #	3/ On calcule directmeent en requêtes le total puis la structure (on le fait directement par 
  #        requetage pour eviter toute adherence aux formats).
  if(OptMRIO==TRUE){
    Lig_Ventil_totaux<-Lig_Ventil[,sum(value),by=.(year,Lig_Indus,Col_Indus,Col_Country,MRIO)]
    Lig_Ventil_totaux<-Lig_Ventil_totaux[Lig_Ventil,on=.(year,Lig_Indus,Col_Indus,Col_Country,MRIO)]
  }else{
    Lig_Ventil_totaux<-Lig_Ventil[,sum(value),by=.(year,Lig_Indus,Col_Indus,Col_Country)]
    Lig_Ventil_totaux<-Lig_Ventil_totaux[Lig_Ventil,on=.(year,Lig_Indus,Col_Indus,Col_Country)]
  }
  Lig_Ventil_totaux[,"value"]<-Lig_Ventil_totaux[,"value"]/Lig_Ventil_totaux[,"V1"]
  
  if(OptMRIO==TRUE){ Ordre_Col<-c("year","Lig_Country","Col_Country","Lig_Indus","Col_Indus","value","MRIO")
  }else{ Ordre_Col<-c("year","Lig_Country","Col_Country","Lig_Indus","Col_Indus","value")}
  Lig_Ventil_totaux<-Lig_Ventil_totaux[,..Ordre_Col]  # reordonne les colonnes
  Lig_Ventil_totaux<-GereInfNA(Lig_Ventil_totaux)
  
  if(verboseCheck==TRUE){ # check : On verifie que la structure somme bien à 1
  print("On verifie que la structure somme bien à 1 :")
    if(OptMRIO==TRUE){ print(Lig_Ventil_totaux[,sum(value),by=.(year,Lig_Indus,Col_Indus,Col_Country,MRIO)]) 
    }else{print(Lig_Ventil_totaux[,sum(value),by=.(year,Lig_Indus,Col_Indus,Col_Country)]) }
  }
  
  #	4/ Ensuite on ventile le montant du point 1 sur la structure du point 3. On les convertit en négatif 
  #     car il faudra les retrancher. On obtient les montants à retrancher aux données du MRIO.
  Lig_Ventil_delta<-Delta[Lig_Ventil_totaux,on=.(Lig_Indus,Col_Country,Col_Indus)]
  Lig_Ventil_delta<-Lig_Ventil_delta[,value_delta:=-value*i.value]
  if(OptMRIO==TRUE){  Ordre_Col<-c("year","i.Lig_Country","Col_Country","Lig_Indus","Col_Indus","value_delta","MRIO") 
  }else{ Ordre_Col<-c("year","i.Lig_Country","Col_Country","Lig_Indus","Col_Indus","value_delta")}
  Lig_Ventil_delta<-Lig_Ventil_delta[,..Ordre_Col]  # selection des colonnes qui nous intéressent
  setnames(Lig_Ventil_delta, "value_delta", "value")  # On renomme value pour le cbind a venir
  setnames(Lig_Ventil_delta, "i.Lig_Country", "Lig_Country")  # On renomme value pour le cbind a venir
  
  #	5/ On ajoute (rbind) la base initiale avec la base du point 1 (ce qui est à ajouter) et la base du 
  #        point 4 (ce qui est à retrancher)
  #     On crée dans un premier temps la base des delta et on contrôle que les ajouts sont biens 
  #     compensés par els retraits
  Delta<-Delta[,type_correc:="Ajout"]
  Lig_Ventil_delta<-Lig_Ventil_delta[,type_correc:="Retranche"]
  base_correc_HRM<-rbind(Delta,Lig_Ventil_delta)
  
  if(verboseCheck==TRUE){ # Check : On verifie que la structure somme bien à 0
    print("On verifie que la structure somme bien à 0 :")
    print(base_correc_HRM[,sum(value),by=.(Col_Country,Col_Indus)])
  }
  
  #     On ajoute la base initiale et son statut
  Base_init<-Base_init[,type_correc:="Initial"]
  Base_HRM_detail<-rbind(Base_init,base_correc_HRM)
  
  
  #	6/ On somme suivant l'ensemble des dimensions du MRIO hormis la variable de statut : ça fait donc 
  #       automatiquement les sommes des montants du point 1 et ça retranche la ventilation du point 4.
  if(OptMRIO==TRUE){Base_HRM<-Base_HRM_detail[,sum(value),by=.(year,Lig_Country, Col_Country,Lig_Indus,Col_Indus,MRIO)]
  }else{ Base_HRM<-Base_HRM_detail[,sum(value),by=.(year,Lig_Country, Col_Country,Lig_Indus,Col_Indus)]}
  setnames(Base_HRM, "V1", "value")
  
  #	7/ Sortie sous forme de tableau, avec éventuel contrôle (on fait la différence entre le MRIO initial et 
  #      le final, et on calcule le colSums pour vérifier que c'est bien nul.
  Base_init_tab<-Base_init
  Base_init_tab$PR<-paste0(Base_init_tab$Lig_Country,"_",Base_init_tab$Lig_Indus)
  Base_init_tab$BR<-paste0(Base_init_tab$Col_Country,"_",Base_init_tab$Col_Indus)
  Base_init_tab<-dcast(Base_init_tab, PR ~ BR, value.var = "value") 
  Base_HRM$PR<-paste0(Base_HRM$Lig_Country,"_",Base_HRM$Lig_Indus)
  Base_HRM$BR<-paste0(Base_HRM$Col_Country,"_",Base_HRM$Col_Indus)
  HRM_out_tab<-dcast(Base_HRM, PR ~ BR, value.var = "value") 
  
  Diff_HRM_Init<-Base_init_tab
  nb_col<-ncol(Diff_HRM_Init)
  Diff_HRM_Init<-HRM_out_tab[,2:nb_col]-Base_init_tab[,2:nb_col]
  
  if(verboseCheck==TRUE){ # Check : On calcule le colSums des corrections pour vérifier que c'est bien nul
    print("On calcule max( colSums ) des corrections pour vérifier que c'est bien à 0 :")
    print(max(colSums(Diff_HRM_Init)))
  }
  
  Return_List<-list(HRM_tab=HRM_out_tab,Diff=Diff_HRM_Init,Init_tab=Base_init_tab,HRM_Base=Base_HRM)
  return(Return_List)
}




# Sélection d'une composante du mrio complet
   #  Attention : ici on supprime la référence au MRIO car on n'est plus sur des bases communes à tous les MRIO mais plutôt individuelles
CompoMRIO<- function(dt_MRIO,typeCompo,date=9999,country="ALL",OptTab=FALSE )  # options: =CI:DF;PROD;VA;A;B;CI_PR;CI_BR;DF_TOT ;L;invB ;OptFullOptions ;OptFullOptionsBonus : les deux dernières options sortent des listes avec l'ensemble des sorties individuelles (le version 'Bonus' est exhaustive mais plus longue car inversions matricielles
{
  interm<-dt_MRIO
  
  List_GEO<-c("ABW","AFG","AGO","AIA","ALA","ALB","AND","ARE","ARG","ARM","ASM","ATA","ATF","ATG","AUS","AUT","AZE","BDI","BEL","BEN","BFA","BGD","BGR","BHR","BHS","BIH","BLM","BLR","BLZ","BMU","BOL","BRA","BRB","BRN","BTN","BVT","BWA","CAF","CAN","CCK","CHE","CHL","CHN","CIV","CMR","COD","COG","COK","COL","COM","CPV","CRI","CUB","CXR","CYM","CYP","CZE","DEU","DJI","DMA","DNK","DOM","DZA","ECU","EGY","ERI","ESH","ESP","EST","ETH","FIN","FJI","FLK","FRA","FRO","FSM","GAB","GBR","GEO","GGY","GHA","GIB","GIN","GLP","GMB","GNB","GNQ","GRC","GRD","GRL","GTM","GUF","GUM","GUY","HKG","HMD","HND","HRV","HTI","HUN","IDN","IMN","IND","IOT","IRL","IRN","IRQ","ISL","ISR","ITA","JAM","JEY","JOR","JPN","KAZ","KEN","KGZ","KHM","KIR","KNA","KOR","KWT","LAO","LBN","LBR","LBY","LCA","LIE","LKA","LSO","LTU","LUX","LVA","MAC","MAF","MAR","MCO","MDA","MDG","MDV","MEX","MHL","MKD","MLI","MLT","MMR","MNE","MNG","MNP","MOZ","MRT","MSR","MTQ","MUS","MWI","MYS","MYT","NAM","NCL","NER","NFK","NGA","NIC","NIU","NLD","NOR","NPL","NRU","NZL","OMN","PAK","PAN","PCN","PER","PHL","PLW","PNG","POL","PRI","PRK","PRT","PRY","PSE","PYF","QAT","REU","ROU","RUS","RWA","SAU","SDN","SEN","SGP","SGS","SHN","SJM","SLB","SLE","SLV","SMR","SOM","SPM","SRB","STP","SUR","SVK","SVN","SWE","SWZ","SYC","SYR","TCA","TCD","TGO","THA","TJK","TKL","TKM","TLS","TON","TTO","TUN","TUR","TUV","TWN","TZA","UGA","UKR","UMI","URY","USA","UZB","VAT","VCT","VEN","VGB","VIR","VNM","VUT","WLF","WSM","YEM","ZAF","ZMB","ZWE","ROW")
  List_BR<-c("A01","A02","A03","AtB","AZ","B","C","C1","C10-C12","C10T12","C13-C15","C13T15","C16","C17","C18","C19","C2","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C3","C30","C31_32","C31_C32","C33","C4","C5","D","D15t16","D17t19","D21t22","D23","D24","D25","D26","D27t28","D29","D30t33","D34t35","D35","DE","Dnec","E","E36","E37-E39","E37T39","F","FZ","G","G45","G46","G47","GZ","H","H49","H50","H51","H52","H53","HZ","I","I60t63","I64","IZ","J","J58","J59_60","J59_J60","J61","J62_63","J62_J63","JZ","K","K64","K65","K66","KZ","L","L68","LtQ","LZ","M69_70","M69_M70","M71","M72","M73","M74_75","M74_M75","MN","N","N77","N78","N79","N80T82","O84","OQ","P85","Q","Q86","Q87_88","R_S","R90T92","R93","RU","S94","S95","S96","T","U")
  List_DF<-c("CONS_h","CONS_g","CONS_np","GFCF","INVEN","P3_S13","P3_S14","P3_S15","P51G","P5M","xCONS_h","xCONS_g","xGFCF","xINV")
  List_BRDF<-c("A01","A02","A03","AtB","AZ","B","C","C1","C10-C12","C10T12","C13-C15","C13T15","C16","C17","C18","C19","C2","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C3","C30","C31_32","C31_C32","C33","C4","C5","D","D15t16","D17t19","D21t22","D23","D24","D25","D26","D27t28","D29","D30t33","D34t35","D35","DE","Dnec","E","E36","E37-E39","E37T39","F","FZ","G","G45","G46","G47","GZ","H","H49","H50","H51","H52","H53","HZ","I","I60t63","I64","IZ","J","J58","J59_60","J59_J60","J61","J62_63","J62_J63","JZ","K","K64","K65","K66","KZ","L","L68","LtQ","LZ","M69_70","M69_M70","M71","M72","M73","M74_75","M74_M75","MN","N","N77","N78","N79","N80T82","O84","OQ","P85","Q","Q86","Q87_88","R_S","R90T92","R93","RU","S94","S95","S96","T","U","CONS_h","CONS_g","CONS_np","GFCF","INVEN","P3_S13","P3_S14","P3_S15","P51G","P5M","xCONS_h","xCONS_g","xGFCF","xINV")
  
  if(date!=9999){
    interm<-interm[year==date,]
  }
  if(country!="ALL"){
    interm<-interm[Lig_Country==country & Col_Country==country,]
  }
  
  if(typeCompo=="CI"){
    interm<-interm[Lig_Indus %in% List_BR & Col_Indus %in% List_BR,]
    if(OptTab==TRUE){interm<-dcast(interm, year + Lig_Country + Lig_Indus  ~ Col_Country + Col_Indus, value.var = "value")} # Mise en format tab 
  }
  if(typeCompo=="DF"){
    interm<-interm[Lig_Indus %in% List_BR & Col_Indus %in% List_DF,]
    if(OptTab==TRUE){interm<-dcast(interm, year + Lig_Country + Lig_Indus  ~ Col_Country + Col_Indus, value.var = "value")} # Mise en format tab 
  }
  if(typeCompo=="PROD"){ # Recalcul a la volee a partir du MRIO pour eviter les incohérences
    interm<-interm[Lig_Indus %in% List_BR & Col_Indus %in% List_BRDF,]
    interm<-interm[,.(sum(value)), by=c("year","Lig_Indus","Lig_Country")]
    setnames(interm, "V1", "value")
    setorder(interm,Lig_Country,Lig_Indus)
  }
  if(typeCompo=="A"){ # Calcul des coefficients techniques
    Part_CI<-CompoMRIO(interm,"CI",date=date)
    Part_Prod<-CompoMRIO(interm,"PROD",date=date)
    setnames(Part_Prod,"Lig_Indus","Col_Indus")
    setnames(Part_Prod,"Lig_Country","Col_Country")
    A_merge<-Part_Prod[Part_CI, on=.(Col_Indus,Col_Country,year)]
    A_merge$value<-A_merge$i.value/A_merge$value
    A_merge<-GereInfNA(A_merge[,i.value:=NULL])
    interm<-A_merge
    if(OptTab==TRUE){interm<-dcast(interm, year + Lig_Country + Lig_Indus  ~ Col_Country + Col_Indus, value.var = "value")} # Mise en format tab 
  }
  if(typeCompo=="B"){ # Calcul des coefficients de débouchés
    Part_CI<-CompoMRIO(interm,"CI",date=date)
    Part_Prod<-CompoMRIO(interm,"PROD",date=date)
    B_merge<-Part_Prod[Part_CI, on=.(Lig_Indus,Lig_Country,year)]
    B_merge$value<-B_merge$i.value/B_merge$value
    B_merge<-GereInfNA(B_merge[,i.value:=NULL])
    interm<-B_merge
    if(OptTab==TRUE){interm<-dcast(interm, year + Lig_Country + Lig_Indus  ~ Col_Country + Col_Indus, value.var = "value")} # Mise en format tab 
  }
  if(typeCompo=="CI_PR"){ # Recalcul a la volee a partir du MRIO pour eviter les incohérences
    interm<-CompoMRIO(interm,"CI",date=date)
    interm<-interm[,.(sum(value)), by=c("year","Lig_Indus","Lig_Country")]
    setnames(interm, "V1", "value")
    setorder(interm,Lig_Country,Lig_Indus)
  }
  if(typeCompo=="CI_BR"){ # Recalcul a la volee a partir du MRIO pour eviter les incohérences
    interm<-CompoMRIO(interm,"CI",date=date)
    interm<-interm[,.(sum(value)), by=c("year","Col_Indus","Col_Country")]
    setnames(interm, "V1", "value")
    setorder(interm,Col_Country,Col_Indus)
  }
  if(typeCompo=="DF_TOT"){ # Recalcul a la volee a partir du MRIO pour eviter les incohérences
    interm<-CompoMRIO(interm,"DF",date=date)
    interm<-interm[,.(sum(value)), by=c("year","Lig_Indus","Lig_Country")]
    setnames(interm, "V1", "value")
    setorder(interm,Lig_Country,Lig_Indus)
  }
  if(typeCompo=="VA"){
    # On extrait le TEI dans un premier temps
    Part_SommeCI<-CompoMRIO(interm,"CI_BR",date=date)
    Part_SommeCI$value<- -Part_SommeCI$value
    # On calcule la prod dans un recond temps
    Part_Prod<-CompoMRIO(interm,"PROD",date=date)
    # On calcule le solde entre la prod et la somme des CI Branche (concept de VA au prix de base)
    setnames(Part_Prod, "Lig_Indus", "Col_Indus") # Recode pour passer les PR en BR
    setnames(Part_Prod, "Lig_Country", "Col_Country") # Recode pour passer les PR en BR
    interm<-rbind(Part_Prod,Part_SommeCI)
    interm<-interm[,.(sum(value)), by=c("year","Col_Indus","Col_Country")]
    setnames(interm, "V1", "value")
    setorder(interm,Col_Country,Col_Indus)
  }
  if(typeCompo=="L"){ # Calcul de l'inverse de Léontief
    A<-CompoMRIO(interm,"A",date=date,OptTab=TRUE)
    nb_ligcol<-nrow(A)
    MatrixA<-as.matrix(A[,4:ncol(A)])
    interm<-inversion_rcpp3(diag(nb_ligcol)-MatrixA)
    colnames(interm)<-colnames(A)[4:ncol(A)]
    rownames(interm)<-colnames(A)[4:ncol(A)]
    interm<-as.data.frame(interm)
  }
  if(typeCompo=="invB"){ # Calcul de l'inverse de Ghosh
    B<-CompoMRIO(interm,"B",date=date,OptTab=TRUE)
    nb_ligcol<-nrow(B)
    MatrixB<-as.matrix(B[,4:ncol(B)])
    interm<-inversion_rcpp3(diag(nb_ligcol)-MatrixB)
    colnames(interm)<-colnames(B)[4:ncol(B)]
    rownames(interm)<-colnames(B)[4:ncol(B)]
    interm<-as.data.frame(interm)
  }
  if(typeCompo=="OptFullOptions"){
    interm1<-CompoMRIO(dt_MRIO,"PROD",date=date,country=country,OptTab=FALSE )
    interm2<-CompoMRIO(dt_MRIO,"CI",date=date,country=country,OptTab=FALSE )
    interm3<-CompoMRIO(dt_MRIO,"DF",date=date,country=country,OptTab=FALSE )
    interm4<-CompoMRIO(dt_MRIO,"VA",date=date,country=country,OptTab=FALSE )
    interm5<-CompoMRIO(dt_MRIO,"CI_PR",date=date,country=country,OptTab=FALSE )
    interm6<-CompoMRIO(dt_MRIO,"CI_BR",date=date,country=country,OptTab=FALSE )
    interm7<-CompoMRIO(dt_MRIO,"DF_TOT",date=date,country=country,OptTab=FALSE )
    interm8<-CompoMRIO(dt_MRIO,"A",date=date,country=country,OptTab=FALSE )
    interm2tab<-CompoMRIO(dt_MRIO,"CI",date=date,country=country,OptTab=TRUE )
    interm3tab<-CompoMRIO(dt_MRIO,"DF",date=date,country=country,OptTab=TRUE )
    interm8tab<-CompoMRIO(dt_MRIO,"A",date=date,country=country,OptTab=TRUE )
    interm<-list(PROD=interm1,CI=interm2,DF=interm3,VA=interm4,CI_PR=interm5,CI_BR=interm6,DF_TOT=interm7,A=interm8,CI_tab=interm2tab,DF_tab=interm3tab,A_tab=interm8tab)
  }
  if(typeCompo=="OptFullOptionsBonus"){
    interm1<-CompoMRIO(dt_MRIO,"PROD",date=date,country=country,OptTab=FALSE )
    interm2<-CompoMRIO(dt_MRIO,"CI",date=date,country=country,OptTab=FALSE )
    interm3<-CompoMRIO(dt_MRIO,"DF",date=date,country=country,OptTab=FALSE )
    interm4<-CompoMRIO(dt_MRIO,"VA",date=date,country=country,OptTab=FALSE )
    interm5<-CompoMRIO(dt_MRIO,"CI_PR",date=date,country=country,OptTab=FALSE )
    interm6<-CompoMRIO(dt_MRIO,"CI_BR",date=date,country=country,OptTab=FALSE )
    interm7<-CompoMRIO(dt_MRIO,"DF_TOT",date=date,country=country,OptTab=FALSE )
    interm8<-CompoMRIO(dt_MRIO,"A",date=date,country=country,OptTab=FALSE )
    interm2tab<-CompoMRIO(dt_MRIO,"CI",date=date,country=country,OptTab=TRUE )
    interm3tab<-CompoMRIO(dt_MRIO,"DF",date=date,country=country,OptTab=TRUE )
    interm8tab<-CompoMRIO(dt_MRIO,"A",date=date,country=country,OptTab=TRUE )
    interm9<-CompoMRIO(dt_MRIO,"B",date=date,country=country,OptTab=FALSE )
    interm10<-CompoMRIO(dt_MRIO,"B",date=date,country=country,OptTab=TRUE )
    interm11<-CompoMRIO(dt_MRIO,"L",date=date,country=country) # Resultat directement sous forme de tableau
    interm12<-CompoMRIO(dt_MRIO,"invB",date=date,country=country) # Resultat directement sous forme de tableau
    interm<-list(PROD=interm1,CI=interm2,DF=interm3,VA=interm4,CI_PR=interm5,CI_BR=interm6,DF_TOT=interm7,A=interm8,CI_tab=interm2tab,DF_tab=interm3tab,A_tab=interm8tab,B=interm9,B_tab=interm10,L=interm11,InvBGhosh=interm12)
  }
  return(interm)
}



# Sélection d'une composante du mrio ECOLE ou A17 (attention priorité par défaut à Figaro sur 2010-2014)
CompoECOLEouA17<- function(dt_MRIO,typeCompo,date=9999,country="ALL",OptTab=FALSE,OptMRIO1014="FIGARO",OptMRIO2000="WIOD" )  #options: =CI:DF;PROD;VA;A;B;CI_PR;CI_BR;DF_TOT ;L;invB ;OptFullOptions ;OptFullOptionsBonus : les deux dernières options sortent des listes avec l'ensemble des sorties individuelles (le version 'Bonus' est exhaustive mais plus longue car inversions matricielles
{
  interm<-dt_MRIO
 
  List_GEO<-c("ABW","AFG","AGO","AIA","ALA","ALB","AND","ARE","ARG","ARM","ASM","ATA","ATF","ATG","AUS","AUT","AZE","BDI","BEL","BEN","BFA","BGD","BGR","BHR","BHS","BIH","BLM","BLR","BLZ","BMU","BOL","BRA","BRB","BRN","BTN","BVT","BWA","CAF","CAN","CCK","CHE","CHL","CHN","CIV","CMR","COD","COG","COK","COL","COM","CPV","CRI","CUB","CXR","CYM","CYP","CZE","DEU","DJI","DMA","DNK","DOM","DZA","ECU","EGY","ERI","ESH","ESP","EST","ETH","FIN","FJI","FLK","FRA","FRO","FSM","GAB","GBR","GEO","GGY","GHA","GIB","GIN","GLP","GMB","GNB","GNQ","GRC","GRD","GRL","GTM","GUF","GUM","GUY","HKG","HMD","HND","HRV","HTI","HUN","IDN","IMN","IND","IOT","IRL","IRN","IRQ","ISL","ISR","ITA","JAM","JEY","JOR","JPN","KAZ","KEN","KGZ","KHM","KIR","KNA","KOR","KWT","LAO","LBN","LBR","LBY","LCA","LIE","LKA","LSO","LTU","LUX","LVA","MAC","MAF","MAR","MCO","MDA","MDG","MDV","MEX","MHL","MKD","MLI","MLT","MMR","MNE","MNG","MNP","MOZ","MRT","MSR","MTQ","MUS","MWI","MYS","MYT","NAM","NCL","NER","NFK","NGA","NIC","NIU","NLD","NOR","NPL","NRU","NZL","OMN","PAK","PAN","PCN","PER","PHL","PLW","PNG","POL","PRI","PRK","PRT","PRY","PSE","PYF","QAT","REU","ROU","RUS","RWA","SAU","SDN","SEN","SGP","SGS","SHN","SJM","SLB","SLE","SLV","SMR","SOM","SPM","SRB","STP","SUR","SVK","SVN","SWE","SWZ","SYC","SYR","TCA","TCD","TGO","THA","TJK","TKL","TKM","TLS","TON","TTO","TUN","TUR","TUV","TWN","TZA","UGA","UKR","UMI","URY","USA","UZB","VAT","VCT","VEN","VGB","VIR","VNM","VUT","WLF","WSM","YEM","ZAF","ZMB","ZWE","ROW")
  List_BR<-c("AGR_INDU","ENRJ","SERV_ABRIT","SERV_EXPO","A01","A02","A03","AtB","AZ","B","C","C1","C10-C12","C10T12","C13-C15","C13T15","C16","C17","C18","C19","C2","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C3","C30","C31_32","C31_C32","C33","C4","C5","D","D15t16","D17t19","D21t22","D23","D24","D25","D26","D27t28","D29","D30t33","D34t35","D35","DE","Dnec","E","E36","E37-E39","E37T39","F","FZ","G","G45","G46","G47","GZ","H","H49","H50","H51","H52","H53","HZ","I","I60t63","I64","IZ","J","J58","J59_60","J59_J60","J61","J62_63","J62_J63","JZ","K","K64","K65","K66","KZ","L","L68","LtQ","LZ","M69_70","M69_M70","M71","M72","M73","M74_75","M74_M75","MN","N","N77","N78","N79","N80T82","O84","OQ","P85","Q","Q86","Q87_88","R_S","R90T92","R93","RU","S94","S95","S96","T","U")
  List_DF<-c("CONS_h","CONS_g","CONS_np","GFCF","INVEN","P3_S13","P3_S14","P3_S15","P51G","P5M","xCONS_h","xCONS_g","xGFCF","xINV")
  List_BRDF<-c("AGR_INDU","ENRJ","SERV_ABRIT","SERV_EXPO","A01","A02","A03","AtB","AZ","B","C","C1","C10-C12","C10T12","C13-C15","C13T15","C16","C17","C18","C19","C2","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C3","C30","C31_32","C31_C32","C33","C4","C5","D","D15t16","D17t19","D21t22","D23","D24","D25","D26","D27t28","D29","D30t33","D34t35","D35","DE","Dnec","E","E36","E37-E39","E37T39","F","FZ","G","G45","G46","G47","GZ","H","H49","H50","H51","H52","H53","HZ","I","I60t63","I64","IZ","J","J58","J59_60","J59_J60","J61","J62_63","J62_J63","JZ","K","K64","K65","K66","KZ","L","L68","LtQ","LZ","M69_70","M69_M70","M71","M72","M73","M74_75","M74_M75","MN","N","N77","N78","N79","N80T82","O84","OQ","P85","Q","Q86","Q87_88","R_S","R90T92","R93","RU","S94","S95","S96","T","U","CONS_h","CONS_g","CONS_np","GFCF","INVEN","P3_S13","P3_S14","P3_S15","P51G","P5M","xCONS_h","xCONS_g","xGFCF","xINV")
  
  if(date!=9999){
    interm<-interm[year==date,]
  }
  if(country!="ALL"){
    interm<-interm[Lig_Country==country & Col_Country==country,]
  }
  if(date>=2010 & date<=2014){ # Cas ou on a 2 bases mrio possibles sur la période : on privilégie FIGARO par défaut mais on conserve la possibilité de récupérer WIOD
    if(OptMRIO1014!="FIGARO"){interm<-interm[MRIO=="WIOD",]}else{interm<-interm[MRIO=="FIGARO",]}
  }  
  if(date==2000){ # Cas ou on a 2 bases mrio possibles en 2000 : on privilégie WIOD par défaut mais on conserve la possibilité de récupérer LR-WIOD
    if(OptMRIO2000=="WIOD"){interm<-interm[MRIO=="WIOD",]}else{interm<-interm[MRIO=="LR_WIOD",]}
  }  
  if(typeCompo=="CI"){
    interm<-interm[Lig_Indus %in% List_BR & Col_Indus %in% List_BR,]
    if(OptTab==TRUE){interm<-dcast(interm, year + Lig_Country + Lig_Indus  ~ Col_Country + Col_Indus, value.var = "value")} # Mise en format tab 
  }
  if(typeCompo=="DF"){
    interm<-interm[Lig_Indus %in% List_BR & Col_Indus %in% List_DF,]
    if(OptTab==TRUE){interm<-dcast(interm, year + Lig_Country + Lig_Indus  ~ Col_Country + Col_Indus, value.var = "value")} # Mise en format tab 
  }
  if(typeCompo=="PROD"){ # Recalcul a la volee a partir du MRIO pour eviter les incohérences
    interm<-interm[Lig_Indus %in% List_BR & Col_Indus %in% List_BRDF,]
    interm<-interm[,.(sum(value)), by=c("MRIO","year","Lig_Indus","Lig_Country")]
    setnames(interm, "V1", "value")
    setorder(interm,Lig_Country,Lig_Indus)
  }
  if(typeCompo=="A"){ # Calcul des coefficients techniques
    Part_CI<-CompoECOLEouA17(interm,"CI",date=date,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000)
    Part_Prod<-CompoECOLEouA17(interm,"PROD",date=date,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000)
    setnames(Part_Prod,"Lig_Indus","Col_Indus")
    setnames(Part_Prod,"Lig_Country","Col_Country")
    A_merge<-Part_Prod[Part_CI, on=.(Col_Indus,Col_Country,year,MRIO)]
    A_merge$value<-A_merge$i.value/A_merge$value
    A_merge<-GereInfNA(A_merge[,i.value:=NULL])
    interm<-A_merge
    if(OptTab==TRUE){interm<-dcast(interm, year + Lig_Country + Lig_Indus  ~ Col_Country + Col_Indus, value.var = "value")} # Mise en format tab 
  }
  if(typeCompo=="B"){ # Calcul des coefficients de débouchés
    Part_CI<-CompoECOLEouA17(interm,"CI",date=date,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000)
    Part_Prod<-CompoECOLEouA17(interm,"PROD",date=date,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000)
    B_merge<-Part_Prod[Part_CI, on=.(Lig_Indus,Lig_Country,year,MRIO)]
    B_merge$value<-B_merge$i.value/B_merge$value
    B_merge<-GereInfNA(B_merge[,i.value:=NULL])
    interm<-B_merge
    if(OptTab==TRUE){interm<-dcast(interm, year + Lig_Country + Lig_Indus  ~ Col_Country + Col_Indus, value.var = "value")} # Mise en format tab 
  }
  if(typeCompo=="CI_PR"){ # Recalcul a la volee a partir du MRIO pour eviter les incohérences
    interm<-CompoECOLEouA17(interm,"CI",date=date,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000)
    interm<-interm[,.(sum(value)), by=c("MRIO","year","Lig_Indus","Lig_Country")]
    setnames(interm, "V1", "value")
    setorder(interm,Lig_Country,Lig_Indus)
  }
  if(typeCompo=="CI_BR"){ # Recalcul a la volee a partir du MRIO pour eviter les incohérences
    interm<-CompoECOLEouA17(interm,"CI",date=date,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000)
    interm<-interm[,.(sum(value)), by=c("MRIO","year","Col_Indus","Col_Country")]
    setnames(interm, "V1", "value")
    setorder(interm,Col_Country,Col_Indus)
  }
  if(typeCompo=="DF_TOT"){ # Recalcul a la volee a partir du MRIO pour eviter les incohérences
    interm<-CompoECOLEouA17(interm,"DF",date=date,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000)
    interm<-interm[,.(sum(value)), by=c("MRIO","year","Lig_Indus","Lig_Country")]
    setnames(interm, "V1", "value")
    setorder(interm,Lig_Country,Lig_Indus)
  }
  if(typeCompo=="VA"){
    # On extrait le TEI dans un premier temps
    Part_SommeCI<-CompoECOLEouA17(interm,"CI_BR",date=date,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000)
    Part_SommeCI$value<- -Part_SommeCI$value
    # On calcule la prod dans un recond temps
    Part_Prod<-CompoECOLEouA17(interm,"PROD",date=date,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000)
    # On calcule le solde entre la prod et la somme des CI Branche (concept de VA au prix de base)
    setnames(Part_Prod, "Lig_Indus", "Col_Indus") # Recode pour passer les PR en BR
    setnames(Part_Prod, "Lig_Country", "Col_Country") # Recode pour passer les PR en BR
    interm<-rbind(Part_Prod,Part_SommeCI)
    interm<-interm[,.(sum(value)), by=c("MRIO","year","Col_Indus","Col_Country")]
    setnames(interm, "V1", "value")
    setorder(interm,Col_Country,Col_Indus)
  }
  if(typeCompo=="L"){ # Calcul de l'inverse de Léontief
    A<-CompoECOLEouA17(interm,"A",date=date,OptTab=TRUE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000)
    nb_ligcol<-nrow(A)
    MatrixA<-as.matrix(A[,4:ncol(A)])
    interm<-inversion_rcpp3(diag(nb_ligcol)-MatrixA)
    colnames(interm)<-colnames(A)[4:ncol(A)]
    rownames(interm)<-colnames(A)[4:ncol(A)]
    interm<-as.data.frame(interm)
  }
  if(typeCompo=="invB"){ # Calcul de l'inverse de Ghosh
    B<-CompoECOLEouA17(interm,"B",date=date,OptTab=TRUE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000)
    nb_ligcol<-nrow(B)
    MatrixB<-as.matrix(B[,4:ncol(B)])
    interm<-inversion_rcpp3(diag(nb_ligcol)-MatrixB)
    colnames(interm)<-colnames(B)[4:ncol(B)]
    rownames(interm)<-colnames(B)[4:ncol(B)]
    interm<-as.data.frame(interm)
  }
  if(typeCompo=="OptFullOptions"){
    interm1<-CompoECOLEouA17(dt_MRIO,"PROD",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm2<-CompoECOLEouA17(dt_MRIO,"CI",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm3<-CompoECOLEouA17(dt_MRIO,"DF",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm4<-CompoECOLEouA17(dt_MRIO,"VA",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm5<-CompoECOLEouA17(dt_MRIO,"CI_PR",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm6<-CompoECOLEouA17(dt_MRIO,"CI_BR",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm7<-CompoECOLEouA17(dt_MRIO,"DF_TOT",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm8<-CompoECOLEouA17(dt_MRIO,"A",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm2tab<-CompoECOLEouA17(dt_MRIO,"CI",date=date,country=country,OptTab=TRUE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm3tab<-CompoECOLEouA17(dt_MRIO,"DF",date=date,country=country,OptTab=TRUE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm8tab<-CompoECOLEouA17(dt_MRIO,"A",date=date,country=country,OptTab=TRUE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm<-list(PROD=interm1,CI=interm2,DF=interm3,VA=interm4,CI_PR=interm5,CI_BR=interm6,DF_TOT=interm7,A=interm8,CI_tab=interm2tab,DF_tab=interm3tab,A_tab=interm8tab)
  }
  if(typeCompo=="OptFullOptionsBonus"){
    interm1<-CompoECOLEouA17(dt_MRIO,"PROD",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm2<-CompoECOLEouA17(dt_MRIO,"CI",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm3<-CompoECOLEouA17(dt_MRIO,"DF",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm4<-CompoECOLEouA17(dt_MRIO,"VA",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm5<-CompoECOLEouA17(dt_MRIO,"CI_PR",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm6<-CompoECOLEouA17(dt_MRIO,"CI_BR",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm7<-CompoECOLEouA17(dt_MRIO,"DF_TOT",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm8<-CompoECOLEouA17(dt_MRIO,"A",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm2tab<-CompoECOLEouA17(dt_MRIO,"CI",date=date,country=country,OptTab=TRUE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm3tab<-CompoECOLEouA17(dt_MRIO,"DF",date=date,country=country,OptTab=TRUE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm8tab<-CompoECOLEouA17(dt_MRIO,"A",date=date,country=country,OptTab=TRUE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm9<-CompoECOLEouA17(dt_MRIO,"B",date=date,country=country,OptTab=FALSE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm10<-CompoECOLEouA17(dt_MRIO,"B",date=date,country=country,OptTab=TRUE,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000 )
    interm11<-CompoECOLEouA17(dt_MRIO,"L",date=date,country=country,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000) # Resultat directement sous forme de tableau
    interm12<-CompoECOLEouA17(dt_MRIO,"invB",date=date,country=country,OptMRIO1014=OptMRIO1014,OptMRIO2000=OptMRIO2000) # Resultat directement sous forme de tableau
    interm<-list(PROD=interm1,CI=interm2,DF=interm3,VA=interm4,CI_PR=interm5,CI_BR=interm6,DF_TOT=interm7,A=interm8,CI_tab=interm2tab,DF_tab=interm3tab,A_tab=interm8tab,B=interm9,B_tab=interm10,L=interm11,InvBGhosh=interm12)
  }
  return(interm)
}

# Fonction pour vider le garbage collector afin d'accroitre le buffer pour les exports XLS
jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}  

# Export de la liste full options en Excel
exportListFullOptions<- function(ListFull,Nomfichier)  
{
  gc()
  jgc()
  write.xlsx(ListFull[["PROD"]], paste0(Nomfichier,".xlsx"), sheetName = "PROD", col.names = TRUE, row.names = TRUE)
  write.xlsx(ListFull[["CI_tab"]], paste0(Nomfichier,".xlsx"), sheetName = "CI_tab", col.names = TRUE, row.names = TRUE,append=TRUE)
  write.xlsx(ListFull[["A_tab"]], paste0(Nomfichier,".xlsx"), sheetName = "A_tab", col.names = TRUE, row.names = TRUE,append=TRUE)
  write.xlsx(ListFull[["DF_tab"]], paste0(Nomfichier,".xlsx"), sheetName = "DF_tab", col.names = TRUE, row.names = TRUE,append=TRUE)
  write.xlsx(ListFull[["DF_TOT"]], paste0(Nomfichier,".xlsx"), sheetName = "DF_TOT", col.names = TRUE, row.names = TRUE,append=TRUE)
  write.xlsx(ListFull[["CI_PR"]], paste0(Nomfichier,".xlsx"), sheetName = "CI_PR", col.names = TRUE, row.names = TRUE,append=TRUE)
  write.xlsx(ListFull[["CI_BR"]], paste0(Nomfichier,".xlsx"), sheetName = "CI_BR", col.names = TRUE, row.names = TRUE,append=TRUE)
  write.xlsx(ListFull[["VA"]], paste0(Nomfichier,".xlsx"), sheetName = "VA", col.names = TRUE, row.names = TRUE,append=TRUE)
  write.xlsx(ListFull[["CI"]], paste0(Nomfichier,".xlsx"), sheetName = "CI", col.names = TRUE, row.names = TRUE,append=TRUE)
  write.xlsx(ListFull[["A"]], paste0(Nomfichier,".xlsx"), sheetName = "A", col.names = TRUE, row.names = TRUE,append=TRUE)
  write.xlsx(ListFull[["DF"]], paste0(Nomfichier,".xlsx"), sheetName = "DF", col.names = TRUE, row.names = TRUE,append=TRUE)
  return(cat("OK printed"))
}

# Ajout de pr et BR par concaténation
AjoutPRBR<- function(dt)  
{
  interm<-dt
  interm$PR<-paste0(interm$Lig_Country,"_",interm$Lig_Indus)
  interm$BR<-paste0(interm$Col_Country,"_",interm$Col_Indus)
  return(interm)
}

# Split de pr et BR en 4 composantes
SplitPRBR<- function(dt)  
{
  interm<-dt
  
 # Gestion du cas particulier UE_OTHERS (le seul a avoir un underscore qui est malheureusement utilisé comme delimiteur)
  interm$PR<-stringr::str_replace(interm$PR, "UE_OTHERS", "UEOTHERS")
  interm$BR<-stringr::str_replace(interm$BR, "UE_OTHERS", "UEOTHERS")
  
  # Split des PR et BR
  interm$Lig_Country<-str_extract(interm$PR, "[^_]+")
  interm$Lig_Indus<-substring(str_extract(interm$PR, "_.+$"),2)
  interm$Col_Country<-str_extract(interm$BR, "[^_]+")
  interm$Col_Indus<-substring(str_extract(interm$BR, "_.+$"),2)
  interm<-interm[,PR:=NULL]
  interm<-interm[,BR:=NULL]
  
  # REACTUALISATION DE UE_OTHER
  interm$Lig_Country<-stringr::str_replace(interm$Lig_Country, "UEOTHERS", "UE_OTHERS")
  interm$Col_Country<-stringr::str_replace(interm$Col_Country, "UEOTHERS", "UE_OTHERS")
  
  return(interm)
}



########################################################################################################################



# Somme par produit du TEI ou autre df, vectorisé  // utilise la fonction vectDF définie par ailleurs
TotPR<- function(df)  
{
interm<-vectDF(rowSums(df))
return(interm)
}

# Count negat dans le TES  // utilise la fonction CountNegat définie par ailleurs
CheckNegatERE<- function(listeTES)  
  # La listeTES doit contenir {trp,tei,tef} ou (matprod, trp_imp,tei_dom,tei_imp,tef_dom,tef_imp)
{
  nbComposantes<-length(listeTES)
  if(nbComposantes==3){ # TES classique
  listout<-list(CountNegat(listeTES[[1]]),CountNegat(listeTES[[2]]),CountNegat(listeTES[[3]]))  
  }
  if(nbComposantes==6){ # TESS
  listout<-list(CountNegat(listeTES[[1]]),CountNegat(listeTES[[2]]),CountNegat(listeTES[[3]]),CountNegat(listeTES[[4]]),CountNegat(listeTES[[5]]),CountNegat(listeTES[[6]]))             
  }
  return (listout)
}

# Fonction de calcul et mise en forme des indicateurs avec 2 options (avec ou sans MAPE)
MEFIndics<- function(df_actualOLD,df_actual,df_prev,nbIndics=4){
Indics_QuasiCompt <-as.data.frame(matrix(nrow=1,ncol=6))
colnames(Indics_QuasiCompt)<-c("THEIL_Accuracy","THEIL_Quality","MAPE","MAE","STPE","RMSE%")
Indics_QuasiCompt[1,]<-cbind(Indic_THEILU_error(df_actualOLD,df_actual,df_prev),Indic_error(df_actual,df_prev,"FULL")) 
if(nbIndics==4){
  return(Indics_QuasiCompt[c(1,2,5,6)])
}else{
  return(Indics_QuasiCompt)
}
}

# Fonction de transformation en vecteur dataframe
vectDF<- function(vect){
  out<-as.data.frame(vect, drop=FALSE)
  return(out)
}

# Fonction de check de cohérence des ERE
CheckERE<- function(listeTES)  
  # La listeTES doit contenir {trp,tei,tef} ou (matprod, trp_imp,tei_dom,tei_imp,tef_dom,tef_imp)
{
  nbComposantes<-length(listeTES)
  if(nbComposantes==3){ # TES classique
    interm1<-listeTES[[1]][1:138,"Tot_ress_pxAcqu"]-(vectDF(rowSums(listeTES[[2]][1:138,]))+listeTES[[3]][1:138,"TOT_Emplois_Finals"])
    interm2<-listeTES[[3]][1:138,"TOT_Emplois_Finals"]-(listeTES[[3]][1:138,"CF_TOT"]+listeTES[[3]][1:138,"FBC_TOT"]+listeTES[[3]][1:138,"Exp_B&S"])+listeTES[[3]][1:138,"CF_TOT"]-(listeTES[[3]][1:138,"CF_men"]+listeTES[[3]][1:138,"CF_APU"]+listeTES[[3]][1:138,"ISBLSM"])+listeTES[[3]][1:138,"FBC_TOT"]-(listeTES[[3]][1:138,"FBCF_TOT"]+listeTES[[3]][1:138,"AcqCess_Obj_valeurs"]+listeTES[[3]][1:138,"VarStocks"])
    listout<-list(interm1[interm1>0.1],interm2[interm1>0.1])  
  }
  if(nbComposantes==6){ # TESS
    interm1<-vectDF((vectDF(rowSums(listeTES[[1]][1:138,]))+listeTES[[2]][1:138,"IMP_TOT_RESS"]))-(vectDF(rowSums(listeTES[[3]][1:138,]))+vectDF(rowSums(listeTES[[4]][1:138,]))+listeTES[[5]][1:138,"TOT_EmplFinals"]+listeTES[[6]][1:138,"TOT_EmplFinals"])
    interm2<-listeTES[[5]][1:138,"TOT_EmplFinals"]-(listeTES[[5]][1:138,"TOT_Emplois"]+vectDF(rowSums(listeTES[[3]][1:138,])))   +   listeTES[[6]][1:138,"TOT_EmplFinals"]-(listeTES[[6]][1:138,"TOT_Emplois"]+vectDF(rowSums(listeTES[[4]][1:138,])))
    listout<-list(interm1[interm1>0.1],interm2[interm1>0.1])                
  }
  return (print(listout))
}



# Fonction de ventilation d'un vecteur sur un TEI (ou potentiellement toute matrice)
Ventil_VectsurTEI<- function(vect,DFtei)  # Options : "Classic" ou "TESS" : Attzention les noms de colonne diffèrent.
{
  MetVect<-CbindMulti(vect,ncol(DFtei))
  DFout<-DFtei*MetVect
  DFout<-GereInfNA(DFout)
  return(DFout)
}


# Fonction de recalcul des totaux du TEF
Recalcuc_Totaux_TEF<- function(df,ci=NULL,Typ_TEF="Classic")  # Options : "Classic" ou "TESS" ou "TESS_A48" : Attention les noms de colonne diffèrent.
{
  if(Typ_TEF=="Classic"){
    # Recalcul des agrégats de la demande finale (type FBC)
    df<-GereInfNA(df)
    df[1:138,"CF_TOT"]<-df[1:138,"CF_men"]+df[1:138,"CF_APU"]+df[1:138,"ISBLSM"]
    df[1:138,"FBC_TOT"]<-df[1:138,"FBCF_TOT"]+df[1:138,"AcqCess_Obj_valeurs"]+df[1:138,"VarStocks"]
    df[1:138,"TOT_Emplois_Finals"]<-df[1:138,"CF_TOT"]+df[1:138,"FBC_TOT"]+df[1:138,"Exp_B&S"]
  }
  
  if(Typ_TEF=="TESS"){
  df[1:138,"CF_TOT"]<-df[1:138,"CF_Men"]+df[1:138,"CF_APU"]+df[1:138,"CF_ISBLSM"]
  df[1:138,"FBC"]<-df[1:138,"FBCF"]+df[1:138,"Obj_Val"]+df[1:138,"VarStocks"]
  df[1:138,"TOT_EmplFinals"]<-df[1:138,"CF_TOT"]+df[1:138,"FBC"]+df[1:138,"Exports"]
  df[1:138,"TOT_Emplois"]<-df[1:138,"TOT_EmplFinals"]+TotPR(ci[1:138,])
  }  
  
  if(Typ_TEF=="TESS_A48"){
    df[1:48,"CF_TOT"]<-df[1:48,"CF_Men"]+df[1:48,"CF_APU"]+df[1:48,"CF_ISBLSM"]
    df[1:48,"TOT_EmplFinals"]<-df[1:48,"CF_TOT"]+df[1:48,"FBCF"]+df[1:48,"VarStocksObjVal"]+df[1:48,"Exports"]
    df[1:48,"TOT_Emplois"]<-df[1:48,"TOT_EmplFinals"]+TotPR(ci[1:48,])
  }
  return(df)
  }


# Fonction pour convertir les infinity en NA et les NA en 0 ou 1 par exemple
GereInfNA <- function(df,impute=0){  # Fonction de contrôle de la cohérence comptable.
  if(ncol(df)==1){ # Si c'est un vecteur qui est gere comme une liste
  df<-lapply(df, function(x) {
    x[is.infinite(x)] <- impute 
    return(x)
    } )
  df<-lapply(df, function(x) {
    x[is.na(x)] <- impute 
    return(x)
  } )
  df<-vectDF(df)
}else{ # Si c'est une matrice avec plusieurs colonnes
  is.na(df)<-sapply(df, is.infinite)
  df[is.na(df)]<-impute
}
  return(df)
}
# test
# Age <- c(23, 41, Inf, 58, 26)
# df <- data.frame( Age)
# toto<-GereInfNA(df)

##########################################

CountNegat <- function(df)  # Fonction de contrôle de la cohérence comptable.
{
  df<-as.data.frame(df)
  df_w<-df
  
  # On check si Varstocks ou fbc est dans la liste des colonnes et on les supprime du DF si c'est le cas
  if("VarStocks" %in% colnames(df)){df_w$VarStocks<-NULL}
  if("FBC" %in% colnames(df)){df_w$FBC<-NULL}
  if("VarStocks_objValeur_(P54)" %in% colnames(df)){df_w$VarStocks_objValeur_(P54)<-NULL}
  if("FBC_TOT" %in% colnames(df)){df_w$FBC_TOT<-NULL}
  
  df_w<-as.data.frame(df_w)
  is.na(df_w)<-sapply(df_w, is.infinite)
  df_w[is.na(df_w)]<-0
  
  FirstTab<-AddRownamesToFirstCol(df_w) # rajoute le nom des lignes dans la premiÃ¨re colonne
  colnames(FirstTab)[1]<-"Produit"
  Cadre<- FirstTab %>% tidyr::pivot_longer(cols=!Produit, 
                                           names_to = "ColumnTab", 
                                           values_to = "CheckNeg")
  Cadre$Lib<-paste0(Cadre$Produit,"_#_",Cadre$ColumnTab)
  listNoms<-Cadre$Lib
  Cadre <- Cadre %>% select(Lib, "CheckNeg")
  Cadre<-GetRownamesFromFirstCol(Cadre)
  Cadre<-as.list(t(Cadre))
  names(Cadre) <- listNoms
  Ncadre<-length(Cadre)

  for(k in 1:Ncadre){
    if(unlist(Cadre[[(Ncadre+1-k)]])>=0){Cadre<-Cadre[-((Ncadre+1-k))] }
  }
   # out<-as.data.frame(Cadre[Cadre<0,],drop=FALSE)
 
   Cadre<-as.data.frame(t(Cadre))
  return (t(Cadre))
}
# Tests
# toto<-TEF_TESS_DOM_2017
# toto[3:5,3:5]<--23
# toto3<-CountNegat(toto) 
 

                  
ComparTwoTab <- function(tab1,tab2,tri=NULL)  # Fonction de comparaison de 2 tableaux avec type de tri au choix : ECART_NIV ou ECART_REL ou RANK (somme du classement des 2 premiers indics)
{
if((dim(tab1)[1]!=dim(tab2)[1]) | (dim(tab1)[2]!=dim(tab2)[2])){return(print("Error dimensions inconsistent"))}
  tab1<-as.data.frame(tab1)
  is.na(tab1)<-sapply(tab1, is.infinite)
  tab1[is.na(tab1)]<-0
  tab2<-as.data.frame(tab2)
  is.na(tab2)<-sapply(tab2, is.infinite)
  tab2[is.na(tab2)]<-0
  
  FirstTab<-tab1 # Initialisation sur la premiere annee avant de boucler sur les autres
  FirstTab<-AddRownamesToFirstCol(FirstTab) # rajoute le nom des lignes dans la premiÃ¨re colonne
  colnames(FirstTab)[1]<-"Produit"
  Cadre<- FirstTab %>% tidyr::pivot_longer(cols=!Produit, 
                                           names_to = "ColumnTab", 
                                           values_to = "tab1")
  Cadre$Lib<-paste0(Cadre$Produit,"_#_",Cadre$ColumnTab)
  Cadre <- Cadre %>% select(Lib, "tab1")
  Cadre<-GetRownamesFromFirstCol(Cadre)
  TabOut<-Cadre
  
  # Traitement du second tableau
    Tab<-tab2 # Initialisation sur la premiere annee avant de boucler sur les autres
    Tab<-AddRownamesToFirstCol(Tab) # rajoute le nom des lignes dans la premiÃ¨re colonne
    colnames(Tab)[1]<-"Produit"
    Cadre<- Tab %>% tidyr::pivot_longer(cols=!Produit, 
                                        names_to = "ColumnTab", 
                                        values_to = "tab2")
    Cadre$Lib<-paste0(Cadre$Produit,"_#_",Cadre$ColumnTab)
    Cadre <- Cadre %>% select(Lib, "tab2")
    Cadre<-GetRownamesFromFirstCol(Cadre)
    TabOut<-cbind(TabOut,Cadre)
  
    TabOut$Ecart_Niv<-abs(TabOut$tab2-TabOut$tab1)
    TabOut$Ecart_Rel<-abs(round(100*(TabOut$tab2-TabOut$tab1)/TabOut$tab1,2))
    is.na(TabOut)<-sapply(TabOut, is.infinite)
    TabOut[is.na(TabOut)]<-0
    
    if(tri=="ECART_NIV"){
      TabOut<-TabOut[order(TabOut$Ecart_Niv,decreasing = TRUE),]
    }
    if(tri=="ECART_REL"){
      TabOut<-TabOut[order(TabOut$Ecart_Rel,decreasing = TRUE),]
    }
    if(tri=="RANK"){
      TabOut$RankCombi<-rank(abs(TabOut$Ecart_Niv))+rank(abs(TabOut$Ecart_Rel))
      TabOut<-TabOut[order(TabOut$RankCombi,decreasing = TRUE),]
    }
  return(round(TabOut,3))
}
#Test
# toto<-ComparTwoTab(TEF_TESS_DOM_2017,TEF_TESS_DOM_2018,tri="ECART_NIV")
# toto2<-ComparTwoTab(TEF_TESS_DOM_2017,TEF_TESS_DOM_2018,tri="ECART_REL")
# toto3<-ComparTwoTab(TEF_TESS_DOM_2017,TEF_TESS_DOM_2018,tri="RANK")


CountNAInf <- function(df)  # Fonction de contrôle de la cohérence comptable.
{
  df<-as.data.frame(df)
  is.na(df)<-sapply(df, is.infinite)
  interm<-sum(is.na(df))
  
  return (print(paste0("Nombre de NA ou de Inf : " ,interm)))
}
# Tests
# toto<-TEF_CT_2016
# toto[3:5,3:5]<-NA
# CountNAInf(toto)


CheckCAFAB <- function(produit)  # Fonction de contrôle de la cohérence comptable.
{
  interm<-TRPClass_an[produit,"corr_CAFAB"]-(
    (TRPClass_an_w[produit,"Imp_services"]-TRPClass_an[produit,"Imp_services"])-
      (TEFClass_an_w[produit,"Exp_B&S"]-TEFClass_an[produit,"Exp_B&S"]))
  return (print(paste0(produit, " : " ,interm)))
}

Struct <- function(df,sens="lig") # = lig ou col   et df est une matrice, pas de vecteurs
{
  df<-as.data.frame(df)
  is.na(df)<-sapply(df, is.infinite)
  df[is.na(df)]<-0
  df<-as.data.frame(df)
  somlig<-colSums(df)
  somcol<-rowSums(df)
  
  if(sens=="lig"){
    interm<-somlig
    vect<-somlig
    for(k in 1:(nrow(df)-1)){
      interm<-rbind(interm,vect)
    }
    interm <-as.data.frame(interm)
    is.na(interm)<-sapply(interm, is.infinite)
    interm[is.na(interm)]<-0
    interm <-as.data.frame(interm)
    dfout<-df/interm
    is.na(dfout)<-sapply(dfout, is.infinite)
    dfout[is.na(dfout)]<-0
  }
  
  if(sens=="col"){
    interm<-somcol
    vect<-somcol
    for(k in 1:(nrow(df)-1)){
      interm<-cbind(interm,vect)
    }
    interm <-as.data.frame(interm)
    is.na(interm)<-sapply(interm, is.infinite)
    interm[is.na(interm)]<-0
    interm <-as.data.frame(interm)
    dfout<-df/interm
    is.na(dfout)<-sapply(dfout, is.infinite)
    dfout[is.na(dfout)]<-0
  }  

  return (dfout)
}

CbindMulti <- function(vect,nb)
{
  vect<-as.data.frame(vect,DROP=FALSE)
  df<-vect
  for(k in 1:(nb-1)){
    df<-cbind(df,vect)
  }
  df <-as.data.frame(df)
  is.na(df)<-sapply(df, is.infinite)
  df[is.na(df)]<-0
  df <-as.data.frame(df)
  return (df)
}

GetRownamesFromFirstCol <- function(df)
{
  df<-as.data.frame(df)
  rownames(df)<-as.character(df[,1])
  dfout<-df[,-1,drop=FALSE]
  return (dfout)
}

AddRownamesToFirstCol <- function(df)
{
  df<-as.data.frame(df)
  df2<-cbind(rownames(df),df)
  df2<-as.data.frame(df2)
  colnames(df2)[1]<-"joint"
  return (df2)
}
#test<-AddRownamesToFirstCol(TESClass_TEI_xxxx)

AddChrToFirstColRownames <- function(df,chr)
{
  df<-as.data.frame(df)
  df2<-df
  df2[,1]<-paste0(chr,df[,1])
  return (df2)
}
#test2<-AddChrToFirstColRownames(test,"G")
#toto3<-t(TESClass_Transferts_xxxx)

AgregNivG_toA64A48_1D <- function(df, transp=FALSE,BoolPCHRCAFAB=FALSE)
{
  Prod138<-read_excel("TabPass_MRIO_FC.xlsx",sheet = "Base_Industries", col_names = TRUE)  
  df<-as.data.frame(df)
  tabpass<-cbind(Prod138$A138,Prod138$A64,Prod138$A48_CT)
  tabpass<-as.data.frame(tabpass)
  colnames(tabpass)[1]<-"A138"
  colnames(tabpass)[2]<-"A64"
  colnames(tabpass)[3]<-"A48_CT"
  if(isTRUE(transp)){df<-t(df)}
  df2<-AddRownamesToFirstCol(df)
  nbcol<-ncol(df2)
  df3<-AddChrToFirstColRownames(df2,"G")
  colnames(df3)[1]<-"A138"
  TabJoined<-left_join(tabpass, df3, by = "A138")
  AgregA64<-TabJoined[,-3] %>% 
    group_by(A64) %>% 
    summarise_at(vars(2:nbcol),sum, na.rm = TRUE)
  AgregA48<-TabJoined[,-2] %>% 
    group_by(A48_CT) %>% 
    summarise_at(vars(2:nbcol),sum, na.rm = TRUE)
  AgregA64<-AgregA64[-67,] # Retirer la derniÃ¨re ligne qui est nulle
  AgregA48<-AgregA48[-51,] # Retirer la derniÃ¨re ligne qui est nulle
  AgregA64<-GetRownamesFromFirstCol(AgregA64)
  AgregA48<-GetRownamesFromFirstCol(AgregA48)
  
    SelecPCHTRCAFAB64<-AgregA64[c('PCHTR','PCAFAB'),] #On extrait les 2 lignes qui ne sont pas au bon endroit afin de les rÃ©introduires Ã  la fin ensuite.
    AgregA64<-AgregA64[-c(55,56),]
    SelecPCHTRCAFAB48<-AgregA48[c('PCHTR','PCAFAB'),] #On extrait les 2 lignes qui ne sont pas au bon endroit afin de les rÃ©introduires Ã  la fin ensuite.
    AgregA48<-AgregA48[-c(43,44),]
    
    if(isTRUE(BoolPCHRCAFAB)){  # Si correct territoriale et CAFAb prÃ©sents : necessiter de reordonner les DF agreges pour placer les 2 lignes a la fin dans le bon ordre
    AgregA64<-rbind(AgregA64,SelecPCHTRCAFAB64)
    AgregA48<-rbind(AgregA48,SelecPCHTRCAFAB48)
    }
    if(isTRUE(transp)){
      AgregA64<-t(AgregA64)
      AgregA48<-t(AgregA48)
      }
  return (list(as.data.frame(AgregA64),as.data.frame(AgregA48)))
}

#toto4<-AgregNivG_toA64A48_1D(TESClass_TRP_xxxx,BoolPCHRCAFAB=TRUE)
#toto5<-AgregNivG_toA64A48_1D(TESClass_CPR_xxxx,transp=TRUE)
#toto6<-AgregNivG_toA64A48_1D(TESS_Dom_TEF_xxxx,BoolPCHRCAFAB=TRUE)
#toto7<-AgregNivG_toA64A48_1D(TESS_Dom_TEI_xxxx,transp=TRUE)

AgregNivG_toA64A48_2D <- function(df, BoolPCHRCAFAB=FALSE)
{# Attention les eventuels PCHTR et CAFAB ne sont qu en ligne car ce sont des produits uniquement
  
  #Agregation des lignes de la matrice carree
  list_res1<-AgregNivG_toA64A48_1D(df, transp=FALSE,BoolPCHRCAFAB=BoolPCHRCAFAB)
  
  #Agregation des colonnes en mobilisant l'option transposee
  list_res2<-AgregNivG_toA64A48_1D(list_res1[1], transp=TRUE,BoolPCHRCAFAB=FALSE)  
  list_res3<-AgregNivG_toA64A48_1D(list_res1[2], transp=TRUE,BoolPCHRCAFAB=FALSE)  
  
  return (list(as.data.frame(list_res2[1]),as.data.frame(list_res3[2])))
}

#toto8<-AgregNivG_toA64A48_2D(TESClass_TEI_xxxx, BoolPCHRCAFAB=TRUE)
#toto9<-AgregNivG_toA64A48_2D(TESS_Dom_TEI_xxxx, BoolPCHRCAFAB=TRUE)

AgregNivA48_toH45_1D <- function(df, transp=FALSE,BoolPCHRCAFAB=FALSE)
{
  Prod138<-read_excel("TabPass_MRIO_FC.xlsx",sheet = "Base_Industries", col_names = TRUE)  
  df<-as.data.frame(df)
  tabpass<-cbind(Prod138$A48_CT,Prod138$H45_CT)
  tabpass<-tabpass[!duplicated(tabpass), ]
  tabpass<-as.data.frame(tabpass)
  colnames(tabpass)[1]<-"A48_CT"
  colnames(tabpass)[2]<-"H45_CT"
  tabpass[,1]<-paste0("F",tabpass[,1])
  tabpass[,2]<-paste0("F",tabpass[,2])
  tabpass<-as.data.frame(tabpass)
  if(isTRUE(transp)){df<-t(df)}
  df2<-AddRownamesToFirstCol(df)
  nbcol<-ncol(df2)
  #df3<-AddChrToFirstColRownames(df2,"F")
  df3<-df2
  colnames(df3)[1]<-"A48_CT"
  TabJoined<-left_join(tabpass, df3, by = "A48_CT")
  TabJoined<-TabJoined[-51,]  # Retirer la derniÃ¨re ligne qui est nulle
  AgregH45<-TabJoined %>% 
    group_by(H45_CT) %>% 
    summarise_at(vars(2:nbcol),sum, na.rm = TRUE)
 #AgregH45<-AgregH45[-48,] # Retirer la derniÃ¨re ligne qui est nulle
  AgregH45<-GetRownamesFromFirstCol(AgregH45)
  
  SelecPCHTRCAFAB45<-AgregH45[c('PCHTR','PCAFAB'),] #On extrait les 2 lignes qui ne sont pas au bon endroit afin de les rÃ©introduires Ã  la fin ensuite.
  AgregH45<-AgregH45[-c(40,41),]
  AgregH45<-ReordCodeEtLib(AgregH45)
  AgregH45<-AgregH45[-c(46,47),] # On retire PCHTR et CAFAB qui sont ajoutés par le réordonnancement
  
  if(isTRUE(BoolPCHRCAFAB)){  # Si correct territoriale et CAFAb prÃ©sents : necessiter de reordonner les DF agreges pour placer les 2 lignes a la fin dans le bon ordre
    AgregH45<-rbind(AgregH45,SelecPCHTRCAFAB45)
  }
  if(isTRUE(transp)){
    AgregH45<-t(AgregH45)
  }
  return (as.data.frame(AgregH45))
}

#toto4<-AgregNivA48_toH45_1D(TESClass_TRP_xxxx,BoolPCHRCAFAB=TRUE)
#toto5<-AgregNivA48_toH45_1D(TESClass_CPR_xxxx,transp=TRUE)
#toto6<-AgregNivA48_toH45_1D(TESS_Dom_TEF_xxxx,BoolPCHRCAFAB=TRUE)
#toto7<-AgregNivA48_toH45_1D(TESS_Dom_TEI_xxxx,transp=TRUE)

AgregNivA48_toH45_2D <- function(df, BoolPCHRCAFAB=FALSE)
{# Attention les eventuels PCHTR et CAFAB ne sont qu en ligne car ce sont des produits uniquement
  
  #Agregation des lignes de la matrice carree
  list_res1<-AgregNivA48_toH45_1D(df, transp=FALSE,BoolPCHRCAFAB=BoolPCHRCAFAB)
  
  #Agregation des colonnes en mobilisant l'option transposee
  list_res2<-AgregNivA48_toH45_1D(list_res1, transp=TRUE,BoolPCHRCAFAB=FALSE)  
  
  return (as.data.frame(list_res2))
}

#toto8<-AgregNivA48_toH45_2D(TESClass_TEI_xxxx, BoolPCHRCAFAB=TRUE)
#toto9<-AgregNivA48_toH45_2D(TESS_Dom_TEI_xxxx, BoolPCHRCAFAB=TRUE)

ReordCodeEtLib <- function(df, OptLib=FALSE,CAFAB=TRUE)
{ #Attention : on suppose que les tables de reordonnancement et de libelles sont dÃ©jÃ  chargÃ©es, sinon les charger ici.
  df<-as.data.frame(df)
  nblig<-nrow(df)
  
  if(nblig<=47){ #DÃ©tection niveau H45 
    df2<-AddRownamesToFirstCol(df)
    nbcol<-ncol(df2)
    df3<-df2  # AddChrToFirstColRownames(df2,"F")
    colnames(df3)[1]<-"H45"
    colnames(TabOrdLib_H45)[1]<-"H45"
    TabJoined<-left_join(TabOrdLib_H45, df3, by = "H45")
  }
  
  if(nblig>47 & nblig<55){ #DÃ©tection niveau A48 
    df2<-AddRownamesToFirstCol(df)
    nbcol<-ncol(df2)
    df3<-AddChrToFirstColRownames(df2,"F")
    colnames(df3)[1]<-"A48"
    colnames(TabOrdLib_48)[1]<-"A48"
    TabJoined<-left_join(TabOrdLib_48, df3, by = "A48")
  }
  
  if(nblig>55 & nblig<75){ #DÃ©tection niveau A64 
    df2<-AddRownamesToFirstCol(df)
    nbcol<-ncol(df2)
    colnames(df2)[1]<-"A64"
    colnames(TabOrdLib_64)[1]<-"A64"
    TabJoined<-left_join(TabOrdLib_64, df2, by = "A64")
  }
  
  if(nblig>75){ #DÃ©tection niveau A138 
    df2<-AddRownamesToFirstCol(df)
    nbcol<-ncol(df2)
    df3<-AddChrToFirstColRownames(df2,"G")
    colnames(df3)[1]<-"A138"
    colnames(TabOrdLib_138)[1]<-"A138"
    TabJoined<-left_join(TabOrdLib_138, df3, by = "A138")
  }
  
  if(!isTRUE(OptLib)){
    TabJoined<-TabJoined[,-2]
  }
  if(!isTRUE(CAFAB)){
    n<-dim(TabJoined)[1]
    TabJoined<-TabJoined[1:(n-2),]
  }  
  TabJoined<-as.data.frame(TabJoined)
  TabJoined<-GetRownamesFromFirstCol(TabJoined)
  TabJoined<-as.data.frame(TabJoined)
  return(TabJoined)
}
#toto1<-TEFAgg[[2]]
#toto2<-ReordCodeEtLib(toto1)
#toto3<-ReordCodeEtLib(toto1, OptLib=TRUE)
#toto4<-ReordCodeEtLib(toto1,CAFAB=TRUE)

#toto5<-TEFAgg[[1]]
#toto6<-ReordCodeEtLib(toto5)

#toto7<-TEFimp
#toto8<-ReordCodeEtLib(toto7)



ReordCodeEtLib_2D <- function(df,CAFAB=TRUE)
{ #Attention : on suppose que les tables de reordonnancement et de libelles sont dÃ©jÃ  chargÃ©es, sinon les charger ici.
  df<-as.data.frame(df)

    df2<-ReordCodeEtLib(df,CAFAB=TRUE)
    tdf2<-t(df2)
    df3<-ReordCodeEtLib(tdf2,CAFAB=FALSE)
    dfout<-t(df3)
    nblig<-nrow(dfout)
    dfout2<-dfout
    
  if(!isTRUE(CAFAB)){ # Par defaut on affiche les lignes PCHTR et CABFAB, mais on peut aussi les supprimer si besoin
    dfout2<-dfout[1:(nblig-2),]
  }  
    dfout2<-as.data.frame(dfout2)
  return(dfout2)
}

#toto1<-TEIAgg[[2]]
#toto2<-ReordCodeEtLib_2D(toto1)
#toto3<-ReordCodeEtLib_2D(toto1,CAFAB=FALSE)

#toto7<-TEI_dom
#toto8<-ReordCodeEtLib_2D(toto7)
#toto9<-ReordCodeEtLib_2D(toto7,CAFAB=FALSE)


######## Indicateurs de comparaison de tableaux

# InterprÃ©tation :
#________________________________________________
#  THEIL Accuracy and Quality  : Theilâ€™s U (Theil, 1971). When U = 0 the forecasts are perfect. That means if U>0 then forecasts are not perfect. As we keep increasing U, the forecasts will keep becoming more and more imperfect. When it reaches 1, it means Forecast is equivalent to Naive Method. Hence, a general pattern can be seen that as U increases forecast is wrong and at 1 forecast is same as naive and hence by extrapolating this we can say that if U>1 then the forecast is worse than naive method forecast.
#________________________________________________
#  MAPE  : The mean absolute percent error (MAPE; Butterfield and Mules, 1980) expresses accuracy as a percentage of the error. Because the MAPE is a percentage, it can be easier to understand than the other accuracy measure statistics. For example, if the MAPE is 5, on average, the forecast is off by 5%. However, sometimes you may see a very large value of MAPE even though the model appears to fit the data well. If any data values are close to 0. Because MAPE divides the absolute error by the actual data, values close to 0 can greatly inflate the MAPE.-> probleme chez nous avec la moyenne et les faibles montants => ne pas accorder trop d'importnace a cet indicateur. 
#________________________________________________
#  MAE : The Mean Absolute Error (MAE) is good to measure forecast accuracy. As the name implies, it is the mean of the absolute error. -> InconvÃ©nient de la moyenne mais avantage de ne pas Ãªtre affectÃ© par les faibles montants contrairement Ã  MAPE. A privilÃ©gier rpar rapport Ã  MAPE.
#________________________________________________
#  STPE  : Standardized Total Percentage Error (STPE; Miller and Blair, 1985). Version amÃ©liorÃ©e de MAPE dans notre cas car ce ne sont pas les Ã©carts relatifs mais les abs(Ã©cart) qui sont comparÃ©s au total du niveau (homogÃ©nÃ©isation). Ne fournit pas d'informations sur la distribution des Ã©carts (a complÃ©ter par RMSE).
#________________________________________________
#  RMSE%  : The Root Mean Squared Error (RMSE) is defined as the square root of the average squared error. -> Avantage pour dÃ©tecter les gros Ã©carts qui surpondÃ¨rent l'indicateur car mis au carrÃ©. => Permet d'identidier si on a bcp de petits Ã©carts ou plutÃ´t quelques grands Ã©carts.
#________________________________________________
#  guide de lecture  : PrivilÃ©gier les 2 THEIL's U, s'appuyer sur STPE pour avoir l'ampleur des Ã©carts et sur RMSE% pour avoir une idÃ©e de la distribution des Ã©carts (si important => certains gros Ã©carts probablement)

Indic_THEILU_error <- function(df_actualOLD,df_actual,df_prev){ 
  # Source pour les formules : https://stats.stackexchange.com/questions/345178/interpretation-of-theils-u2-statistic-forecasting-methods-and-applications
  df_actualOLD<-as.data.frame(df_actualOLD)
  df_actual<-as.data.frame(df_actual)
  df_prev<-as.data.frame(df_prev)
  df_actualOLD[is.na(df_actualOLD)]<-0
  df_actual[is.na(df_actual)]<-0
  df_prev[is.na(df_prev)]<-0
  df_actualOLD<-as.data.frame(df_actualOLD)
  df_actual<-as.data.frame(df_actual)
  df_prev<-as.data.frame(df_prev)
  
  if(dim(df_actual)[[1]]!=dim(df_prev)[[1]] |dim(df_actual)[[2]]!=dim(df_prev)[[2]] ){return(print("Error dimensions inconsistent"))}
  
  # method =="THEIL_Quality"
    # when U = 0 the forecasts are perfect. That means if U>0 then forecasts are not perfect. As we keep increasing U, the 
    # forecasts will keep becoming more and more imperfect. When it reaches 1, it means Forecast is equivalent to Naive Method. 
    # Hence, a general pattern can be seen that as U increases forecast is wrong and at 1 forecast is same as naive and hence 
    # by extrapolating this we can say that if U>1 then the forecast is worse than naive method forecast, i.e. APE of the forecast
    # is even bigger than naive method forecasts.
    V1<-(df_prev - df_actual)/df_actualOLD
    V1<-as.data.frame(V1)
    is.na(V1) <- sapply(V1, is.infinite)
    V1[is.na(V1)] <- 0

    V2<-(df_actual - df_actualOLD)/df_actualOLD
    V2<-as.data.frame(V2)
    is.na(V2) <- sapply(V2, is.infinite)
    V2[is.na(V2)] <- 0
    
    res1<-sqrt(sum(V1*V1,na.rm=TRUE))/sqrt(sum(V2*V2,na.rm=TRUE))
  
  # method =="THEIL_Accuracy")
    nbligcol<-nrow(df_actualOLD)*ncol(df_actualOLD)
    
    V1<-(df_prev - df_actual)/df_actualOLD
    V1<-as.data.frame(V1)
    is.na(V1) <- sapply(V1, is.infinite)
    V1[is.na(V1)] <- 0
    
    V2<-(df_actual - df_actualOLD)/df_actualOLD
    V2<-as.data.frame(V2)
    is.na(V2) <- sapply(V2, is.infinite)
    V2[is.na(V2)] <- 0
    
    V3<-(df_prev - df_actualOLD)/df_actualOLD
    V3<-as.data.frame(V3)
    is.na(V3) <- sapply(V3, is.infinite)
    V3[is.na(V3)] <- 0
    
    res2<-sqrt(1/(nbligcol)*sum(V1*V1,na.rm=TRUE))/(sqrt(1/(nbligcol)*sum(V2*V2,na.rm=TRUE)) + sqrt(1/(nbligcol)*sum(V3*V3,na.rm=TRUE))   )
  
  return(as.data.frame(t(c(res2,res1)))) # Accuracy and quality
}



Indic_error <- function(df_actual,df_prev,meth){ #meth=STPE;MAPE;FULL
  df1<-df_actual
  df2<-df_prev
  df1<-as.data.frame(df1)
  df2<-as.data.frame(df2)
  df1[is.na(df1)]<-0
  df2[is.na(df2)]<-0
  df1<-as.data.frame(df1)
  df2<-as.data.frame(df2)
  
  if(dim(df_actual)[[1]]!=dim(df_prev)[[1]] |dim(df_actual)[[2]]!=dim(df_prev)[[2]] ){return(print("Error dimensions inconsistent"))}
  
  if(meth=="MAPE"){
    nbligcol<-nrow(df1)*ncol(df1)
    interm<-abs(df1 - df2)/abs(df1)
    interm<-as.data.frame(interm)
    #interm[!is.finite(interm)] <- 0
    is.na(interm)<-sapply(interm, is.infinite)
    interm[is.na(interm)]<-0
    res<- 100*1/nbligcol*sum(interm,na.rm=TRUE) 
  }
  
  if(meth=="MAE"){
    # The Mean Absolute Error (MAE) is a very good KPI to measure forecast accuracy
    nbligcol<-nrow(df1)*ncol(df1)
     res<- 100*1/nbligcol*sum(abs(df1 - df2),na.rm=TRUE) 
  }
  if(meth=="RMSE%"){
    # The Root Mean Squared Error (RMSE) : Compared to MAE, RMSE does not treat each error the same. It gives more importance to
    # the most significant errors. That means that one big error is enough to get a very bad RMSE.
    nbligcol<-nrow(df1)*ncol(df1)
    res<- 100*sqrt(1/nbligcol*sum((df1 - df2)*(df1 - df2),na.rm=TRUE)) / (1/nbligcol*sum(df1))
  }
  if(meth=="STPE"){
    res<-100*sum(abs(df1 - df2),na.rm=TRUE)/sum(df1)
  }
  
  if(meth=="FULL"){ 
    res<-as.data.frame(cbind(Indic_error(df1,df2,meth="MAPE"),Indic_error(df1,df2,meth="MAE"),Indic_error(df1,df2,meth="STPE"),Indic_error(df1,df2,meth="RMSE%")))
    }
  
  return(res)
}

# Tests
# df1<-matrix(nrow=4,ncol=4)
# df1[,]=1:16
# print(df1)
# df2<-df1
# df2[4,1]<-5
# df2[3,4]<-16
# print(df2)
# print(Indic_error(df1,df2,"FULL")) 




### Fonction de prevision d'une liste de tableaux
# Attention : tous les tableaux doivent avoir strictement le mÃªme format
# On les transforme en serie, puis ts, puis prev (ou mean-median), puis re-tableau
Prev_Tab <- function(List_Tab,BeginDate,horizonprev, meth="MEAN"){ #meth=MEAN;MEDIAN;ARIMA;ETS etc.
  nbTab<-length(List_Tab)
  FirstTab<-List_Tab[[1]] # Initialisation sur la premiere annee avant de boucler sur les autres
  FirstTab<-AddRownamesToFirstCol(FirstTab) # rajoute le nom des lignes dans la premiÃ¨re colonne
  colnames(FirstTab)[1]<-"Produit"
  col2<-colnames(FirstTab)[2]
  Cadre<- FirstTab %>% tidyr::pivot_longer(cols=!Produit, 
                                           names_to = "ColumnTab", 
                                           values_to = toString(BeginDate))
  Cadre$Lib<-paste0(Cadre$Produit,"_#_",Cadre$ColumnTab)
  Cadre <- Cadre %>% select(Lib, toString(BeginDate))
  Cadre<-GetRownamesFromFirstCol(Cadre)
  TabOut<-Cadre
  
  # Boucle sur les N-1 autres tableaux
  for(k in 2:nbTab){
    Tab<-List_Tab[[k]] # Initialisation sur la premiere annee avant de boucler sur les autres
    Tab<-AddRownamesToFirstCol(Tab) # rajoute le nom des lignes dans la premiÃ¨re colonne
    colnames(Tab)[1]<-"Produit"
    col2<-colnames(FirstTab)[2]
    Cadre<- Tab %>% tidyr::pivot_longer(cols=!Produit, 
                                             names_to = "ColumnTab", 
                                             values_to = toString((BeginDate+k-1)))
    Cadre$Lib<-paste0(Cadre$Produit,"_#_",Cadre$ColumnTab)
    Cadre <- Cadre %>% select(Lib, toString((BeginDate+k-1)))
    Cadre<-GetRownamesFromFirstCol(Cadre)
    TabOut<-cbind(TabOut,Cadre)
  }

  # Transformation de notre table en time series
  tTabOut<-t(TabOut)
  tTabOut<-AddRownamesToFirstCol(tTabOut)
  colnames(tTabOut)[1]<-"Dates"
  tTabOut$Dates<-as.Date(tTabOut$Dates, format="%Y")
  time_series_xts <- xts(tTabOut[,-1], order.by=as.Date(tTabOut[,1]))
  print(time_series_xts)
  
  # Partie PrÃ©vision
  
   if(meth=="MEAN"){
     Prev<-as.data.frame(sapply(time_series_xts, function(x) mean(x) ) ,drop=FALSE)
     colnames(Prev)[1]<-"Prev"
   }
   
  if(meth=="MEDIAN"){
    Prev<-as.data.frame(sapply(time_series_xts, function(x) median(x) ) ,drop=FALSE)
    colnames(Prev)[1]<-"Prev"
  }
    
  # Reformatage sous forme de tableau
  Prev<-AddRownamesToFirstCol(Prev)
  # separer en 2 colonnes avec le hashtag
  Prev$Lig<-sub("_#_.*", "", Prev$joint)
  Prev$Col<-sub(".*_#_", "", Prev$joint)
  Prev<-Prev[,-1]
  dfout<-Prev %>% tidyr::pivot_wider(names_from = "Col", values_from = Prev) 
  dfout<-GetRownamesFromFirstCol(dfout)
  
  return(dfout)
}

#Tests
#List_Tab<-list(TEF_CT_2016,TEF_CT_2017,TEF_CT_2018)
#BeginDate<-2017
#horizonprev<-1
#Toto<-Prev_Tab(List_Tab,BeginDate,horizonprev, meth="MEAN")
#toto2<-Prev_Tab(List_Tab,BeginDate,horizonprev, meth="MEDIAN")


### Fonction de serisation d'une liste de tableaux
Serisation <- function(List_Tab,BeginDate){ 
  nbTab<-length(List_Tab)
  FirstTab<-List_Tab[[1]] # Initialisation sur la premiere annee avant de boucler sur les autres
  FirstTab<-AddRownamesToFirstCol(FirstTab) # rajoute le nom des lignes dans la premiÃ¨re colonne
  colnames(FirstTab)[1]<-"Produit"
  col2<-colnames(FirstTab)[2]
  Cadre<- FirstTab %>% tidyr::pivot_longer(cols=!Produit, 
                                           names_to = "ColumnTab", 
                                           values_to = toString(BeginDate))
  Cadre$Lib<-paste0(Cadre$Produit,"_#_",Cadre$ColumnTab)
  Cadre <- Cadre %>% select(Lib, toString(BeginDate))
  Cadre<-GetRownamesFromFirstCol(Cadre)
  TabOut<-Cadre
  
  # Boucle sur les N-1 autres tableaux
  for(k in 2:nbTab){
    Tab<-List_Tab[[k]] # Initialisation sur la premiere annee avant de boucler sur les autres
    Tab<-AddRownamesToFirstCol(Tab) # rajoute le nom des lignes dans la premiÃ¨re colonne
    colnames(Tab)[1]<-"Produit"
    col2<-colnames(FirstTab)[2]
    Cadre<- Tab %>% tidyr::pivot_longer(cols=!Produit, 
                                        names_to = "ColumnTab", 
                                        values_to = toString((BeginDate+k-1)))
    Cadre$Lib<-paste0(Cadre$Produit,"_#_",Cadre$ColumnTab)
    Cadre <- Cadre %>% select(Lib, toString((BeginDate+k-1)))
    Cadre<-GetRownamesFromFirstCol(Cadre)
    TabOut<-cbind(TabOut,Cadre)
  }
  dfout<-as.data.frame(TabOut)
  dfout<-dfout[rowSums(dfout)>0,]
  dfout<-as.data.frame(TabOut)
  dfout<-dfout[order(dfout[,1],decreasing = TRUE),]
  return(dfout)
}

# Tests
# listeTAB<-list(TRP_TESS_IMP_2017,TRP_TESS_IMP_2018)
# toto<-Serisation(listeTAB,2017)


#REFERENCES article : Temursho , U., Oosterhaven , J. and M.A. Cardenete (2019) , A multi - regional generalized RAS updating technique , IOpedia Research Paper No. 2, September 2019 , www. IOpedia .eu

#M 1 function [X,r,s,T] = mrgras (X0 ,u,v,G,Q,W, eps )
mrgras <- function(X0 ,u,v,G,Q,W, eps=0.00001,theshld=5000) {
  #M 2 % PURPOSE : estimate a new multi - regional (or any partitioned ) matrix X as
  #M 3 % close as possible to a given matrix X0 subject to the row sums , column
  #M 4 % sums and non - overlapping aggregation constraints , using MR - GRAS approach
  #M 5 % -------------------------------------------------------------------------
  #M 6 % USAGE :
  #M 7 % Write X = mrgras (X0 ,u,v,G,Q,W) OR [X,r,s,T] = mrgras (X0 ,u,v,G,Q,W) with
  #M 8 % or without eps as the seventh optional input argument , where
  #M 9 %
  #M 10 % INPUT :
  #M 11 % -> X0 = benchmark ( original ) matrix , not necessarily square
  #M 12 % -> u = column vector of row totals (new row sums )
  #M 13 % -> v = column vector of column totals ( new column sums )
  #M 14 % -> W = non - overlapping aggregation constraints matrix
  #M 15 % -> G & Q = the row and column aggregator matrices such that G*X0*Q = W;
  #M 16 % non - overlapping aggregation necessarily implies that the
  #M 17 % column sums of G and the row sums of Q must be all unity
  #M 18 % -> eps = convergence tolerance level ; if empty , the default threshold
  #M 19 % level is 0.1e -5 (=0.000001)
  #M 20 % -> In case of missing w_IJ , set the corresponding missing number to
  #M 21 % w_IJ = 1010101 ( assuming that 1010101 is not among the w_IJ 's)
  #M 22 %
  #M 23 % OUTPUT (input - output analysis - related interpretation ):
  #M 24 % -> X = updated / balanced / adjusted / projected matrix
  #M 25 % -> r = substitution effects ( row multipliers )
  #M 26 % -> s = fabrication effects ( column multipliers )
  #M 27 % -> T = technology or regional effects ( aggregation multipliers )
  #M 28 % -------------------------------------------------------------------------
  #M 29 % REFERENCES :
  #M 30 % Temursho , U., Oosterhaven , J. and M.A. Cardenete (2019) , A multi - regional
  #M 31 % generalized RAS updating technique , IOpedia Research Paper No. 2,
  #M 32 % September 2019 , www. IOpedia .eu
  #M 33 % -------------------------------------------------------------------------
  #M 34 % NOTE FROM THE AUTHOR : Using this program and publishing the results in
  #M 35 % the form of a report , working / discussion paper , journal article , etc .
  #M 36 % requires citation of the above reference paper .
  #M 37 % -------------------------------------------------------------------------
  #M 38 % Written by: Umed Temursho , May 2019
  #M 39 % E- mail : utemursho@gmail . com
  #M 40
  #M 41 [m,n] = size (X0);
  m<-nrow(X0)
  n<-ncol(X0)
  #M 42 [h,k] = size (W);
  h<-nrow(W)
  k<-ncol(W)  
  #M 43 N = zeros (m,n);
  N<-matrix(0,m,n)
  #M 44 N(X0 <0) = -X0(X0 <0) ;
  N[X0 <0]<--X0[X0<0]
  #M 45 N = sparse (N); % could save memory with large - scale matrices
  #M 46 P = X0+N;
  P<-X0+N
  #M 47 P = sparse (P);
  #M 48 %
  #M 49 r0 = ones (m ,1) ; % initial guess for r in step 0
  r0<-matrix(1,m,1)
  r0<-as.vector(r0)
  #M 50 T0 = ones (h,k); % initial guess for T in step 0
  T0<-matrix(1,h,k)
  #M 51 Te = G '* T0*Q '; % T expanded to fit the dimention of X0
  Te<-t(G)%*%T0%*%t(Q)
  #M 52 p_rt = (P.* Te) '*r0;
  p_rt<-t(P*Te)%*%r0
  p_rt<-as.vector(p_rt)
  
  #M 53 n_rt = (N.* invM (Te)) '* invd (r0)* ones (m ,1);
  OneM<-matrix(1,m,1)
  OneM<-as.vector(OneM)
  n_rt <-t(N*invM(Te))%*%invd(r0)%*%OneM
  n_rt<-as.vector(n_rt)
  
  #M 54 s1 = invd (2* p_rt )*(v+ sqrt (v .^2+4* p_rt .* n_rt )); % first step s
  s1<-invd(2*p_rt)%*%(v+sqrt(v^2+4*p_rt*n_rt)) 
  s1<-as.vector(s1)
  
  #M 55 ss = -invd (v)* n_rt ;
  ss<--invd(v)%*%n_rt
  ss<-as.vector(ss)
  
  #M 56 s1( p_rt ==0) = ss( p_rt ==0) ;
  s1[p_rt==0]<-ss[p_rt==0]
  
  #M 57 %
  #M 58 p_st = (P.* Te)*s1;
  p_st<-(P*Te)%*%s1
  p_st<-as.vector(p_st)
  #M 59 n_st = (N.* invM (Te))* invd (s1)* ones (n ,1) ;
  OneN<-matrix(1,n,1)
  OneN<-as.vector(OneN)
  # n_st<-(N*invM(Te))*invd(s1)*vect_1_n    Version origine
  n_st<-(N*invM(Te))%*%invd(s1)%*%OneN  
  n_st<-as.vector(n_st)
  #M 60 r1 = invd (2* p_st )*(u+ sqrt (u .^2+4* p_st .* n_st )); % first step r
  r1<-invd(2*p_st)%*%(u+sqrt(u^2+4*p_st*n_st ))
  r1<-as.vector(r1)
  #M 61 rr = -invd (u)* n_st ;
  rr<--invd(u)%*%n_st
  rr<-as.vector(rr)
  #M 62 r1( p_st ==0) = rr( p_st ==0) ;
  r1[p_st==0]<-rr[p_st==0]
  
  #M 63 %
  #M 64 P_rs = G*( diag (r1)*P* diag (s1))*Q;
  P_rs<-G%*%(diag(r1)%*%P%*%diag(s1))%*%Q
  #M 65 N_rs = G*( invd (r1)*N* invd (s1))*Q;
  N_rs<-G%*%(invd(r1)%*%N%*%invd(s1))%*%Q
  #M 66 T1 = invM (2* P_rs ) .*( W+ sqrt (W .^2+4* P_rs .* N_rs )); % first step T
  T1<-invM(2*P_rs)*(W+sqrt(W^2+4*P_rs*N_rs))
  #M 67 TT = -invM (W).* N_rs ;
  TT<--invM(W)*N_rs
  #M 68 T1( P_rs ==0) = TT( P_rs ==0) ;
  T1[P_rs==0]<-TT[P_rs==0]
  #M 69 T1(W ==1010101) = 1; % set t_IJ =1 for missing aggregation total w_IJ
  T1[W ==1010101]<-1
  #M 70 Te = G '* T1*Q ';
  T1<-as.matrix(T1)
  Te<-t(G)%*%T1%*%t(Q) 
  #M 71 %
  #M 72 p_rt = (P.* Te) '*r1;
  p_rt<-t(P*Te)%*%r1
  p_rt<-as.vector(p_rt)
  #M 73 n_rt = (N.* invM (Te)) '* invd (r1)* ones (m ,1);
  n_rt<-t(N*invM(Te))%*%invd(r1)%*%OneM 
  n_rt<-as.vector(n_rt)
  #M 74 s2 = invd (2* p_rt )*(v+ sqrt (v .^2+4* p_rt .* n_rt )); % second step s
  s2<-invd(2*p_rt)%*%(v+sqrt(v^2+4*p_rt*n_rt ))
  s2<-as.vector(s2)
  #M 75 ss = -invd (v)* n_rt ;
  ss<--invd(v)%*%n_rt
  ss<-as.vector(ss)
  #M 76 s2( p_rt ==0) = ss( p_rt ==0) ;
  s2[p_rt==0]<-ss[p_rt==0]
  
  #M 77 %
  #M 78 p_st = (P.* Te)*s2;
  p_st<-(P*Te)%*%s2
  p_st<-as.vector(p_st)
  #M 79 n_st = (N.* invM (Te))* invd (s2)* ones (n ,1) ;
  n_st<-(N*invM(Te))%*%invd(s2)%*%OneN
  n_st<-as.vector(n_st)
  #M 80 r2 = invd (2* p_st )*(u+ sqrt (u .^2+4* p_st .* n_st )); % second step r
  r2<-invd(2*p_st)%*%(u+sqrt(u^2+4*p_st*n_st ))
  r2<-as.vector(r2)
  #M 81 rr = -invd (u)* n_st ;
  rr<--invd(u)%*%n_st
  rr<-as.vector(rr)
  #M 82 r2( p_st ==0) = rr( p_st ==0) ;
  r2[p_st==0]<-rr[p_st==0]
  
  #M 83 %
  #M 84 P_rs = G*( diag (r2)*P* diag (s2))*Q;
  P_rs<-G%*%(diag(r2)%*%P%*%diag(s2))%*%Q
  #M 85 N_rs = G*( invd (r2)*N* invd (s2))*Q;
  N_rs<-G%*%(invd(r2)%*%N%*%invd(s2))%*%Q
  #M 86 T2 = invM (2* P_rs ) .*( W+ sqrt (W .^2+4* P_rs .* N_rs )); % second step T
  T2<-invM(2*P_rs)*(W+sqrt(W^2+4*P_rs*N_rs ))
  #M 87 TT = -invM (W).* N_rs ;
  TT<--invM(W)*N_rs
  #M 88 T2( P_rs ==0) = TT( P_rs ==0) ;
  T2[P_rs==0]<-TT[P_rs==0]
  #M 89 T2(W ==1010101) = 1; % set t_IJ =1 for missing w_IJ
  T2[W==1010101]<-1
  
  #M 90 %
  #M 91 tmax = max ( max ( abs (T2 -T1)));
  tmax<-max(abs(T2-T1))
  #M 92 dif = [s2 -s1;r2 -r1; tmax ];
  dif<-rbind(c(s2-s1),c(r2-r1),tmax)
  #M 93 iter = 1 % first iteration
  iter<-1 
  #M 94 if nargin < 7 || isempty ( eps)
  #M 95 eps = 0.1e -5; % default tolerance level
  #M 96 end
  if(nargs()<7 || is.null(eps)){eps<-0.000001}
  #M 97 M = max ( abs ( dif ));
  M<-max(abs(dif))
  
  #M 98 while (M > eps )
  iter=0
  while(M>eps & iter < theshld){  # Default conditions : M>eps or 5000 iterations
    #M 99 s1 = s2;
    s1<-s2
    #M 100 r1 = r2;
    r1<-r2
    #M 101 T1 = T2;
    T1<-T2
    #M 102 Te = G '* T1*Q ';
    T1<-as.matrix(T1)
    Te<-t(G)%*%T1%*%t(Q) 
    #M 103 %
    #M 104 p_rt = (P.* Te) '*r1;
    p_rt<-t(P*Te)%*%r1
    p_rt<-as.vector(p_rt)
    #M 105 n_rt = (N.* invM (Te)) '* invd (r1)* ones (m ,1);
    n_rt<-t(N*invM(Te))%*%invd(r1)%*%OneM
    n_rt<-as.vector(n_rt)
    #M 106 s2 = invd (2* p_rt )*(v+ sqrt (v .^2+4* p_rt .* n_rt )); % next step s
    s2<-invd(2*p_rt)%*%(v+sqrt(v^2+4*p_rt*n_rt))
    s2<-as.vector(s2)
    #M 107 ss = -invd (v)* n_rt ;
    ss<--invd(v)%*%n_rt
    ss<-as.vector(ss)
    #M 108 s2( p_rt ==0) = ss( p_rt ==0) ;
    s2[p_rt==0]<-ss[p_rt==0]
    
    #M 109 %
    #M 110 p_st = (P.* Te)*s2;
    p_st<-(P*Te)%*%s2
    p_st<-as.vector(p_st)
    #M 111 n_st = (N.* invM (Te))* invd (s2)* ones (n ,1) ;
    n_st<-(N*invM(Te))%*%invd(s2)%*%OneN
    n_st<-as.vector(n_st)
    #M 112 r2 = invd (2* p_st )*(u+ sqrt (u .^2+4* p_st .* n_st )); % next step r
    r2<-invd(2*p_st)%*%(u+sqrt(u^2+4*p_st* n_st))
    r2<-as.vector(r2)
    #M 113 rr = -invd (u)* n_st ;
    rr<--invd(u)%*%n_st 
    rr<-as.vector(rr)
    #M 114 r2( p_st ==0) = rr( p_st ==0) ;
    r2[p_st==0]<-rr[p_st==0]
    
    #M 115 %
    #M 116 P_rs = G*( diag (r2)*P* diag (s2))*Q;
    P_rs<-G%*%(diag(r2)%*%P%*%diag(s2))%*%Q
    #M 117 N_rs = G*( invd (r2)*N* invd (s2))*Q;
    N_rs<-G%*%(invd(r2)%*%N%*%invd(s2))%*%Q
    #M 118 T2 = invM (2* P_rs ) .*( W+ sqrt (W .^2+4* P_rs .* N_rs )); % next step T
    T2<-invM(2*P_rs)*(W+sqrt(W^2+4*P_rs*N_rs))
    #M 119 TT = -invM (W).* N_rs ;
    TT<--invM(W)*N_rs
    #M 120 T2( P_rs ==0) = TT( P_rs ==0) ;
    T2[P_rs==0]<-TT[P_rs==0]
    #M 121 T2(W ==1010101) = 1; % set t_IJ =1 for missing w_IJ
    T2[W==1010101]<-1
    
    #M 122 %
    #M 123 tmax = max ( max ( abs (T2 -T1)));
    tmax<-max(abs(T2-T1))
    #M 124 dif = [s2 -s1;r2 -r1; tmax ];
    dif<-rbind(c(s2-s1),c(r2-r1),tmax)
    #M 125 iter = iter +1
    iter <- iter +1
    #M 126 M = max ( abs ( dif ));
    M<-max(abs(dif))
    #M 127 end
  }
  print(paste0(iter," iterations required to ensure convergence (if equal to threshold : check convergence)"))
  #M 128 s = s2; % final step s
  s <- s2
  #M 129 r = r2; % final step r
  r <- r2
  #M 130 T = T2; % final step T
  Tfin <- T2
  #M 131 Te = G '*T*Q ';
  Tfin<-as.matrix(Tfin)
  Te <- t(G)%*%Tfin%*%t(Q)
  #M 132 %
  #M 133 X = Te .*( diag (r)*P* diag (s))-invM (Te) .*( invd (r)*N* invd (s)); % updated matrix
  X <- Te*(diag(r)%*%P%*%diag(s))-invM(Te)*(invd(r)%*%N%*%invd(s))
  #M 134 end
  return(list(X,r,s,Tfin))
}
#M 135
#M 136 function invd = invd (x) % auxiliary function used above
invd <- function(x) {
  #M 137 invd = 1./ x;
  sortie<-1/x
  #M 138 invd (x ==0) = 1;
  sortie[x==0]<-1
  #M 139 invd = diag ( invd );
  #M 140 end
  return(diag(sortie))
}
#M 141
#M 142 function invM = invM (X) % auxiliary function used above
invM <- function(X) {
  #M 143 invM = 1./ X;
  sortie<-1/X
  #M 144 invM (X ==0) = 1;
  sortie[X==0]<-1
  #M 145 end
  return(sortie)
}
#M 146 % --------------------------------------------------------------------------
#M 147 % END OF THE CODE
#M 148 % -------------------------------------------------------------------------


#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################


##############################################################
## Fonction qui retourne une matrice carrÃ©e donnÃ©e en entrÃ©e ##
##############################################################

## La fonction prend en entrÃ©e une matrice carrÃ©e sous format csv (il faut indiquer le chemin)
## La fonction crÃ©e la matrice sous R



matrice <- function(fichier)
{
  donnees<-read.table(fichier,sep=";")
  A <- as.matrix (donnees)
  return (A)
}


##############################################################
## Fonction qui inverse une matrice carrÃ©e donnÃ©e en entrÃ©e ##
##############################################################

## La fonction prend en entrÃ©e une matrice carrÃ©e
## La fonction crÃ©e la matrice inverse de cette matrice



inverse <- function(mat)
{
  return (solve(mat))
}



###################################################################################
## Fonction qui retourne I-A (I+A pour la 2) pour une matrice A carrÃ©e en entrÃ©e ##
###################################################################################

## La fonction prend en entrÃ©e une matrice carrÃ©e.
## La fonction crÃ©e la matrice identitÃ© moins la matrice en entrÃ©e.


identite <- function(A)
{
  rang <- nrow(A)
  I <- diag(nrow = rang)
  return (I-A)
}




####################################################
## Fonction de calcul des coefficients techniques ##
####################################################

## La fonction prend en entrÃ©e :
## - un TEI domestique ou importÃ© (A) sous forme de matrice carrÃ©e
## - la production ou vecteur des importations totale de chaque branche (B) sous forme de vecteur
## La fonction crÃ©e la matrice des coefficients techniques domestiques ou importÃ©s.


coef_techniques <- function (A,B)
{
  tei <- A
  prod <- B
  ctec <- matrix(nrow = nrow(tei), ncol = ncol(tei))
  if (ncol(tei) == length(prod)) {
    for (i in 1:nrow(tei)) {
      for (j in 1:ncol(tei)) {
        if (prod[j]==0) {
          ctec[i,j] <- 0
        }else{
          ctec[i,j] <- tei[i,j]/prod[j]
        }
      }
    }
    return (ctec)
  }else {
    cat("Erreur : la taille du vecteur de production et du TEI ne sont pas cohÃ©rentes")
  }
}

coef_debouches <- function (A,B)
{
  tei <- A
  prod <- B
  ctec <- matrix(nrow = nrow(tei), ncol = ncol(tei))
  if (ncol(tei) == length(prod)) {
    for (i in 1:nrow(tei)) {
      for (j in 1:ncol(tei)) {
        if (prod[i]==0) {
          ctec[i,j] <- 0
        }else{
          ctec[i,j] <- tei[i,j]/prod[i]
        }
      }
    }
    return (t(ctec))
  }else {
    cat("Erreur : la taille du vecteur de production et du TEI ne sont pas cohÃ©rentes")
  }
}

############################################
## Calcul des effets directs et indirects ##
############################################

## Les fonctions prennent en entrÃ©e la matrice des coefficients techniques et retournent les
## multiplicateurs directs et indirects

direct <- function(A)
{
  rang <- nrow(A)
  I <- diag(nrow = rang)
  return (I+A)
}

indirect <- function(A)
{
  rang <- nrow(A)
  I <- diag(nrow = rang)
  I_A <- identite(A=A)
  inv_I_A <- inverse(mat=I_A)
  return (inv_I_A-I-A)
}

indirect_imp <- function(A,B) {
  rang <- nrow(A)
  I <- diag(nrow = rang)
  I_A <- identite(A=A)
  inv_I_A <- inverse(mat=I_A)
  return (B %*% inv_I_A-B)
}

####################################################
## CrÃ©ation indices de prix et recalcul de valeur ##
####################################################

indice_prix <- function(valeur, volume)
{
  ind_prix <- matrix(nrow = nrow(valeur), ncol = ncol(valeur))
  if (ncol(valeur) == ncol(volume) & nrow(valeur) == nrow(volume)) {
    for (i in 1:nrow(valeur)) {
      for (j in 1:ncol(valeur)) {
        if (volume[i,j]==0) {
          ind_prix[i,j] <- 0
        } else {
          ind_prix[i,j] <- valeur[i,j]/volume[i,j]
        }
      }
    }
  } else {
    cat("Erreur : les tailles des matrices volume et valeur ne sont pas identiques")
  }
  return (ind_prix)
}


valeur <- function(prix, volume)
{
  valeur <- matrix(nrow = nrow(prix), ncol = ncol(prix))
  if (ncol(prix) == ncol(volume) & nrow(prix) == nrow(volume)) {
    for (i in 1:nrow(prix)) {
      for (j in 1:ncol(prix)) {
        valeur[i,j] <- prix[i,j]*volume[i,j]
      }
    }
  } else {
    cat("Erreur : les tailles des matrices volume et valeur ne sont pas identiques")
  }
  return (valeur)
}



########################### fonction puissance n?cessaire pour la fonction suivante
powA = function(a,n)
{ 
  if (n==0)  return (diag(nrow(a)))
  if (n==1)  return (a)
  if (n==2)  return (a%*%a)
  if (n>2) return ( a%*%powA(a,n-1))
}

#mat <- matrix(1:9, nrow=3)
#print(mat)
#print(powA(mat,10))

#################################################################################
##   AVIONIC :  Variation du prix de production en fonction d'un choc exogÃ¨ne de prix    ##
#################################################################################

Avio_variation_prix <- function(tei, prod, num, var, coeff_trans=NULL, power=NULL){

  if(! is.null(coeff_trans)) {

    ## 1) Calcul des coefficients techniques

    ctec <- coef_techniques(A= tei, B= prod)

    ## 2) CrÃ©ations des matrices en intermÃ©diaires (amputÃ©es du produit variant)

    ctec_prime <- t(ctec[-num,-num])
    a <- as.matrix(ctec[num,])
    a_i <- as.matrix(a[-num])

    # coeff_effet_direct<-coeff_trans[num] # Ajout Alex correction formule
    coeff <- diag(as.vector(coeff_trans[-num,-num]))

    ## 3) Equation delta_p = (I-A*coeff)^(-1)*gamma*a_iT*delta_pi

    I_A <- identite(A=ctec_prime%*%coeff)
    inv_I_A <- inverse(mat=I_A)

    delta_p <- inv_I_A%*%a_i%*%(var/100)

    debut <- as.matrix(delta_p[1:num-1])
    fin <- as.matrix(delta_p[num:length(delta_p)])
    delta_p <- as.matrix(c(debut*100,var,fin*100))

    ## 4) Calcul de la variation totale du prix

    valeur_x_var <- t(prod)*delta_p/100
    delta_total <- sum(valeur_x_var)/sum(prod)
    elast_prix <- delta_total / var

    ## 5) Effet direct a_iT*delta_pi et effet indirect (I-A)^(-1)*a_iT*delta_pi*-a_iT*delta_pi

    delta_p_direct <- a_i%*%(var/100)+ctec_prime%*%coeff%*%a_i%*%(var/100)  # Ajout 1er tour : modifi? AB 10-05-2022
    # a_i%*%(var/100)*as.numeric(coeff_effet_direct)
    
    if(! is.null(power)) {    #Calcul uniquement de la puissance indiqu?e --> stock? dans effet direct, et reste dans indirect
      if(power==0){delta_p_direct <-a_i%*%(var/100)}
      if(power==1){delta_p_direct <-ctec_prime%*%coeff%*%a_i%*%(var/100)}
      if(power>1){delta_p_direct <-powA(ctec_prime%*%coeff,power)%*%a_i%*%(var/100)}
    }
    
    debut_dir <- as.matrix(delta_p_direct[1:num-1])
    fin_dir <- as.matrix(delta_p_direct[num:length(delta_p_direct)])
    delta_p_direct <- as.matrix(c(debut_dir*100,var,fin_dir*100))
    
    if(! is.null(power)) {    #Calcul uniquement de la puissance indiqu?e --> stock? dans effet direct, et reste dans indirect
      if(power>=1){delta_p_direct <- as.matrix(c(debut_dir*100,0,fin_dir*100))}
    }

    delta_p_indirect = delta_p - delta_p_direct

    var_prix <- list(var_prix= delta_p, var_prix_dir=delta_p_direct, var_prix_indir=delta_p_indirect, var_tot=delta_total, elast = elast_prix)

    return(var_prix)

    cat("ATTENTION \n Rappel, les rÃ©sultats ne seront pas les mÃªmes en fonction de si vous avez entrÃ© un TEI seul domestique ou domestique et importÃ©. Se reporter Ã  la documentation")

  } else {
    ## 1) Calcul des coefficients techniques

    ctec <- coef_techniques(A= tei, B= prod)

    ## 2) CrÃ©ations des matrices en intermÃ©diaires (amputÃ©es du produit variant)

    ctec_prime <- t(ctec[-num,-num])
    a <- as.matrix(ctec[num,])
    a_i <- as.matrix(a[-num])

    ## 3) Equation delta_p = (I-A)^(-1)*a_iT*delta_pi

    I_A <- identite(A=ctec_prime)
    inv_I_A <- inverse(mat=I_A)

    delta_p <- inv_I_A%*%a_i%*%(var/100)

    debut <- as.matrix(delta_p[1:num-1])
    fin <- as.matrix(delta_p[num:length(delta_p)])
    delta_p <- as.matrix(c(debut*100,var,fin*100))

    ## 4) Calcul de la variation totale du prix

    valeur_x_var <- t(prod)*delta_p
    delta_total <- sum(valeur_x_var)/sum(prod)
    elast_prix <- delta_total / var


    ## 5) Effet direct a_iT*delta_pi et effet indirect (I-A)^(-1)*a_iT*delta_pi*-a_iT*delta_pi

    delta_p_direct <- a_i%*%(var/100)+ctec_prime%*%a_i%*%(var/100) # Ajout 1er tour : modifi? AB 10-05-2022

    if(! is.null(power)) {    #Calcul uniquement de la puissance indiqu?e --> stock? dans effet direct, et reste dans indirect
      if(power==0){delta_p_direct <-a_i%*%(var/100)}
      if(power==1){delta_p_direct <-ctec_prime%*%a_i%*%(var/100)}
      if(power>1){delta_p_direct <-powA(ctec_prime,power)%*%a_i%*%(var/100)}
    }
    
    debut_dir <- as.matrix(delta_p_direct[1:num-1])
    fin_dir <- as.matrix(delta_p_direct[num:length(delta_p_direct)])
    delta_p_direct <- as.matrix(c(debut_dir*100,var,fin_dir*100))
    
    if(! is.null(power)) {    #Calcul uniquement de la puissance indiqu?e --> stock? dans effet direct, et reste dans indirect
      if(power>=1){delta_p_direct <- as.matrix(c(debut_dir*100,0,fin_dir*100))}
    }

    delta_p_indirect = delta_p - delta_p_direct


    var_prix <- list(var_prix= delta_p, var_prix_dir=delta_p_direct, var_prix_indir=delta_p_indirect, var_tot=delta_total, elast = elast_prix)

    return(var_prix)

    cat("ATTENTION \n Rappel, les rÃ©sultats ne seront pas les mÃªmes en fonction de si vous avez entrÃ© un TEI seul domestique ou domestique et importÃ©. Se reporter Ã  la documentation")
  }
}


################################################################
###  Fonction d'agregation d'une sortie 6 colonnes 138+1 total en format A17 pour les sorties
AggA17<-function(table139){
  tabW<-table139 #keep table
  #tabW<-tabW[1:138,]
  tabW<-as.data.frame(tabW)
  tabW[,3:6]<-as.numeric(unlist(tabW[,3:6])) #unlist(
  
  # Recalculer les niveaux pour agr?ger
  Prod_3<-cbind(tabW[,3],tabW[,3],tabW[,3])
  tabW[,4:6]<-tabW[,4:6]*Prod_3
  
  #load Tab pass
  TabPass<-read_excel("TabPass_MRIO_FC.xlsx",sheet = "Base_Industries", col_names = TRUE)
  TabPass<-TabPass[1:138,c("A17","A138")]
  colnames(tabW)[2]<-"A138"
  tabW$A138<-paste0("G",tabW$A138)
  tabW<-tabW[,2:6]
  
  # Jointure puis agr?gation
  Interm<-left_join(TabPass,tabW, by = "A138")
  Interm2<-Interm %>% 
    group_by(A17) %>% 
    summarise_at(vars(2:5),sum, na.rm = TRUE)
  
  # Recalcul des % pour les colonnes 3 ? 5
  Interm3<-Interm2
  Interm3[,3:5]<-Interm3[,3:5]/cbind(Interm3[,2],Interm3[,2],Interm3[,2])
  
  # Rajout du total qui etait dans la table initial
  Interm4<-table139[139,2:6]
  colnames(Interm4)<-colnames(Interm3)
  Interm5<-rbind(Interm3,Interm4)
  colnames(Interm5)[2]<-"Conso"
  
  return(Interm5) 
}

################################################################
###  Fonction d'agregation d'une sortie 6 colonnes 138+1 total en format A38 pour les sorties
AggA38<-function(table139){
  tabW<-table139 #keep table
  #tabW<-tabW[1:138,]
  tabW<-as.data.frame(tabW)
  tabW[,3:6]<-as.numeric(unlist(tabW[,3:6])) #unlist(
  
  # Recalculer les niveaux pour agr?ger
  Prod_3<-cbind(tabW[,3],tabW[,3],tabW[,3])
  tabW[,4:6]<-tabW[,4:6]*Prod_3
  
  #load Tab pass
  TabPass<-read_excel("TabPass_MRIO_FC.xlsx",sheet = "Base_Industries", col_names = TRUE)
  TabPass<-TabPass[1:138,c("A38","A138")]
  colnames(tabW)[2]<-"A138"
  tabW$A138<-paste0("G",tabW$A138)
  tabW<-tabW[,2:6]
  
  # Jointure puis agr?gation
  Interm<-left_join(TabPass,tabW, by = "A138")
  Interm2<-Interm %>% 
    group_by(A38) %>% 
    summarise_at(vars(2:5),sum, na.rm = TRUE)
  
  # Recalcul des % pour les colonnes 3 ? 5
  Interm3<-Interm2
  Interm3[,3:5]<-Interm3[,3:5]/cbind(Interm3[,2],Interm3[,2],Interm3[,2])
  
  # Rajout du total qui etait dans la table initial
  Interm4<-table139[139,2:6]
  colnames(Interm4)<-colnames(Interm3)
  Interm5<-rbind(Interm3,Interm4)
  colnames(Interm5)[2]<-"Conso"
  
  #Interm5[is.na(Interm5)] <- 0
  
  return(Interm5) 
}

#toto2<-AggA17(toto)
#toto<-AggA17(Choc35B)


################################################################
###  Fonction de mise en forme et export en XLS

MiseEnFormeExport<-function(sortieAvionic,nom_export="variations_A138.xlsx"){

  var_tot_pcent <- as.data.frame(sortieAvionic[[1]])
  colnames(var_tot_pcent)<-"Var_tot"
  var_dir_pcent <- as.data.frame(sortieAvionic[[2]])
  colnames(var_dir_pcent)<-"Var_dir"
  var_ind_pcent <- as.data.frame(sortieAvionic[[3]])
  colnames(var_ind_pcent)<-"Var_ind"

  variations_A138<-cbind(List138[,1:2],Prod138,var_tot_pcent,var_dir_pcent,var_ind_pcent)

  #recalcul et ajout du total
  var_tot_pcent_TOTAL <- as.data.frame(sortieAvionic[[4]])
  var_elasticite <-as.data.frame(sortieAvionic[[5]])
  Prod_3<-cbind(Prod138,Prod138,Prod138)
  niv_variations_A138<-variations_A138
  niv_variations_A138[,4:6]<-niv_variations_A138[,4:6]*Prod_3

  variations_A138[nrow(variations_A138) + 1,] <- c("Total des 138 produits","TOTAL", sum(Prod138),sum(niv_variations_A138[,4])/sum(Prod_3[,1]),sum(niv_variations_A138[,5])/sum(Prod_3[,1]),sum(niv_variations_A138[,6])/sum(Prod_3[,1]))

  write.xlsx(variations_A138, nom_export, sheetName = "variation", col.names = TRUE, row.names = TRUE)
}

################################################################
###  Fonction de mise en forme tout court et sauvegarde
# ATTENTION : incorpore le passage en prix d'acquisition avec ou sans coeffs suppl?mentaires
# les trois bouciers sont en 8,9 10 (pour la liste)

MiseEnForme<-function(sortieAvionic,ListCoeffBouclier=NULL,ExportPrixProd=NULL){
  # chargement des coefs pour passage prix acquisition + boucliers
  CoefsPassPrixAcq<-read_excel("Passage TESS ves Prix_acq_A139_2018.xlsx",sheet = "ImpCoefs", col_names = TRUE) 
  Prod138<-read_excel("TESS_A139_2018.xlsx",sheet = "Prod", col_names = FALSE)  # Pour 2016 : tes_sym-2016.xlsx
  List138<-read_excel("tes_sym-2016.xlsx",sheet = "List138", col_names = FALSE) 
  pond_conso<-CoefsPassPrixAcq$Conso_Niv2018_Pacqu  # CoefsPassPrixAcq$Conso_Niv2018_Pbase
  Struct_MC<-CoefsPassPrixAcq$Struct_MC
  Struct_MT<-CoefsPassPrixAcq$Struct_MT
  
  var_tot_pcent <- as.data.frame(sortieAvionic[[1]])
  colnames(var_tot_pcent)<-"Var_tot"
  var_dir_pcent <- as.data.frame(sortieAvionic[[2]])
  colnames(var_dir_pcent)<-"Var_dir"
  var_ind_pcent <- as.data.frame(sortieAvionic[[3]])
  colnames(var_ind_pcent)<-"Var_ind"

  variations_A138<-cbind(List138[,1:2],pond_conso,var_tot_pcent[1:138,],var_dir_pcent[1:138,],var_ind_pcent[1:138,])
  S1<-variations_A138
  
  # Option : Export pour comparaison a l'IPPI
  if(!is.null(ExportPrixProd)){
    variations_A138<-cbind(List138[,1:2],Prod138,var_tot_pcent[1:138,],var_dir_pcent[1:138,],var_ind_pcent[1:138,])
  Interm17<-AggA17(variations_A138)
  Interm38<-AggA38(variations_A138)
  write.xlsx(Interm17, paste0(ExportPrixProd,".xlsx"), sheetName = "A17", col.names = TRUE, row.names = TRUE, append=TRUE)
  write.xlsx(Interm38, paste0(ExportPrixProd,".xlsx"), sheetName = "A38", col.names = TRUE, row.names = TRUE, append=TRUE)
  write.xlsx(variations_A138, paste0(ExportPrixProd,".xlsx"), sheetName = "A138", col.names = TRUE, row.names = TRUE, append=TRUE)
  variations_A138<-cbind(List138[,1:2],pond_conso,var_tot_pcent[1:138,],var_dir_pcent[1:138,],var_ind_pcent[1:138,])
  }
  
  #recalcul des niveaux
  #var_tot_pcent_TOTAL <- as.data.frame(sortieAvionic[[4]])
  #Prod_3<-cbind(Prod138,Prod138,Prod138) ATTENTION on passe de prod a conso ICI
  Conso_3<-cbind(pond_conso,pond_conso,pond_conso)
  niv_variations_A138<-variations_A138
  niv_variations_A138[,4:6]<-niv_variations_A138[,4:6]*Conso_3
  niv_variations_A138[,3]<-pond_conso
  colnames(niv_variations_A138)[3]<-"conso"
  S2<-niv_variations_A138
  
  # Recalcul des layer MC et MT qui d?pendent des resultats du choc, et donc aussi le multiplicateur total
  S3<-CoefsPassPrixAcq
     # Calcul des effets en niveau sur les marges par proratisation de la part des marges sur chaque produit.
        # Attention : avec le dernier coefficient on se ram?ne ? la part des marges qui concerne la conso uniquement car il y en a bcp ailleurs sur la demande finale.
  Effet_Niv_MC<- (sum(niv_variations_A138[80:81,4])+niv_variations_A138[79,4]*0.49129)*0.48538  # *part qui revient ? la conso
  Effet_Niv_MT<- (niv_variations_A138[82,4]*9.06/100+niv_variations_A138[83,4]*5.72/100+niv_variations_A138[84,4]*27.14/100)*0.06692  # *part qui revient ? la conso
  #print(Effet_Niv_MC)
  #print(Effet_Niv_MT)
     # R?partition par produit des marges en niveau
  Niv_Struct_MC<-Struct_MC*Effet_Niv_MC
  Niv_Struct_MT<-Struct_MT*Effet_Niv_MT
  Mult_MC<-(CoefsPassPrixAcq$Conso_Niv2018_Pbase*CoefsPassPrixAcq$Mult_Layer1_TVA*CoefsPassPrixAcq$Mult_Layer2_ImpSub+Niv_Struct_MC)/(CoefsPassPrixAcq$Conso_Niv2018_Pbase*CoefsPassPrixAcq$Mult_Layer1_TVA*CoefsPassPrixAcq$Mult_Layer2_ImpSub)
  CoefsPassPrixAcq$Mult_Layer3_MC<-Mult_MC
  CoefsPassPrixAcq[is.na(CoefsPassPrixAcq)] <- 0
      # Ajustements sp?cifiques aux produits commerce et transport
  CoefsPassPrixAcq$Mult_Layer3_MC[79]<-0.509 # pour garder la part hors marges
  CoefsPassPrixAcq$Mult_Layer3_MC[80:81]<-0 # mise a 0 car il n'y a que des marges sur ces produits
  Mult_MT<-(CoefsPassPrixAcq$Conso_Niv2018_Pbase*CoefsPassPrixAcq$Mult_Layer1_TVA*CoefsPassPrixAcq$Mult_Layer2_ImpSub*CoefsPassPrixAcq$Mult_Layer3_MC+Niv_Struct_MT)/(CoefsPassPrixAcq$Conso_Niv2018_Pbase*CoefsPassPrixAcq$Mult_Layer1_TVA*CoefsPassPrixAcq$Mult_Layer2_ImpSub*CoefsPassPrixAcq$Mult_Layer3_MC)
  CoefsPassPrixAcq$Mult_Layer4_MT<-Mult_MT
  CoefsPassPrixAcq[is.na(CoefsPassPrixAcq)] <- 0
  # Ajustements sp?cifiques aux produits transport concern?s par les marges
  CoefsPassPrixAcq$Mult_Layer4_MT[82]<-0.90941 # pour garder la part hors marges
  CoefsPassPrixAcq$Mult_Layer4_MT[83]<-0.94279 # pour garder la part hors marges
  CoefsPassPrixAcq$Mult_Layer4_MT[84]<-0.72858 # pour garder la part hors marges
  MultTot<-CoefsPassPrixAcq$Mult_Layer1_TVA*CoefsPassPrixAcq$Mult_Layer2_ImpSub*CoefsPassPrixAcq$Mult_Layer3_MC*CoefsPassPrixAcq$Mult_Layer4_MT*CoefsPassPrixAcq$Mult_Layer5_PCHTR*CoefsPassPrixAcq$Mult_Layer6_recal
  CoefsPassPrixAcq$Mult_Full<-MultTot
  CoefsPassPrixAcq[19,1]<-1  # Ici exceptionnellement pour le tabac on place un multiplicateur unitaire car prix reglemente et evolution flat confirmee sur la periode de simulation.
  CoefsPassPrixAcq[is.na(CoefsPassPrixAcq)] <- 0
  S4<-CoefsPassPrixAcq
  
  
  # Passage en prix aquisition coef total de passage
  CoefTot_3<-cbind(CoefsPassPrixAcq[,1],CoefsPassPrixAcq[,1],CoefsPassPrixAcq[,1])
  niv_variations_A138[,4:6]<-niv_variations_A138[,4:6]*CoefTot_3
  S5<-niv_variations_A138

  # Eventuel ajout des boucliers si la liste est renseign?e
  vectbouclier<-rep(1,138)
  vectbouclier<-as.data.frame(vectbouclier)
  if(length(ListCoeffBouclier)>0){
    for(k in 1:length(ListCoeffBouclier)){vectbouclier<-vectbouclier*CoefsPassPrixAcq[,ListCoeffBouclier[[k]]]}
    CoefBouc_3<-cbind(vectbouclier,vectbouclier,vectbouclier)
    niv_variations_A138[,4:6]<-niv_variations_A138[,4:6]*CoefBouc_3
  }
  S6<-niv_variations_A138
  
  # R?int?gration de la conso en niveau au prix d'acquisition
  niv_variations_A138$conso<-CoefsPassPrixAcq$Conso_Niv2018_Pacqu
  colnames(niv_variations_A138[3])<-"Niv_Conso_2018"
  S7<-niv_variations_A138
  
  # ajout du total
  niv_variations_A138<-as.data.frame(niv_variations_A138)
  niv_variations_A138[,3:6]<-as.numeric(unlist(niv_variations_A138[,3:6]))
  niv_variations_A138[nrow(niv_variations_A138) + 1,] <- c("Total des 138 produits","TOTAL", sum(niv_variations_A138[,3]),sum(niv_variations_A138[,4])/sum(niv_variations_A138[,3]),sum(niv_variations_A138[,5])/sum(niv_variations_A138[,3]),sum(niv_variations_A138[,6])/sum(niv_variations_A138[,3]))
  niv_variations_A138[,3:6]<-as.numeric(unlist(niv_variations_A138[,3:6]))
  S8<-niv_variations_A138
  
  # Recalcul des % pour les 3 derni?res colonnes
  niv_variations_A138[1:138,4]<-niv_variations_A138[1:138,4]/niv_variations_A138[1:138,3]
  niv_variations_A138[1:138,5]<-niv_variations_A138[1:138,5]/niv_variations_A138[1:138,3]
  niv_variations_A138[1:138,6]<-niv_variations_A138[1:138,6]/niv_variations_A138[1:138,3]
  niv_variations_A138[is.na(niv_variations_A138)] <- 0
  
  # Toilettage intitules
  colnames(niv_variations_A138)[1]<-"libelles"
  colnames(niv_variations_A138)[2]<-"A138"
  colnames(niv_variations_A138)[3]<-"Niv_Conso_2018"
  colnames(niv_variations_A138)[4]<-"Effet_tot"
  colnames(niv_variations_A138)[5]<-"Effet_direct"
  colnames(niv_variations_A138)[6]<-"Effet_indirect"
  S9<-niv_variations_A138
  
  return(niv_variations_A138)  # Ou pour tests ?tapes par ?tapes : list(S1,S2,S3,S4,S5,S6,S7,S8,S9)  / niv_variations_A138
}

# Tests
#Choc1_ssCTniBouc<-Avio_variationLEONTIEF_prix(TEI138, t(Prod138), c(69), c(100.064), coeff_trans=NULL)   
#toto<-MiseEnForme(Choc1_ssCTniBouc,c(8,9))
#toto2<-AggA17(toto)





################################################################
###  Fonction de mise en forme ICIO tout court et sauvegarde

MiseEnFormeICIO<-function(sortieAvionic,mrio){

  var_tot_pcent <- as.data.frame(sortieAvionic[[1]])
  colnames(var_tot_pcent)<-"Var_tot"
  var_dir_pcent <- as.data.frame(sortieAvionic[[2]])
  colnames(var_dir_pcent)<-"Var_dir"
  var_ind_pcent <- as.data.frame(sortieAvionic[[3]])
  colnames(var_ind_pcent)<-"Var_ind"

  variations_ICIO<-cbind(rownames(mrio[["TEI"]][["FRAM"]]),mrio[["output"]][["FRAM"]],var_tot_pcent,var_dir_pcent,var_ind_pcent)

  #recalcul et ajout du total
  Prod_3<-cbind(mrio[["output"]][["FRAM"]],mrio[["output"]][["FRAM"]],mrio[["output"]][["FRAM"]])
  niv_variations_ICIO<-variations_ICIO
  niv_variations_ICIO[,3:5]<-niv_variations_ICIO[,3:5]*Prod_3
  
  colnames(variations_ICIO)[1]<-"CountryBranche"
  return(variations_ICIO)
}



################################################################
###  Fonction de mise en forme type TCD pour mise en forme des sorties d'ICIO et recalcul des totaux

MiseEnColonneResICIO<-function(df,nom_var){
  
  dflong <- df %>% add_column(Country=substr(df$CountryBranche, 1 , 3),.before = "CountryBranche")
  dflong <- dflong %>% add_column(Branche=substr(df$CountryBranche, 5 , nchar(df$CountryBranche)),.before = "CountryBranche")
  
  dflong<-dflong[,c("Country","Branche",as.character(nom_var))]
  
  dfout<-dflong %>% tidyr::pivot_wider(names_from = "Country", values_from = nom_var) 
  
  #intermY<- intermX %>% tidyr::pivot_wider(names_from = "EmissionsDe", values_from = toString(annee)) 
  #cfpAgg_CFP_MatthieuLong <- cfpAgg_CFP_Matthieu %>% tidyr::pivot_longer(!EnProvenanceDe, names_to = "EmiGenerePar", values_to = "Emi") 
  #intermY<- intermX %>% tidyr::pivot_wider(names_from = "EmissionsDe", values_from = toString(annee)) 
  
  return(dfout)
}

CalculTotaux<-function(df,nom_var){
  
  df1<-MiseEnColonneResICIO(df,as.character(nom_var))
  df2<-MiseEnColonneResICIO(df,"output")
  
  df1<-as.data.frame(df1)
  df2<-as.data.frame(df2)
  
  dfprod<-df1 #init
  dfprod[,2:46]<-df1[,2:46]*df2[,2:46]
  
  calctot<-colSums(dfprod[,2:46])/colSums(df2[,2:46])
  calctot<-as.data.frame(calctot,drop=FALSE)
  
  return(calctot)
}

MEF<-function(df,nom_var){
  
  df1<-MiseEnColonneResICIO(df,as.character(nom_var))
  df2<-CalculTotaux(df,as.character(nom_var))
  
  vectTot<-c("Total",t(df2))
  
  out<-rbind(df1,vectTot)
  
  return(out)
}

################################################################
#### Variante de la fonction en prix autorisant un choc sur plusieurs produits exogeneises

### Les produits concernes sont dans une liste, de meme que les variations.
### Les chocs restent exogenes, donc traitement par blocs endo/exo 
### cf Valadkhani, Abbas and Mitchell, W. F.: Assessing the Impact of Change in Petroleum Prices on Inflation and Household Expenditures in Australia 2002. https://ro.uow.edu.au/commpapers/402


Avio_variationMULTI_prix <- function(tei, prod, num, var, coeff_trans=NULL, power=NULL, onlyTEIdom=NULL, onlyTEIdomOptContrib=NULL,formule=NULL){ # Num and var are lists of same size

  if(! is.null(coeff_trans)) {

    ## 0.1) Reshape les matrices pour placer les produits ? impacter en fin de tableaux
    reshape<-SortTEIetProd(tei, t(prod), num)
    tei_sorted<-reshape[[3]]
    prod_sorted<-reshape[[4]]
    nb_produits_impact <-length(num)
    nb_produits_tot<-length(rownames(tei))
    
    reshapeCoeff<-SortTEIetProd(tei, coeff_trans, num)
    coeff_sorted<-reshapeCoeff[[4]]  #Ici on n utilise que le tri sur l'?quivalent de la prod qui est ici les coeffs de transmission
    
    coeff <- diag(as.vector(coeff_sorted[1:(nb_produits_tot-nb_produits_impact)]))
    
    ## 1) Calcul des coefficients techniques
    ctec <- coef_techniques(A= tei_sorted, B= t(prod_sorted))
    
    ## 2) CrÃ©ations des matrices en intermÃ©diaires (amputÃ©es du produit variant)
    
    # extraction de A' de la sous-matrice des endog?nes
    tctec<-t(ctec)
    NmoinsMPlusUn<-nb_produits_tot-nb_produits_impact+1
    NmoinsM<-nb_produits_tot-nb_produits_impact
    AprimeEE<-tctec[1:NmoinsM,1:NmoinsM] # Premier matrice bloc diagonal
    AprimeEE_save<-AprimeEE
    AprimeXE<-tctec[1:NmoinsM,NmoinsMPlusUn:nb_produits_tot] 
    prodE<-prod_sorted[1:NmoinsM]
    
    # ATTENTION : dans le cas du choc sur les importations uniquement (option onlyTEIdom) on modifie AprimeEE pour ne garder que la partie domestique dans l inverse de Leontiev
    #    Si 'onlyTEIdomOptContrib' n'est pas renseign? : On ne change pas AprimeXE car la formule est :  (I-Adom'EE)^(-1)*A'XE*delta_p(exogenes)
    #    Si 'onlyTEIdomOptContrib'='dom' : On change AprimeXE en AprimeXE_dom pour obtenir la contribution domestique :  (I-Adom'EE)^(-1)*A'XE_dom*delta_p(exogenes)
    #    Si 'onlyTEIdomOptContrib'='imp' : On change AprimeXE en AprimeXE_imp pour obtenir la contribution domestique :  (I-Adom'EE)^(-1)*A'XE_imp*delta_p(exogenes)    
       if(! is.null(onlyTEIdom)) {
      reshapeBis<-SortTEIetProd(onlyTEIdom, t(prod), num)
      tei_sortedBis<-reshapeBis[[3]]
      prod_sortedBis<-reshapeBis[[4]]
      nb_produits_impact <-length(num)
      nb_produits_tot<-length(rownames(onlyTEIdom))
      ctecBis <- coef_techniques(A= tei_sortedBis, B= t(prod_sortedBis))
      tctecBis<-t(ctecBis)
      NmoinsMPlusUn<-nb_produits_tot-nb_produits_impact+1
      NmoinsM<-nb_produits_tot-nb_produits_impact
      AprimeEE<-tctecBis[1:NmoinsM,1:NmoinsM] # Premier matrice bloc diagonal pour la matrice du TEI importe uniquement
      AprimeXEdom<-tctecBis[1:NmoinsM,NmoinsMPlusUn:nb_produits_tot] 

      # Calcul de AprimeXEimp en s appuyant sur le solde entre TEI et TEIdom
      TEIimp<-tei-onlyTEIdom
      reshapeTer<-SortTEIetProd(TEIimp, t(prod), num)
      tei_sortedTer<-reshapeTer[[3]]
      prod_sortedTer<-reshapeTer[[4]]
      nb_produits_impact <-length(num)
      nb_produits_tot<-length(rownames(onlyTEIdom))
      ctecTer <- coef_techniques(A= tei_sortedTer, B= t(prod_sortedTer))
      tctecTer<-t(ctecTer)
      NmoinsMPlusUn<-nb_produits_tot-nb_produits_impact+1
      NmoinsM<-nb_produits_tot-nb_produits_impact
      AprimeXEimp<-tctecBis[1:NmoinsM,NmoinsMPlusUn:nb_produits_tot]       
    }
    
    if(! is.null(onlyTEIdomOptContrib)) {
      if(onlyTEIdomOptContrib=="dom"){AprimeXE<-AprimeXEdom}
      if(onlyTEIdomOptContrib=="imp"){AprimeXE<-AprimeXEimp}  
    }
    
    if(! is.null(formule)) {    # ici on r?attribue le AprimeEE de d?part dans l'inversion de L?ontief dans le cas de la formule 4 de Matthieu
      if(formule=="f2"){AprimeEE<-AprimeEE}
      if(formule=="f4"){AprimeEE<-AprimeEE_save}  
    }
    
    
    
    ## 3) Equation delta_p(endogenes) = (I-A'EE*diag(coeff))^(-1)*A'XE*delta_p(exogenes)
    ##  ex dim avec 3 exo :  [n-3,1]    =  [n-3,n-3]*[n-3,3]*[3,1]
    
    I_AprimeEE <- identite(A=AprimeEE%*%coeff)  # ctec_prime
    inv_I_AprimeEE <- inverse(mat=I_AprimeEE)
    
    VectShock<-as.matrix(var)
    
    var_prixE<-inv_I_AprimeEE%*%AprimeXE%*%VectShock

    
    # Reconstitution du vecteur de sortie en r?int?rant les chocs exog?nes au bon endroit
    var_prix<-var_prixE
    dim(var_prixE)
    for(k in 1:nb_produits_impact){
      NumAinserer<-num[[k]]
      ValAinserer<-var[[k]]
      NumAinsererMoins1<-NumAinserer-1
      NumAinsererPlus1<-NumAinserer+1
      TailleVar_prix<-length(var_prix)
      var_prix<-c(var_prix[1:NumAinsererMoins1],ValAinserer,var_prix[NumAinserer:TailleVar_prix])
    }
    
    # Calcul de l effet direct
    delta_p_directE<-(diag((nb_produits_tot-nb_produits_impact))+AprimeEE%*%coeff)%*%AprimeXE%*%VectShock
    delta_p_direct<-delta_p_directE
    for(k in 1:nb_produits_impact){
      NumAinserer<-num[[k]]
      ValAinserer<-var[[k]]
      NumAinsererMoins1<-NumAinserer-1
      NumAinsererPlus1<-NumAinserer+1
      Tailledelta_p_direct<-length(delta_p_direct)
      delta_p_direct<-c(delta_p_direct[1:NumAinsererMoins1],ValAinserer,delta_p_direct[NumAinserer:Tailledelta_p_direct])
    }
    
    ### Calcul uniquement de la puissance indiqu?e --> stock? dans effet direct, et reste dans indirect
    if(! is.null(power)) {    
      if(power==0){delta_p_directE <-AprimeXE%*%VectShock}
      if(power==1){delta_p_directE <-AprimeEE%*%coeff%*%AprimeXE%*%VectShock}
      if(power>1){delta_p_directE <-powA(AprimeEE%*%coeff,power)%*%AprimeXE%*%VectShock}
    
      delta_p_direct<-delta_p_directE
      for(k in 1:nb_produits_impact){
        NumAinserer<-num[[k]]
        ValAinserer<-var[[k]]
        NumAinsererMoins1<-NumAinserer-1
        NumAinsererPlus1<-NumAinserer+1
        Tailledelta_p_direct<-length(delta_p_direct)
        delta_p_direct<-c(delta_p_direct[1:NumAinsererMoins1],ValAinserer,delta_p_direct[NumAinserer:Tailledelta_p_direct])
      }
      
      if(power>=1){
        delta_p_direct<-delta_p_directE
        for(k in 1:nb_produits_impact){
          NumAinserer<-num[[k]]
          ValAinserer<-var[[k]]
          NumAinsererMoins1<-NumAinserer-1
          NumAinsererPlus1<-NumAinserer+1
          Tailledelta_p_direct<-length(delta_p_direct)
          delta_p_direct<-c(delta_p_direct[1:NumAinsererMoins1],0,delta_p_direct[NumAinserer:Tailledelta_p_direct])
        }
        }
    }
    
    # Calcul par solde de l effet indirect
    delta_p_indirect<-var_prix-delta_p_direct
    
    # Calcul de l effet total et de l elasticite
    valeur_x_var <- t(prod)*var_prix/100
    delta_total <- sum(valeur_x_var)/sum(prod)
    elast_prix <- delta_total # Faux mais pas utilise
    
    var_prix<-as.matrix(var_prix,drop=FALSE)
    delta_p_direct<-as.matrix(delta_p_direct,drop=FALSE)
    delta_p_indirect<-as.matrix(delta_p_indirect,drop=FALSE)
    
    var_prix <- list(var_prix= var_prix, var_prix_dir=delta_p_direct, var_prix_indir=delta_p_indirect, var_tot=delta_total, elast = elast_prix)
    
    return(var_prix)
    
  } else {  ########################## Cas o? il n'y a pas de coefficients de transmission  ##############

    ## 0.1) Reshape les matrices pour placer les produits ? impacter en fin de tableaux
    reshape<-SortTEIetProd(tei, t(prod), num)
    tei_sorted<-reshape[[3]]
    prod_sorted<-reshape[[4]]
    nb_produits_impact <-length(num)
    nb_produits_tot<-length(rownames(tei))

    ## 1) Calcul des coefficients techniques
    ctec <- coef_techniques(A= tei_sorted, B= t(prod_sorted))

    ## 2) CrÃ©ations des matrices en intermÃ©diaires (amputÃ©es du produit variant)
    
    # extraction de A' de la sous-matrice des endog?nes
    tctec<-t(ctec)
    NmoinsMPlusUn<-nb_produits_tot-nb_produits_impact+1
    NmoinsM<-nb_produits_tot-nb_produits_impact
    AprimeEE<-tctec[1:NmoinsM,1:NmoinsM] # Premier matrice bloc diagonal
    AprimeXE<-tctec[1:NmoinsM,NmoinsMPlusUn:nb_produits_tot] 
    prodE<-prod_sorted[1:NmoinsM]
    
    # ATTENTION : dans le cas du choc sur les importations uniquement (option onlyTEIdom) on modifie AprimeEE pour ne garder que la partie domestique
    #     On ne change pas AprimeXE car la formule est :  (I-Adom'EE)^(-1)*A'XE*delta_p(exogenes)
    if(! is.null(onlyTEIdom)) {
      reshapeBis<-SortTEIetProd(onlyTEIdom, t(prod), num)
      tei_sortedBis<-reshapeBis[[3]]
      prod_sortedBis<-reshapeBis[[4]]
      nb_produits_impact <-length(num)
      nb_produits_tot<-length(rownames(onlyTEIdom))
      ctecBis <- coef_techniques(A= tei_sortedBis, B= t(prod_sortedBis))
      tctecBis<-t(ctecBis)
      NmoinsMPlusUn<-nb_produits_tot-nb_produits_impact+1
      NmoinsM<-nb_produits_tot-nb_produits_impact
      AprimeEE<-tctecBis[1:NmoinsM,1:NmoinsM] # Premier matrice bloc diagonal pour la matrice du TEI importe uniquement
    }
    
    ## 3) Equation delta_p(endogenes) = (I-A'EE)^(-1)*A'XE*delta_p(exogenes)
    ##  ex dim avec 3 exo :  [n-3,1]    =  [n-3,n-3]*[n-3,3]*[3,1]

    I_AprimeEE <- identite(A=AprimeEE)  # ctec_prime
    inv_I_AprimeEE <- inverse(mat=I_AprimeEE)
    
    VectShock<-as.matrix(var)
    
    var_prixE<-inv_I_AprimeEE%*%AprimeXE%*%VectShock

    # Reconstitution du vecteur de sortie en r?int?rant les chocs exog?nes au bon endroit
    var_prix<-var_prixE
    dim(var_prixE)
    for(k in 1:nb_produits_impact){
      NumAinserer<-num[[k]]
      ValAinserer<-var[[k]]
      NumAinsererMoins1<-NumAinserer-1
      NumAinsererPlus1<-NumAinserer+1
      TailleVar_prix<-length(var_prix)
      var_prix<-c(var_prix[1:NumAinsererMoins1],ValAinserer,var_prix[NumAinserer:TailleVar_prix])
    }

    # Calcul de l effet direct
    delta_p_directE<-(diag((nb_produits_tot-nb_produits_impact))+AprimeEE)%*%AprimeXE%*%VectShock
    delta_p_direct<-delta_p_directE
    for(k in 1:nb_produits_impact){
      NumAinserer<-num[[k]]
      ValAinserer<-var[[k]]
      NumAinsererMoins1<-NumAinserer-1
      NumAinsererPlus1<-NumAinserer+1
      Tailledelta_p_direct<-length(delta_p_direct)
      delta_p_direct<-c(delta_p_direct[1:NumAinsererMoins1],ValAinserer,delta_p_direct[NumAinserer:Tailledelta_p_direct])
    }
    
    ### Calcul uniquement de la puissance indiqu?e --> stock? dans effet direct, et reste dans indirect
    if(! is.null(power)) {    
      if(power==0){delta_p_directE <-AprimeXE%*%VectShock}
      if(power==1){delta_p_directE <-AprimeEE%*%AprimeXE%*%VectShock}
      if(power>1){delta_p_directE <-powA(AprimeEE,power)%*%AprimeXE%*%VectShock}
      
      delta_p_direct<-delta_p_directE
      for(k in 1:nb_produits_impact){
        NumAinserer<-num[[k]]
        ValAinserer<-var[[k]]
        NumAinsererMoins1<-NumAinserer-1
        NumAinsererPlus1<-NumAinserer+1
        Tailledelta_p_direct<-length(delta_p_direct)
        delta_p_direct<-c(delta_p_direct[1:NumAinsererMoins1],ValAinserer,delta_p_direct[NumAinserer:Tailledelta_p_direct])
      }
      
      if(power>=1){
        delta_p_direct<-delta_p_directE
        for(k in 1:nb_produits_impact){
          NumAinserer<-num[[k]]
          ValAinserer<-var[[k]]
          NumAinsererMoins1<-NumAinserer-1
          NumAinsererPlus1<-NumAinserer+1
          Tailledelta_p_direct<-length(delta_p_direct)
          delta_p_direct<-c(delta_p_direct[1:NumAinsererMoins1],0,delta_p_direct[NumAinserer:Tailledelta_p_direct])
        }
      }
    }
    
    # Calcul par solde de l effet indirect
    delta_p_indirect<-var_prix-delta_p_direct
    
    # Calcul de l effet total et de l elasticite
    valeur_x_var <- t(prod)*var_prix/100
    delta_total <- sum(valeur_x_var)/sum(prod)
    elast_prix <- delta_total # Faux mais pas utilise
    
    var_prix<-as.matrix(var_prix,drop=FALSE)
    delta_p_direct<-as.matrix(delta_p_direct,drop=FALSE)
    delta_p_indirect<-as.matrix(delta_p_indirect,drop=FALSE)
    
    var_prix <- list(var_prix= var_prix, var_prix_dir=delta_p_direct, var_prix_indir=delta_p_indirect, var_tot=delta_total, elast = elast_prix)
    
    return(var_prix)
  }
}

#testREF<-Avio_variationLEONTIEF_prix(TEI138, t(Prod138), c(27,69,70), c(31.323, 4.499, 51.145))
#test<-Avio_variationMULTI_prix(TEI138, t(Prod138), c(27,69,70), c(31.323, 4.499, 51.145))
#comparTest<-cbind(testREF[[1]],test[[1]])
#comparTest<-as.data.frame(comparTest)
#comparTest$ratio<-comparTest[,1]/comparTest[,2]
#MEFtest<-MiseEnForme(test,c(11,12,13))
#MEFtestREF<-MiseEnForme(testREF,c(11,12,13))

  
################################################################
#### Variante LEONTIEF de la fonction en prix autorisant un choc sur plusieurs produits

### Les produits concern?s sont dans une liste, de m?me que les variations.


Avio_variationLEONTIEF_prix <- function(tei, prod, num, var, coeff_trans=NULL){ # Nul and var are lists of same size
nb_shocks<-length(num)
  if(! is.null(coeff_trans)) {

    vect_num<-rep(0,nrow(tei))
    for(k in 1:nb_shocks){
      vect_num[num[k]]<-var[k]
    }
    vect_num<-as.matrix(vect_num,drop=FALSE)
    ## 1) Calcul des coefficients techniques

    ctec <- coef_techniques(A= tei, B= prod)

    ## 2) CrÃ©ations des matrices en intermÃ©diaires (amputÃ©es du produit variant)

    ctec_prime <- t(ctec)

    coeff <- diag(as.vector(coeff_trans))

    ## 3) Equation delta_p = (I-A*coeff)^(-1)*delta_pi

    I_A <- identite(A=ctec_prime%*%coeff)
    inv_I_A <- inverse(mat=I_A)

    delta_p <- inv_I_A%*%(vect_num/100)

    delta_p <- as.matrix(delta_p*100)

    ## 4) Calcul de la variation totale du prix

    valeur_x_var <- t(prod)*delta_p/100
    delta_total <- sum(valeur_x_var)/sum(prod)
    elast_prix <- delta_total / var

    ## 5) Effet direct a_iT*delta_pi et effet indirect (I-A)^(-1)*a_iT*delta_pi*-a_iT*delta_pi

    delta_p_direct <- (diag(nrow(tei))+ctec_prime%*%coeff)%*%(vect_num/100)*100

    delta_p_indirect = delta_p - delta_p_direct

    var_prix <- list(var_prix= delta_p, var_prix_dir=delta_p_direct, var_prix_indir=delta_p_indirect, var_tot=delta_total, elast = elast_prix)

    return(var_prix)

    cat("ATTENTION \n Rappel, les rÃ©sultats ne seront pas les mÃªmes en fonction de si vous avez entrÃ© un TEI seul domestique ou domestique et importÃ©. Se reporter Ã  la documentation")

  } else {

    vect_num<-rep(0,nrow(tei))
    for(k in 1:nb_shocks){
      vect_num[num[k]]<-var[k]
    }
    vect_num<-as.matrix(vect_num,drop=FALSE)

    ## 1) Calcul des coefficients techniques

    ctec <- coef_techniques(A= tei, B= prod)

    ## 2) CrÃ©ations des matrices en intermÃ©diaires (amputÃ©es du produit variant)

    ctec_prime <- t(ctec)

    ## 3) Equation delta_p = (I-A*coeff)^(-1)*delta_pi

    I_A <- identite(A=ctec_prime)
    inv_I_A <- inverse(mat=I_A)

    delta_p <- inv_I_A%*%(vect_num/100)

    delta_p <- as.matrix(delta_p*100)

    ## 4) Calcul de la variation totale du prix

    valeur_x_var <- t(prod)*delta_p/100
    delta_total <- sum(valeur_x_var)/sum(prod)
    elast_prix <- delta_total / var

    ## 5) Effet direct a_iT*delta_pi et effet indirect (I-A)^(-1)*a_iT*delta_pi*-a_iT*delta_pi

    delta_p_direct <- (diag(nrow(tei))+ctec_prime)%*%(vect_num/100)*100

    delta_p_indirect = delta_p - delta_p_direct

    var_prix <- list(var_prix= delta_p, var_prix_dir=delta_p_direct, var_prix_indir=delta_p_indirect, var_tot=delta_total, elast = elast_prix)

    return(var_prix)

    cat("ATTENTION \n Rappel, les rÃ©sultats ne seront pas les mÃªmes en fonction de si vous avez entrÃ© un TEI seul domestique ou domestique et importÃ©. Se reporter Ã  la documentation")
  }
}

# Function to sort a dataframe by row and column according to lists
SortTEIetProd=function(TEI, Prod,TargetList){
  nb_target<-length(TargetList)
  wTEI<-TEI
  wProd<-Prod

  # TEI
  for(i in 1:nb_target){                    # Lignes
    ToAdd<-wTEI[TargetList[[i]],]
    wTEI<-rbind(wTEI,ToAdd)  # Ajoute en dupliquant la ligne a la fin
  }
  for(i in 1:nb_target){                    # Lignes
    wTEI<-wTEI[-TargetList[[nb_target-i+1]],]  #Supprime la ligne
  }
  for(j in 1:nb_target){                    # Colonnes
    ToAdd<-wTEI[,TargetList[[j]]]
    wTEI<-cbind(wTEI,ToAdd)  # Ajoute en dupliquant la colonne ? la fin
  }
  for(j in 1:nb_target){                    # Colonnes
    wTEI<-wTEI[,-TargetList[[nb_target-j+1]]]  #Supprime la colonne
  }

  # Prod
  for(i in 1:nb_target){   # Lignes
    ToAdd<-wProd[TargetList[[i]]]
    wProd<-c(wProd,ToAdd)  # Ajoute en dupliquant la ligne ? la fin
    wProd<-as.matrix(wProd)
  }
  for(i in 1:nb_target){   # Lignes
    wProd<-wProd[-TargetList[[nb_target-i+1]]] #Supprime la ligne
    wProd<-as.matrix(wProd)
  }

  out<-list(as.data.frame(TEI),as.data.frame(Prod,drop=FALSE),as.data.frame(wTEI),as.data.frame(t(wProd),drop=FALSE))
  return(out)

 }

#test<-SortTEIetProd(TEI138, t(Prod138),c(3,4))
#print(dim(test[[1]]))
#print(dim(test[[3]]))
#print(dim(test[[2]]))
#print(dim(test[[4]]))

#########################################################
#########################################################
#                                                       #
#    AVIONIC - Compte des m?nages par cat?gories (CMpC) #
#                                                       #
#########################################################
#########################################################

#######################################################################################################
## FONCTION DE VENTILATION D UN VECTEUR SELON LE CMpC ############################################################
##
## Cette fonction permet de ventiler un vecteur de format fixe (COCIOP 1+2 digit) tel qu il ressort notamment des programmes d agregation.
#######################################################################################################

## On commence par charger la table de structure du CMpC qui est implicitement du niveau COCIOP 1+2 digit
# si n?cessaire : tab_Struct_CMpC <- read.csv(file="Tab_Struct_CMpC.csv", header=TRUE, sep=";") # Chargement de la table de structure CMpC
# stringsAsFactors = FALSE

# Creation d'une fonction pour agreger tout vecteur a tout niveau de nomenclature
# Variables : vecteur en entree = vecteur en sortie d AVIONIC (cf fonctions pr?c?dentes)
# A noter : la structure des donnees est relativement contrainte : structure du CMpC entre les colonnes 4:43 et vecteur en input se retrouve en colonne 47 et 48 lignes <- eventuellement a adapter si la structure du CMpC change

Ventile_Vecteur <- function(tablVectEntree)  {
  VecteurAajouter <- tablVectEntree   # On suppose que cette table est d?j? dans R, notamment issue d'une pr?c?dente agr?gation
  # Si cette table est n'est pas d?j? dans R : VecteurAajouter <- read.csv(file="XYZ.csv", header=TRUE, sep=";")
  
  # On commence par faire une proc SQL pour ajouter le vecteur d entree a la table de structure du CMpC
  Tab_struct_CMpC_plusVecteur<<-sqldf("select * from tab_Struct_CMpC a, VecteurAajouter b where a.FONC1et2=b.Agrege") 
  
  ### M?thode avec des matrices 
  Tab_struct_CMpC_foisInterm<-as.matrix(Tab_struct_CMpC_plusVecteur)
  
  # Dans MAT1 on place les ?l?ments de structure
  MAT1=as.matrix(Tab_struct_CMpC_foisInterm[,4:43])
  
  # Dans MAT2 on place les 40 r?plication du vecteur du choc ou du contenu qui nous arrive en sortie d'AVIONIC
  MAT2<- matrix(nrow=47, ncol=40)
  
  # On convertit les ?l?ments de structure en num?rique pour pouvoir proc?der ? la multiplication
  for (i in 1:40) { 
    MAT2[,i]=Tab_struct_CMpC_foisInterm[,47]
  }
  # Export des 2 matrices pour en faciliter la lecture
  # write.table(MAT1, "Mat1.xls") # Export 
  # write.table(MAT2, "Mat2.xls") # Export 
  # On d?finit le format de la matrice r?sultat 
  PRODMAT12<- matrix(nrow=47, ncol=40)
  
  # Conversion en format num?rique avant le produit matriciel terme ? terme
  m1 <- mapply(MAT1, FUN=as.numeric)
  MAT1 <- matrix(data=m1, ncol=40, nrow=47)
  m2 <- mapply(MAT2, FUN=as.numeric)
  MAT2 <- matrix(data=m2, ncol=40, nrow=47)
  
  # Produit matriciel terme ? terme
  PRODMAT12<-MAT1*MAT2
  write.table(PRODMAT12, "Mat12.xls") # Export 
  Tab_struct_CMpC_foisInterm[,4:43]<-PRODMAT12 # pour que ?a reste en variable globale
  Tab_struct_CMpC_foisVecteur<<-Tab_struct_CMpC_foisInterm
  return
}

# Creation d'une fonction pour calculer la strucutre resultant du CMpC et la comparer ? la structure moyenne
# Variables : table en entree = table en sortie de la fonction pr?c?dente
# A noter : la structure des donnees est relativement contrainte : structure du CMpC entre les colonnes 4:43 et vecteur en input se retrouve en colonne 47 et 48 lignes <- eventuellement a adapter si la structure du CMpC change

resultat_CMpC <- function(tablEntree)  {
  tabW <- tablEntree   # On suppose que cette table est d?j? dans R, notamment issue d'une pr?c?dente agr?gation
  # Si cette table est n'est pas d?j? dans R : VecteurAajouter <- read.csv(file="XYZ.csv", header=TRUE, sep=";")
  
  # On commence par faire une proc SQL pour ajouter le vecteur d entree a la table de structure du CMpC
  f<-as.data.frame(Tab_struct_CMpC_foisVecteur)
  
  RESULTAT<-matrix(data=NA,nrow=3,ncol=40)
  for (i in 4:43) {
    f[,c(i)]<-as.numeric(as.character(f[,c(i)]))
    tempo<-aggregate(f[,c(i)] ~ NiveauCOICOP, f , sum) 
    tempo<-as.matrix(tempo)
    tempo<<-tempo
    RESULTAT[,i-3]<-tempo[,2]
  }
  TabAgreg<<-RESULTAT
  ### Lecture de TabAgreg
  # la ligne 1 de Tabagrer correspond ? l'agr?gation du niveau 1 COICOP
  # la ligne 2 au niveau 2 COCIOP 
  # la ligne 3 : uniquement directement le Total (sans ventilation au sein des d?compositions du compte des m?nages par cat?gories)
  
  return
}


#######################################################################################################
## FONCTION AGREG FONCTIONNELLE #######################################################################
##
## Cette fonction permet d agreger un vecteur de tout niveau NA en nomenclature fonctionnelle 2 digit
#######################################################################################################
# Lorsqu'on dispose d'une table agr?gr?e (cf ci-dessus) ou d'une table directement ? un niveau NA
# Niveaux possibles : A10, A17, A38, A64, A88, A129
# On va appliquer la table de passage ad'hoc pour passer d'un niveau NA a la nomenc fonctionnelle
# En nomenclature fonctionnelle on peut appliquer le compte des m?nages par cat?gories pour obtenir des d?compositions

# En toute logique, ce passage en fonctionnelle fait suite ? une agr?gation du type de la fonction pr?c?dente.
# La table a agr?ger en fonctionnelle doit au pr?alable ?tre dans la var globale TabAfter
# Si beson de la charger : ajouter TabAfter <- read.csv(file="chemin", header=TRUE, sep=";")

# Variables : niveauNA_depart = niveau de nomenclature de d?part en NA. Exemple : niveauNA_depart = "NA17". le niveau d'arriv?e est connu par avance, c'est la fonctionnelle COICOP 2 digits. 


Agreg_NA_fonc <- function(niveauNA_depart,TabAfter, variable)  {
  Chemin <-  paste0("TabPass_", niveauNA_depart)
  Chemin <- paste0(Chemin, "_vers_fonc")  
  tabPass <- get(Chemin) 
  
  
  
  
  # si import depuis un fichier externe CSV :
  #Chemin <-  paste0("TabPass_", niveauNA_depart)
  #Chemin <- paste0(Chemin, "_vers_fonc.csv")  
  #tabPass <- read.csv(file=Chemin, header=TRUE, sep=";")
  
  tabAagreger <- TabAfter # A charger au pr?alable si besoin ; l? on consid?re qu'elle est d?j? l?. Si besoin ajouter TabAfter <- read.csv(file="chemin", header=TRUE, sep=";")
  
  # Requ?te SQL d'agr?gation : 
  Requete <- paste0("select a.Fonc , sum(a.Coef*b.",variable)
  Requete <- paste0(Requete,")as VALFONC from tabPass a, tabAagreger b where a.Pr_" , niveauNA_depart)
  Requete <- paste0(Requete, "=b.")
  Requete <- paste0(Requete, niveauNA_depart) 
  Requete <- paste0(Requete, " group by a.Fonc")
  cat(Requete)
  TabAfter_fonc<<-sqldf(Requete)  # On d?finit une variable globale pour r?cup?rer la table en sortie de la fonction
  
  ## Passage ou on calcule le niveau 1 digit de la COICOP afin de disposer des niveaux 1 et 2 de la COICOP dans le fichier de sortie
  tabPass_niv1COICOP <- TabPass_foncdet_vers_fonc10 # si n?cessaire : read.csv(file="TabPass_foncdet_vers_fonc10.csv", header=TRUE, sep=";")  
  tabAagreger <- TabAfter_fonc # Est issu de la variable globale calcul?e pr?c?demment
  TabAfter_fonc_niv1et2COICOP<<-sqldf("select * , sum(VALFONC) as VAL_FONC from tabPass_niv1COICOP a, tabAagreger b where a.Detail=b.Fonc group by Agrege") 
  TabAfter_fonc_niv1et2COICOP<<-subset(TabAfter_fonc_niv1et2COICOP, select = c( "OrdreAffich", "Agrege","Libelle",  "VAL_FONC" )) 
  return
}





























