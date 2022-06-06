library(tidyverse)

import_limesurvey <- function(edfile="results-survey881846.csv", dir=".") {
  
  extradatafile <- paste0(dir,"/extradata/extradata.csv")
  
  extradata <- read.csv2(extradatafile)

  ed <- read.csv(paste0(dir,"/extradata/",edfile)) %>%
    mutate(
      Libellé = paste0(na.omit(Universite,Universite.other.,
                            greta, greta.other.,
                            regrp, regrp.other.,
                            ecole, ecole.other.,
                            autre, autre.other.))
      ) %>%
    mutate(
      across(starts_with("hec."), ~ as.numeric(gsub(" ","", .x))),
      across(starts_with("dotations."), ~ as.numeric(gsub(" ","", .x)))
      ) %>%
    select(Libellé,docs.chcle.:dotobs) 
  
  bind_rows(extradata,ed) %>%
    write.csv2(extradatafile, row.names = FALSE)
}

read_and_pivot_extradata <- function() {
  ed <- read.csv2("extradata/extradata.csv") %>%
    select(Libellé,hec.2010_HeE.:dotliens.2020.) %>%
    pivot_longer(matches("(.*).(20..)_(.*)."),
                 names_pattern = "(.*).(20..)_(.*).",
                 names_to = c("groupe","rentrée","type"),
                 values_to = "valeur") %>%
    select(Libellé,rentrée,groupe,type,valeur) %>%
    pivot_wider(names_from = c(groupe,type),
                values_from = valeur)

  
}

get_extradata_array <- function(extradata, qid, twodim = FALSE) {
  pat <- ifelse(twodim, paste0(qid,".(.*)_(.*)."), paste0(qid,".(.*)(.*)."))
  extradata %>%
    select(starts_with(paste0(qid,"."))) %>% 
    pivot_longer(
      cols = everything(),
      names_pattern = pat,
      names_to = c("key1","key2"),
      values_to = "valeur"
    ) %>% 
    filter(!is.na(valeur), !valeur == "") %>%
    { if(!twodim) mutate(., key2 = "lien") else . } %>% 
    pivot_wider(
      names_from = key2,
      values_from = valeur
    )
}

percent_format <- function(x) {
  sprintf("%+0.1f%%", round(x*100,1))
}

euro_k <- function(x) {
  return(paste0(format(round(x/1000,0), big.mark=" ")," k€"))
}

merge_and_add_kpis <- function(fbp) {
  
  fbp$extradata.kpi <- read.csv("extradata/fbp_extradata.csv") %>% 
    filter(UAI == fbp$UAI) %>%
    mutate(
      across(Heures.HeC:Heures.HeCV,
             ~ case_when(
               HC.Format.HeuresEuros == "Euros" ~ as.integer(.x / 41.41),
               TRUE ~ .x
             )
      )
    ) %>%
    mutate(Rentrée = as.character(Rentrée)) %>%
    select(Rentrée,Heures.HeE:Dotations.vieet)

  fbp$esr <- full_join(fbp$esr,fbp$extradata.kpi) 
  
  fbp$esr.pnl <- fbp$esr %>%
    transmute (
      Rentrée = Rentrée,
      UAI = UAI,
      kpi.H.heePetu = Heures.HeE / (kpi.ETU.S.cycle1_L + kpi.ETU.S.cycle2_M),
      kpi.H.hctPtit = Heures.HeCT / kpi.ENS.S.titulaires,
      kpi.H.hcvPhee = 1 - Heures.HeCT / Heures.HeC,
      kpi.D.ens = Dotations.doten,
      kpi.D.aapens = Dotations.aapen,
      kpi.D.rech = Dotations.dotre,
      kpi.D.aaprech = Dotations.aapre,
      kpi.D.vieetu = Dotations.vieet
    ) %>%
    pivot_longer(-c(Rentrée,UAI), names_to="kpi", values_to="valeur") %>%
    mutate(
      valeur_label = case_when(
        kpi == "kpi.H.hcvPhee" ~ scales::percent(valeur,0.1),
        startsWith(kpi, "kpi.D") ~ euro_k(valeur),
        TRUE ~ format(valeur, big.mark=" ", digits=1),
      )
    ) %>%
    filter(!is.na(valeur)) %>%
    bind_rows(fbp$esr.pnl) %>%
    mutate(kpi = factor(kpi))
  
  return(fbp)
}
  



fbp_get_data <- function(libellé, rentrée = 2019, rentrée.min = 2000, dir=".") {

  fbp <- list()
  fbp$Libellé <- libellé

  fbp$etab <- kpiESR::esr.etab %>% filter(Etablissement==libellé)
  if(nrow(fbp$etab)==0) stop("UAI non trouvé.")
  
  fbp$groupe <- as.character(fbp$etab$Groupe)
  fbp$UAI <- fbp$etab$UAI
    
  fbp$extradata <- read.csv2("extradata/extradata.csv") %>%
    filter(Libellé == libellé)
  
  
  fbp$plots <- kpiESR::kpiesr_plot_all(rentrée, fbp$UAI, fbp$groupe) 
  
  fbp$esr <- kpiESR::esr %>% filter(UAI == fbp$UAI) 
  fbp$esr.pnl <- kpiESR::esr.pnl %>% filter(UAI == fbp$UAI) %>%
    filter(as.character(Rentrée) >= rentrée.min) 
  
  fbp <- merge_and_add_kpis(fbp)
  
  fbp$esr.pnl <- fbp$esr.pnl %>% 
    filter(!is.na(valeur)) %>%
    group_by(kpi) %>%
    filter(valeur != 0) %>%
    mutate( Evolution = valeur / first(valeur) - 1 )
  
  fbp$limits.evolution <- range(
    filter(fbp$esr.pnl,
           kpi %in% c("kpi.K.dotPres",
                      "kpi.K.titPetu",
                      "kpi.K.titPens",
                      #"kpi.K.recPect",
                      #"kpi.K.forPetu",
                      "kpi.H.hcvPhee",
                      "kpi.H.hctPtit",
                      "kpi.H.heePetu"))$Evolution,
      na.rm = TRUE, finite=1)*1.1
  
  return(fbp)
}
# 
# fbp1 <- fbp_get_data("Université de test")
# fbp2 <- fbp_get_data("Université de Lorraine")
# fbp3 <- fbp_get_data("Université de Strasbourg")
# fbp <- fbp_get_data("Université de Tours", 2012)
# 

