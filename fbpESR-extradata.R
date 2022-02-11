library(tidyverse)

read.extradata <- function(libellé, dir=".") {

  df <- read.csv(paste0(dir,"/extradata/extradata.csv")) %>%
    mutate(
      Libellé = paste0(na.omit(Universite,Universite.other.,
                            greta, greta.other.,
                            regrp, regrp.other.,
                            ecole, ecole.other.,
                            autre, autre.other.))
    ) %>%
    filter(Libellé == libellé)
  
  if(nrow(df) ==0) {
    warning("Etablissement non trouvé dans les données additionnelles.")
    return(list(extradata = NULL, 
                extradata.kpi = NULL, 
                extradata.liens = NULL))
  }

  extradata.kpi <- 
    bind_rows(
      df %>% select(starts_with("hec.")) %>% 
        mutate_all(
          function(x) as.numeric(gsub(" ","", x))
          #function(x) as.numeric(str_replace(x," ",""))
        ) %>%
        pivot_longer(
          cols = everything(),
          names_pattern = "hec.(20..)_(.*).",
          names_to = c("Rentrée", "kpi")
        ) %>%
        mutate(kpi = paste0("heures.",kpi)),
    
      df %>% select(starts_with("dotations.")) %>% 
        mutate_all(
          function(x) as.numeric(gsub(" ","", x))
          #function(x) as.numeric(str_replace(x," ",""))
        ) %>%
        pivot_longer(
          cols = everything(),
          names_pattern = "dotations.(20..)_(.*).",
          names_to = c("Rentrée", "kpi"),
          names_prefix = c("","dotations.")
        ) %>%
        mutate(kpi = paste0("dotations.",kpi))
    ) %>%
    pivot_wider(
      names_from = kpi,
      values_from = value)
  
  
  return(list(extradata = df, 
              extradata.kpi = extradata.kpi))
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
  
  fbp$esr <- merge(all = TRUE,
    fbp$esr,
    fbp$extradata.kpi) 
  
  if(fbp$extradata$hecunite == "Heures") {
    hec.labels <- function(valeur) paste(round(valeur,1.0),"h")
  } else {
    hec.labels <- function(valeur) paste(format(round(valeur/1000,2), big.mark=" "),"k€")
  }
  
  fbp$esr.pnl <- fbp$esr %>%
    transmute (
      Rentrée = Rentrée,
      UAI = UAI,
      kpi.H.heePetu = heures.HeE / (kpi.ETU.S.cycle.1.L + kpi.ETU.S.cycle.2.M),
      kpi.H.hctPtit = heures.HeCT / kpi.ENS.S.titulaires,
      kpi.H.hcvPhee = 1 - heures.HeCT / heures.HeC,
      kpi.D.ens = dotations.doten,
      kpi.D.aapens = dotations.aapen,
      kpi.D.rech = dotations.dotre,
      kpi.D.aaprech = dotations.aapre,
      kpi.D.vieetu = dotations.vieet
    ) %>%
    pivot_longer(-c(Rentrée,UAI), names_to="kpi", values_to="valeur") %>%
    mutate(
      valeur_label = case_when(
        kpi == "kpi.H.hcvPhee" ~ scales::percent(valeur,0.1),
        startsWith(kpi, "kpi.D") ~ euro_k(valeur),
        TRUE ~ hec.labels(valeur),
      )
    ) %>%
    filter(!is.na(valeur)) %>%
    bind_rows(fbp$esr.pnl) %>%
    mutate(kpi = factor(kpi))
  
  return(fbp)
}
  



fbp_get_data <- function(libellé, rentrée = 2019, rentrée.min = 2000, dir=".") {

  fbp <- read.extradata(libellé, dir)
  fbp$Libellé <- libellé
  
  fbp$etab <- kpiESR::esr.etab %>% filter(Etablissement==libellé)
  if(nrow(fbp$etab)==0) stop("UAI non trouvé.")
  
  fbp$groupe <- as.character(fbp$etab$Groupe)
  fbp$UAI <- fbp$etab$UAI
  
  fbp$plots <- kpiESR::kpiesr_plot_all(rentrée, fbp$UAI, fbp$groupe) 
  
  fbp$esr <- kpiESR::esr %>% filter(UAI == fbp$UAI) 
  fbp$esr.pnl <- kpiESR::esr.pnl %>% filter(UAI == fbp$UAI) %>%
    filter(as.character(Rentrée) >= rentrée.min) 
  
  if(!is.null(fbp$extradata.kpi)) {
    fbp <- merge_and_add_kpis(fbp)
  }
  
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
# fbp1 <- fbp_get_data("Université", "Université de test")
# fbp2 <- fbp_get_data("Université", "Université de Lorraine")
# fbp3 <- fbp_get_data("Université", "Université de Strasbourg")
# fbp <- fbp_get_data("Université", "Université de Tours", 2012)
# 

