library(tidyverse)


read.extradata <- function() {
  ed1 <- read.csv("results-survey881846-v1.csv") %>%
    rename(
      documents.lien1. = bilans.bil1.,
      documents.lien2. = bilans.bil2.
    ) %>%
    mutate(
      documents.nom1. = "Enquête auprès des personnels",
      documents.nom2. = "Bilan de l'Université de Strasbourg"
    )
  
  ed2 <- read.csv("results-survey881846-v2.csv")
  
  ed <- merge(ed1,ed2,all=TRUE)
  return(ed)
}

process_extradata <- function(extradata, libellé) {

  df <- extradata %>%
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
  
  
  extradata.liens <-
    merge(
      df %>% 
        select(starts_with("hecliens.")) %>% 
        pivot_longer(
          cols = everything(),
          names_pattern = "hecliens.(20..).",
          names_to = c("Rentrée"),
          values_to = "heures.liens"
          ),
      
      df %>% select(starts_with("dotliens.")) %>% 
        pivot_longer(
          cols = everything(),
          names_pattern = "dotliens.(20..).",
          names_to = c("Rentrée"),
          values_to = "dotations.liens"
        )
    )
  
  
  return(list(extradata = df, 
              extradata.kpi = extradata.kpi, 
              extradata.liens = extradata.liens))
}

percent_format <- function(x) {
  sprintf("%+0.1f%%", round(x*100,1))
}

euro_k <- function(x) {
  return(paste0(format(round(x/1000,0), big.mark=" ")," k€"))
}

merge_and_add_kpis <- function(fbp) {
  
  fbp$etab <- merge(all = TRUE,
    fbp$etab,
    fbp$extradata.kpi) 
  
  fbp$etab.pnl <- fbp$etab %>%
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
    pivot_longer(-c(Rentrée,UAI), names_to="kpi") %>%
    mutate(
      value_label = case_when(
        kpi == "kpi.H.hcvPhee" ~ scales::percent(value,0.1),
        startsWith(kpi, "kpi.D") ~ euro_k(value),
        TRUE ~ paste(round(value,1.0),"h"),
      )
    ) %>%
    filter(!is.na(value)) %>%
    bind_rows(fbp$etab.pnl) %>%
    mutate(kpi = factor(kpi))
  
  return(fbp)
}
  



fbp_get_data <- function(type, libellé, rentrée.min = 2000) {

  extradata <- read.extradata()
  
  fbp <- process_extradata(extradata, libellé)
  
  fbp$Libellé <- libellé
  fbp$UAI <- kpiESR::esr.uais[[type]][[libellé]]
  if(is.null(fbp$UAI)) error("UAI non trouvé.")
  
  fbp$etab <- kpiESR::esr %>% filter(UAI == fbp$UAI) 
  fbp$etab.pnl <- kpiESR::esr.pnl %>% filter(UAI == fbp$UAI) %>%
    filter(as.character(Rentrée) >= rentrée.min) 
  
  if(!is.null(fbp$extradata.kpi)) {
    fbp <- merge_and_add_kpis(fbp)
  }
  
  fbp$etab.pnl <- fbp$etab.pnl%>% 
    filter(!is.na(value)) %>%
    group_by(kpi) %>%
    mutate( Evolution = value / first(value) - 1 )
  
  fbp$limits.evolution <- range(
    filter(fbp$etab.pnl,
           kpi %in% c("kpi.K.proPres","kpi.K.titPetu",
                      "kpi.K.titPens","kpi.K.titPetu",
                      "kpi.H.hcvPhee","kpi.H.hctPtit",
                      "kpi.H.heePetu"))$Evolution,
      na.rm = TRUE, finite=1)*1.1
  
  return(fbp)
}
# 
# fbp1 <- fbp_get_data("Université", "Université de test")
# fbp2 <- fbp_get_data("Université", "Université de Lorraine")
# fbp3 <- fbp_get_data("Université", "Université de Strasbourg", 2012)
# 

