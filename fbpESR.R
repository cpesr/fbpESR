

percent_format <- function(x) {
  sprintf("%+0.1f%%", round(x*100,1))
}

rentrée.ref = 2012
rentrée.last = 2018

esr.stats <- esr.pnl %>%
  filter(Rentrée == rentrée.last, Type == "Université") %>%
  group_by(kpi) %>%
  summarise(moy = mean(value,na.rm=T),med = median(value,na.rm=T))

etab.pnl <- filter(esr.pnl, UAI == uai, Rentrée >= rentrée.ref)
etab <- filter(esr, UAI == uai, Rentrée >= rentrée.ref)

etab.libellé <- paste0("l'",as.character(etab.pnl[1,"Libellé"]))

netab <- esr.pnl %>%
  group_by(Rentrée) %>%
  summarise(n = max(rang, na.rm = T))
netab <- max(netab$n)


extra.pnl <- 
  read.csv(paste0("extradata/",uai,".csv")) %>%
    merge(etab) %>%
    transmute (
      Rentrée = Rentrée,
      UAI = UAI,
      kpi.K.heePetu = kpi.HEC.P.heures / (kpi.ETU.S.cycle.1.L + kpi.ETU.S.cycle.2.M),
      kpi.K.hctPtit = kpi.HEC.S.heures_comp_tit / kpi.ENS.S.titulaires,
      kpi.K.hcvPhee = (kpi.HEC.S.heures_comp - kpi.HEC.S.heures_comp_tit) / kpi.HEC.P.heures
    ) %>%
  pivot_longer(-c(Rentrée,UAI), names_to="kpi") %>%
    mutate(
      value_label = case_when(
        kpi == "kpi.K.hcvPhee" ~ scales::percent(value,0.1),
        TRUE ~ as.character(round(value,1.0)),
      )
    )


evol.graph <- function(thekpi, data = etab.pnl) {
  data %>%
    filter(kpi == thekpi) %>%
    mutate(
      Evolution = value / first(value) - 1
    ) %>%
    ggplot(aes(x=Rentrée,y=Evolution, label=value_label, group=1)) + 
    #geom_vline(xintercept=c("2012","2016")) +
    geom_line(size=1) + 
    geom_point(aes(fill=Evolution), shape = 21, colour = "black", size = 13, stroke = 1) + 
    geom_text(color="black", fontface="bold") +
    scale_y_continuous(labels = percent_format, limits=c(-0.2,0.35)) +
    scale_fill_distiller(palette = "RdBu", limits=c(-0.35,0.35)) +
    theme_hc() + guides(fill = FALSE)
}


txt_val <- function(thekpi, therentrée, data = etab.pnl) {
  filter(data,kpi == thekpi, Rentrée == therentrée)$value_label
}

txt_rang <- function(thekpi, therentrée) {
  rang <- filter(etab.pnl,kpi == thekpi, Rentrée == therentrée)$rang
  paste0(rang,"/",netab)
}

txt_evol <- function(thekpi, therentrée1, therentrée2, data = etab.pnl) {
  filter(data, kpi == thekpi, Rentrée %in% c(therentrée1,therentrée2)) %>%
    summarise(evol = percent_format(last(value) / first(value) - 1))
}

txt_obs <- function(etab.libellé, thekpiname, thekpi, therentrée1, therentrée2, data = etab.pnl, obs = TRUE) {
  paste0(
    "Le ", thekpiname, " de ", etab.libellé, " est passé de ",
    txt_val(thekpi, rentrée.ref, data), " en ", therentrée1, 
    ifelse(obs, paste0(" (classement : ", txt_rang(thekpi, therentrée1),")"),""),
    " à ", txt_val(thekpi, therentrée2, data), " en ", therentrée2, 
    ifelse(obs, paste0(" (classement : ", txt_rang(thekpi, therentrée2), ")"),""), 
    ". Son évolution sur cette période est de ", txt_evol(thekpi, therentrée1, therentrée2, data),
    ".")
}
