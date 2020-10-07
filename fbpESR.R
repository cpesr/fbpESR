
source("fbpESR-extradata.R")

missingdataplot <- ggplot(data=NULL,aes(x=0,y=0,label="Données manquantes")) + 
  geom_text() + xlim(-5,5) + ylim(-5,5) + theme_void()

esr.stats <- kpiESR::esr.pnl %>% na.omit() %>%
  filter(Type == "Université") %>%
  group_by(kpi, Rentrée) %>%
  summarise(moy = mean(value,na.rm=T),med = median(value,na.rm=T))

netab <- esr.pnl %>%
  group_by(Rentrée) %>%
  summarise(n = max(rang, na.rm = T))
netab <- max(netab$n)



evol.graph <- function(fbp, thekpi) {
  fbp$etab.pnl %>%
    filter(kpi == thekpi) %>%
    ggplot(aes(x=Rentrée,y=Evolution, label=value_label, group=1)) + 
    #geom_vline(xintercept=c("2012","2016")) +
    geom_line(size=1) + 
    geom_point(aes(fill=Evolution), shape = 21, colour = "black", size = 17, stroke = 1) + 
    geom_text(color="black", fontface="bold") +
    scale_y_continuous(labels = percent_format, limits=fbp$limits.evolution) +
    scale_fill_distiller(palette = "RdBu", limits=fbp$limits.evolution) +
    theme_hc() + guides(fill = FALSE)
}

small_style <- kpiesr_style(
  point_size = 12,
  line_size = 2,
  text_size = 3,
  primaire_plot.margin = ggplot2::unit(c(0.25,0,0,0), "cm"),
  bp_width = 1,
  bp_text_x = -0.21 )

dotation.graph <- function(fbp) {
  fbp$etab.pnl %>%
    filter(str_detect(kpi, "kpi.D" )) %>%
    ggplot(aes(x=Rentrée,y=value, label=value_label, group=kpi, color=kpi)) + 
    #geom_vline(xintercept=c("2012","2016")) +
    geom_line(size=1) + 
    #geom_point(aes(fill=Evolution), shape = 21, colour = "black", size = 17, stroke = 1) + 
    #geom_text(color="black", fontface="bold") +
    scale_color_brewer(
      limits = c("kpi.D.vieetu","kpi.D.ens","kpi.D.aapens", "kpi.D.rech","kpi.D.aaprech"),
      labels = c("Vie étudiante","Enseignement","Enseignement (AAPI)", "Recherche", "Recherche (AAPI)"),
      name = "Dotations N1-N2",
      palette = "Paired", direction = -1
    ) +
    scale_y_continuous(labels = euro_k, name = "Dotation") +
    theme_hc() + guides(fill = FALSE) +
    theme(legend.position="right")
}

#dotation.graph(fbp)

txt_val <- function(fbp, thekpi, therentrée) {
  filter(fbp$etab.pnl, kpi == thekpi, Rentrée == therentrée)$value_label
}

txt_rang <- function(fbp, thekpi, therentrée) {
  rang <- filter(fbp$etab.pnl, kpi == thekpi, Rentrée == therentrée)$rang
  paste0(rang,"/",netab)
}

txt_evol <- function(fbp, thekpi, therentrée1, therentrée2) {
  filter(fbp$etab.pnl, kpi == thekpi, Rentrée %in% c(therentrée1,therentrée2)) %>%
    summarise(evol = percent_format(last(value) / first(value) - 1)) %>%
    select(evol) %>%
    as.character()
}

txt_obs <- function(fbp, thekpiname, thekpi, therentrée1, therentrée2, obs = TRUE) {
  paste0(
    "Le ", thekpiname, " de ", fbp$Libellé, " est passé de ",
    txt_val(fbp, thekpi, rentrée.ref), " en ", therentrée1, 
    ifelse(obs, paste0(" (classement : ", txt_rang(fbp, thekpi, therentrée1),")"),""),
    " à ", txt_val(fbp, thekpi, therentrée2), " en ", therentrée2, 
    ifelse(obs, paste0(" (classement : ", txt_rang(fbp, thekpi, therentrée2), ")"),""), 
    ". Son évolution sur cette période est de ", txt_evol(fbp, thekpi, therentrée1, therentrée2),
    ".")
}



