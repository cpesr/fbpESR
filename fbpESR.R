
source("fbpESR-extradata.R")

missingdataplot <- ggplot(data=NULL,aes(x=0,y=0,label="Données manquantes")) + 
  geom_text(size=10) + xlim(-2,2) + ylim(-2,2) + theme_void()

esr.stats <- kpiESR::esr.pnl %>% na.omit() %>%
  filter(Type == "Université") %>%
  group_by(kpi, Rentrée) %>%
  summarise(moy = mean(value,na.rm=T),med = median(value,na.rm=T)) %>%
  arrange(Rentrée)

netab <- kpiESR::esr.pnl %>%
  group_by(Rentrée) %>%
  summarise(n = max(rang, na.rm = T))
netab <- max(netab$n)


plot_evolution <- function(fbp, thekpi) {
  df <- fbp$etab.pnl %>% filter(kpi == thekpi)
  if(nrow(df) == 0) stop("Données manquantes")
  
  ggplot(df, aes(x=Rentrée,y=Evolution, label=value_label, group=1)) + 
    #geom_vline(xintercept=c("2012","2016")) +
    geom_line(size=1) + 
    geom_point(aes(fill=Evolution), shape = 21, colour = "black", size = 17, stroke = 1) + 
    geom_text(color="black", fontface="bold") +
    scale_y_continuous(labels = percent_format, limits=fbp$limits.evolution) +
    scale_fill_distiller(palette = "RdBu", limits=fbp$limits.evolution) +
    theme_hc() + guides(fill = FALSE)
}


small_style <- kpiESR::kpiesr_style(
  point_size = 12,
  line_size = 2,
  text_size = 3,
  primaire_plot.margin = ggplot2::unit(c(0.25,0,0,0), "cm"),
  bp_width = 1,
  bp_text_x = -0.21 )

plot_dotation <- function(fbp, ...) {
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

# txt_val <- function(fbp, thekpi, therentrée) {
#   filter(fbp$etab.pnl, kpi == thekpi, Rentrée == therentrée)$value_label
# }
# 
# txt_rang <- function(fbp, thekpi, therentrée) {
#   rang <- filter(fbp$etab.pnl, kpi == thekpi, Rentrée == therentrée)$rang
#   paste0(rang,"/",netab)
# }
# 
# txt_evol <- function(fbp, thekpi, therentrée1, therentrée2) {
#   filter(fbp$etab.pnl, kpi == thekpi, Rentrée %in% c(therentrée1,therentrée2)) %>%
#     summarise(evol = percent_format(last(value) / first(value) - 1)) %>%
#     select(evol) %>%
#     as.character()
# }

txt_obs <- function(fbp, thekpiname, thekpi) {
  r1 <- filter(fbp$etab.pnl, kpi == thekpi) %>% head(1)
  r2 <- filter(fbp$etab.pnl, kpi == thekpi) %>% tail(1)
  
  if(nrow(r1) == 0 || nrow(r2) == 0) {
    return("")
  }
  
  paste0(
    "_Observation :_ ",
    thekpiname, " de ", fbp$Libellé, " est passé de ",
    r1$value_label, " en ", r1$Rentrée, 
    " à ", r2$value_label, " en ", r2$Rentrée, 
    ifelse(!is.na(r1$rang) && !is.na(r2$rang),
           paste0(". Son classement est passé de ", 
                  r1$rang," à ", r2$rang, " sur ",netab),""),
    ". L'évolution est de ", percent_format((r2$value / r1$value) - 1),
    ".")
}

txt_median <- function(thekpi, stat_percent = TRUE) {
  s <- filter(esr.stats, kpi == thekpi) %>% tail(1)
  val <- ifelse(stat_percent, scales::percent(s$med, 0.1), round(s$med,2))
  ifelse(nrow(s) == 0, "",
    paste0("Taux médian des universités en ", s$Rentrée, " : ", val,".")
    )
}

plot_histoire <- function(...) {
  wikidataESR::wdesr_load_and_plot(wdid, c('prédécesseur', 'séparé_de'), depth=10, 
                      node_label = "alias_date",
                      legend_position="none",
                      node_sizes = 40, arrow_gap = 0.0, margin_y = 0.15) 
}

plot_composition <- function(...) {
  wikidataESR::wdesr_load_and_plot(wdid, c('composante','associé'), depth=2,
                      legend_position="left")
}

plot_association <- function(...) {
  wikidataESR::wdesr_load_and_plot(wdid, c('composante_de', 'associé_de', 'membre_de'), depth=2, 
                      legend_position="none", margin_y = 0.1) 
}

plot_profil <- function(fbp, pl,...) {
  plot_grid(ncol=2,  plotlist=pl) 
}

error_list <- list()

make_slide <- function(plotfun, thekpi, thekpiname, definition, questions, 
                       small=FALSE, 
                       stat_percent = TRUE,
                       observation = FALSE) {
  
  p <- NULL
  try(p <- plotfun(fbp, thekpi), silent = TRUE)
  if(is.null(p)) {
    error_list[thekpiname] <- thekpiname
    return()
  }
  
  cat("## ",thekpiname,"\n\n")

  cat("_Définition :_ ", definition,"\n\n")

  cat(txt_median(thekpi, stat_percent),"\n\n")

  print(p)

  if(observation) cat(txt_obs(fbp, thekpiname, thekpi),"\n\n")

  cat("### Questions politiques afférentes\n\n", questions)

}

