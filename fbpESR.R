
source("fbpESR-extradata.R", chdir=TRUE)

missingdataplot <- ggplot(data=NULL,aes(x=0,y=0,label="Données manquantes")) + 
  geom_text(size=10) + xlim(-2,2) + ylim(-2,2) + theme_void()

esr.stats <- kpiESR::esr.pnl %>% na.omit() %>%
  filter(Type == "Université") %>%
  group_by(kpi, Rentrée) %>%
  summarise(moy = mean(valeur,na.rm=T),med = median(valeur,na.rm=T)) %>%
  arrange(Rentrée)

netab <- kpiESR::esr.pnl %>%
  group_by(Rentrée) %>%
  summarise(n = max(rang, na.rm = T))
netab <- max(netab$n)
  

plot_evolution <- function(fbp, thekpi) {
  df <- fbp$esr.pnl %>% filter(kpi == thekpi)
  if(nrow(df) == 0) stop("Données manquantes")
  
  fbp$limits.evolution[1] <- min(c(0.01,df$Evolution, fbp$limits.evolution[1]))
  fbp$limits.evolution[2] <- max(c(0.01,df$Evolution, fbp$limits.evolution[2]))*1.1
  mle = max(abs(fbp$limits.evolution))
  
  ggplot(df, aes(x=Rentrée,y=Evolution, label=valeur_label, group=1)) + 
    #geom_vline(xintercept=c("2012","2016")) +
    geom_hline(yintercept = 0, color="grey") +
    geom_line(size=1) + 
    geom_point(aes(fill=Evolution), shape = 21, colour = "black", size = 17, stroke = 1) + 
    geom_text(color="black", fontface="bold") +
    scale_y_continuous(labels = percent_format, limits=fbp$limits.evolution) +
    #scale_y_continuous(labels = percent_format) +
    scale_fill_distiller(palette = "RdBu", limits=c(-mle,mle)) +
    coord_cartesian(clip="off") +
    theme_hc() + guides(fill = FALSE)
}


plot_evolutions <- function(fbp, lfc) {
  df <- fbp$esr.pnl %>% filter(kpi %in% lfc$factors)
  if(nrow(df) == 0) stop("Données manquantes")
  
  mle = max(abs(df$Evolution))
  
  ggplot(df, aes(x=Rentrée,y=Evolution, label=valeur_label, group=kpi, color=kpi, linetype=kpi)) + 
    #geom_vline(xintercept=c("2012","2016")) +
    geom_hline(yintercept = 0, color="grey") +
    geom_line(size=1) + 
    #geom_point(aes(fill=Evolution), shape = 21, colour = "black", size = 17, stroke = 1) + 
    geom_text_repel(data = filter(df, Rentrée == max(as.character(Rentrée))), 
              color="black", fontface="bold", hjust=0, nudge_x = 0.05, direction = "y") +
    scale_y_continuous(labels = percent_format) +
    scale_color_manual(values = lfc$colors, labels = lfc$labels, limits = lfc$factors,
                         name = "Indicateurs") +
    scale_linetype_discrete(labels = lfc$labels, limits = lfc$factors,
                       name = "Indicateurs") +
    scale_fill_distiller(palette = "RdBu", limits=c(-mle,mle)) +
    guides(fill = FALSE) +
    coord_cartesian(clip="off") 
}

 
 
# plot_evolutions(fbp,kpiesr_lfc$FIN)
# plot_evolutions(fbp,kpiesr_lfc$ETU)
# plot_evolutions(fbp,kpiesr_lfc$ENS)

plot_dotation <- function(fbp, ...) {
  fbp$esr.pnl %>%
    filter(str_detect(kpi, "kpi.D" )) %>%
    ggplot(aes(x=Rentrée,y=valeur, label=valeur_label, group=kpi, color=kpi)) + 
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
#   filter(fbp$esr.pnl, kpi == thekpi, Rentrée == therentrée)$valeur_label
# }
# 
# txt_rang <- function(fbp, thekpi, therentrée) {
#   rang <- filter(fbp$esr.pnl, kpi == thekpi, Rentrée == therentrée)$rang
#   paste0(rang,"/",netab)
# }
# 
# txt_evol <- function(fbp, thekpi, therentrée1, therentrée2) {
#   filter(fbp$esr.pnl, kpi == thekpi, Rentrée %in% c(therentrée1,therentrée2)) %>%
#     summarise(evol = percent_format(last(valeur) / first(valeur) - 1)) %>%
#     select(evol) %>%
#     as.character()
# }

txt_obs <- function(fbp, thekpiname, thekpi) {
  r1 <- filter(fbp$esr.pnl, kpi == thekpi) %>% head(1)
  r2 <- filter(fbp$esr.pnl, kpi == thekpi) %>% tail(1)
  
  if(nrow(r1) == 0 || nrow(r2) == 0) {
    return("")
  }
  
  paste0(
    "_Observation :_ ",
    thekpiname, " de ", fbp$Libellé, " est passé de ",
    r1$valeur_label, " en ", r1$Rentrée, 
    " à ", r2$valeur_label, " en ", r2$Rentrée, 
    ", soit ",percent_format((r2$valeur / r1$valeur) - 1),".",
    ifelse(!is.na(r1$rang) && !is.na(r2$rang),
           paste0(" Son rang est passé de ", 
                  r1$rang," à ", r2$rang, " sur ",netab,"."),""))
}

txt_median <- function(thekpi, unit = "%") {
  s <- filter(esr.stats, kpi == thekpi) %>% tail(1)
  val <- case_when(
    unit == "%" ~ scales::percent(s$med, 0.1),
    unit == "€" ~ paste(round(s$med,0),unit),
    TRUE ~ paste(round(s$med,2),unit))
  ifelse(nrow(s) == 0, "",
    paste0("Taux médian des universités en ", s$Rentrée, " : ", val,".")
    )
}

plot_histoire <- function(...) {
  wikidataESR::wdesr_load_and_plot(wdid, c('prédécesseur', 'séparé_de'), depth=10, 
                      size = 1,
                      node_label = "alias_date",
                      legend_position="none") 
}

plot_composition <- function(...) {
  wikidataESR::wdesr_load_and_plot(wdid, c('composante','associé'), depth=2,
                      legend_position="left")
}

plot_association <- function(...) {
  wikidataESR::wdesr_load_and_plot(wdid, c('composante_de', 'associé_de', 'membre_de'), depth=2, 
                      legend_position="none", margin_y = 0.1) 
}


plot_profil <- function(fbp, thekpi,...) {
  plot_grid(ncol=2,  
            plotlist = list(
              kpiesr_plot_primaire(rentrée.last, fbp$UAI, kpiesr_lfc[[substr(thekpi,1,3)]]),
              kpiesr_plot_norm_simple_try(rentrée.last, fbp$UAI, kpiesr_lfc[[thekpi]],
                                 style=kpiESR::kpiesr_style(
                                   point_size = 10,
                                   line_size = 1,
                                   text_size = 3,
                                   kvt_scale_text_size = 2,
                                   kvt_scale_point_size = 1,
                                   #kvt_style="square",
                                   kvt_plot.margin = ggplot2::unit(c(-0.3,-1,-0.3,-1), "cm")
                                   )
            )
            )
  )
}

plot_kpiesr_profil <- function(fbp, thekpi,...) {
  plot_grid(ncol=2,  
              fbp$plots[[paste0(thekpi,".abs")]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
              fbp$plots[[paste0(thekpi,".norm")]]  + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  )
}

plot_kpiesr <- function(fbp, thekpi) {
  fbp$plots[[thekpi]]  + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
}

error_list <- list()

make_slide <- function(plotfun, thekpi, thekpiname, definition, questions="", 
                       small=FALSE, 
                       unit = "%",
                       median = TRUE,
                       observation = FALSE) {
  
  p <- NULL
  try(p <- plotfun(fbp, thekpi), silent = TRUE)
  if(is.null(p)) {
    error_list[thekpiname] <- thekpiname
    return()
  }
  
  cat("## ",thekpiname,"\n\n")

  cat("_Définition :_ ", definition,"\n\n")

  if(median) cat(txt_median(thekpi, unit),"\n\n")

  print(p)

  if(observation) cat(txt_obs(fbp, thekpiname, thekpi),"\n\n")

  #cat("### Questions politiques afférentes\n\n", questions)

}

### Maps

make_slide_simple <- function(plot, title) {
  
  cat("## ",title,"\n\n")
  
  #cat("_Définition :_ ", definition,"\n\n")
  
  #if(median) cat(txt_median(thekpi, unit),"\n\n")
  
  print(plot)
  
  #if(observation) cat(txt_obs(fbp, thekpiname, thekpi),"\n\n")
  
  #cat("### Questions politiques afférentes\n\n", questions)
  
}

univ <- kpiESR::esr %>%
  filter(Groupe == "Universités et assimilés") %>% 
  filter(! is.na(kpi.FIN.P.ressources)) %>%
  left_join(kpiESR::esr.etab) %>%
  mutate(PerimEx = replace_na(PerimEx,"Autre") ) 

plot_map <- function(fbp, xvar, yvar,rentrée=2019,xlabel=1,ylabel=1) {
  
  the.univ <- univ %>%
    filter(Rentrée == rentrée, Etablissement == fbp$Libellé)
  
  univ %>%
    filter(Rentrée == rentrée) %>% 
    mutate(label = ifelse(Etablissement == "Université de Lorraine", NA, Etablissement)) %>%
    ggplot(aes_string(x= xvar, y=yvar,size="kpi.ETU.P.effectif", color = "PerimEx")) +
    ggrepel::geom_text_repel(aes(label=label), size = 3, color="grey") +
    geom_point() + 
    ggrepel::geom_text_repel(data = the.univ, aes(label=Etablissement), size=5, color="black", xlim=c(xlabel,xlabel), ylim=c(ylabel,ylabel)) +
    geom_point(data = the.univ, shape = 21, stroke = 1, color="black", fill="#ae2573") +
    scale_color_discrete(name="Type") +
    scale_fill_discrete(name="", guide="none") +
    scale_size(range=c(0.1,6), name="Etudiants") +
    theme_cpesr() + 
    theme(
      panel.grid.major.x = element_line(color = "#D8D8D8", size = 0.1),
      panel.grid.major.y = element_line(color = "#D8D8D8", size = 0.1)) +
    guides(colour = guide_legend(override.aes = list(size=4)))
}

