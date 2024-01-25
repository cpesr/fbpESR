load(file="files/sise.RData")

plot_l3vsM1 <- function(libellé) {
bind_rows(
  sise %>%
    group_by(Rentrée,Niveau = paste(LMD, substr(NIVEAU,2,2), sep="")) %>%
    summarise(Etudiants = sum(Etudiants)) %>%
    mutate(Etablissement = "ESR"),
  sise %>%
    filter(Etablissement == libellé) %>%
    group_by(Rentrée,Niveau = paste(LMD, substr(NIVEAU,2,2), sep="")) %>%
    summarise(Etudiants = sum(Etudiants)) %>%
    mutate(Etablissement = libellé) ) %>%
  
  filter(Niveau %in% c("L3","M1")) %>%
  mutate(Rentrée = ifelse(Niveau == "L3", Rentrée+1,Rentrée)) %>%
  filter(Rentrée != 2012, Rentrée != 2023) %>%
  ggplot(aes(x=Rentrée, y= Etudiants, color=Niveau)) +
  geom_line(linewidth = 1.5) + geom_point(shape = 21, stroke = 2, size = 2, fill="white") + 
  scale_x_continuous(labels = ~ paste0("L3 ", .x-1,"\nM1 ",.x), breaks = seq(2011,2030,2), name="Cohorte") +
  scale_y_continuous(labels = ~ paste0(.x/1e3,"k"), name = "Inscriptions étudiantes") +
  facet_wrap(.~Etablissement, scales = "free_y") +
  ggtitle("Etudiant⋅e⋅s  inscrit⋅e⋅s en L3 à l'année N et M1 à l'année N+1" )
}


plot_SCSPvsMS <- function(libellé) {
  kpiESR::esr %>%
    filter(Etablissement %in% c(type,libellé), Rentrée > 2008) %>%
    mutate(Etablissement = factor(Etablissement, levels=c(type,libellé))) %>%
    mutate(diff = kpi.FIN.S.SCSP-kpi.FIN.S.masseSalariale) %>% 
    ggplot(aes(x=Rentrée,y=diff, color=diff)) +
    geom_hline(yintercept = 1) +
    geom_line(size=1) + geom_point(shape = 21, stroke=2,size=2,fill="white") +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_y_continuous(labels = ~ paste(.x/1e6,"M€"), name = "SCSP-MS") +
    scale_color_distiller(palette="Reds") +
    facet_wrap(.~Etablissement, scales = "free_y") +
    ggtitle("Différence entre SCSP et masse salariale des universités") +
    theme(legend.position = "None", panel.grid.minor.x = element_blank())
}



plot_ip <- function(pid) {

  ipm <- read.csv2("files/fr-esr-insertion_professionnelle-master.csv", dec='.', na.strings = c("","nd","ns","fe",".")) %>%
    mutate(Établissement = recode(Établissement, "Toutes universités et établissements assimilés" = "Toutes universités")) %>%
    mutate(Discipline = recode(Discipline,"Lettres, langues, arts" = "Ensemble Lettres, langues, arts")) %>%
    filter(Diplôme == "MASTER LMD", situation == "30 mois après le diplôme", Annee == 2020) %>% 
    filter(startsWith(Discipline,"Ensemble")) %>%
    mutate(Code.du.domaine = factor(Code.du.domaine, levels=c("DEG","STS","SHS","LLA"))) %>%
    mutate(diff=Taux.d.emploi.salarié.en.France-Taux.d.insertion) %>%
    pivot_longer(c(Taux.d.emploi.salarié.en.France,Taux.d.emploi,Taux.d.insertion), names_to ="Indicateur", values_to="Valeur") %>%
    mutate(Indicateur = factor(Indicateur,
                               levels=c("Taux.d.insertion","Taux.d.emploi","Taux.d.emploi.salarié.en.France"),
                               labels = c("Taux d'insertion","Taux d'emploi","Taux d'emploi salarié en France")))
  
  ipm %>% 
    filter(Id_Paysage == pid) %>%
    ggplot(aes(x=Code.du.domaine,y=Valeur,fill=Code.du.domaine)) +
    geom_col(alpha=0.8) +
    geom_point(data = ipm %>% filter(Établissement == "Toutes universités"), color="black") +
    scale_x_discrete(name="") +
    scale_y_continuous(labels = ~ scales::percent(.x/100), name="Taux") +
    facet_grid(.~Indicateur) +
    expand_limits(y=100) +
    theme(legend.position = "None") +
    ggtitle("Comparaison des trois taux des diplômés de Master","Diplômés de la session 2020, à 30 mois, hors Master d'enseignement")

}