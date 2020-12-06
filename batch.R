
rmdfiles <- c(
  "angers",
  "caen",
  "lorraine",
  "nantes",
  "nice",
  "orleans",
  "paris13",
  "tours",
  "uha",
  "unistra",
  "upem",
  "uppa",
  "urca",
  "ut2j",
  "paris1",
  "rouen"
)
  
for(rmdfile in rmdfiles) {
  rmarkdown::render(paste0("fiches/fbp_",rmdfile,".Rmd"))
}
