
rmdfiles <- c(
  "angers",
  "caen",
  "lorraine",
  "nantes",
  "nice",
  "orleans",
  "paris13",
  "tours",
  "ub",
  "uha",
  "unistra",
  "upem",
  "uppa",
  "urca",
  "ut2j",
  "paris1",
  "paris3",
  "rouen",
  "upec"
)
 

for(rmdfile in rmdfiles) {
  print(rmdfile)
  rmarkdown::render(paste0("fiches/fbp_",rmdfile,".Rmd"))
}
