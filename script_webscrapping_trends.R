
# recuperation des donnees de popularite upr (google trends)
#########################################

# note methodologique:
# google trends publie un indice de popularite de recherche *relatif*, c'est a dire
# sous la forme d'un nombre entre 1 et 100 quelle que soit la recherche. On ne peut donc 
# pas mettre bout a bout des recherches separees (ex: jours d'un mois) sans les reponderer par 
# les poids mensuels. 

library(gtrendsR)

# 1. Recuperer manuellement les poids mensuels sur tout l'historique de 10 ans
browseURL(url="https://www.google.fr/trends/explore?date=2007-03-01%202017-02-01&geo=FR&q=asselineau")
browseURL(url="https://www.google.fr/trends/explore?date=2007-03-01%202017-02-01&geo=FR&q=upr")
p.upr <- read.csv2("poids_mensuels_upr.csv", skip=2, sep=",")
p.ass <- read.csv2("poids_mensuels_asselineau.csv", skip=2, sep=",")
p <- data.frame('date'=p.upr$Mois, 'score.asselineau'=p.ass$asselineau...France., 'score.upr'=p.upr$upr...France.) %>%
  mutate('date'=date %>% as.character %>% paste0("-01") %>% as.Date("%Y-%m-%d"))

# INITIALISATION
session = gconnect("marc.agenis@gmail.com", "") # mettre ici mon mot de passe
# boucle de download mensuel/journalier
my.seq = seq.Date(from=as.Date("2007-03-01"), to=as.Date("2017-02-01"), by="month")
i=1
res <- gtrends("upr", geo=c("FR"), start_date=my.seq[i], end_date=my.seq[i+1]-1)$trend %>%
  mutate(hits=hits*p[i, "score.upr"])

# BOUCLE (parfois ca bugue car on atteint limite de telechargement
#         il faut alors reaffecter i-1 a i, puis relancer la boucle for)
for (k in 1:length(my.seq)){
i=i+1
( temp <- gtrends("upr", geo=c("FR"), start_date=my.seq[i], end_date=my.seq[i+1]-1)$trend %>%
  mutate(hits=hits*p[i, "score.upr"]) )
print(last(temp$start))
res <- rbind(res, temp)
#Sys.sleep(rpois(1, 60)) # si besoin de contourner le quota de telechargement
}

# TRAITEMENT FINAL
res %<>% mutate(date=as.Date(start, tz="CET")) %>% 
  dplyr::select(date, score.upr=hits) %>%
  arrange(date) %>%
  filter(!duplicated(date)) %>% 
  tail(-24)
res.upr=res
# save(res.asselineau, file="asselineau.trends.Rdata")
# repeter de meme en intervertissant asselineau et upr
# save(res.upr, file="upr.trends.Rdata")


# # en manuel: sur l'api google
# faire export .csv puis copier la colonne A entierement
# i=... # renseigner le bon indice
# (temp=read.table(file = "clipboard", sep = ",", header=TRUE, skip=2))
# temp2=data.frame(start=temp$Jour, keyword="upr", "hits"=temp$upr...France., location="FR") %>%
#   mutate(hits=hits*p[i, "score.upr"])
# res <- rbind(res, temp2)
