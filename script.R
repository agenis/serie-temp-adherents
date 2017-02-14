
#################################################################
# Series temporelles : dynamique d’adhesions a un parti politique
#################################################################

# derniere MaJ: 12/02/2017
# contact: marc.agenis@gmail.com

# Chargement scripts annexes
############################

setwd("~/dossier personnel/ACTION/upr/serie temp adherents")
source('~/dossier personnel/ACTION/upr/serie temp adherents/script_fonctions.R', encoding = 'UTF-8', echo=F)
source('~/dossier personnel/ACTION/upr/serie temp adherents/script_packages.R', encoding = 'UTF-8', echo=F)
source('~/dossier personnel/ACTION/upr/serie temp adherents/script_data.R', encoding = 'UTF-8', echo=F)


# Tableau de donnees
####################

# dates manquantes aucune (deja corrige 1 seule)
which((diff(dfj$date) %>% as.numeric)!=1)
which((diff(dfh$date) %>% as.numeric)!=1)
# valeurs manquantes ou outliers
sum(is.na(dfj)) # non
sum(is.na(dfh)) # non
dfj %>% filter(nb<0) # OK pas de baisse.
dfh %>% filter(nb<0) # OK pas de baisse.
# valeurs extremes, resume statistique. 
# note: la donnee importee ici est deja corrigee pour les quelques valeurs negatives (voir script_data.R)
dfj %>% dplyr::select(cum, nb) %>% summary
dfh %>% dplyr::select(cum, nb) %>% summary

# quelques statistiques interessantes
#####################################

# plot rapide, pour journalier seul
ggplot(data = dfj, aes(date, cum)) + geom_line() + ylab("Cumul")
ggplot(data = dfj, aes(date, nb)) + geom_line() + ylab("Increment")

# proportions jours vides
table(dfj$nb==0) %>% prop.table %>% pc2
# dernier jour sans aucune adhesion
dfj %>% filter(nb==0) %>% tail %>% arrange(desc(date)) # ca remonte a avril 2016, deux dates, et avant en juin 2015!
# representation des jours sans adherents
(dfj$nb==0) %>% t %>% data.frame %>% Show(xaxt='n')
axis(1, at=seq(0.075, 0.925, length.out = 9), label=2008:2016)
# repartition des jours sans adherents: les 5 premiere annees rassemblent 95% des journees sans adherents 
dfj %>% group_by(.annee) %>% mutate(aucun=nb==0) %>% 
  summarise(prop=sum(aucun)/NROW(dfj)) %>% 
  mutate(prop=pc2(prop/sum(prop))) %>% 
  mutate(cumprop=cumsum(prop)) 

# pics d'adhesions (quantiles de distribution)
quantile(dfj$nb, probs=c(.95, .99, .999, .9999), type=1)
# graphique du nombre d'adhesions par jour en heatmap
dfj %>% dplyr::select(nb) %>% sqrt %>% t %>% data.frame %>% Show(xaxt='n', yaxt='n')
axis(1, at=seq(0.075, 0.925, length.out = 9), label=2008:2016)# repartition des jours sans adhesion
# analyse de type Gini. A quelle date a-t-on atteint 10%, 50% ou 90% du nombre total d'adherents?
( out <- Vectorize(QuantileFunc, SIMPLIFY=FALSE)(c(0.1, 0.5, 0.87)) )
# => sur les derniers 22 mois, l'UPR a engrange autant d'adhesions que sur toute les annees auparavant
difftime(out[[2]]$date[1], tail(dfj$date, 1), units="days") %>% as.numeric %>% '/'(30.5) # nombre de mois
# => l'UPR a mis 5 ans pour accumuler 10% de ses premiers adherents
difftime(out[[1]]$date[1], head(dfj$date, 1), units="days") %>% as.numeric %>% '/'(365) # nombre de mois
# => 10% des adherents UPR ont adhere sur le dernier mois et demi => des gens nouveaux!
difftime(out[[3]]$date[1], tail(dfj$date, 1), units="days") %>% as.numeric # nombre de mois
# adherions quotidiennes moyennes
mean(dfj$nb)

# dynamique de la serie temporelle
##################################

# ACF (auto correlation)
dfj$nb %>% tail(365*5) %>% diff %>% ggACF # on exclut les premieres annees quasi-constantes
dfh$nb %>% tail(365*5) %>% ggACF

# effet du week end? non, meme pas en interaction
dfh %>% lm(data=., nb~jour.t0*.is_we) %>% anova
# graphique horaire en hexagones, puis en lisse 
dfh %>% filter(nb<10, nb>=0) %>% ggplot(data=.) + 
  aes(x=.heure, y=nb) + geom_hex() + 
  geom_smooth(col="orange", size=2, aes(group=1), method="loess") +
  xlab("heure de la journee") + ylab("adhesions") + scale_fill_gradient("nombre de cas (%)")
dfh %>% group_by(.is_we, .heure) %>% summarise('mean'=mean(nb), 'sd'=sd(nb)) %>% 
  ggplot(.) + aes(x=.heure, y=mean) + geom_line() + geom_smooth(aes(group=1))
# analogie evolution des acces tweeter
browseURL(url="http://www.1ere-position.fr/blog/meilleures-heures-pour-publier-facebook-twitter-emailing-blog")

# loi de distribution des donnees horaires
# on separe le jeu de donnees en deux selon l'heure d ejournee car valeurs tres differentes sinon. 
dfh1 <- dfh %>% filter(.heure_num<=10)
dfh2 <- dfh %>% filter(.heure_num>10)
# Hypothese de la loi de probabilite horaire - POISSON (heures creuses)
set.seed(1)
(distr.poiss1 <- MASS::fitdistr(dfh1$nb, densfun="Poisson"))
table(dfh1$nb); table(rpois(NROW(dfh1), distr.poiss1$estimate[1]))
# hypothese de la loi de probabilite horaire - POISSON (heures pleines)
(distr.poiss2 <- MASS::fitdistr(dfh2$nb, densfun="Poisson"))
table(dfh2$nb); table(rpois(NROW(dfh2), distr.poiss2$estimate[1]))
# hypothese de la loi de probabilite horaire - EXPONENTIAL
(distr.expo1 <- MASS::fitdistr(dfh1$nb, densfun="exponential"))
table(dfh1$nb); rexp(NROW(dfh1), distr.expo1$estimate[1]) %>% cut(breaks=0:max(dfh1$nb)) %>% table
# hypothese de la loi de probabilite horaire - EXPONENTIAL
(distr.expo2 <- MASS::fitdistr(dfh2$nb, densfun="exponential"))
table(dfh2$nb); rexp(NROW(dfh2), distr.expo2$estimate[1]) %>% cut(breaks=0:max(dfh2$nb)) %>% table
# hypothese de la loi de probabilite horaire - GEOMETRIQUE
(distr.geom1 <- MASS::fitdistr(dfh1$nb, densfun="geometric"))
table(dfh1$nb); table(rgeom(NROW(dfh1), distr.geom1$estimate[1]))
# hypothese de la loi de probabilite horaire - GEOMETRIQUE
(distr.geom2 <- MASS::fitdistr(dfh2$nb, densfun="geometric"))
table(dfh1$nb); table(rgeom(NROW(dfh2), distr.geom2$estimate[1]))
# COMPARAISON des log-vraisemblances (la plus basse de chaque "numero" est conservee)
# attention on ne peut pas comparer les pleines avec les creuses
c("poisson1"=distr.poiss1$loglik, "poisson2"=distr.poiss2$loglik, 
  "exponentiel1"=distr.expo1$loglik, "exponentiel2"=distr.expo2$loglik,
  "geometrique1"=distr.geom1$loglik, "geometrique2"=distr.geom2$loglik)
# poisson est tres bien pour les heures creuses, mais pour les heures plus fournies
# une distribution exponentielle est plus adaptee

# effet de l'ete? oui tres significatif
dfj %>% mutate(ete = .mois %in% 6:8) %>% lm(data=., nb~jour.t0*ete) %>% anova
# plot serie avec marquage en bleu des mois d'ete
ggplot(data = dfj, aes(date, nb, col=ete)) + geom_line(aes(group=1)) + ylab("Cumul")

# Lien avec la popularite Google Trends
#######################################

# script webscrapping donnees google trends: executer manuellement
# "script_webscrapping_trends.R"
# correlations simples
cor(dfj$score.asselineau, dfj$score.upr)
cor(dfj$score.asselineau, dfj$nb)
cor(dfj$score.upr, dfj$nb)
# plot compare avec dates elections
ggplot(dfj) + aes(x=date) +
  geom_line(aes(y=score.upr*2), col="green", alpha=0.7) +
  geom_line(aes(y=score.asselineau*2), col="blue", alpha=0.5) +
  geom_line(aes(y=cum), col="orange", alpha=0.7, size=2) +
  ylab("evolution adherents et tendances google") +
  geom_text(aes(x=dfj$date[200]), y=15000, label="adherents totaux", col="orange", hjust="left") +
  geom_text(aes(x=dfj$date[200]), y=13000, label="recherches 'upr'", col="green", hjust="left") +
  geom_text(aes(x=dfj$date[200]), y=11000, label="recherches 'asselineau'", col="blue", hjust="left") +
  geom_text(aes(x=dfj$date[200]), y=9000, label="elections", col="red", hjust="left") +
  geom_vline(xintercept=as.numeric(elections), col="red", alpha=0.7, linetype=4)
# y a t-til un effet de retard entre consultation web et adhesions?
sapply(-120:120, function(x) cor(dfj$nb, dfj$score.asselineau %>% shift(-x), use="complete.obs")) %>% plot(x=-120:120, y=., type="l", xlab="decalage temporel (jours) entre recherche et adhesions", ylab="correlation des courbes"); abline(v=0, col="red")
sapply(-120:120, function(x) cor(dfj$nb, dfj$score.upr %>% shift(-x), use="complete.obs")) %>% plot(x=-120:120, y=., type="l", xlab="decalage temporel (jours) entre recherche et adhesions", ylab="correlation des courbes"); abline(v=0, col="red")

# Modelisation de la courbe cumulee
###################################

# calage lineaire, quadratique et expo jusquea 2012:
mod1 <- nls(cum ~ c+a * exp(jour.t0/(365*b)), data = head(dfj, 1750), start = list(a = 28, b = 1.1, c=0))
modL <- lm(data=head(dfj, 1750), cum~jour.t0)
modP <- glm(data=head(dfj, 1750), cum~poly(jour.t0, 2))
# graphique de comparaison, 5 premieres annees
dfj %>% mutate(pred.quad=predict(modP, dfj), pred.expo=predict(mod1, dfj), pred.lin=predict(modL, dfj)) %>%
  head(1800) %>%
  ggplot(.) + aes(x=date) + 
  geom_line(aes(y=cum), size=1) +
  geom_line(aes(y=pred.expo), col="red", size=2, alpha=0.35) +
  geom_line(aes(y=pred.quad), col="blue", size=2, alpha=0.35) +
  geom_line(aes(y=pred.lin), col="yellow", size=2, alpha=0.35) +
  coord_cartesian(ylim=c(0,1200)) + ylab("cumul d'adherents et modeles")
# le modele expo calcule ci-dessus ne fonctionne plus du tout des 2012:
plot.ts(dfj$cum %>% head(3000)); lines(predict(mod1, dfj), col="red")

# ensemble des 10 ans: meilleur modele explicatif
mod3 <- nls(cum ~ c+a * exp(jour.t0/(365*b)), data = head(dfj, Inf), start = list(a = 28, b = 1.1, c=0))
modP2 <- glm(data=head(dfj, Inf), cum~poly(jour.t0, 2))
modP3 <- glm(data=head(dfj, Inf), cum~poly(jour.t0, 3))
# regression par morceaux manuelle:
modL1 <- glm(data=dfj[0000:1600,], cum~jour.t0)
modL2 <- glm(data=dfj[1601:2500,], cum~jour.t0)
modL3 <- glm(data=dfj[2501:3500,], cum~jour.t0)
modL4 <- glm(data=dfj[3501:3600,], cum~jour.t0)
segmented=c(fitted(modL1), fitted(modL2), fitted(modL3), fitted(modL4))
# plot
dfj %>% mutate(pred.quad=predict(modP3, dfj), pred.expo=predict(mod3, dfj), pred.lin=segmented) %>%
  ggplot(.) + aes(x=date) + 
  geom_line(aes(y=cum), size=2.5, alpha=0.3) +
  geom_line(aes(y=pred.quad), col="blue", size=1, alpha=0.5) +
  geom_line(aes(y=pred.expo), col="red", size=1, alpha=0.5) +
  geom_line(aes(y=pred.lin), col="orange", size=1, alpha=1) +
  coord_cartesian(ylim=c(0, 15000)) + ylab("cumul d'adherents et modeles")
# caracterisation de l'erreur par modele
PredError(dfj$cum, fitted(modP2), "MAE")
PredError(dfj$cum, fitted(modP3), "MAE")
PredError(dfj$cum, fitted(mod3), "MAE")
PredError(dfj$cum, segmented, "MAE")

# Liens  divers proposes dans le rapport
browseURL(url="https://www.upr.fr/")
browseURL(url="https://en.wikipedia.org/wiki/Metcalfe%27s_law")


# Lissage et Impact des elections
#################################

# Lissage par une courbe smoothing spline 
fitsd <- smooth.spline(x=dfj$jour.t0, y=dfj$nb, spar=0.4)
# lissage adhesions basales par quantile spline regression (library fields)
fit20 = qsreg(x=dfj$jour.t0, y=dfj$nb, alpha=0.3, lam=0.25)
# Plot avec elections
dfj %>% mutate(fitsd=fitted(fitsd), quantile=fitted(fit20)) %>% ggplot(.) +
  geom_line(aes(x=date, y=nb), alpha=0.2) +
  geom_line(aes(x=date, y=fitsd), alpha=0.7) +
  geom_line(aes(x=date, y=quantile), alpha=0.7, col="blue") +
  geom_vline(xintercept=as.numeric(elections), col="red", alpha=0.7, linetype=4) +
  coord_cartesian(ylim=c(0,40))
dfj %>% mutate(fitsd=fitted(fitsd), quantile=fitted(fit20)) %>% tail(2000) %>% ggplot(aes(x=date, y=fitsd/quantile)) +
  geom_line(alpha=0.7) +
  geom_area(alpha=0.7)+
  geom_vline(xintercept=as.numeric(elections), col="red", alpha=0.7, linetype=4) +
  coord_cartesian(ylim=c(0, 7)) +
  ylab("effet election/evenements seul")
# attention toutefois les parametres de lissage influent fortement sur ce "facteur".
tail(fitted(fitsd)/fitted(fit20), 2000) %>% summary

# FIN
#####

# page gitlab personnelle:
browseURL(url="https://gitlab.com/users/agenis/projects")
