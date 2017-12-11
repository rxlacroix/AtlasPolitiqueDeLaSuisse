library(FactoMineR)
library(factoextra)
library(stringi)
library(psych)
library(REdaS)
library(rgdal)

data <- read.csv("votations.csv", header = TRUE, sep = ";", encoding = "latin1")

rownames(data) <- make.names(data$Commune, unique=TRUE)

data <- data[, -1]

colnames(data) <- stri_sub(colnames(data),2) # enleve les X 

data <- data[,order(as.Date(substr(colnames(data),1, 10), format="%d.%m.%Y"))] # trie les colonnes par date


#subset 1982-2002

data_old <- data[,3:186]

KMO(data_old)
bart_spher(data_old)

acp <- principal(data_old, nfactors = 3, rotate = "varimax")

summary(acp)
loadings(acp) # pc loadings
scores <- as.data.frame(acp$scores) 

par(pty="s")
plot(-scores$RC3, scores$RC1, xlab = "", ylab = "", xaxt='n', yaxt="n", pch=10, cex=.3)
mtext('Droite', side=4, line=2, las=1)
mtext('Gauche', side=2, line=2, las=1)
mtext("Libéral", side =3, line=1)
mtext("Conservateur", side =1, line=1)



### partie carto

communes <- read.csv("communes.csv")
communes$Habitants <- as.numeric(gsub('![[:alnum:]]*[[:space:]]|[[:punct:]]', '', communes$Habitants))
communes$Commune  <- gsub('-', ' ', communes$Commune)


scores$Commune <- row.names(scores)
scores$Commune <- stri_sub(scores$Commune,3)
scores$Commune <- gsub('\\.\\.', ' (', scores$Commune)
scores$Commune <- gsub('H\\.', 'H)', scores$Commune)
scores$Commune <- gsub('G\\.', 'G)', scores$Commune)
scores$Commune <- gsub('Z\\.', 'Z)', scores$Commune)
scores$Commune <- gsub('E\\.', 'E)', scores$Commune)
scores$Commune <- gsub('L\\.', 'L)', scores$Commune)
scores$Commune <- gsub('U\\.', 'U)', scores$Commune)
scores$Commune <- gsub('R\\.', 'R)', scores$Commune)
scores$Commune <- gsub('O\\.', 'O)', scores$Commune)
scores$Commune <- gsub('W\\.', 'W)', scores$Commune)
scores$Commune <- gsub('D\\.', 'D)', scores$Commune)
scores$Commune <- gsub('S\\.', 'S)', scores$Commune)
scores$Commune <- gsub('\\.', ' ', scores$Commune)
scores$Commune <- gsub('\\(Urne commune', '', scores$Commune)
scores$Commune <- gsub('St \\(', 'St\\. ', scores$Commune)
scores$Commune <- gsub('Biel Bienne', 'Biel/Bienne', scores$Commune)

scoresprefusion <- scores

scores <- merge(scores, communes, by= "Commune")

scores <- scores[,1:6]
scores$langue <- ""
scores$langue[scores$Code.commune <1720] <- "ALL"
scores$langue[scores$Code.commune <5400 & scores$Code.commune > 5000] <- "ITA" # TI
scores$langue[scores$Code.commune >5400 & scores$Code.commune < 5940] <- "FRA" # VD
scores$langue[scores$Code.commune >6400] <- "FRA" # JU GE NE
scores$langue[scores$Code.commune >1000 & scores$Code.commune < 1720] <- "ALL"
scores$langue[scores$Code.commune >2400 & scores$Code.commune < 4960] <- "ALL"
scores$langue[scores$Code.commune >2000 & scores$Code.commune < 2340] <- "FRA"
scores$langue[scores$Code.commune >6000 & scores$Code.commune < 6012] <- "ALL"
scores$langue[scores$Code.commune >6020 & scores$Code.commune < 6040] <- "FRA"
scores$langue[scores$Code.commune >6040 & scores$Code.commune < 6080] <- "ALL"
scores$langue[scores$Code.commune >6080 & scores$Code.commune < 6100] <- "FRA"
scores$langue[scores$Code.commune >6100 & scores$Code.commune < 6120] <- "ALL"
scores$langue[scores$Code.commune >6120 & scores$Code.commune < 6160] <- "FRA"
scores$langue[scores$Code.commune >6160 & scores$Code.commune < 6210] <- "ALL"
scores$langue[scores$Code.commune >6210 & scores$Code.commune < 6270] <- "FRA"
scores$langue[scores$Code.commune >6270 & scores$Code.commune < 6301] <- "ALL"

scores$Colour[scores$langue == "FRA"]="#FF3D00"
scores$Colour[scores$langue == "ALL"]="#5DE100"
scores$Colour[scores$langue == "ITA"]="#FFBC00"

write.csv(scores, "scores.csv")

plot(-scores$RC3, scores$RC1, xlab = "", ylab = "", xaxt='n', yaxt="n", cex=sqrt(scores$Habitants/10000),pch=16, col=scores$Colour)
mtext('Droite', side=4, line=2, las=1)
mtext('Gauche', side=2, line=2, las=1)
mtext("Libéral", side =3, line=1)
mtext("Conservateur", side =1, line=1)


## ArcGIS

##

## KDE pondéré par la population + ombrage -- cell size 0.005 radius 0.4

## KDE pondéré par la population pour chaque langue (créer des couches de pts pour chaque langue)

## Différence raster entre les KDE de langues pour définir les zones d'extension (Minus-Spatial Analyst)


