data <- read_csv2(file.choose())
#boxplot(data$Glas, main="Mein Titel", ylab="Titel y-Achse")

# --- Packages installieren, falls nicht vorhanden
if(!"cluster" %in% rownames(installed.packages())) install.packages("cluster")
if(!"factoextra" %in% rownames(installed.packages())) install.packages("factoextra")
if(!"ggplot2" %in% rownames(installed.packages())) install.packages("ggplot2")
if(!"proxy" %in% rownames(installed.packages())) install.packages("proxy")
if(!"TSdist" %in% rownames(installed.packages())) install.packages("TSdist")

# --- Packages laden
library("cluster")
library("factoextra")
library("ggplot2")
library("proxy")
library("TSdist")


# file: CSV_Abfallstatistik_Steuerfuesse_Bevoelkerung_2012_2019_durchschnitte_nur_numerisch
xraw<-mydata
if(!"ggplot2" %in% rownames(installed.packages())) install.packages("ggplot2")
library(ggplot2)
ggplot(xraw,aes(xraw$Durchschnitt_Steuerfuss_ueber_alle_Jahre, xraw$total_pro_kopf_durchschnitt))+geom_point()+geom_smooth()
ggplot(xraw,aes(xraw$Durchschnitt_Steuerfuss_ueber_alle_Jahre, xraw$Glas_pro_Kopf_durchschnitt))+geom_point()+geom_smooth(method=lm)+labs(title = "Vergleich Steuerfuss mit Menge Glas",
                                                                                                                                          x = "Ø Steuerfuss 2012-2019",
                                                                                                                                          y = "Ø Menge Glas 2012-2019")
ggplot(xraw,aes(xraw$Durchschnitt_Steuerfuss_ueber_alle_Jahre, xraw$Metalle_pro_Kopf_durchschnitt))+geom_point()+geom_smooth(method=lm)+labs(title = "Vergleich Steuerfuss mit Menge Metall",
                                                                                                                                          x = "Ø Steuerfuss 2012-2019",
                                                                                                                                          y = "Ø Menge Metall 2012-2019")
plot(xraw$Durchschnitt_Steuerfuss_ueber_alle_Jahre, xraw$Glas_pro_Kopf_durchschnitt, xlab="Steuerfuss", ylab="Glas pro Kopf")
plot(xraw$Durchschnitt_Steuerfuss_ueber_alle_Jahre, xraw$total_pro_kopf_durchschnitt, xlab="Steuerfuss", ylab="Glas+Metalle+Altöl pro Kopf")

boxplot(xraw$total_pro_kopf_durchschnitt)
boxplot(xraw$total_pro_kopf_durchschnitt ~ xraw$Durchschnitt_Steuerfuss_ueber_alle_Jahre, main="titel")

# ztransformation
xstd <- scale(xraw)
xstd
head(xstd)
str(xstd)
summary(xstd)

boxplot(summary(xstd[,1]), main = "Steuerfuss (by Z-TRANSFORMATION)")
boxplot(summary(xstd[,2]), main = "Glas pro Kopf (by Z-TRANSFORMATION)")
boxplot(summary(xstd[,3]), main = "Metalle pro Kopf (by Z-TRANSFORMATION)")
boxplot(summary(xstd[,4]), main = "Altöl pro Kopf (by Z-TRANSFORMATION)")
boxplot(summary(xstd[,5]), main = "glas+Metalle+altöl pro Kopf (by Z-TRANSFORMATION)")
boxplot(summary(xstd[,6]), main = "Anzahl personen (by Z-TRANSFORMATION)")

# ----------------------------------------
# --- CLUSTER ANALYSIS by KMEANS algorithm
# ----------------------------------------
set.seed(12345)

# --- Calculate and Visualize CLUSTER
# --- no of clusters = 3 
kmres3 <- kmeans(xstd,centers=3,nstart=25)
fviz_cluster(kmres3, xstd)

View(kmres3)


# aenderungen
summary(aenderungen)
boxplot(aenderungen$Glas_pro_Kopf_aenderung, main="Änderungen Glas pro Kopf 2012 bis 2019", ylab="Änderung anzahl Tonnen")
ggplot(xraw,aes(aenderungen$Durchschnitt_Steuerfuss_ueber_alle_Jahre, aenderungen$Total_Glas_Metalle_altoel_pro_kopf_aenderung))+geom_point()+geom_smooth(method=lm)
ggplot(xraw,aes(aenderungen$Steuerfuss_aenderung, aenderungen$Metalle_pro_Kopf_durchschnitt))+geom_point()+geom_smooth(method=lm)
