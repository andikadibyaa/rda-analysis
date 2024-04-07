library(vegan)
data1<-read.xlsx('datarda1.xlsx')
data2<-read.xlsx('datarda2.xlsx')
head(data1)
head(data2)
# Menggabungkan kedua dataset berdasarkan kolom "Kode"
merged_data <- merge(data1, data2, by = "Kode")
merged_data

# Memisahkan variabel lingkungan dan fitoplankton
env_data <- merged_data[, c("Kedalaman.Laut", "T", "S", "pH", "pHmV", "DO", "TDS", "Turbidity", "ORP", "Konduktivitas", "Secchi", "TSS")]
phyto_data<-merged_data[, 2:33]
phyto_data
# Mengubah kolom-kolom menjadi tipe data numerik
env_data$Kedalaman.Laut <- as.numeric(env_data$Kedalaman.Laut)
env_data$T <- as.numeric(env_data$T)
env_data$S <- as.numeric(env_data$S)
env_data$pH <- as.numeric(env_data$pH)
env_data$pHmV <- as.numeric(env_data$pHmV)
env_data$DO <- as.numeric(env_data$DO)
env_data$TDS <- as.numeric(env_data$TDS)
env_data$Turbidity <- as.numeric(env_data$Turbidity)
env_data$Konduktivitas <- as.numeric(env_data$Konduktivitas)
env_data$Secchi <- as.numeric(env_data$Secchi)
env_data$TSS <- as.numeric(env_data$TSS)
str(env_data)
# Mengubah kolom-kolom dalam phyto_data menjadi tipe data numerik
phyto_data$CRT <- as.numeric(phyto_data$CRT)
phyto_data$CTC <- as.numeric(phyto_data$CTC)
phyto_data$CCD <- as.numeric(phyto_data$CCD)
phyto_data$CCP <- as.numeric(phyto_data$CCP)
phyto_data$CND <- as.numeric(phyto_data$CND)
phyto_data$CLT <- as.numeric(phyto_data$CLT)
phyto_data$CDT <- as.numeric(phyto_data$CDT)
phyto_data$DPS <- as.numeric(phyto_data$DPS)
phyto_data$DTM <- as.numeric(phyto_data$DTM)
phyto_data$EMP <- as.numeric(phyto_data$EMP)
phyto_data$HML <- as.numeric(phyto_data$HML)
phyto_data$LDR <- as.numeric(phyto_data$LDR)
phyto_data$LPA <- as.numeric(phyto_data$LPA)
phyto_data$LNB <- as.numeric(phyto_data$LNB)
phyto_data$MNR <- as.numeric(phyto_data$MNR)
phyto_data$NTZ <- as.numeric(phyto_data$NTZ)
phyto_data$ODT <- as.numeric(phyto_data$ODT)
phyto_data$PRS <- as.numeric(phyto_data$PRS)
phyto_data$PBS <- as.numeric(phyto_data$PBS)
phyto_data$PPD <- as.numeric(phyto_data$PPD)
phyto_data$PSNTZ <- as.numeric(phyto_data$PSNTZ)
phyto_data$PSS <- as.numeric(phyto_data$PSS)
phyto_data$RBN <- as.numeric(phyto_data$RBN)
phyto_data$RZS <- as.numeric(phyto_data$RZS)
phyto_data$SPX <- as.numeric(phyto_data$SPX)
phyto_data$TLN <- as.numeric(phyto_data$TLN)


# Melakukan analisis RDA
rda_result <- rda(phyto_data ~ ., data = env_data)

# Melihat hasil analisis RDA
summary(rda_result)

### Analisis Triplot ###
dca<-decorana(phyto_data)
dca
rda1<-rda(phyto_data~.,env_data)
rda1
rda3<-rda(phyto_data~TDS+TSS+pH+ORP, data=env_data)
plot(rda3)
plot(rda3, display=c('sp','bp'))
plot(rda1, display=c('sp','bp'))
plot(rda1)

##pemeriksaan signifikansi
anova.cca(spe.rda.signif, step = 1000)


### Parameter Secchi ###
## Stasiun besar awang
rda2<-rda(slice(phyto_data,1:6),slice(env_data,1:6))
rda2
plot(rda2)
plot(rda2, display=c('sp','bp'))
