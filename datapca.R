setwd('C:/Users/andik/OneDrive/Documents/PCARDA')
getwd()
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(FactoMineR)  
library(factoextra)
library(stats)
library(MASS)
datapca<-read.xlsx('pca.xlsx') #Membaca data
head(datapca) #Menampilkan sebagian kecil data
str(datapca) # Mengecek kelas masing2 variabel

###Statistika Deskriptif###
summary(datapca) #cek statdes

# Total jumlah AW1 hingga SR5 untuk setiap kelompok
data_group <- datapca %>%
  group_by(Group) %>%
  summarize(
    total = sum(AW1, AW2, AW3, AW4, AW5, PM1, PM2, PM3, PM4, PM5, SR1, SR2, SR3, SR4, SR5)
  ) %>%
mutate(percentage = total / sum(total) * 100)  # Menghitung persentase
# Membuat pie chart dengan label persentase
ggplot(data_group, aes(x = "", y = percentage, fill = Group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Persentase Total Jumlah berdasarkan Kelompok", x = NULL, y = NULL) +
  scale_fill_brewer(palette = "Set3") +  # Memilih skema warna
  theme_minimal() +  # Memilih tema plot
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5))  # Menambahkan label persentase


###Analisis PCA###

# Memilih kolom-kolom yang diperlukan untuk analisis PCA (kolom stasiun)
fitoplankton_stasiun <- datapca[, 4:ncol(datapca)]  # karena analisis dimulai dari kolom 4
head(fitoplankton_stasiun)
# Melakukan PCA
pca_result <- prcomp(datapca[, -c(1:3)], scale. = TRUE)
pca_result
fito_pca
# Mendapatkan hasil koordinat spesies per titik/stasiun
fito_coord <- as.data.frame(fito_pca$ind$coord)
fito_coord
pca_result <- prcomp(datapca, scale. = TRUE)

# Menampilkan proporsi variasi yang dijelaskan oleh setiap komponen utama
proporsi_variasi <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
proporsi_variasi

# Visualisasi scatter plot PCA
fviz_pca_ind(fito_pca, col.ind = "cos2", gradient.cols = c("blue", "red"), repel = TRUE, geom.ind = "point")

# Memilih jumlah komponen utama
jumlah_komponen <- which(cumsum(proporsi_variasi) >= 0.8)[1]
jumlah_komponen

pca_result
# Visualisasi data dalam ruang Komponen Utama PC1 dan PC2
pca_df <- as.data.frame(pca_result$x)
ggplot(data = pca_df, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(title = "Visualisasi Data dalam Ruang Komponen Utama PC1 dan PC2",
       x = "Komponen Utama 1",
       y = "Komponen Utama 2")



plot(pca_df$PC1, pca_df$PC2, col = "blue", pch = 16, 
     xlab = "PC1", ylab = "PC2", main = "Distribusi Spesies per Lokasi")
legend("topright", legend = unique(datapca$Kode.spesies), col = "blue", pch = 16)


# Menampilkan koefisien rotasi terhadap PC1 dan PC2
rotasi1 <- pca_result$rotation[, 1:2]  # Ambil koefisien rotasi untuk PC1 dan PC2
rownames(rotasi1) <- colnames(c('AW1', 'AW2', 'AW3', 'AW4', 'AW5', 'PM1', 'PM2', 'PM3', 'PM4', 'PM5', 'SR1', 'SR2', 'SR3', 'SR4', 'SR5'))  # Atur nama baris sebagai nama spesies
print(rotasi1)

# Menjalankan K-means clustering
set.seed(123)  # Atur seed untuk hasil yang konsisten
kmeans_result <- kmeans(pca_result$x[, 1:2], centers = 3)  # 3 klaster
cluster_labels <- kmeans_result$cluster  # Label klaster untuk setiap observasi

# Plot hasil klastering
plot(pca_result$x[, 1:2], col = cluster_labels, pch = 16,
     xlab = "PC1", ylab = "PC2", main = "K-means Clustering of PCA Components")

str(datapca)
datapca1<-datapca[, -c(1:3)]
str(datapca1)
pr.out <- prcomp(datapca1, center = TRUE, scale = TRUE, retx = TRUE)
pr.out
#namoilin screeplot
screeplot(pr.out,type = "line")
abline(h = 1, col = "red", lty = 3)
#nampilin biplot
biplot(pr.out, scale = 0)
autoplot(pr.out, data = datapca, colour = 'Kode.spesies', 
         loadings = TRUE, loadings.label = TRUE, 
         loadings.label.size = 3, scale = 0)
fviz_pca_ind(pr.out, label="none", habillage=datapca$Kode.spesies)













