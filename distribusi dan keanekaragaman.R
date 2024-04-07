library(tidyr)
library(dplyr)
library(ggplot2)
library(viridis)
getwd()
###Untuk Dataset Awang###
# Membuat dataset awal
data_awal <- read.xlsx('dataawang.xlsx')
head(data_awal)
# Mengubah struktur dataset
data_baru <- pivot_longer(data_awal, cols = starts_with("AW"), 
                          names_to = "Lokasi", values_to = "Jumlah_Individu")
data_baru$Lokasi <- gsub("AW", "Teluk Awang ", data_baru$Lokasi)

# Menampilkan dataset yang telah diubah
print(data_baru)
head(data_baru)

# Grouping data berdasarkan lokasi/stasiun
data_grouped <- data_baru %>%
  group_by(Lokasi, Kode.spesies) %>%
  summarise(Jumlah_Individu = sum(Jumlah_Individu))

# Filter data untuk Teluk Awang saja (stasiun besar)
teluk_awang <- data_grouped %>%
  filter(grepl("Teluk Awang", Lokasi))

# Visualisasi distribusi spesies di Teluk Awang
ggplot(teluk_awang, aes(x = Kode.spesies, y = Jumlah_Individu, fill = Lokasi)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Kode Spesies", y = "Jumlah Individu", title = "Distribusi Spesies di Teluk Awang") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#FF9999", "#66CCCC", "#CC99FF", "#99CC99", "#FFCC99")) # Warna berbeda untuk setiap stasiun


###Untuk Dataset Paremas###
# Membuat dataset awal
data_awal2 <- read.xlsx('dataparemas.xlsx')
head(data_awal2)
# Mengubah struktur dataset
data_baru2 <- pivot_longer(data_awal2, cols = starts_with("PM"), 
                          names_to = "Lokasi", values_to = "Jumlah_Individu")
data_baru2$Lokasi <- gsub("PM", "Teluk Paremas ", data_baru2$Lokasi)

# Menampilkan dataset yang telah diubah
print(data_baru2)
head(data_baru2)

# Grouping data berdasarkan lokasi/stasiun
data_grouped2 <- data_baru2 %>%
  group_by(Lokasi, Kode.spesies) %>%
  summarise(Jumlah_Individu = sum(Jumlah_Individu))

# Filter data untuk Teluk Paremas saja (stasiun besar)
teluk_paremas <- data_grouped2 %>%
  filter(grepl("Teluk Paremas", Lokasi))

# Visualisasi distribusi spesies di Teluk Paremas
ggplot(teluk_paremas, aes(x = Kode.spesies, y = Jumlah_Individu, fill = Lokasi)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Kode Spesies", y = "Jumlah Individu", title = "Distribusi Spesies di Teluk Paremas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#FF9999", "#66CCCC", "#CC99FF", "#99CC99", "#FFCC99")) # Warna berbeda untuk setiap stasiun


###Untuk Dataset Seriwe###
# Membuat dataset awal
data_awal3 <- read.xlsx('dataseriwe.xlsx')
head(data_awal3)
# Mengubah struktur dataset
data_baru3 <- pivot_longer(data_awal3, cols = starts_with("SR"), 
                           names_to = "Lokasi", values_to = "Jumlah_Individu")
data_baru3$Lokasi <- gsub("SR", "Teluk Seriwe ", data_baru3$Lokasi)

# Menampilkan dataset yang telah diubah
print(data_baru3)
head(data_baru3)
# Grouping data berdasarkan lokasi/stasiun
data_grouped3 <- data_baru3 %>%
  group_by(Lokasi, Kode.spesies) %>%
  summarise(Jumlah_Individu = sum(Jumlah_Individu))

# Filter data untuk Teluk Seriwe saja
teluk_seriwe <- data_grouped3 %>%
  filter(grepl("Teluk Seriwe", Lokasi))
teluk_seriwe

# Visualisasi distribusi spesies di Teluk Seriwe
ggplot(teluk_seriwe, aes(x = Kode.spesies, y = Jumlah_Individu, fill = Lokasi)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Kode Spesies", y = "Jumlah Individu", title = "Distribusi Spesies di Teluk Seriwe") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#FF9999", "#66CCCC", "#CC99FF", "#99CC99", "#FFCC99")) # Warna berbeda untuk setiap stasiun


### Visualisasi Stasiun Besar###

# Menggabungkan data per stasiun besar
teluk_awang_agg <- teluk_awang %>%
  group_by(Kode.spesies) %>%
  summarise(Jumlah_Individu = sum(Jumlah_Individu))

teluk_paremas_agg <- teluk_paremas %>%
  group_by(Kode.spesies) %>%
  summarise(Jumlah_Individu = sum(Jumlah_Individu))

teluk_seriwe_agg <- teluk_seriwe %>%
  group_by(Kode.spesies) %>%
  summarise(Jumlah_Individu = sum(Jumlah_Individu))

# Gabungkan semua data menjadi satu
all_data <- bind_rows(
  mutate(teluk_awang_agg, Stasiun = "Teluk Awang"),
  mutate(teluk_paremas_agg, Stasiun = "Teluk Paremas"),
  mutate(teluk_seriwe_agg, Stasiun = "Teluk Seriwe")
)

# Visualisasi distribusi spesies per stasiun besar
ggplot(all_data, aes(x = Kode.spesies, y = Jumlah_Individu, fill = Stasiun)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Kode Spesies", y = "Jumlah Individu", title = "Distribusi Spesies per Stasiun Besar") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#FF9999", "#66CCCC", "#CC99FF")) # Warna berbeda untuk setiap stasiun


species_colors <- c(
  "#1f77b4", "#aec7e8", "#ff7f0e", "#ffbb78", "#2ca02c", "#98df8a", 
           "#d62728", "#ff9896", "#9467bd", "#c5b0d5", "#8c564b", "#c49c94", 
           "#e377c2", "#f7b6d2", "#7f7f7f", "#c7c7c7", "#bcbd22", "#dbdb8d", 
           "#17becf", "#9edae5", "#393b79", "#5254a3", "#6b6ecf", "#9c9ede", 
           "#637939", "#8ca252", "#b5cf6b", "#cedb9c", "#8c6d31", "#bd9e39", 
           "#e7ba52", "#e7cb94", "#843c39", "#ad494a"
)


ggplot(all_data, aes(x = Stasiun, y = Jumlah_Individu, fill = Kode.spesies)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Lokasi", y = "Jumlah Individu", title = "Distribusi Spesies per Lokasi") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = species_colors)  # Warna berbeda untuk setiap kode spesies

# Sediakan palet 
species_colors <- rainbow(32)
                             
# Visualisasi distribusi spesies di Teluk Awang
ggplot(teluk_awang, aes(x = Lokasi, y = Jumlah_Individu, fill = Kode.spesies)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Lokasi", y = "Jumlah Individu", title = "Distribusi Spesies di Teluk Awang") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = species_colors)  # Warna berbeda untuk setiap kode spesies

# Visualisasi distribusi spesies di Teluk Paremas
ggplot(teluk_paremas, aes(x = Lokasi, y = Jumlah_Individu, fill = Kode.spesies)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Lokasi", y = "Jumlah Individu", title = "Distribusi Spesies di Teluk Paremas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = species_colors)  # Warna berbeda untuk setiap kode spesies

# Visualisasi distribusi spesies di Teluk Seriwe
ggplot(teluk_seriwe, aes(x = Lokasi, y = Jumlah_Individu, fill = Kode.spesies)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Lokasi", y = "Jumlah Individu", title = "Distribusi Spesies di Teluk Seriwe") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = species_colors)  # Warna berbeda untuk setiap kode spesies
















