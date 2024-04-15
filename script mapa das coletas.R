# Install packages:
if (!require("rgdal")) install.packages("rgdal")
if (!require("sp")) install.packages("sp")
if (!require("rgeos")) install.packages("rgeos")
if (!require("scales")) install.packages("scales")
if (!require("maps")) install.packages("maps")
if (!require("raster")) install.packages("raster")

# Load libraries
library(rgdal)
library(sp)
library(rgeos)
library(scales)  
library(maps) 
library(raster)

### BACKGROUND MAP

# Import the shapefile files
shp_Br <- readOGR("C:/Users/User/Documents/Doutorado/figura 1", "BR_UF_2020", stringsAsFactors=FALSE, encoding="UTF-8")
shp_SA <- readOGR("C:/Users/User/Documents/Doutorado/figura 1", "GEOFT_PAIS", stringsAsFactors=FALSE, encoding="UTF-8")
shp_Mgv <- readOGR("C:/Users/User/Documents/Doutorado/figura 1", "veg_mangue_a", stringsAsFactors=FALSE, encoding="UTF-8")
raster_prec_12 <- raster("C:/Users/User/Documents/Doutorado/figura 1/wc2.1_2.5m_bio_12.tif")
raster_prec_13 <- raster("C:/Users/User/Documents/Doutorado/figura 1/wc2.1_2.5m_bio_13.tif")


# Defining longitude and latitude limits
limits_longitude <- c(-55, -34.5)
limits_latitude <- c(-12, 5)

# Defining colors for raster_prep
pall <- viridis::viridis(5)
rev_pall <- rev(pall)

# Map of South America + Brazilian states for the limits assigned above
plot(raster_prec, col = rev_pall, alpha = 0.8, xlim = limits_longitude, ylim = limits_latitude) # precipitation layer
plot(shp_Mgv, col = "darkgray", border = "darkgray", xlim = limits_longitude, ylim = limits_latitude, add = TRUE) # mangrove layer
plot(shp_SA, col = NULL, border = "black", add = TRUE) # south america countries layer
plot(shp_Br, col = NULL, border = "black", add = TRUE) # brazilian states of Brazil layer


# Adding coordinates to axes
axis(1, at = pretty(limits_longitude, n = 3))
axis(2, at = pretty(limits_latitude, n = 3))

# Adding scale (no option for aesthetic)
map.scale(ratio = FALSE)

### POINTS: 

# Setting working directory:
setwd("C:/Users/User/Documents/Doutorado/figura 1")

# Importing tables:
data_all <- read.table("data_all.tsv", sep = "\t", header = T)

# Replace commas with periods
data_all$lat <- as.numeric(gsub(",", ".", data_all$lat))
data_all$lon <- as.numeric(gsub(",", ".", data_all$lon))

# Remove lines without coordinates
data_all <- data_all[complete.cases(data_all), ]

# R. mangle with propagules
points(x = data_all$lon[data_all$especie == "mangle" & data_all$t_c == "c" & data_all$props. == "sim"],
       y = data_all$lat[data_all$especie == "mangle" & data_all$t_c == "c" & data_all$props. == "sim"],
       pch = 16, col = "#D81B60", cex = 1.5)

# R. mangle without propagules
points(x = data_all$lon[data_all$especie == "mangle" & data_all$t_c == "c" & data_all$props. == "não"],
       y = data_all$lat[data_all$especie == "mangle" & data_all$t_c == "c" & data_all$props. == "não"],
       pch = 1, col = "#D81B60", cex = 1.5)

# L. racemosa with propagules
points(x = data_all$lon[data_all$especie == "racemosa" & data_all$t_c == "c" & data_all$props. == "sim"],
       y = data_all$lat[data_all$especie == "racemosa" & data_all$t_c == "c" & data_all$props. == "sim"],
       pch = 16, col = "#1E88E5", cex = 1.5)

# L. racemosa without propagules
points(x = data_all$lon[data_all$especie == "racemosa" & data_all$t_c == "c" & data_all$props. == "não"],
       y = data_all$lat[data_all$especie == "racemosa" & data_all$t_c == "c" & data_all$props. == "não"],
       pch = 1, bg = "#1E88E5", cex = 1.5)

# A. schaueriana with propagules
points(x = data_all$lon[data_all$especie == "schaueriana" & data_all$t_c == "c" & data_all$props. == "sim"],
       y = data_all$lat[data_all$especie == "schaueriana" & data_all$t_c == "c" & data_all$props. == "sim"],
       pch = 16, col = "#FFC107", cex = 1.5)

# A. schaueriana without propagules
points(x = data_all$lon[data_all$especie == "schaueriana" & data_all$t_c == "c" & data_all$props. == "não"],
       y = data_all$lat[data_all$especie == "schaueriana" & data_all$t_c == "c" & data_all$props. == "não"],
       pch = 1, col = "#FFC107", cex = 1.5)

# A. germinans with propagules
points(x = data_all$lon[data_all$especie == "germinans" & data_all$t_c == "c" & data_all$props. == "sim"],
       y = data_all$lat[data_all$especie == "germinans" & data_all$t_c == "c" & data_all$props. == "sim"],
       pch = 16, col = "#004D40", cex = 1.5)

# A. germinans without propagules
points(x = data_all$lon[data_all$especie == "germinans" & data_all$t_c == "c" & data_all$props. == "não"],
       y = data_all$lat[data_all$especie == "germinans" & data_all$t_c == "c" & data_all$props. == "não"],
       pch = 1, col = "#004D40", cex = 1.5)

# caso eu queira mexer na transparencia:
# pch = 10, col = alpha("#D81B60", 0.2), bg = alpha("#D81B60", 0.2), cex = 1.5)


# Soil
points(x = data_all$lon[data_all$solo_qf == "sim"],
       y = data_all$lat[data_all$solo_qf == "sim"],
       pch = 18, col = "black", cex = 1.3)

# Add caption
legend("topright", legend = c("R. mangle", "L. racemosa", "A. schaueriana", "A. germinans", "c/ props.", "s/ props.", "Solo"),
       pch = c(21, 21, 21, 21, 1, 10, 18), col = c("#D81B60", "#1E88E5", "#FFC107", "#004D40", "black", "black", "black"),
       pt.bg = c("#D81B60", "#1E88E5", "#FFC107", "#004D40", "black", "black", "black"), pt.cex = 1.5,
       title = "Legend")





# alternative
especies <- c("mangle", "racemosa", "schaueriana", "germinans")

# Simplificar o shapefile shp_Mgv
shp_Mgv_simplified <- gSimplify(shp_Mgv, tol = 0.01) # Ajuste a toler?ncia conforme necess?rio
shp_SA_simplified <- gSimplify(shp_SA, tol = 0.01) # Ajuste a toler?ncia conforme necess?rio
shp_Br_simplified <- gSimplify(shp_Br, tol = 0.01) # Ajuste a toler?ncia conforme necess?rio

# Define as cores do gradiente de cinza claro a escuro
gray_colors <- gray.colors(5)
rev_gray_colors <- rev(gray_colors)

# Plotar os mapas usando o gradiente de cinza
par(mfrow = c(2, 2))

for (i in 1:length(especies)) {
  especie <- especies[i]
  
  # Plotar mapa
  plot(raster_prec, col = rev_gray_colors, alpha = 0.8, xlim = limits_longitude, ylim = limits_latitude)
  plot(shp_Mgv_simplified, col = "darkgreen", border = "darkgreen", add = TRUE)
  plot(shp_SA_simplified, col = NULL, border = "black", add = TRUE)
  plot(shp_Br_simplified, col = NULL, border = "black", add = TRUE)
  
  # Plotar pontos espec?ficos para a esp?cie atual
  points(x = data_all$lon[data_all$especie == especie & data_all$t_c == "c" & data_all$props. == "sim"],
         y = data_all$lat[data_all$especie == especie & data_all$t_c == "c" & data_all$props. == "sim"],
         pch = 16, col = "#D81B60", cex = 1.5)
  
  points(x = data_all$lon[data_all$especie == especie & data_all$t_c == "c" & data_all$props. == "nao"],
         y = data_all$lat[data_all$especie == especie & data_all$t_c == "c" & data_all$props. == "nao"],
         pch = 1, col = "#D81B60", cex = 1.5)
  
  # Adicionar t?tulo para cada mapa
  title(paste("Mapa para a esp?cie", especie))
}










# Plotar os mapas usando o gradiente de cinza
par(mfrow = c(2, 2))

for (i in 1:length(especies)) {
  especie <- especies[i]
  
  # Plotar mapa
  plot(raster_prec, col = rev_gray_colors, alpha = 0.8, xlim = limits_longitude, ylim = limits_latitude)
  plot(shp_Mgv_simplified, col = "#42EF4F", border = "#42EF4F", add = TRUE)
  plot(shp_SA_simplified, col = NULL, border = "black", add = TRUE)
  plot(shp_Br_simplified, col = NULL, border = "black", add = TRUE)
  
  # Plotar pontos espec?ficos para a esp?cie atual
  points(x = data_all$lon[data_all$especie == especie & data_all$t_c == "c" & data_all$props. == "sim"],
         y = data_all$lat[data_all$especie == especie & data_all$t_c == "c" & data_all$props. == "sim"],
         pch = 16, col = alpha("#FF4F4F", 0.5), cex = 1.5)
  
  points(x = data_all$lon[data_all$especie == especie & data_all$t_c == "c" & data_all$props. == "nao"],
         y = data_all$lat[data_all$especie == especie & data_all$t_c == "c" & data_all$props. == "nao"],
         pch = 1, col = alpha("#FF4F4F", 0.5), cex = 1.5)
  
  # Adicionar t?tulo para cada mapa
  title(paste("Especie:", especie))
}

# Definir a legenda do raster
legend_colors <- rev_gray_colors
legend_labels <- c("Low", "", "", "", "High") # Ajuste os r?tulos conforme necess?rio
legend_title <- "Precipitation"

# Criar a legenda separadamente
legend("bottomright", legend = legend_labels, fill = legend_colors, title = legend_title, bty = "n")







# Exemplo de dados
nomes <- c("Aracaju", "Ponta dos Mangues", "Natal", "Guamare", "Mossoro", "Fortaleza", "Itarema", "Guriu", "Parnaiba", "Sao luiz", "Godofredo Viana", "Braganca", "Curuca", "Soure", "Afua", "Santana", "Bailique", "Sucuriju", "Goiabal")
siglas <- c("ARA", "PDM", "NAT", "GMR", "MSS", "FOR", "ITR", "GUR", "PHB", "SLZ", "GDV", "BRG", "CUR", "SOU", "AFU", "STN", "BLQ", "SCJ", "GBL")
latitudes <- c(-10.9095, -10.556407, -5.785838, -5.106733, -5.169897, -3.751303, -2.926184, -2.849134, -2.872454, -2.959773, -1.387906, -1.048650, -0.727738, -0.724805, -0.157428, -0.054060, 0.872475, 1.677525, 2.622085)
longitudes <- c(-37.0748, -36.567982, -35.198350, -36.320322, -37.337517, -38.520987, -39.906839, -40.593270, -41.877947, -44.313047, -45.747675, -46.772101, -47.850004, -48.513766, -50.388363, -51.174020, -50.047986, -49.932768, -50.849999)

# Criar o dataframe
dados <- data.frame(nome = nomes, sigla = siglas, lat = latitudes, lon = longitudes)


# Definindo longitude e latitude limites para o segundo mapa
limits_longitude_sa <- c(-90, -30)
limits_latitude_sa <- c(-60, 15)

# Plotar os mapas usando o gradiente de cinza
par(mfrow = c(3, 2))

# Primeiro mapa: Todos os shapefiles + raster
plot(raster_prec, col = rev_pall, alpha = 0.8, xlim = limits_longitude, ylim = limits_latitude)
#plot(raster_prec_13, col = rev_pall, alpha = 0.8, xlim = limits_longitude, ylim = limits_latitude)
plot(shp_Mgv_simplified, col = "#62EC00", border = "#62EC00", add = TRUE)
plot(shp_SA_simplified, col = NULL, border = "black", add = TRUE)
plot(shp_Br_simplified, col = NULL, border = "black", add = TRUE)
map.scale(ratio = FALSE)

# pontos das localidades
points(x = dados$lon, y = dados$lat,
       pch = 3, col = "red", cex = 0.5)
text(dados$lon, dados$lat, labels = dados$sigla, pos = 1, offset = 0.5, col = "black", cex = 0.5)

# Segundo mapa: Am?rica do Sul inteira
plot(shp_SA_simplified, col = NULL, border = "black", xlim = limits_longitude_sa, ylim = limits_latitude_sa)
box()
axis(1)
axis(2)
map.scale(ratio = FALSE)

# quadrado
rect(limits_longitude[1], limits_latitude[1], limits_longitude[2], limits_latitude[2], border = "black", lwd = 2, lty = "solid", col = alpha("white", 0))

# Criar um vetor de cores para cada esp?cie
cores <- rainbow(length(especies))

# Inicializar um vetor vazio para armazenar os nomes das esp?cies
nomes_cores <- c()

# Loop sobre as esp?cies para os mapas restantes
for (i in 1:length(especies)) {
  especie <- especies[i]
  
  # Adicionar o nome da esp?cie e a cor correspondente ao vetor de nomes das cores
  nomes_cores <- c(nomes_cores, paste(especie, ":", cores[i]))
  
  # Plotar mapa
  plot(shp_Mgv_simplified, col = "#62EC00", border = "#62EC00", xlim = limits_longitude, ylim = limits_latitude)
  plot(shp_SA_simplified, col = NULL, border = "black", add = TRUE)
  
  # Coletar dados para a esp?cie atual
  dados_especie <- data_all[data_all$especie == especie & data_all$t_c == "c", ]
  
  # Plotar pontos espec?ficos para a esp?cie atual com cores diferentes
  points(x = dados_especie$lon[dados_especie$props. == "sim"],
         y = dados_especie$lat[dados_especie$props. == "sim"],
         pch = 16, col = alpha(cores[i], 0.5), bg = alpha(cores[i], 0.5), cex = 1.5)
  
  points(x = dados_especie$lon[dados_especie$props. == "nao"],
         y = dados_especie$lat[dados_especie$props. == "nao"],
         pch = 1, col = alpha(cores[i], 0.5), bg = alpha(cores[i], 0.5), cex = 1.5)
  
  # Adicionar t?tulo para cada mapa
  title(paste("Especie:", especie))
  map.scale(ratio = FALSE)
  box()
  axis(1)
  axis(2)
}

# Adicionar a legenda de cores
legend("topright", legend = nomes_cores, fill = cores, title = "Esp?cies", bty = "n", ncol = 1)










# TALVEZ JOGAR FORA ISSO

# Loop sobre as esp?cies para os mapas restantes
for (i in 1:length(especies)) {
  especie <- especies[i]
  
  # Plotar mapa
  #plot(raster_prec, col = rev_gray_colors, alpha = 0.8, xlim = limits_longitude, ylim = limits_latitude)
  plot(shp_Mgv_simplified, col = "#62EC00", border = "#62EC00", xlim = limits_longitude, ylim = limits_latitude)
  plot(shp_SA_simplified, col = NULL, border = "black", add = TRUE)
  #plot(shp_Br_simplified, col = NULL, border = "black", add = TRUE)
  
  # Plotar pontos espec?ficos para a esp?cie atual
  points(x = data_all$lon[data_all$especie == especie & data_all$t_c == "c" & data_all$props. == "sim"],
         y = data_all$lat[data_all$especie == especie & data_all$t_c == "c" & data_all$props. == "sim"],
         pch = 16, col = alpha("#FF4F4F", 0.5), cex = 1.5)
  
  points(x = data_all$lon[data_all$especie == especie & data_all$t_c == "c" & data_all$props. == "nao"],
         y = data_all$lat[data_all$especie == especie & data_all$t_c == "c" & data_all$props. == "nao"],
         pch = 1, col = alpha("#FF4F4F", 0.5), cex = 1.5)
  
  # Adicionar t?tulo para cada mapa
  title(paste("Especie:", especie))
  map.scale(ratio = FALSE)
  box()
  axis(1)
  axis(2)
}














# PARA ESCOLHER A VARIAVEL BIOCLIMATICA DO WORLDCLIM

# Caminho para os arquivos TIFF
caminho <- "C:/Users/User/Documents/Doutorado/figura 1/"

# Inicializar uma lista para armazenar os raster
lista_rasters <- list()

# Loop para ler os arquivos TIFF e armazen?-los na lista
for (i in 1:19) {
  nome_arquivo <- paste0("wc2.1_2.5m_bio_", i, ".tif")
  caminho_completo <- paste0(caminho, nome_arquivo)
  raster_atual <- raster(caminho_completo)  # Ler o arquivo TIFF
  lista_rasters[[i]] <- raster_atual  # Armazenar o raster na lista
}

# Plotar cada raster em mapas diferentes
par(mfrow = c(4, 5))  # Definir layout da janela gr?fica

for (i in 1:19) {
  plot(lista_rasters[[i]], main = paste("Raster", i), xlim = c(-55, -34.5), ylim = c(-12, 5))  # Plotar o raster atual com t?tulo correspondente e limites de latitude e longitude
}






# Somente para visualizar dados para tabela! 

# Convertendo 'props.' para uma variável de texto, se não for
data_all$props. <- as.character(data_all$props.)

# Usando a função aggregate para contar quantas árvores possuem ou não possuem props. por espécie em cada local
resultado <- aggregate(props. ~ especie + local, data = data_all, FUN = function(x) c(Com = sum(x == "sim"), Sem = sum(x == "não")))

# Renomeando as colunas para maior clareza
colnames(resultado) <- c("Espécie", "Local", "Props.")

# Exibindo o resultado
print(resultado)

