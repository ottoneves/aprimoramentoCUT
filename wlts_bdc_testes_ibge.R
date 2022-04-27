## TESTES COM A FERRAMENTA WLTS DO BRAZIL DATA CUBE - INPE
## COLETA AUTOMATIZADA DE AMOSTRAS DE TREINAMENTO

# Carregando os pacotes necessários
library(magrittr) # Pacote para usar o operador pipe %>%
library(leaflet) # Pacote com leaflet para lidar com os mapas
library(sf) # Pacote Simple Features, para lidar com shapefiles
library(dplyr) # Pacote dplyr para lidar com data frame
library(RColorBrewer) # Pacote contendo cores de etiqueta
library(tibble)       # Pacote para representar a estrutura do frame de dados
library(tidyr)        # Pacote para transformar a estrutura do quadro de dados
library(ggplot2)     # Pacote ggplot
library(ggalluvial) # Pacote ggalluvial usado para criar o plot
library(cowplot)    # Pacote cowplot para os temas
library(rwlts)

# Configurando o sf para lidar com coordenadas planas
sf::sf_use_s2(F)

#Carregando o shapefile contendo as amostras
amostra1_shp <- sf::st_read("~/R/2022_amostra1_testes", quiet = TRUE)
head(amostra1_shp)

#Visualizando os pontos fornecidos 
plot(amostra1_shp$geometry)

#Chamando o serviço R_wlts
wlts_bdc <- "https://brazildatacube.dpi.inpe.br/wlts/"

#Buscando a trajetória dos pontos no MCUT (IBGE)
amostra1_trj_ibge <- get_trajectory(wlts_bdc,
                                    latitude = amostra1_shp$top,
                                    longitude = amostra1_shp$left,
                                    collections = "ibge_cobertura_uso_terra")
head(amostra1_trj_ibge$result)


#Filtrando as áreas que permanecem estáveis no MCUT-IBGE
# ?? como fazer um loop que faça uma busca por: agrupado por "point_id", quem, para cada "date", manteve a mesma "class".

#Pegar o valor das classe do IBGE para o ano de 2018
amostra1_ibge_2018 <- filter(amostra1_trj_ibge$result, date == 2018)

head(amostra1_ibge_2018)


#Trajetórias dos outros mapeamentos
#Amostra Mapbiomas Cerrado v. 5
amostra1_mapbiomas <- get_trajectory(wlts_bdc, 
                                     latitude = amostra1_shp$top, 
                                     longitude = amostra1_shp$left, 
                                     collections = "mapbiomas_cerrado-v5")

amostra1_mapbiomas_2018 <- filter(amostra1_mapbiomas$result, date == 2018)
head(amostra1_mapbiomas_2018)

#Amostra TerraClass Cerrado
amostra1_terraclass <- get_trajectory(wlts_bdc, 
                                      latitude = amostra1_shp$top, 
                                      longitude = amostra1_shp$left, 
                                      collections = "terraclass_cerrado")

amostra1_terraclass_2018 <- filter(amostra1_terraclass$result, date == 2018)

head(amostra1_terraclass_2018)

#Olhar as amostras juntas em um mesmo dataframe
amostra1.df <- data.frame(id=amostra1_ibge_2018$point_id, ibge_2018=amostra1_ibge_2018$class, mapbiomas_2018=amostra1_mapbiomas_2018$class, terraclass=amostra1_terraclass_2018$class)
amostra1.df

#Criar regras para "tradução" de legenda e criar campo novo "label" 
#?? montar um conjunto de if --- then --- else? 


#Exportar dataframe para csv
write.csv(amostra1.df,"~/R/df_amostra1.csv", row.names = FALSE)


# AUTORES:
# ?? existe um padrão para citar autores?

# REFERÊNCIAS: 
# 
#Fabiana Zioti, Felipe Menino Carlos, Felipe Carvalho de Souza, Rennan F. B. Marujo, Karine Reis Ferreira, Gilberto R. Queiroz. Notebook Kaggle: https://www.kaggle.com/code/manuelaalvarenga/ibge-wlts-example-r
#
#Ferreira, K.R.; Queiroz, G.R.; Vinhas, L.; Marujo, R.F.B.; Simoes, R.E.O.; Picoli, M.C.A.; Camara, G.; Cartaxo, R.; Gomes, V.C.F.; Santos, L.A.; Sanchez, A.H.; Arcanjo, J.S.; Fronza, J.G.; Noronha, C.A.; Costa, R.W.; Zaglia, M.C.; Zioti, F.; Korting, T.S.; Soares, A.R.; Chaves, M.E.D.; Fonseca, L.M.G. 2020. Earth Observation Data Cubes for Brazil: Requirements, Methodology and Products. Remote Sens. 12, no. 24: 4033. DOI: 10.3390/rs12244033.
#
#Zioti, F.; Gomes, V.C.F.; Ferreira, K.R.; Queiroz, G.R.; Rodriguez, E. L. 2019. Um ambiente para acesso e análise de trajetórias de uso e cobertura da Terra. Anais do XIX Simpósio Brasileiro de Sensoriamento Remoto.São José dos Campos, INPE, 2019. Online .