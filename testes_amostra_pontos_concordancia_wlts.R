## Instalar o pacote `sits` e suas dependências a partir do github
# OBS: requer o uso do pacote devtools
#devtools::install_github("e-sensing/sits", dependencies = TRUE)

#Carregar os pacotes
library(sf)
library(dplyr)
library(rwlts)

#Configurar o ambiente com as credenciais para acessar o servidor do Brazil Data Cube (acess token)
Sys.setenv(
  "BDC_ACCESS_KEY" = "inserir-chave-bdc"
)

# Configurando o sf para lidar com coordenadas planas
sf::sf_use_s2(F)

#Carregando shapefile com os centroides MCUT Brasil 1km² 
centroides_shp <- sf::st_read("~/R/centroides_ret_envolvente_intersec_BR", quiet = TRUE)
head(centroides_shp)


#Filtrando as áreas estáveis
pontos_estaveis <- filter(centroides_shp, USO2000== USO2010 & USO2010==USO2012 & USO2012==USO2014 & USO2014==USO2016 & USO2016==USO2018)
head(pontos_estaveis)
dim(pontos_estaveis)

#### Das áreas estáveis, extrair quantidades fixas (lotes de 500?) de amostras aleatórias sem reposição.
#### A ideia é buscar as trajetórias e depois rodar o SITS por partes, para não sobrecarregar, avaliar se é necessário. 
#### Método, a partir da função sample()? 

#Extraindo a lat_long do shapefile
lat_long <- as.data.frame(st_coordinates(pontos_estaveis))
head(lat_long)


#Chamando o serviço R_wlts
wlts_bdc <- "https://brazildatacube.dpi.inpe.br/wlts/"

#Listando a coleções disponíveis
rwlts::list_collections(wlts_bdc)

#Pegar o valor das classe do IBGE para o ano de 2018
time1 <- Sys.time()
time1
amostra_estaveis_ibge <- get_trajectory(wlts_bdc, 
                                         latitude = lat_long$Y, 
                                         longitude = lat_long$X, 
                                         collections = "ibge_cobertura_uso_terra")

amostra_ibge_2018 <- filter(amostra_estaveis_ibge$result, date == 2018)
time2 <- Sys.time()
time2
head(amostra_ibge_2018)

#Amostra Mapbiomasv. 6
time1 <- Sys.time()
time1
amostra_mapbiomas <- get_trajectory(wlts_bdc, 
                                     latitude = lat_long$Y, 
                                     longitude = lat_long$X, 
                                     collections = "mapbiomas-v6")

amostra_mapbiomas_2018 <- filter(amostra_mapbiomas$result, date == 2018)
time2 <- Sys.time()
time2
head(amostra_mapbiomas_2018)

#Amostra TerraClass Cerrado
time1 <- Sys.time()
time1
amostra_terraclass <- get_trajectory(wlts_bdc, 
                                      latitude = lat_long$Y, 
                                      longitude = lat_long$X, 
                                      collections = "terraclass_cerrado")

amostra_terraclass_2018 <- filter(amostra_terraclass$result, date == 2018)
time2 <- Sys.time()
time2
head(amostra_terraclass_2018)

#Olhar as amostras juntas em um mesmo dataframe

####OBS: fiz sucessivos merges, pois a amostra do MapBiomas em alguns casos apresentou quantidade menor de feições e com isso não consegui unificar o dataframe de uma só vez

merge <- merge(amostra_ibge_2018, amostra_mapbiomas_2018, by = "point_id")
View(merge)
merge2 <- merge(merge, amostra_terraclass_2018, by = "point_id")
View(merge2)

lat_long_id <- dplyr::mutate(lat_long, point_id = row_number())
dim(lat_long_id)

merge3 <- merge(merge2, lat_long_id, by="point_id")
head(merge3)

amostra_concordancia.df <- data.frame(point_id=merge3$point_id, latitude=merge3$Y, longitude=merge3$X, ibge_2018=merge3$class.x, mapbiomas_2018=merge3$class.y, terraclass_2018=merge3$class)
View(amostra_concordancia.df)


####Criar regras para "tradução" de legenda e criar campo novo "label" 
#### Há uma legenda Nível 3 para o mapeamento, mas sua "tradução" deve ser feita a partir do conjunto das combinações para aumentar a compatibilidade.
#### Há uma tabela de domínio das combinações em excel. Importar ela e buscar correspondências no dataframe gerado? Como fazer? 


#Exportar dataframe para csv
write.csv(amostra_concordancia10.df,"~/R/amostra_concordancia.csv", fileEncoding = "utf-8", row.names = FALSE)

## Referências:

# Rolf Simoes, Gilberto Camara, Felipe Souza, Pedro Andrade, Lorena Santos, Karine Ferreira, Gilberto
#Queiroz, Alexandre Carvalho, Victor Maus (2021), SITS: Data Analysis and Machine Learning using
#Satellite Image Time Series. URL https://github.com/e-sensing/sits.
citation("sits")
