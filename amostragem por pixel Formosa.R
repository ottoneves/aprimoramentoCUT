##########################################
####### método de polígono (baseado no método de fatias): concordância ESA com MB & estabilidade MB 2018 com MB 2020
### ATENÇÃO: esse script é somente uma prova de conceito, versão de produção deverá conter modificações 
# modifiçaões previstas:  
#                        (1) datas para estabilidade temporal devem ser discutidas e adequadas
#                        (2) dicionário de classes equivalente precisa confirmado
#                        (3) subdividir os biomas em regiões biogeográficas, climáticas ou ecológicas
#                        
#
# insumos necessários: 
# - classificação de Cobertura e Uso da Terra:
#       Mabiomas (coleção 6 para anos de 2018 e 2020); 
#       ESA WorldCover
# - shapefile de Formosa

library(raster)
library(sf)
library(terra)
library(units)


pastaGIS<-"/home/fred/GIS/" #será diferente em cada computador

poligono<-st_read(paste(pastaGIS,"Formosa-GO/agFormosa.shp",sep=""))


### demorado -> rodar só na primeira vez (pacote terra)
mb.2018.ori<-rast(paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2018.tif"))
mb.2020.ori<-rast(paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2020.tif"))

poligono<-st_transform(poligono,crs=st_crs(mb.2018.ori))
poligono<-vect(x=poligono)

esa1<-rast(paste0(pastaGIS,"ESA/ESA_WORLDCOVER_10M_2020_V100/MAP/ESA_WorldCover_10m_2020_v100_S15W048_Map/ESA_WorldCover_10m_2020_v100_S15W048_Map.tif"))
esa2<-rast(paste0(pastaGIS,"ESA/ESA_WORLDCOVER_10M_2020_V100/MAP/ESA_WorldCover_10m_2020_v100_S15W051_Map/ESA_WorldCover_10m_2020_v100_S15W051_Map.tif"))
esa3<-rast(paste0(pastaGIS,"ESA/ESA_WORLDCOVER_10M_2020_V100/MAP/ESA_WorldCover_10m_2020_v100_S18W048_Map/ESA_WorldCover_10m_2020_v100_S18W048_Map.tif"))
esa4<-rast(paste0(pastaGIS,"ESA/ESA_WORLDCOVER_10M_2020_V100/MAP/ESA_WorldCover_10m_2020_v100_S18W051_Map/ESA_WorldCover_10m_2020_v100_S18W051_Map.tif"))
esa.mos<-terra::mosaic(x=esa1,esa2,esa3,esa4,fun="min",filename=paste0(pastaGIS,"ESA/ESA_WORLDCOVER_10M_2020_V100/MAP/mosaicoFormosa.tif"))

crop(esa.mos,snap="in",ext(poligono),filename=paste0(pastaGIS,"ESA/ESA_WORLDCOVER_10M_2020_V100/MAP/mosaicoFormosa_pol_ext.tif"))
esa.poligono.prov.2020<-rast(paste0(pastaGIS,"ESA/ESA_WORLDCOVER_10M_2020_V100/MAP/mosaicoFormosa_pol_ext.tif"))
mask(esa.poligono.prov.2020,poligono,filename=paste0(pastaGIS,"ESA/ESA_WORLDCOVER_10M_2020_V100/MAP/mosaicoFormosa_mask_pol.tif"),overwrite=T)

rm(esa1,esa2,esa3,esa4,esa.mos,esa.poligono.prov.2020)
gc()

crop(mb.2018.ori,snap="in",ext(poligono),filename=paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2018_formosa_ext.tif"))
mb.poligono.prov.2018<-rast(paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2018_formosa_ext.tif"))
mask(mb.poligono.prov.2018,poligono,filename=paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2018_mask_formosa.tif"),overwrite=T)

crop(mb.2020.ori,snap="in",ext(poligono),filename=paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2020_formosa_ext.tif"))
mb.poligono.prov.2020<-rast(paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2020_formosa_ext.tif"))
mask(mb.poligono.prov.2020,poligono,filename=paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2020_mask_formosa.tif"),overwrite=T)

rm(mb.2018.ori,mb.2020.ori,esa.2020.ori,mb.poligono.prov.2018,mb.poligono.prov.2020)
gc()
########### FIM: demorado -> rodar só na primeira vez

#raster
mb.2018<-raster(paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2018_mask_formosa.tif"))
mb.2020<-raster(paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2020_mask_formosa.tif"))
esa.2020<-raster(paste0(pastaGIS,"ESA/ESA_WORLDCOVER_10M_2020_V100/MAP/mosaicoFormosa_mask_pol.tif"))

#####################################
### amostragem propriamente dita
#####################################
fatias<-3 #número de fatias: número influencia memória RAM utilizada (com pequena penalidade de tempo de processamento)
n.fatia<-c(100,100,100,100,100,100,100,100) #número de amostras desejadas por fatia por classe ESA
porc.extra.n.fatia<-c(50,50,500,50,50,700,50,500) #porcentagem extra (em cada classe) na pré-seleção para compensar perdas na comparação com outro mapeamento e instabilidade temporal
n.total<-c(200,200,200,200,200,200,200,200) #número máximo de amostras em toda a área para cada classe
d.min<-5000 #distância mínima (m) entre pontos

classe.esa.vetor<-c(10,20,30,40,50,60,80,90) #valores das classes ESA WorldCover desejados
classe.mb.lista<-list(esa1=c(3),esa2=c(4),esa3=c(12),esa4=c(18,19,39,20,40,41,36,46,47,48),esa5=c(24),esa6=c(30,25),esa7=c(33,31),esa8=c(11))
#               valores do MapBiomas equivalentes aos da classificação da ESA WorldCover 
#               ATENÇÃO: manter a sequência de posições com vetor classe.ESA.vetor

# ATENÇÃO: equivalência entre MB e ESA nesse contexto deve ser orientada ao objetivo. 
# ex. apesar de ESA "Tree Cover" englobar conceitualmente MB "Forest Plantation" (silvicultura),
# se o objetivo for obter amostras para classe IBGE "Vegetação florestal", deve ser deixado de fora o MB "Forest Plantation"

verbose<-T

(tabela.n<-data.frame(classe.esa=classe.esa.vetor,n.fatia=n.fatia,porc.extra=porc.extra.n.fatia,n.total=n.total))
corresp.lista<-list()
for(a in 1:length(classe.esa.vetor)){
  corresp.lista[[paste("ESA",classe.esa.vetor[a])]][1:length(classe.mb.lista[[a]])]<-classe.mb.lista[[a]]
}
corresp.lista #para conferir o preenchimento


amostras<-data.frame(x=NULL,y=NULL,fatia=NULL,classe.esa=NULL)
contador<-0
min.linhas.fatia<-floor(seq(from=1,to=nrow(esa.2020),length.out=fatias+1))
inicio<-Sys.time()
for (classe.esa in classe.esa.vetor){
  if(verbose==T){print(paste("###################### Iniciando classe:",classe.esa))}
  
  for (i in 1:fatias){
    if(verbose==T){print(paste("-------------------iniciando fatia", i))}
    
    n.linhas.fatia.i<-min.linhas.fatia[i+1]-min.linhas.fatia[i] #tamanho é ligeiramente irregular devido ao arredondamento
    fatia.i<-getValuesBlock(esa.2020,row=min.linhas.fatia[i],nrows=n.linhas.fatia.i,col=1,ncols=ncol(esa.2020),format="matrix")
    fatia.i.sel<-which(fatia.i==classe.esa,arr.ind=T)
    
    n.i.sel<-nrow(fatia.i.sel)
    if(verbose==T){print(paste("Número de pixels na categoria:", n.i.sel))}
    
    n.desejado<-tabela.n$n.fatia[tabela.n$classe.esa==classe.esa]
    extra<-tabela.n$porc.extra[tabela.n$classe.esa==classe.esa]

    max.fatia<-round(n.desejado+(n.desejado*(extra/100)))
    
    if(n.i.sel>0 & n.i.sel<max.fatia){
      fatia.i.sel.am<-fatia.i.sel[sample(1:n.i.sel,size=n.i.sel),]
      xy.sel.am<-data.frame(x=xFromCol(esa.2020,col=fatia.i.sel.am[,2]),y=yFromRow(esa.2020,row=(fatia.i.sel.am[,1]+min.linhas.fatia[i]-1)))
    }
    
    if(n.i.sel>max.fatia){
      fatia.i.sel.am<-fatia.i.sel[sample(1:n.i.sel,size=max.fatia),]
      xy.sel.am<-data.frame(x=xFromCol(esa.2020,col=fatia.i.sel.am[,2]),y=yFromRow(esa.2020,row=(fatia.i.sel.am[,1]+min.linhas.fatia[i]-1)))
    }
    
    rm(fatia.i,fatia.i.sel)
    gc()
    
    for (j in 1:max.fatia){
      if(verbose==T){print(paste("fatia",i,"amostra",j,"valor ESA",extract(esa.2020,xy.sel.am[j,])))}
      
      if(n.i.sel==0){
        if(verbose==T){print("sem pixels da categoria nessa fatia")}
        break
      }
      val.mb.2020j<-extract(mb.2020,xy.sel.am[j,])
      if(val.mb.2020j%in%classe.mb.lista[[which(classe.esa.vetor==classe.esa)]]){             #checa concordância ESAxMB
        
        if(extract(mb.2018,xy.sel.am[j,])%in%classe.mb.lista[[which(classe.esa.vetor==classe.esa)]]){         #checa estabilidade entre 2018 e 2020 no MB
          contador<-contador+1
          amostras<-rbind(amostras,data.frame(x=xy.sel.am$x[j],y=xy.sel.am$y[j],fatia=i,classe.esa=classe.esa))
          
          if(contador==n.desejado){
            contador<-0
            break
          }
        } else {
          if(verbose==T){print("amostra descartada: mudança entre 2018 e 2020 no MB")}
          }
        
        if(j==max.fatia){
          if(verbose==T){print(paste("limite atingido com somente",contador,"pontos de concordância estáveis"))}
        }
      } else { if(verbose==T){print(paste("amostra descartada: MB2020 discorda de ESA na coordenada",xy.sel.am[j,1],xy.sel.am[j,2],"MB =",val.mb.2020j)) } }
    }
  }
}


#remoção de pontos muito próximos
amostras.sf<-st_as_sf(amostras,coords=c("x","y"),crs=st_crs(esa.2020))
m.dist<-st_distance(amostras.sf)
units(d.min)<-as_units("m")
diag(m.dist)<-NA
m.dist[lower.tri(m.dist)]<-NA
perto<-which(m.dist<d.min,arr.ind=T)
amostras<-amostras[-perto[,1],]

#removendo amostras excedentes
for (i in 1:length(classe.esa.vetor)){
  n.classe.i<-nrow(amostras[amostras$classe.esa==classe.esa.vetor[i],])
  excedente<-n.classe.i-tabela.n$n.total[i]
  
  print(paste("excedente na classe ESA",classe.esa.vetor[i],"é de",excedente,"pontos (números negativos indicam menos pontos do que desejado)"))
    
  if(excedente>0){
    amostras<-amostras[-sample(which(amostras$classe.esa==classe.esa.vetor[i]),size=excedente),]
  }
}



metadados<-list(execut=data.frame(inicio=inicio, fim=Sys.time(),tempo=Sys.time()-inicio),
                poligono=print(poligono),fatias=fatias,n=tabela.n,correpondencia=corresp.lista, 
                n=table(amostras$classe.esa))

write.csv(amostras,"amostrasESA.csv")
capture.output(print(metadados), file = "amostrasESA_metadados.txt")

