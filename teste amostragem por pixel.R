##########################################
####### método de fatias finas (baseado na linha): concordância MB TC e estabilidade 2018 a 2020
### ATENÇÃO: esse script é somente uma prova de conceito, versão de produção deverá conter modificações 
# modifiçaões previstas: (1) ser baseado em mapeamento produzido com imagens Sentinel (ex. ESA World Cover); 
#                        (2) datas para estabilidade temporal devem ser discutidas e adequadas
#                        (3) dicionário de classes equivalente precisa ser implementado manualmente
#                        (4) remoção de agregados de pontos amostrados em classes raras
#                        (5) subdividir os biomas em regiões biogeográficas, climáticas ou ecológicas
#                        
#
# insumos necessários: 
# - classificação de Cobertura e Uso da Terra do Mabiomas (coleção 6 para anos de 2018 e 2020); TerraClass Cerrado 2018
# - mapa de Biomas IBGE

library(raster)
library(sf)
library(terra)


pastaGIS<-"/home/fred/GIS/" #será diferente em cada computador

### demorado -> rodar só na primeira vez
mb.2018<-rast(paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2018.tif")) #terra
mb.2020<-rast(paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2020.tif")) #terra
biomas<-st_read(paste(pastaGIS,"Biomas/Biomas_250mil/lm_bioma_250.shp",sep=""))
biomas<-st_transform(biomas,crs=st_crs(mb.2018))
cerrado<-biomas[biomas$Bioma=="Cerrado",]

cerrado<-vect(x=cerrado)
crop(mb.2018,snap="in",ext(cerrado),filename=paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2018_cer_ext.tif"))
mb.cerrado.prov<-rast(paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2018_cer_ext.tif"))
mask(mb.cerrado.prov,cerrado,filename=paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2018.mask.cer.tif"),overwrite=T)

crop(mb.2020,snap="in",ext(cerrado),filename=paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2020_cer_ext.tif"))
mb.cerrado.prov.2020<-rast(paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2020_cer_ext.tif"))
mask(mb.cerrado.prov.2020,cerrado,filename=paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2020.mask.cer.tif"),overwrite=T)
### FIM: demorado -> rodar só na primeira vez

#raster
mb.mask<-raster(paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2018.mask.cer.tif"))
mb.mask.2020<-raster(paste0(pastaGIS,"MapBiomas/coleção 6/brasil_coverage_2020.mask.cer.tif"))
tc<-raster(paste0(pastaGIS,"TerraClass/CER.2018.M/CER.2018.tif")) #raster

### amostragem propriamente dita
fatias<-100 #número de fatias: número influencia memória RAM utilizada (com pequena penalidade de tempo de processamento)
n.fatia<-5 #número de amostras desejadas por fatia
max.fatia<-10 #número de pré-selecionados por fatia (para comparar com outro mapeamento)
n.total<-200 #número de amostras em toda a área para a classe
classe.mb<-3 #valor da classe MapBiomas em questão
classe.tc<-c(1,2) #valores equivalentes na classificação do Terra Class

amostras<-data.frame(x=NULL,y=NULL,fatia=NULL,classe.mb=NULL)
contador<-0
min.linhas.fatia<-floor(seq(from=1,to=nrow(mb.mask),length.out=fatias+1))

for (i in 1:fatias){
  print(paste("-------------------iniciando fatia", i))
  
  n.linhas.fatia.i<-min.linhas.fatia[i+1]-min.linhas.fatia[i] #tamanho é ligeiramente irregular devido ao arredendamento
  fatia.i<-getValuesBlock(mb.mask,row=min.linhas.fatia[i],nrows=n.linhas.fatia.i,col=1,ncols=ncol(mb.mask),format="matrix")
  fatia.i.sel<-which(fatia.i==classe.mb,arr.ind=T)
  
  #compara
  n.i.sel<-nrow(fatia.i.sel)
  
  if(n.i.sel>0 & n.i.sel<max.fatia){
    fatia.i.sel.am<-fatia.i.sel[sample(1:n.i.sel,size=n.i.sel),]
    xy.sel.am<-data.frame(x=xFromCol(mb.mask,col=fatia.i.sel.am[,2]),y=yFromRow(mb.mask,row=(fatia.i.sel.am[,1]+min.linhas.fatia[i]-1)))
  }
  
  if(n.i.sel>max.fatia){
    fatia.i.sel.am<-fatia.i.sel[sample(1:n.i.sel,size=max.fatia),]
    xy.sel.am<-data.frame(x=xFromCol(mb.mask,col=fatia.i.sel.am[,2]),y=yFromRow(mb.mask,row=(fatia.i.sel.am[,1]+min.linhas.fatia[i]-1)))
  }
  
  for (j in 1:max.fatia){
    print(paste("fatia",i,"amostra",j,"valor mb",extract(mb.mask,xy.sel.am[j,])))
    
    if(n.i.sel==0){
      print("sem pixels da categoria nessa fatia")
      break
    }
    
    if(extract(tc,xy.sel.am[j,])%in%classe.tc){
      if(extract(mb.mask.2020,xy.sel.am[j,])%in%classe.mb){
        contador<-contador+1
        amostras<-rbind(amostras,data.frame(x=xy.sel.am$x[j],y=xy.sel.am$y[j],fatia=i,classe.mb=classe.mb))
        
        if(contador==n.fatia){
          contador<-0
          break
        }
      } else {print("amostra descartada: mudança entre MB 2018 e 2020")}
      
      if(j==max.fatia){print(paste("limite atingido com somente",nrow(amostras[amostras$fatia==i,]),"pontos de concordância"))}
      
    }
  }
}

#TODO: remover amostras que ficaram muito perto uma da outra. Situação deve ocorrer em classes muito raras e que são agrupados.
#      Isso geraria agrupados de pixels em cada fatia em que é rara.

if(nrow(amostras)>n.total){
  amostras.reduzido<-amostras[sample(1:nrow(amostras),size=n.total),]
  print("use objeto amostras.reduzido")} else{print("use objeto amostras")}




