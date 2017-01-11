#########################################
# PEC 55
# Big Data Project
# Neylson Crepalde e Maria Alice Silveira
#########################################

#########
#Facebook
#########
library(readr)
library(descr)

event = read_tsv('/home/neylson/Documentos/Neylson Crepalde/Doutorado/big_data_projects/PEC55/PEC55_search_event_2016_11_21_12_55_21.tab')
page  = read_tsv('/home/neylson/Documentos/Neylson Crepalde/Doutorado/big_data_projects/PEC55/PEC55_search_page_2016_11_21_12_52_13.tab')

# classificando os eventos
event$afavor = c('contra','neutro','neutro','contra','contra','contra','neutro','contra','contra','contra','neutro','contra','contra', #13
                 'neutro','contra','neutro','contra','neutro','contra','contra','contra','contra','contra','contra','neutro','contra', #26
                 'contra','neutro','neutro','contra','contra','contra','contra','contra','contra','contra','contra','contra','contra', #39
                 'neutro','contra','contra','contra','contra','contra','contra','a favor','contra','contra','contra','contra','contra', #52
                 'contra','contra','contra','contra','contra','contra','neutro','contra')
length(event$afavor)
freq(event$afavor)

# retirando as páginas que não são sobre a PEC 55
page = page[1:44,]
page = page[-7,]
page = page[-14,]
page = page[-26,]
page = page[-30,]
page = page[-32,]

# classificando as páginas
page$afavor = c('neutro','contra','contra','contra','neutro','contra','contra','contra','contra','contra','contra','contra','contra', #13
                'contra','contra','contra','contra','contra','contra','contra','contra','contra','contra','contra','contra','neutro', #26
                'contra','contra','contra','contra','contra','contra','contra','contra','contra','contra','contra','contra','contra')
length(page$afavor)
freq(page$afavor)

# Levando em conta a contagem de curtidas na página
page.fan = c()
for (i in 1:nrow(page)){
  count = rep(page$afavor[i], page$fan_count[i])
  page.fan = c(page.fan, count)
}
freq(page.fan)

# Levando em conta a contagem de confirmações de participação e
# demonstração de interesse nos eventos
event.concordancia = c()
for (i in 1:nrow(event)){
  concord = event$attending_count[i] + event$interested_count[i]
  count = rep(event$afavor[i], concord)
  event.concordancia = c(event.concordancia, count)
}
freq(event.concordancia)


#Verificando o espaço de tempo para os eventos
min(event$start_time)

#testes estatísticos com as proporções

x_evento = matrix(data = c(6.33,93.67,12.9231, 87.0769), ncol = 2, byrow = T)
t1 = prop.test(x_evento, correct = F) #não rejeita a H0 de proporções iguais
x_page = matrix(data = c(6.33,93.67,5.127,94.873), ncol=2, byrow = T)
t2 = prop.test(x_page, correct = F) #não rejeita a H0 de proporções iguais




#Plotando
library(maptools)
library(maps)

estados = readShapePoly('/home/neylson/Documentos/Neylson Crepalde/Doutorado/Artigos meus/PEC55/estados_2010.shp')
map(estados, col="#191919", fill=TRUE, bg="#000000", lwd=0.08)
#map(estados, col="#f2f2f2", fill=TRUE, bg="white", lwd=0.08)
title('Eventos - PEC 55 - Facebook', col.main="white", cex.main=1)

for (row in 1:nrow(event)){
  if (event$afavor[row] == 'contra'){
    points(event$lng[row], event$lat[row], pch=19, col=adjustcolor('red', 0.9), cex = 0.8)
  }
  if (event$afavor[row] == 'neutro'){
    points(event$lng[row], event$lat[row], pch=19, col=adjustcolor('green', 0.7), cex = 0.8)
  }
  else{
    points(event$lng[row], event$lat[row], pch=19, col=adjustcolor('lightblue', 0.5), cex = 0.8)
  }
}

legend('bottomleft', pch = 19, col= c(adjustcolor('red', 0.9),adjustcolor('green', 0.9),adjustcolor('lightblue', 0.9)),
       legend = c('Contra','Neutro','A Favor'), text.col = 'white', bg = 'black')

#
library(ggmap)
bra = get_map(location = 'Brazil', zoom = 4)
mapPoints <- ggmap(bra)+geom_point(aes(x = lng, y = lat, color=afavor),
                                   data = event, size=.6)+
  scale_color_manual(values=c("#0080FF", "#FE2E2E", "#0B6121"), 
                     name="Posicionamento", alpha = .5)
mapPoints


########
#Twitter
########
library(jsonlite)

rest = read_csv2('/home/neylson/Documentos/Neylson Crepalde/Doutorado/big_data_projects/PEC55/PEC55_rest.txt')

stream = fromJSON(sprintf("[%s]", 
    paste(readLines("/home/neylson/Documentos/Neylson Crepalde/Doutorado/big_data_projects/PEC55/PEC55_stream_21_11_2016_12_23.txt"),
          collapse=",")))

names(rest)
names(stream)
