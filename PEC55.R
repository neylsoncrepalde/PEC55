#########################################
# PEC 55
# Big Data Project
# Neylson Crepalde e Maria Alice Silveira
#########################################

library(readr)
library(descr)

event = read_tsv('/home/neylson/Documentos/Neylson Crepalde/Doutorado/big_data_projects/PEC55_search_event_2016_11_21_12_55_21.tab')
page  = read_tsv('/home/neylson/Documentos/Neylson Crepalde/Doutorado/big_data_projects/PEC55_search_page_2016_11_21_12_52_13.tab')

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

#Plotando
library(maptools)
estados = readShapePoly('/home/neylson/Documentos/Neylson Crepalde/Doutorado/Artigos meus/PEC55/estados_2010.shp')
map(estados, col="#191919", fill=TRUE, bg="#000000", lwd=0.08)
map(estados, col="#f2f2f2", fill=TRUE, bg="white", lwd=0.08)
