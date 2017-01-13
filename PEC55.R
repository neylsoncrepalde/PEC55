#########################################
# PEC 55
# Big Data Project
# Neylson Crepalde e Maria Alice Silveira
#########################################


################################
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

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

dataset = c(rest$text, stream$text)
head(dataset)

#############################################
#Análises de texto
library(tm)
library(wordcloud)
library(magrittr)

dataset_tm = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", dataset)
dataset_tm <- gsub("https", "", dataset_tm)
dataset_tm <- gsub("http", "", dataset_tm)
grep("http", dataset_tm)

dataset_tm <- dataset_tm %>% tolower %>% removePunctuation %>% removeWords(., stopwords('pt'))
head(dataset_tm)

pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]

wordcloud(dataset_tm, min.freq = 5, random.order = F, colors = pal, max.words = 100)

corpus = Corpus(VectorSource(dataset_tm))
tdm = TermDocumentMatrix(corpus)
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.96)
df <- as.data.frame(inspect(tdm))
dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2)

rect.hclust(fit.ward2, k=7)

library(igraph)
matriz <- as.matrix(df)
g <- graph_from_incidence_matrix(matriz)
is.bipartite(g)
g
plot(g, vertex.size=4, vertex.label=V(g)$name, vertex.color=as.numeric(V(g)$type))
g2 <- bipartite_projection(g, which = "FALSE")
deg = degree(g2)
plot(g2, edge.width=log(E(g2)$weight), vertex.label.cex=deg/15,
     edge.color=adjustcolor("grey60", .5),
     vertex.label.color=adjustcolor("blue", .7),
     vertex.shape="none")

##################################################
# Preparando dataset para traduzir no Python

write.table(dataset, '/home/neylson/Documentos/Neylson Crepalde/Doutorado/big_data_projects/PEC55/dataset.csv',
            sep=',', row.names = F, col.names = 'pt')

###############################################
# Importando os tweets traduzidos

dataset_traduzido = read_csv('/home/neylson/Documentos/Neylson Crepalde/Doutorado/big_data_projects/PEC55/dataset_traduzido2.csv')
View(dataset_traduzido)

#################################################
# Análise de sentimentos

library(syuzhet)
library(reshape2)
library(coreNLP)
library(ggplot2)
library(ggthemes)
emotions_nrc = get_nrc_sentiment(dataset_traduzido$en)
sentiments.syu = get_sentiment(dataset_traduzido$en, method = 'syuzhet')
sentiments.bing = get_sentiment(dataset_traduzido$en, method = 'bing')
sentiments.afinn = get_sentiment(dataset_traduzido$en, method = 'afinn')
sentiments.nrc = get_sentiment(dataset_traduzido$en, method = 'nrc')

#Plotando emotions_nrc
col = '#F5A9A9'
par(mfrow=c(2,1))
barplot(
  sort(colSums(prop.table(emotions_nrc[ ,9:10]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1,
  col = col,
  main = "Sentimentos", xlab="Porcentagem"
)

barplot(
  sort(colSums(prop.table(emotions_nrc[ ,1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1,
  col = col,
  main = "Emoções", xlab="Porcentagem"
)
par(mfrow=c(1,1))

#Plotando sentimentos
g1 = ggplot(NULL, aes(x=1:6193, y=sentiments.syu))+geom_line()+
  geom_hline(yintercept = 0, col = 3, lty = 2)+
  theme_tufte()+labs(x='',y='Sentiment score',title='Sentiment Analysis - Syuzhet')
g2 = ggplot(NULL, aes(x=1:6193, y=sentiments.bing))+geom_line()+
  geom_hline(yintercept = 0, col = 3, lty = 2)+
  theme_tufte()+labs(x='',y='Sentiment score',title='Sentiment Analysis - Bing')
g3 = ggplot(NULL, aes(x=1:6193, y=sentiments.afinn))+geom_line()+
  geom_hline(yintercept = 0, col = 3, lty = 2)+
  theme_tufte()+labs(x='',y='Sentiment score',title='Sentiment Analysis - Arup Finn')
g4 = ggplot(NULL, aes(x=1:6193, y=sentiments.nrc))+geom_line()+
  geom_hline(yintercept = 0, col = 3, lty = 2)+
  theme_tufte()+labs(x='',y='Sentiment score',title='Sentiment Analysis - Mohammad-Turney')

multiplot(g1,g2,g3,g4, cols=2)

#############################################
# Sentiment Analysis com machine learning
library(RTextTools)
library(e1071)





