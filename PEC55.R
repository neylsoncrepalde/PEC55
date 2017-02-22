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

###############
# Análise de conteúdo FACEBOOK

library(tm)
library(wordcloud)
library(magrittr)

names(event)
head(event$description)
names(page)
head(page$description)

descricoes = c(event$description, page$description)
length(descricoes)

texto = descricoes %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords('pt')) %>% removeWords(., c('pec','241','55', '24155'))

pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]

wordcloud(texto, min.freq = 3, random.order = F, colors = pal, max.words = 100)

corpus = Corpus(VectorSource(texto))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.90)
df <- as.data.frame(inspect(tdm))
dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2)

rect.hclust(fit.ward2, k=8)

###################################################
# Analise de sentimentos Facebook

mbl = read_tsv('/home/neylson/Documentos/Neylson Crepalde/Doutorado/Artigos meus/PEC55/PEC55 - dados/MBL/page_204223673035117_2017_02_17_15_59_08_topcomments.tab')
endireita_brasil = read_tsv('/home/neylson/Documentos/Neylson Crepalde/Doutorado/Artigos meus/PEC55/PEC55 - dados/Endireita Brasil/page_97663407343_2017_02_16_01_46_51_comments.tab')
vem_pra_rua = read_tsv('/home/neylson/Documentos/Neylson Crepalde/Doutorado/Artigos meus/PEC55/PEC55 - dados/Vem pra Rua/page_344408492407172_2017_02_16_01_45_04_comments.tab')

direita = rbind(mbl, endireita_brasil[,c(2,4:6,9:11)], vem_pra_rua[,c(2,4:6,9:11)])

une = read_tsv('/home/neylson/Documentos/Neylson Crepalde/Doutorado/Artigos meus/PEC55/PEC55 - dados/UNE/page_241149405912525_2017_02_16_01_53_23_comments.tab')
midia_ninja = read_tsv('/home/neylson/Documentos/Neylson Crepalde/Doutorado/Artigos meus/PEC55/PEC55 - dados/Mídia Ninja/page_164188247072662_2017_02_16_02_08_36_comments.tab')
jornalistas_livres = read_tsv('/home/neylson/Documentos/Neylson Crepalde/Doutorado/Artigos meus/PEC55/PEC55 - dados/Jornalistas Livres/page_292074710916413_2017_02_16_02_30_42_comments.tab')

esquerda = rbind(une[,c(2,4:6,9:11)], midia_ninja[,c(2,4:6,9:11)], jornalistas_livres[,c(2,4:6,9:11)])

#Posts
direita_posts = direita$post_text %>% tolower %>% 
  gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", .) %>% removePunctuation %>%
  removeWords(., stopwords('pt'))
esquerda_posts = esquerda$post_text %>% tolower %>% 
  gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", .) %>% removePunctuation %>%
  removeWords(., stopwords('pt'))

wordcloud(direita_posts, min.freq = 3, max.words = 100, random.order = F, colors = pal)
wordcloud(esquerda_posts, min.freq = 3, max.words = 100, random.order = F, colors = pal)

corpus = Corpus(VectorSource(direita_posts))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.90)
df <- as.data.frame(inspect(tdm))
dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2)

rect.hclust(fit.ward2, k=6)############
###

corpus = Corpus(VectorSource(esquerda_posts))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.85)
df <- as.data.frame(inspect(tdm))
dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2)

rect.hclust(fit.ward2, k=6)############




#Comentários
direita_coments = direita$comment_message %>% tolower %>% 
  removePunctuation %>% removeWords(., stopwords('pt'))
esquerda_coments = esquerda$comment_message %>% tolower %>% 
  removePunctuation %>% removeWords(., stopwords('pt'))

wordcloud(direita_coments, min.freq = 3, max.words = 100, random.order = F, colors = pal)
wordcloud(esquerda_coments, min.freq = 3, max.words = 100, random.order = F, colors = pal)

###
corpus = Corpus(VectorSource(direita_coments))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.94)
df <- as.data.frame(inspect(tdm))
dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2)

rect.hclust(fit.ward2, k=6)############
###

corpus = Corpus(VectorSource(esquerda_coments))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.85)
df <- as.data.frame(inspect(tdm))
dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2)

rect.hclust(fit.ward2, k=6)############


#########
#Escrevendos dados para tradução automática

setwd('~/Documentos/Neylson Crepalde/Doutorado/Artigos meus/PEC55')
names(direita_posts) = 'pt'
names(esquerda_posts) = 'pt'
names(direita_coments) = 'pt'
names(esquerda_coments) = 'pt'
#write.csv(direita_posts, 'direita_posts.csv', row.names = F)
#write.csv(esquerda_posts, 'esquerda_posts.csv', row.names = F)
#write.csv(direita_coments, 'direita_coments.csv', row.names = F)
#write.csv(esquerda_coments, 'esquerda_coments.csv', row.names = F)

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
  theme_tufte()+labs(x='',y='Sentiment score',title='Sentiment Analysis - AFinn')
g4 = ggplot(NULL, aes(x=1:6193, y=sentiments.nrc))+geom_line()+
  geom_hline(yintercept = 0, col = 3, lty = 2)+
  theme_tufte()+labs(x='',y='Sentiment score',title='Sentiment Analysis - NRC')

multiplot(g1,g2,g3,g4, cols=2)

lapply(list(sentiments.syu, sentiments.bing, sentiments.afinn, sentiments.nrc), summary)

sentiments.df = data.frame(sentiments.syu, sentiments.bing, sentiments.afinn, sentiments.nrc)
stargazer::stargazer(sentiments.df, type='text')

#############################################
# Sentiment Analysis com machine learning
library(RTextTools)
library(e1071)

#Classificando manualmente 100 tweets
dataset[1:100]
class_manual = c('neutro','contra','contra','contra','contra','contra','contra','contra','contra','contra',
                 'contra','contra','contra','contra','contra','neutro','neutro','contra','a favor','a favor',
                 'contra','neutro','a favor','contra','a favor','neutro','contra','neutro','contra','a favor',
                 'a favor','neutro','contra','neutro','contra','neutro','neutro','neutro','neutro','contra',
                 'neutro','contra','neutro','neutro','contra','contra','contra','contra','a favor','neutro',
                 'contra','a favor','contra','contra','neutro','a favor','contra','a favor','contra','a favor',
                 'contra','contra','neutro','contra','contra','contra','a favor','contra','contra','contra',
                 'neutro','contra','contra','a favor','contra','a favor','contra','a favor','neutro','neutro',
                 'contra','contra','a favor','contra','contra','contra','neutro','contra','neutro','neutro',
                 'a favor','contra','a favor','a favor','contra','a favor','contra','contra','contra','contra')
class_manual = as.factor(class_manual)

#Tornando a classificação binária
class_binario = c()
for (i in class_manual){
  if (i != 'contra'){
    cla = 'não contra'
    class_binario = c(class_binario, cla)
  }
  else{
    cla = 'contra'
    class_binario = c(class_binario, cla)
  }
}
class_binario %<>% as.factor

#Treinando o modelo
train = dataset[1:100]
test = dataset[101:length(dataset)]

matrix= create_matrix(train, language="pt", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE) %>% as.matrix

classifier = naiveBayes(x=matrix[1:70,], y=class_binario[1:70])
predicted = predict(classifier, matrix[71:100]); predicted
table(predicted, class_binario[71:100])
recall_accuracy(predicted, class_binario[71:100])
freq(predicted)

###############################################
# tentando outros algoritmos de machine learning
container = create_container(matrix, class_binario, trainSize = 1:70, testSize = 71:100, virgin = F)
models = train_models(container, algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE", "GLMNET"))
results = classify_models(container, models)

# Tabelas de acurácia e 
table(results$MAXENTROPY_LABEL, class_binario[71:100])
recall_accuracy(results$MAXENTROPY_LABEL, class_binario[71:100])
table(results$SVM_LABEL, class_binario[71:100])
recall_accuracy(results$SVM_LABEL, class_binario[71:100])
table(results$FORESTS_LABEL, class_binario[71:100])
recall_accuracy(results$FORESTS_LABEL, class_binario[71:100])
table(results$BAGGING_LABEL, class_binario[71:100])
recall_accuracy(results$BAGGING_LABEL, class_binario[71:100])
table(results$TREE_LABEL, class_binario[71:100])
recall_accuracy(results$TREE_LABEL, class_binario[71:100])
table(results$GLMNET_LABEL, class_binario[71:100])
recall_accuracy(results$GLMNET_LABEL, class_binario[71:100])

#cross-validation
N=4
set.seed(2014)
cross_validate(container,N,"MAXENT")
cross_validate(container,N,"TREE")
cross_validate(container,N,"SVM")
cross_validate(container,N,"RF")
cross_validate(container,N,"GLMNET")

#Os melhores são Máxima Entropia e Support Vector Machines
# Vamos aos dados
matrix2 = create_matrix(dataset, language="pt", 
                        removeStopwords=FALSE, removeNumbers=TRUE, 
                        stemWords=FALSE) %>% as.matrix

container2 = create_container(matrix2, class_binario, trainSize = 1:100, testSize = 101:length(dataset), virgin = F)
models2 = train_models(container2, algorithms=c("MAXENT" , "SVM", "RF"))
results2 = classify_models(container2, models2)

freq(results2$SVM_LABEL,plot=F)
freq(results2$MAXENTROPY_LABEL,plot=F)
freq(results2$FORESTS_LABEL,plot=F)  #WOW!

xSVM = matrix(data = c(6.33,93.67,29.64,70.36), ncol=2, byrow=T)
xMAXENT = matrix(data = c(6.33,93.67,48.13,51.87), ncol=2, byrow=T)
xRF = matrix(data = c(6.33,93.67,4.194,95.806), ncol=2, byrow=T)

prop.test(xSVM, correct = F)
prop.test(xMAXENT, correct = F)
prop.test(xRF, correct = F)
