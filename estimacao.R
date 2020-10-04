
setwd("C:/Users/Davi/Desktop/universidade/3_semestre/metodos_2/saeb/")


pacman::p_load(tidyverse,ggplot2,plotly,gapminder,stringr,gmodels)

data <- read.csv("amostra_190026570.csv", encoding = "UTF-8")


## Amostras ####


#sortendo as amostras de tamanho 30
amostras30 <- lapply(seq(1,50), function(x){
  sample(seq(1,2000),30)
  
  })

#sortendo as amostras de tamanho 100
amostras100 <- lapply(seq(1,50), function(x){
  sample(seq(1,2000),100)
  
})


#junta o tamanho da amostra
amostras <- c(amostras100,amostras30)


#cor
cor <- "#215493"


#padrao graficos
padrao <- list(
  theme_minimal(),
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")),
  theme(legend.position="top"))


## Comparação do Parâmetro ####
comp_par<-function(data,parametro){
  return(sapply(1:50, function(x){
    if((parametro > data[50+x,"Upper"]) && (parametro < data[100+x,"Upper"])){
      return("Contido")
    }
    
    if((parametro < data[50+x,"Upper"]) | (parametro > data[100+x,"Upper"])){
      return("Não Contido")
    }
    
  }))}


## label do eixo x

eixo_x <- c("Menor Valor", "Estimativa", "Maior Valor")



## area ####





parametro_area <- as.numeric(table(data$AREA)[2]/sum(table(data$AREA)[1],table(data$AREA)[2]))





#dataframe com os valores do intervalo de confianÃÂ§a
#da variÃÂ¡vel area, e o tamanho da amostra
area<-sapply(amostras,function(amostra){
  
  if(length(amostra) == 30){
    c <- data[amostra,"AREA"]
    c <- as.numeric(gsub("Interior", 1, gsub("Capital", 0, c)))
    c <- ci.binom(c,0.95)
    c[5] <- 30
    return(c)
  }
  
  if(length(amostra) == 100){
    c <- data[amostra,"AREA"]
    c <- as.numeric(gsub("Interior", 1, gsub("Capital", 0, c)))
    c <- ci.binom(c,0.95)
    c[5] <- 100
    return(c)
  }
  
  
      
}) %>% as.data.frame(.) %>% t

area <- as.data.frame(area) %>% 
  rename("Estimativa" = "V1",
         "Lower" = "V2",
         "Upper" = "V3",
         "Desvio" = "V4",
         "Tamanho da Amostra" = "V5")










#boxplot do IC da variavel area de amostras com tamanho 30

map_area_30 <- area%>% 
  filter(`Tamanho da Amostra` == 30) %>% 
  select(Lower,Upper,Estimativa) %>% 
  gather(Lower,Upper,Estimativa,1:3)

  
levels <- c("Lower","Estimativa","Upper")

map_area_30$Lower <- factor(map_area_30$Lower, levels = levels)

ggplot(map_area_30,aes(x= Lower, y=Upper))+
  geom_boxplot(fill=c(cor), width = 0.5) +
  scale_x_discrete(labels= eixo_x)+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Intervalo de Confiança", y="Proporção") +
  padrao

ggsave('imagens/estimacao/boxplot_area_30.png',width=158,height=93,units='mm')





## grafico do parametro da nota de area para amostras de tamanho 30


comp_par(map_area_30,parametro_area) %>% 
  table() %>% 
  data.frame() %>% 
  rename("Sit" = ".") %>% 
  ggplot(aes(Sit,Freq, label = Freq)) +
  geom_col(fill = cor)+
  geom_text(vjust=-0.5, size=4)+
  scale_y_continuous(limits = c(0,60))+
  labs(x="Parâmetro", y="Frequência Absoluta")+
  padrao


ggsave('imagens/estimacao/barras_area_30.png',width=158,height=93,units='mm')





#boxplot do IC da variavel area de amostras com tamanho 100

map_area_100 <- area%>% 
  filter(`Tamanho da Amostra` == 100) %>% 
  select(Lower,Upper,Estimativa) %>% 
  gather(Lower,Upper,Estimativa,1:3)

levels <- c("Lower","Estimativa","Upper")

map_area_100$Lower <- factor(map_area_100$Lower, levels = levels)

ggplot(map_area_100,aes(x= Lower, y=Upper))+
  geom_boxplot(fill=c(cor), width = 0.5) +
  scale_x_discrete(labels= eixo_x)+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Intervalo de Confiança", y="Proporção") +
  padrao

ggsave('imagens/estimacao/boxplot_area_100.png',width=158,height=93,units='mm')



## grafico do parametro da area para amostras de tamanho 100


comp_par(map_area_100,parametro_area) %>% 
  table() %>% 
  data.frame() %>% 
  rename("Sit" = ".") %>% 
  ggplot(aes(Sit,Freq, label = Freq)) +
  geom_col(fill = cor)+
  geom_text(vjust=-0.5, size=4)+
  scale_y_continuous(limits = c(0,60))+
  labs(x="Parâmetro", y="Frequência Absoluta")+
  padrao


ggsave('imagens/estimacao/barras_area_100.png',width=158,height=93,units='mm')













## sexo ####

#dataframe com os valores do intervalo de confianÃÂ§a
#da variÃÂ¡vel sexo, e o tamanho da amostra


sexo<-sapply(amostras,function(amostra){
  
  if(length(amostra) == 30){
    c <- data[amostra,"SEXO"]
    c <- c[!is.na(c)]
    c <- as.numeric(gsub("Feminino", 1, gsub("Masculino", 0, c)))
    c <- ci.binom(c,0.95)
    c[5] <- 30
    return(c)
  }
  
  if(length(amostra) == 100){
    c <- data[amostra,"SEXO"]
    c <- c[!is.na(c)]
    c <- as.numeric(gsub("Feminino", 1, gsub("Masculino", 0, c)))
    c <- ci.binom(c,0.95)
    c[5] <- 100

    return(c)
  }
  
  
  
}) %>% as.data.frame(.) %>% t

sexo <- as.data.frame(sexo) %>% 
  rename("Estimativa" = "V1",
         "Lower" = "V2",
         "Upper" = "V3",
         "Desvio" = "V4",
         "Tamanho da Amostra" = "V5")


## parametro sexo feminino

parametro_sexo <- as.numeric(table(data$SEXO)[1]/sum(table(data$SEXO)[1],table(data$SEXO)[2]))


#boxplot do IC da variavel area de amostras com tamanho 30

map_sexo_30 <- sexo%>% 
  filter(`Tamanho da Amostra` == 30) %>% 
  select(Lower,Upper,Estimativa) %>% 
  gather(Lower,Upper,Estimativa,1:3)

levels <- c("Lower","Estimativa","Upper")

map_sexo_30$Lower <- factor(map_sexo_30$Lower, levels = levels)

ggplot(map_sexo_30,aes(x= Lower, y=Upper))+
  geom_boxplot(fill=c(cor), width = 0.5) +
  scale_x_discrete(labels= eixo_x)+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Intervalo de Confiança", y="Proporção") +
  padrao


ggsave('imagens/estimacao/boxplot_sexo_30.png',width=158,height=93,units='mm')




## grafico do parametro do sexo feminino  para amostras de tamanho 30


comp_par(map_sexo_30,parametro_sexo) %>% 
  table() %>% 
  data.frame() %>% 
  rename("Sit" = ".") %>% 
  ggplot(aes(Sit,Freq, label = Freq)) +
  geom_col(fill = cor)+
  geom_text(vjust=-0.5, size=4)+
  scale_y_continuous(limits = c(0,60))+
  labs(x="Parâmetro", y="Frequência Absoluta")+
  padrao


ggsave('imagens/estimacao/barras_sexo_30.png',width=158,height=93,units='mm')



#boxplot do IC da variavel area de amostras com tamanho 100

map_sexo_100 <- sexo%>% 
  filter(`Tamanho da Amostra` == 100) %>% 
  select(Lower,Upper,Estimativa) %>% 
  gather(Lower,Upper,Estimativa,1:3)

levels <- c("Lower","Estimativa","Upper")

map_sexo_100$Lower <- factor(map_sexo_100$Lower, levels = levels)

ggplot(map_sexo_100,aes(x= Lower, y=Upper))+
  geom_boxplot(fill=c(cor), width = 0.5) +
  scale_x_discrete(labels= eixo_x)+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Intervalo de Confiança", y="Proporção") +
  padrao


ggsave('imagens/estimacao/boxplot_sexo_100.png',width=158,height=93,units='mm')



## grafico do parametro do sexo feminino  para amostras de tamanho 30


comp_par(map_sexo_100,parametro_sexo) %>% 
  table() %>% 
  data.frame() %>% 
  rename("Sit" = ".") %>% 
  ggplot(aes(Sit,Freq, label = Freq)) +
  geom_col(fill = cor)+
  geom_text(vjust=-0.5, size=4)+
  scale_y_continuous(limits = c(0,60))+
  labs(x="Parâmetro", y="Frequência Absoluta")+
  padrao


ggsave('imagens/estimacao/barras_sexo_100.png',width=158,height=93,units='mm')






## notas de portugues ####

port<-sapply(amostras,function(amostra){
  
  if(length(amostra) == 30){
    c <- data[amostra,"NOTA_LP"]
    c <- c[!is.na(c)]
    c <- ci(c,0.95)
    c[5] <- 30
    return(c)
  }
  
  if(length(amostra) == 100){
    c <- data[amostra,"NOTA_LP"]
    c <- c[!is.na(c)]
    c <- ci(c,0.95)
    c[5] <- 100
    
    return(c)
  }
  
  
  
}) %>% as.data.frame(.) %>% t


port <- as.data.frame(port) %>% 
  rename("Estimativa" = "Estimate",
         "Lower" = "CI lower",
         "Upper" = "CI upper",
         "Desvio" = "Std. Error",
         "Tamanho da Amostra" = "V5")



#boxplot do IC da variavel Notas de Portugues de amostras com tamanho 30

map_port_30 <- port%>% 
  filter(`Tamanho da Amostra` == 30) %>% 
  select(Lower,Upper,Estimativa) %>% 
  gather(Lower,Upper,Estimativa,1:3)

levels <- c("Lower","Estimativa","Upper")

map_port_30$Lower <- factor(map_port_30$Lower, levels = levels)

ggplot(map_port_30,aes(x= Lower, y=Upper))+
  geom_boxplot(fill=c(cor), width = 0.5) +
  scale_x_discrete(labels= eixo_x)+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Intervalo de Confiança", y="Média") +
  padrao

ggsave('imagens/estimacao/boxplot_mat_30.png',width=158,height=93,units='mm')




## grafico do parametro da nota de port para amostras de tamanho 30


comp_par(map_port_30,mean(data$NOTA_LP)) %>% 
  table() %>% 
  data.frame() %>% 
  rename("Sit" = ".") %>% 
  ggplot(aes(Sit,Freq, label = Freq)) +
  geom_col(fill = cor)+
  geom_text(vjust=-0.5, size=4)+
  scale_y_continuous(limits = c(0,60))+
  labs(x="Parâmetro", y="Frequência Absoluta")+
  padrao


ggsave('imagens/estimacao/barras_port_30.png',width=158,height=93,units='mm')


#boxplot do IC da variavel Notas de Portugues de amostras com tamanho 100

map_port_100 <- port%>% 
  filter(`Tamanho da Amostra` == 100) %>% 
  select(Lower,Upper,Estimativa) %>% 
  gather(Lower,Upper,Estimativa,1:3)

levels <- c("Lower","Estimativa","Upper")

map_port_100$Lower <- factor(map_port_100$Lower, levels = levels)

ggplot(map_port_100,aes(x= Lower, y=Upper))+
  geom_boxplot(fill=c(cor), width = 0.5) +
  scale_x_discrete(labels= eixo_x)+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Intervalo de Confiança", y="Média") +
  padrao 


ggsave('imagens/estimacao/boxplot_mat_100.png',width=158,height=93,units='mm')


## grafico do parametro da nota de port para amostras de tamanho 30


comp_par(map_port_100,mean(data$NOTA_LP)) %>% 
  table() %>% 
  data.frame() %>% 
  rename("Sit" = ".") %>% 
  ggplot(aes(Sit,Freq, label = Freq)) +
  geom_col(fill = cor)+
  geom_text(vjust=-0.5, size=4)+
  scale_y_continuous(limits = c(0,60))+
  labs(x="Parâmetro", y="Frequência Absoluta")+
  padrao


ggsave('imagens/estimacao/barras_port_100.png',width=158,height=93,units='mm')



## notas de matematica####

mat<-sapply(amostras,function(amostra){
  
  if(length(amostra) == 30){
    c <- data[amostra,"NOTA_MT"]
    c <- c[!is.na(c)]
    c <- ci(c,0.95)
    c[5] <- 30
    return(c)
  }
  
  if(length(amostra) == 100){
    c <- data[amostra,"NOTA_MT"]
    c <- c[!is.na(c)]
    c <- ci(c,0.95)
    c[5] <- 100
    
    return(c)
  }
  
  
  
}) %>% as.data.frame(.) %>% t


mat<- as.data.frame(mat) %>% 
  rename("Estimativa" = "Estimate",
         "Lower" = "CI lower",
         "Upper" = "CI upper",
         "Desvio" = "Std. Error",
         "Tamanho da Amostra" = "V5")



#boxplot do IC da variavel Notas de Matemática de amostras com tamanho 30

map_mat_30 <- mat%>% 
  filter(`Tamanho da Amostra` == 30) %>% 
  select(Lower,Upper,Estimativa) %>% 
  gather(Lower,Upper,Estimativa,1:3)

levels <- c("Lower","Estimativa","Upper")

map_mat_30$Lower <- factor(map_mat_30$Lower, levels = levels)

ggplot(map_mat_30,aes(x= Lower, y=Upper))+
  geom_boxplot(fill=c(cor), width = 0.5) +
  scale_x_discrete(labels= eixo_x)+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Intervalo de Confiança", y="Média") +
  padrao

ggsave('imagens/estimacao/boxplot_port_30.png',width=158,height=93,units='mm')


## grafico do parametro da nota de mat para amostras de tamanho 30

comp_par(map_mat_30,mean(data$NOTA_MT)) %>% 
  table() %>% 
  data.frame() %>% 
  rename("Sit" = ".") %>% 
  ggplot(aes(Sit,Freq, label = Freq)) +
  geom_col(fill = cor)+
  geom_text(vjust=-0.5, size=4)+
  scale_y_continuous(limits = c(0,60))+
  labs(x="Parâmetro", y="Frequência Absoluta")+
  padrao
  
ggsave('imagens/estimacao/barras_mat_30.png',width=158,height=93,units='mm')

#boxplot do IC da variavel Notas de Matemática de amostras com tamanho 100

map_mat_100 <- mat%>% 
  filter(`Tamanho da Amostra` == 100) %>% 
  select(Lower,Upper,Estimativa) %>% 
  gather(Lower,Upper,Estimativa,1:3)

levels <- c("Lower","Estimativa","Upper")

map_mat_100$Lower <- factor(map_mat_100$Lower, levels = levels)

ggplot(map_mat_100,aes(x= Lower, y=Upper))+
  geom_boxplot(fill=c(cor), width = 0.5) +
  scale_x_discrete(labels= eixo_x)+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Intervalo de Confiança", y="Média") +
  padrao


ggsave('imagens/estimacao/boxplot_port_100.png',width=158,height=93,units='mm')


## grafico do parametro da nota de mat para amostras de tamanho 100
comp_par(map_mat_100,mean(data$NOTA_MT)) %>% 
  table() %>% 
  data.frame() %>% 
  rename("Sit" = ".") %>% 
  ggplot(aes(Sit,Freq, label = Freq)) +
  geom_col(fill = cor)+
  geom_text(vjust=-0.5, size=4)+
  scale_y_continuous(limits = c(0,60))+
  labs(x="Parâmetro", y="Frequência Absoluta")+
  padrao


ggsave('imagens/estimacao/barras_mat_100.png',width=158,height=93,units='mm')