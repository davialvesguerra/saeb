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


#padrao graficos
padrao <- list( labs(x="Intervalo de Confiança", y="Amostras", color = "Parâmetro"),
  scale_color_manual(values = c("#215493", "red")),
  theme_minimal(),
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")),
  theme(legend.position="top"))


parametro_area <- as.numeric(table(data$AREA)[2]/sum(table(data$AREA)[1],table(data$AREA)[2]))


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



##area ####


#area n 30

area30 <- area %>% 
  select(Lower, Upper, `Tamanho da Amostra`) %>% 
  filter(`Tamanho da Amostra` == 30) %>% 
  mutate(capture_mu = ifelse(Lower < parametro_area & Upper > parametro_area, "Contido", "Não Contido"))


area30 <- data.frame(ci_id = c(1:50, 1:50),
                     ci_bounds = c(area30$Lower, area30$Upper),
                     capture_mu = c(area30$capture_mu, area30$capture_mu))


ggplot(data = area30, aes(x = ci_bounds, y = ci_id,group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  
  geom_line() +           
  geom_vline(xintercept = parametro_area, color = "darkgray")+
  padrao


ggsave('imagens/estimacao/ic_area_30.png',width=158,height=93,units='mm')



#area n 100

area100 <- area %>% 
  select(Lower, Upper, `Tamanho da Amostra`) %>% 
  filter(`Tamanho da Amostra` == 100) %>% 
  mutate(capture_mu = ifelse(Lower < parametro_area & Upper > parametro_area, "Contido", "Não Contido"))


area100 <- data.frame(ci_id = c(1:50, 1:50),
                     ci_bounds = c(area100$Lower, area100$Upper),
                     capture_mu = c(area100$capture_mu, area100$capture_mu))


ggplot(data = area100, aes(x = ci_bounds, y = ci_id,group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  
  geom_line() +           
  geom_vline(xintercept = parametro_area, color = "darkgray")+
  padrao


ggsave('imagens/estimacao/ic_area_100.png',width=158,height=93,units='mm')



## sexo ####




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



##area ####


#sexo n 30

sexo30 <- sexo %>% 
  select(Lower, Upper, `Tamanho da Amostra`) %>% 
  filter(`Tamanho da Amostra` == 30) %>% 
  mutate(capture_mu = ifelse(Lower < parametro_sexo & Upper > parametro_sexo, "Contido", "Não Contido"))


sexo30 <- data.frame(ci_id = c(1:50, 1:50),
                     ci_bounds = c(sexo30$Lower, sexo30$Upper),
                     capture_mu = c(sexo30$capture_mu, sexo30$capture_mu))


ggplot(data = sexo30, aes(x = ci_bounds, y = ci_id,group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  
  geom_line() +           
  geom_vline(xintercept = parametro_sexo, color = "darkgray")+
  padrao


ggsave('imagens/estimacao/ic_sexo_30.png',width=158,height=93,units='mm')



#sexo n 100

sexo100 <- sexo %>% 
  select(Lower, Upper, `Tamanho da Amostra`) %>% 
  filter(`Tamanho da Amostra` == 100) %>% 
  mutate(capture_mu = ifelse(Lower < parametro_sexo & Upper > parametro_sexo, "Contido", "Não Contido"))


sexo100 <- data.frame(ci_id = c(1:50, 1:50),
                     ci_bounds = c(sexo100$Lower, sexo100$Upper),
                     capture_mu = c(sexo100$capture_mu, sexo100$capture_mu))


ggplot(data = sexo100, aes(x = ci_bounds, y = ci_id,group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  
  geom_line() +           
  geom_vline(xintercept = parametro_sexo, color = "darkgray")+
  padrao


ggsave('imagens/estimacao/ic_sexo_100.png',width=158,height=93,units='mm')







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



parametro_port <- mean(data$NOTA_LP)




#port n 30



port30 <- port %>% 
  select(Lower, Upper, `Tamanho da Amostra`) %>% 
  filter(`Tamanho da Amostra` == 30) %>% 
  mutate(capture_mu = ifelse(Lower < parametro_port & Upper > parametro_port, "Contido", "Não Contido"))


port30 <- data.frame(ci_id = c(1:50, 1:50),
                      ci_bounds = c(port30$Lower, port30$Upper),
                      capture_mu = c(port30$capture_mu, port30$capture_mu))


ggplot(data = port30, aes(x = ci_bounds, y = ci_id,group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +
  labs(title = "Intervalo de Confiança\n para a Variável Nota de Português,\n retiradas de 50 amostas de tamanho 30")+
  geom_line() +           
  geom_vline(xintercept = parametro_port, color = "darkgray")+
  padrao


ggsave('imagens/estimacao/ic_port_30.png',width=158,height=93,units='mm')
#port n 100

port100 <- port %>% 
  select(Lower, Upper, `Tamanho da Amostra`) %>% 
  filter(`Tamanho da Amostra` == 100) %>% 
  mutate(capture_mu = ifelse(Lower < parametro_port & Upper > parametro_port, "Contido", "Não Contido"))


port100 <- data.frame(ci_id = c(1:50, 1:50),
                      ci_bounds = c(port100$Lower, port100$Upper),
                      capture_mu = c(port100$capture_mu, port100$capture_mu))


ggplot(data = port100, aes(x = ci_bounds, y = ci_id,group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  
  geom_line() +           
  geom_vline(xintercept = parametro_port, color = "darkgray")+
  padrao


ggsave('imagens/estimacao/ic_port_100.png',width=158,height=93,units='mm')









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

parametro_mat <- mean(data$NOTA_MT)



#mat n 30

mat30 <- mat %>% 
  select(Lower, Upper, `Tamanho da Amostra`) %>% 
  filter(`Tamanho da Amostra` == 30) %>% 
  mutate(capture_mu = ifelse(Lower < parametro_mat & Upper > parametro_mat, "Contido", "Não Contido"))


mat30 <- data.frame(ci_id = c(1:50, 1:50),
                      ci_bounds = c(mat30$Lower, mat30$Upper),
                      capture_mu = c(mat30$capture_mu, mat30$capture_mu))


ggplot(data = mat30, aes(x = ci_bounds, y = ci_id,group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  
  geom_line() +           
  geom_vline(xintercept = parametro_mat, color = "darkgray")+
  padrao


ggsave('imagens/estimacao/ic_mat_30.png',width=158,height=93,units='mm')





#mat n 100

mat100 <- mat %>% 
  select(Lower, Upper, `Tamanho da Amostra`) %>% 
  filter(`Tamanho da Amostra` == 100) %>% 
  mutate(capture_mu = ifelse(Lower < parametro_mat & Upper > parametro_mat, "Contido", "Não Contido"))


mat100 <- data.frame(ci_id = c(1:50, 1:50),
                      ci_bounds = c(mat100$Lower, mat100$Upper),
                      capture_mu = c(mat100$capture_mu, mat100$capture_mu))


ggplot(data = mat100, aes(x = ci_bounds, y = ci_id,group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  
  geom_line() +           
  geom_vline(xintercept = parametro_mat, color = "darkgray")+
  padrao


ggsave('imagens/estimacao/ic_mat_100.png',width=158,height=93,units='mm')



