setwd("C:/Users/Davi/Desktop/universidade/3_semestre/metodos_2/saeb/")

pacman::p_load(tidyverse,ggplot2,stringr,EnvStats,goftest,moments)


data <- read.csv("amostra_190026570.csv", encoding = "UTF-8")



set.seed(121);
amostra30 <- data[sample(seq(1,2000),30),];
amostra100 <- data[sample(seq(1,2000),100),]



urbana30 <- amostra30 %>% 
  select(NOTA_MT, LOCALIZACAO) %>% 
  filter(LOCALIZACAO == "Urbana") %>% 
  select(NOTA_MT) %>% 
  unlist %>% 
  as.numeric


urbana100 <- amostra100 %>% 
  select(NOTA_MT, LOCALIZACAO) %>% 
  filter(LOCALIZACAO == "Urbana") %>% 
  select(NOTA_MT) %>% 
  unlist %>% 
  as.numeric









rural30 <- amostra30 %>% 
  select(NOTA_MT, LOCALIZACAO) %>% 
  filter(LOCALIZACAO == "Rural") %>% 
  select(NOTA_MT) %>% 
  unlist %>% 
  as.numeric


rural100 <- amostra100 %>% 
  select(NOTA_MT, LOCALIZACAO) %>% 
  filter(LOCALIZACAO == "Rural") %>% 
  select(NOTA_MT) %>% 
  unlist %>% 
  as.numeric










municipal30 <- amostra30 %>% 
  select(NOTA_LP, DEPENDENCIA_ADM) %>% 
  filter(DEPENDENCIA_ADM == "Municipal") %>% 
  select(NOTA_LP) %>% 
  unlist %>% 
  as.numeric



municipal100 <- amostra100 %>% 
  select(NOTA_LP, DEPENDENCIA_ADM) %>% 
  filter(DEPENDENCIA_ADM == "Municipal") %>% 
  select(NOTA_LP) %>% 
  unlist %>% 
  as.numeric










estadual30 <- amostra30 %>% 
  select(NOTA_LP, DEPENDENCIA_ADM) %>% 
  filter(DEPENDENCIA_ADM %in% c("Estadual","Federal")) %>% 
  select(NOTA_LP) %>% 
  unlist %>% 
  as.numeric






estadual100 <- amostra100 %>% 
  select(NOTA_LP, DEPENDENCIA_ADM) %>% 
  filter(DEPENDENCIA_ADM %in% c("Estadual","Federal")) %>% 
  select(NOTA_LP) %>% 
  unlist %>% 
  as.numeric






#  municipal x estadual/federal ####



## Testes 
wilcox.test(municipal30,estadual30)

wilcox.test(municipal100,estadual100)



## GrÃƒÂ¡ficos 
ggplot(amostra30, aes(x=DEPENDENCIA_ADM, y=NOTA_LP)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  
  labs(x="Dependência das Escolas", y="Notas em Língua Portuguesa") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

ggsave('imagens/atividade4_2/depend_30.png',width=158,height=93,units='mm')

ggplot(amostra100, aes(x=DEPENDENCIA_ADM, y=NOTA_LP)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  
  labs(x="Dependência das Escolas", y="Notas em Língua Portuguesa") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

ggsave('imagens/atividade4_2/depend_100.png',width=158,height=93,units='mm')


# rural x urbana ####

# testes

wilcox.test(urbana30,rural30)
ks.test(urbana30,rural30)
gofTest(urbana30,rural30, test = "cvm")



gofTest(data$NOTA_LP[sample(seq(1,2000),30)], test = "sw")

wilcox.test(urbana100,rural100)



## GrÃƒÂ¡ficos 
ggplot(amostra30, aes(x=LOCALIZACAO, y=NOTA_MT)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  
  labs(x="Localização das Escolas", y="Notas em Matemática") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

ggsave('imagens/atividade4_2/local_30.png',width=158,height=93,units='mm')

ggplot(amostra100, aes(x=LOCALIZACAO, y=NOTA_MT)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  
  labs(x="Localização das Escolas", y="Notas em Matemática") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

ggsave('imagens/atividade4_2/local_100.png',width=158,height=93,units='mm')


## medidas resumo ####



summary(estadual30)
sd(estadual30)
skewness(estadual30)
kurtosis(estadual30)


summary(municipal30)
sd(municipal30)
skewness(municipal30)
kurtosis(municipal30)






summary(estadual100)
sd(estadual100)
kurtosis(estadual100)
skewness(estadual100)

summary(municipal100)
sd(municipal100)
skewness(municipal100)
kurtosis(municipal100)








summary(rural30)
sd(rural30)
kurtosis(rural30)
skewness(rural30)

summary(urbana30)
sd(urbana30)
kurtosis(urbana30)
skewness(urbana30)






summary(rural100)
sd(rural100)
skewness(rural100)
kurtosis(rural100)


summary(urbana100)
sd(urbana100)
skewness(urbana100)
kurtosis(urbana100)






#1) testar normalidadde:

#usar Kolmogorov, Shapiro Wilk e Anderson Darling

#   - se for normal, usar: t student, 
#   - se nÃ£o for normal, usar: Wilcoxon, Kolmogorov-Smirnov, CramÃ©r-von Mises



vetor_de_coisas <- list(estadual30, estadual100, municipal30, municipal100,  rural100, urbana30, urbana100)

testes <- c("sw","ad","lillie")

normal<-sapply(testes, function(teste){
  sapply(vetor_de_coisas, function(varial){
    gofTest(varial, test = teste)["p.value"]
  })
}) %>% data.frame %>% t


normal <- as.data.frame(normal)
names(normal) <- c("estadual30","estadual100","municipal30","municipal100","rural100","urbana30","urbana100")
normal




##teste t student ####

t.test(estadual30,municipal30, paired = F)
t.test(estadual100,municipal100, paired = F)


t.test(urbana30,rural30, paired = F)
t.test(urbana100,rural100, paired = F)

##testes de homocedasticidade####
var.test(municipal30, estadual30)
var.test(municipal100, estadual100)


var.test(urbana30, rural30)
var.test(urbana100, rural100)










##questão 2####

amostra30 %>% 
  select(NOTA_LP, NOTA_MT) %>% 
  gather("Notas","Valores") %>% 
  ggplot(aes(x=Notas, y=Valores)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  scale_x_discrete(labels = c("Notas de Português","Notas de Matemática"))+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Matérias", y="Notas") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 
  

ggsave('imagens/atividade4_2/materias.png',width=158,height=93,units='mm')


port30 <- amostra30$NOTA_LP
mat30 <- amostra30$NOTA_MT

summary(port30)
sd(port30)
kurtosis(port30)
skewness(port30)

summary(mat30)
sd(mat30)
kurtosis(mat30)
skewness(mat30)



##teste t student ####
t.test(port30,mat30, paired = F)


##testes de homocedasticidade####
var.test(port30,mat30)








