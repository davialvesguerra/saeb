setwd("C:/Users/Davi/Desktop/universidade/3_semestre/metodos_2/saeb/")

pacman::p_load(tidyverse,ggplot2,stringr,EnvStats,goftest,moments, DescTools)


data <- read.csv("amostra_190026570.csv", encoding = "UTF-8")



set.seed(126);
amostra <- data[sample(1:2000,500),];
amostra


## variavel principal ####
#perspectiva


## variáveis para comparar ####
# escolaridade da mãe
# área
# sexo
# raça
# nota matemática





## escolaridade da mãe ####

data %>% 
  rename("Escolaridade" = ESC_MAE) %>% 
  mutate(Escolaridade = sapply(Escolaridade, function(x){
    if(x %in% "Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano do Ensino Fundamental"){return("Ensino Fundametal II Incompleto")}
    if(x %in% "Completou a 8.ª série/9.º ano do Ensino Fundamental, mas não completou o Ensino Médio"){return("Ensino Médio Incompleto")}
    if(x %in% "Completou o Ensino Médio, mas não completou a Faculdade"){return("Ensino Superior Incompleto")}
    if(x %in% "Completou a Faculdade"){return("Ensino Superior")}
    if(x %in% c("Não completou a 4.ª série/5.º ano do Ensino Fundametal","Não completou a 4.ª série/5.º ano do Ensino Fundamental")){return("Ensino Fundametal I Incompleto")}
    else(x)
  })) %>%
  select(PERSPECTIVAS,Escolaridade) %>% 
  group_by(PERSPECTIVAS, Escolaridade) %>%
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)) %>% 
  drop_na() %>% 
  ggplot(aes(x=PERSPECTIVAS, y= freq, fill= PERSPECTIVAS)) +
  geom_bar(stat="identity",position='dodge') +
  labs(x ="Escolaridade da Mãe", fill = "Perspectivas")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        legend.position = c(0.7, 0.17),
        legend.key.size = unit(.4, "cm")) +
  facet_wrap(~Escolaridade) +
  scale_fill_manual(values = c("#A11D21", "#003366","gray","#00000F"))

ggsave("imagens/trab_final/graf_barras1.png", width = 158, height = 93, units = "mm")


chisq.test(data$PERSPECTIVAS,data$ESC_MAE)

level <- c("Somente trabalhar", "Somente continuar estudando","Continuar estudando e trabalhar","Ainda não sei")
data$PERSPECTIVAS <- factor(data$PERSPECTIVAS, levels = level, ordered = T)

label <- c("Somente\ntrabalhar", "Somente\ncontinuar\nestudando","Continuar\nestudando e\ntrabalhar","Ainda\nnão sei")

data %>% 
  select(PERSPECTIVAS, SEXO) %>% 
  group_by(PERSPECTIVAS, SEXO) %>% 
  summarise(n = n()) %>% 
  mutate(freq = round((n/sum(n))*100,2)) %>% 
  mutate(porcentagens = str_c(freq, '%')) %>% 
  drop_na() %>%
  ggplot(aes(x=PERSPECTIVAS, y = freq, fill= SEXO, label = porcentagens)) +
  geom_bar(stat="identity",position='dodge') +
  labs(x ="Perspectivas", y = "Frequência Relativa",fill = "Sexo")+
  scale_y_continuous(breaks = seq(0,75,15),
                     limits = c(0,75),
                     labels = str_c(seq(0,75,15),'%'))+
  geom_text(hjust = -0.1, size = 4, position = position_dodge(width = 0.9)) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top") +
  coord_flip()+
  scale_fill_manual(values = c("#A11D21", "#003366"))+
  scale_x_discrete(labels= label)


ggsave("imagens/trab_final/graf_barras2.png", width = 158, height = 93, units = "mm")



##raça ####

data %>% 
  select(PERSPECTIVAS, RACA_COR) %>% 
  group_by(PERSPECTIVAS, RACA_COR) %>% 
  summarise(n = n()) %>% 
  mutate(freq = round((n/sum(n))*100,2)) %>% 
  mutate(porcentagens = str_c(freq, '%')) %>% 
  drop_na() %>%
  ggplot(aes(x=PERSPECTIVAS, y = freq, fill= RACA_COR, label = porcentagens)) +
  geom_bar(stat="identity",position = position_dodge2(preserve = "single")) +
  labs(x ="Perspectivas", y = "Frequência Relativa",fill = "Área")+
  scale_y_continuous(breaks = seq(0,50,10),
                     limits = c(0,50),
                     labels = str_c(seq(0,50,10),'%'))+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top") +
  scale_fill_manual(values = c("#A11D21", "#003366","yellow","#00000F","red","green"))+
  scale_x_discrete(labels= label)



ggsave("imagens/trab_final/graf_barras3.png", width = 158, height = 93, units = "mm")



## raça ####

data %>% 
  select(PERSPECTIVAS, AREA) %>% 
  group_by(PERSPECTIVAS, AREA) %>% 
  summarise(n = n()) %>% 
  mutate(freq = round((n/sum(n))*100,2)) %>% 
  mutate(porcentagens = str_c(freq, '%')) %>% 
  drop_na() %>%
  ggplot(aes(x=PERSPECTIVAS, y = freq, fill= AREA, label = porcentagens)) +
  geom_bar(stat="identity",position='dodge') +
  labs(x ="Perspectivas", y = "Frequência Relativa",fill = "Área")+
  scale_y_continuous(breaks = seq(0,100,15),
                     limits = c(0,100),
                     labels = str_c(seq(0,100,15),'%'))+
  geom_text(vjust = -0.5, size = 4, position = position_dodge(width = 0.9)) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top") +
  scale_fill_manual(values = c("#A11D21", "#003366"))+
  scale_x_discrete(labels= label)



chisq.test(data$PERSPECTIVAS, data$SEXO)

ggsave("imagens/trab_final/graf_barras4.png", width = 158, height = 93, units = "mm")



## nota de matematica ####
data %>% 
  select(PERSPECTIVAS,NOTA_MT) %>% 
  drop_na() %>% 
  ggplot(aes(x=PERSPECTIVAS, y=NOTA_MT)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Perspectiva", y="Nota em Matemática")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))+
  scale_x_discrete(labels= label)+
  scale_y_continuous(limits = c(100,450))
  


ggsave("imagens/trab_final/graf_boxplot.png", width = 158, height = 93, units = "mm")


kruskal.test(data$NOTA_MT,data$PERSPECTIVAS)
