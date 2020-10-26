setwd("C:/Users/Davi/Desktop/universidade/3_semestre/metodos_2/saeb/")

pacman::p_load(tidyverse,ggplot2,stringr,EnvStats,goftest,moments)


data <- read.csv("amostra_190026570.csv", encoding = "UTF-8")



mat30 <- data$NOTA_MT[sample(seq(1,2000),30)]
mat100 <- data$NOTA_MT[ sample(seq(1,2000),100)]

port30 <-data$NOTA_LP[ sample(seq(1,2000),30)]
port100 <-data$NOTA_LP[ sample(seq(1,2000),100)]


media_port <- mean(data$NOTA_LP)
media_mat <- mean(data$NOTA_MT)

sd_port <- sd(data$NOTA_LP)
sd_mat <- sd(data$NOTA_MT)



##matematica####


#check the normality of math´s notes

#breaking the variable
df_mat30<-as.data.frame(table(cut(mat30, breaks = seq(120,440,30))))%>% 
  mutate(min = sapply(seq(1,length(Var1)), function(x){as.numeric(str_extract_all(Var1, "[0-9]{3}")[[x]][1])}),
         max = sapply(seq(1,length(Var1)), function(x){as.numeric(str_extract_all(Var1, "[0-9]{3}")[[x]][2])}),
         pto_medio = (min+max)/2) %>% 
  select(pto_medio,Freq) %>%
  rename(freq_obs = Freq) %>% 
  mutate(freq_rel = freq_obs/sum(freq_obs)) %>% 
  mutate(amostra = sapply(1:length(pto_medio), function(x) "Matemática \n (n = 30)")) %>% 
  mutate(freq_esp = sapply(1:length(pto_medio), function(x){
    
    if(x == 1){return((pnorm(pto_medio[1], media_mat, sd_mat))*30)}
    #get the probability
    else{return((pnorm(pto_medio[x], media_mat, sd_mat) - pnorm(pto_medio[x-1], media_mat, sd_mat))*30)}}))
  

  
#breaking the variable
df_mat100<-as.data.frame(table(cut(mat100, breaks = seq(120,440,30))))%>% 
  mutate(min = sapply(seq(1,length(Var1)), function(x){as.numeric(str_extract_all(Var1, "[0-9]{3}")[[x]][1])}),
         max = sapply(seq(1,length(Var1)), function(x){as.numeric(str_extract_all(Var1, "[0-9]{3}")[[x]][2])}),
         pto_medio = (min+max)/2) %>% 
  select(pto_medio,Freq) %>%
  rename(freq_obs = Freq) %>% 
  mutate(freq_rel = freq_obs/sum(freq_obs)) %>% 
  mutate(amostra = sapply(1:length(pto_medio), function(x) "Matemática \n (n = 100)")) %>% 
  mutate(freq_esp = sapply(1:length(pto_medio), function(x){
    
    if(x == 1){return((pnorm(pto_medio[1], media_mat, sd_mat))*100)}
    #get the probability
    else{return((pnorm(pto_medio[x], media_mat, sd_mat) - pnorm(pto_medio[x-1], media_mat, sd_mat))*100)}}))



df_mat<-as.data.frame(table(cut(data$NOTA_MT, breaks = seq(120,440,30))))%>% 
  mutate(min = sapply(seq(1,length(Var1)), function(x){as.numeric(str_extract_all(Var1, "[0-9]{3}")[[x]][1])}),
         max = sapply(seq(1,length(Var1)), function(x){as.numeric(str_extract_all(Var1, "[0-9]{3}")[[x]][2])}),
         pto_medio = (min+max)/2) %>% 
  select(pto_medio,Freq) %>%
  rename(freq_obs = Freq) %>% 
  mutate(freq_rel = freq_obs/sum(freq_obs)) %>% 
  mutate(amostra = sapply(1:length(pto_medio), function(x) "Matemática \n (n = 100)")) %>% 
  mutate(freq_esp = sapply(1:length(pto_medio), function(x){
    
    if(x == 1){return((pnorm(pto_medio[1], media_mat, sd_mat))*2000)}
    #get the probability
    else{return((pnorm(pto_medio[x], media_mat, sd_mat) - pnorm(pto_medio[x-1], media_mat, sd_mat))*2000)}}))


#breaking the variable
df_port30<-as.data.frame(table(cut(port30, breaks = seq(120,440,30))))%>% 
  mutate(min = sapply(seq(1,length(Var1)), function(x){as.numeric(str_extract_all(Var1, "[0-9]{3}")[[x]][1])}),
         max = sapply(seq(1,length(Var1)), function(x){as.numeric(str_extract_all(Var1, "[0-9]{3}")[[x]][2])}),
         pto_medio = (min+max)/2) %>% 
  select(pto_medio,Freq) %>%
  rename(freq_obs = Freq) %>% 
  mutate(freq_rel = freq_obs/sum(freq_obs)) %>% 
  mutate(amostra = sapply(1:length(pto_medio), function(x) "Português \n (n = 30)")) %>% 
  mutate(freq_esp = sapply(1:length(pto_medio), function(x){
    
    if(x == 1){return((pnorm(pto_medio[1], media_port, sd_port))*30)}
    #get the probability
    else{return((pnorm(pto_medio[x], media_port, sd_port) - pnorm(pto_medio[x-1], media_port, sd_port))*30)}}))



#breaking the variable
df_port100<-as.data.frame(table(cut(port100, breaks = seq(120,440,30))))%>% 
  mutate(min = sapply(seq(1,length(Var1)), function(x){as.numeric(str_extract_all(Var1, "[0-9]{3}")[[x]][1])}),
         max = sapply(seq(1,length(Var1)), function(x){as.numeric(str_extract_all(Var1, "[0-9]{3}")[[x]][2])}),
         pto_medio = (min+max)/2) %>% 
  select(pto_medio,Freq) %>%
  rename(freq_obs = Freq) %>% 
  mutate(freq_rel = freq_obs/sum(freq_obs)) %>% 
  mutate(amostra = sapply(1:length(pto_medio), function(x) "Português \n (n = 100)")) %>% 
  mutate(freq_esp = sapply(1:length(pto_medio), function(x){
    
    if(x == 1){return((pnorm(pto_medio[1], media_port, sd_port))*100)}
    #get the probability
    else{return((pnorm(pto_medio[x], media_port, sd_port) - pnorm(pto_medio[x-1], media_port, sd_port))*100)}}))





#breaking the variable
df_port<-as.data.frame(table(cut(data$NOTA_LP, breaks = seq(120,440,30))))%>% 
  mutate(min = sapply(seq(1,length(Var1)), function(x){as.numeric(str_extract_all(Var1, "[0-9]{3}")[[x]][1])}),
         max = sapply(seq(1,length(Var1)), function(x){as.numeric(str_extract_all(Var1, "[0-9]{3}")[[x]][2])}),
         pto_medio = (min+max)/2) %>% 
  select(pto_medio,Freq) %>%
  rename(freq_obs = Freq) %>% 
  mutate(freq_rel = freq_obs/sum(freq_obs)) %>% 
  mutate(amostra = sapply(1:length(pto_medio), function(x) "port30")) %>% 
  mutate(freq_esp = sapply(1:length(pto_medio), function(x){
    
    if(x == 1){return((pnorm(pto_medio[1], media_port, sd_port))*2000)}
    #get the probability
    else{return((pnorm(pto_medio[x], media_port, sd_port) - pnorm(pto_medio[x-1], media_port, sd_port))*2000)}}))




#qui quadrado of nota matematica
df_mat30 %>% summarise(x = (freq_obs-freq_esp)^2/freq_esp) %>% sum(x)
df_mat100 %>% summarise(x = (freq_obs-freq_esp)^2/freq_esp) %>% sum(x)
df_mat %>% summarise(x = (freq_obs-freq_esp)^2/freq_esp) %>% sum(x)


df_port30 %>% summarise(x = (freq_obs-freq_esp)^2/freq_esp) %>% sum(x)
df_port100 %>% summarise(x = (freq_obs-freq_esp)^2/freq_esp) %>% sum(x)
df_port %>% summarise(x = (freq_obs-freq_esp)^2/freq_esp) %>% sum(x)

#qui quadrado esperado
qchisq(0.95,9)


#graficosinho caso precise(e de tempo)
rbind(df_mat100, df_port100, df_mat30, df_port30) %>% 
  select(-freq_rel) %>% 
  gather("frequencias","valores",-pto_medio, -amostra) %>% 
  ggplot(aes(pto_medio,valores, fill = frequencias)) +
  geom_col(position = "dodge")+
  facet_wrap(~amostra, labeller = labeller(dose = c("Matemática","Português"), supp = c("Amostras de tamanho 30","Amostras de tamanho 100"))  )+
  theme_classic()+
  scale_fill_manual("Tipos de Frequência",labels = c("Esperada","Observada"),values=c("red", "#215493"))+
  labs(x = "Notas",y = "Frequências")+
  theme(legend.position="top")

ggsave("aderencia.png", width = 158, height = 93, units = "mm")


####Questao 2####
##Shapiro Wilk ####

#hypothesis test: p-valor and W(statistic)

# h0: the sample follow a normal distribution
# h1: the sample not follow a normal distribution

#not reject the null hypothesis if W_obs > W_esp and p_valor > alpha 

#for samples with length > 50, for aplha = 0.05, the W_esp = 0.947
#for samples with length = 30, for aplha = 0.05, the W_esp = 0.9 27
gofTest(data$NOTA_MT, test = "ad")[c("p.value","statistic")]


sapply(list(mat30, mat100, port30, port100), function(x){
  
  #the list must contain the withdrawals samples of dataframe
  gofTest(x, test = "sw")[c("p.value","statistic")]}) %>% 
  data.frame %>% 
  rename("Mat30" = "X1","Mat100" = "X2","Port30"= "X3","Port100"= "X4")



##anderson-darling####

#hypothesis test: p-valor and A^2(statistic)


# h0: the sample follow a normal distribution
# h1: the sample not follow a normal distribution

#not reject the null hypothesis if  p_valor > alpha 

sapply(list(mat30, mat100, port30, port100), function(x){
  
  #the list must contain the withdrawals samples of dataframe
  gofTest(x, test = "ad")[c("p.value","statistic")]}) %>% 
  data.frame %>% 
  rename("Mat30" = "X1","Mat100" = "X2","Port30"= "X3","Port100"= "X4")





##lilliefors####

#hypothesis test: p-valor and D(statistic)


# h0: the sample follow a normal distribution
# h1: the sample not follow a normal distribution

#not reject the null hypothesis if  p_valor > alpha 

sapply(list(mat30, mat100, port30, port100), function(x){
  
  #the list must contain the withdrawals samples of dataframe
  gofTest(x, test = "lillie")[c("p.value","statistic")]}) %>% 
  data.frame %>% 
  rename("Mat30" = "X1","Mat100" = "X2","Port30"= "X3","Port100"= "X4")



##medidas resumo####


#portugues 30
summary(port30)
mean(port30)
sd(port30)
kurtosis(port30)
skewness(port30)


#portugues 100
summary(port100)
mean(port100)
sd(port100)
kurtosis(port100)
skewness(port100)



#matematica 30
summary(mat30)
mean(mat30)
sd(mat30)
kurtosis(mat30)
skewness(mat30)



#matematica 100
summary(mat100)
mean(mat100)
sd(mat100)
kurtosis(mat100)
skewness(mat100)



















df_mat30 %>% 
  select(pto_medio,freq_obs,freq_esp) %>% 
  gather("frequencias","valores",-pto_medio) %>% 
  ggplot(aes(x=pto_medio, y=valores, fill=frequencias)) +
  geom_bar(stat="identity", position ="dodge") +
  labs(x = "Notas de Matemática", y = "Frequência", fill = "Frequências")+
  scale_fill_manual(labels = c("Esperado","Observado"),values=c("red", "#215493")) +
  #scale_alpha_manual(values=c(.1, .8))+
  theme_classic()+
  scale_x_continuous(breaks = seq(120,440,30))








#graph to compare the Nota de Matemática´s density with the normal density

data %>% 
  ggplot(aes(x=NOTA_MT)) + 
  geom_histogram(breaks = seq(120,440,20), 
                 aes(col=I("white"), y = ..density..), 
                 fill = '#215493') + 
  scale_x_continuous(breaks = seq(120,440,20)) +
  labs(x='Notas em Matemática', 
       y="Densidade") + 
  theme_classic() + 
  stat_function(fun=dnorm,
                color="black",
                args=list(mean=mean(data$NOTA_MT), 
                          sd=sd(data$NOTA_MT)))








