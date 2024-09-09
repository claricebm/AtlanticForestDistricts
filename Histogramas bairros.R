#CARREGANDO PACOTES
library(tidyverse)
library(patchwork)

#CARRREGANDO DADOS
dados_bh<-read.csv("Bairros - Belo Horizonte.csv",sep=";")
dados_ct<-read.csv("Bairros - Curitiba.csv",sep=";")
dados_jp<-read.csv("Bairros - João Pessoa.csv",sep=";")
dados_mc<-read.csv("Bairros - Maceio.csv",sep=";")
dados_nt<-read.csv("Bairros - Natal.csv",sep=";")
dados_rj<-read.csv("Bairros - Rio de Janeiro.csv",sep=";")
dados_ssa<-read.csv("Bairros - Salvador.csv",sep=";")
dados_sp<-read.csv("Distritos - Sao Paulo.csv",sep=";")
dados_vt<-read.csv("Bairros - Vitoria.csv",sep=";")

#PROCESSANDO DADOS

#BH
dados_bh_novo <- dados_bh %>%
  select(NOME, AREA_KM2) %>%
  mutate(AREA_KM2 = as.numeric(gsub(",", ".", AREA_KM2)))

n_bairros_bh<-length(unique(dados_bh_novo$NOME))
bairros_bh_maior_1km2<-length(which(dados_bh_novo$AREA_KM2>=1))
porcentagem_bh<-(bairros_bh_maior_1km2/n_bairros_bh)*100

plot_bh<-ggplot(dados_bh_novo, aes(x = AREA_KM2)) +
  geom_histogram(binwidth = 0.5,                  
                 fill = "lightblue",             
                 color = "black",                 
                 alpha = 0.7) +
  geom_vline(xintercept = 1,                      
             color = "red",                       
             linetype = "dashed",                
             linewidth = 1) +                          
  labs(title = "Belo Horizonte", 
       subtitle = "20.50% dos bairros com área maior ou igual a 1km²",
       x = "",                          
       y = "Frequência",                           
       caption = "Total de 493 bairros") +  
  theme_minimal() +                               
  theme(plot.title = element_text(size = 16,      
                                  face = "bold"), 
        axis.text = element_text(size = 12),      
        axis.title = element_text(size = 13))     

#Curitiba
dados_ct_novo <- dados_ct %>%
  select(NOME, area_km2) %>%
  mutate(area_km2 = as.numeric(gsub(",", ".", area_km2)))

n_bairros_ct<-length(unique(dados_ct_novo$NOME))
bairros_ct_maior_1km2<-length(which(dados_ct_novo$area_km2>=1))
porcentagem_ct<-(bairros_ct_maior_1km2/n_bairros_ct)*100

plot_ct<-ggplot(dados_ct_novo, aes(x = area_km2)) +
  geom_histogram(binwidth = 0.5,                  
                 fill = "lightblue",             
                 color = "black",                 
                 alpha = 0.7) +
  geom_vline(xintercept = 1,                      
             color = "red",                       
             linetype = "dashed",                
             linewidth = 1) +                          
  labs(title = "Curitiba", 
       subtitle = "97.33% dos bairros com área maior ou igual a 1km²",
       x = "",                          
       y = "",                           
       caption = "Total de 75 bairros") +  
  theme_minimal() +                               
  theme(plot.title = element_text(size = 16,      
                                  face = "bold"), 
        axis.text = element_text(size = 12),      
        axis.title = element_text(size = 13))     

#João Pessoa
dados_jp_novo <- dados_jp %>%
  select(N_BAIRRO, area_km2) %>%
  mutate(area_km2 = as.numeric(gsub(",", ".", area_km2)))

n_bairros_jp<-length(unique(dados_jp_novo$N_BAIRRO))
bairros_jp_maior_1km2<-length(which(dados_jp_novo$area_km2>=1))
porcentagem_jp<-(bairros_jp_maior_1km2/n_bairros_jp)*100

plot_jp<-ggplot(dados_jp_novo, aes(x = area_km2)) +
  geom_histogram(binwidth = 0.5,                  
                 fill = "lightblue",             
                 color = "black",                 
                 alpha = 0.7) +
  geom_vline(xintercept = 1,                      
             color = "red",                       
             linetype = "dashed",                
             linewidth = 1) +                          
  labs(title = "João Pessoa", 
       subtitle = "75% dos bairros com área maior ou igual a 1km²",
       x = "",                          
       y = "",                           
       caption = "Total de 64 bairros") +  
  theme_minimal() +                               
  theme(plot.title = element_text(size = 16,      
                                  face = "bold"), 
        axis.text = element_text(size = 12),      
        axis.title = element_text(size = 13))     

#Maceio
dados_mc_novo <- dados_mc %>%
  select(ï..Bairro, area_km2) %>%
  mutate(area_km2 = as.numeric(gsub(",", ".", area_km2)))

n_bairros_mc<-length(unique(dados_mc_novo$ï..Bairro))
bairros_mc_maior_1km2<-length(which(dados_mc_novo$area_km2>=1))
porcentagem_mc<-(bairros_mc_maior_1km2/n_bairros_mc)*100

plot_mc<-ggplot(dados_mc_novo, aes(x = area_km2)) +
  geom_histogram(binwidth = 0.5,                  
                 fill = "lightblue",             
                 color = "black",                 
                 alpha = 0.7) +
  geom_vline(xintercept = 1,                      
             color = "red",                       
             linetype = "dashed",                
             linewidth = 1) +                          
  labs(title = "Maceió", 
       subtitle = "80% dos bairros com área maior ou igual a 1km²",
       x = "",                          
       y = "Frequência",                           
       caption = "Total de 50 bairros") +  
  theme_minimal() +                               
  theme(plot.title = element_text(size = 16,      
                                  face = "bold"), 
        axis.text = element_text(size = 12),      
        axis.title = element_text(size = 13))     

#Natal
dados_nt_novo <- dados_nt %>%
  select(ï..BAIRRO, area_km2) %>%
  mutate(area_km2 = as.numeric(gsub(",", ".", area_km2)))

n_bairros_nt<-length(unique(dados_nt_novo$ï..BAIRRO))
bairros_nt_maior_1km2<-length(which(dados_nt_novo$area_km2>=1))
porcentagem_nt<-(bairros_nt_maior_1km2/n_bairros_nt)*100

plot_nt<-ggplot(dados_nt_novo, aes(x = area_km2)) +
  geom_histogram(binwidth = 0.5,                  
                 fill = "lightblue",             
                 color = "black",                 
                 alpha = 0.7) +
  geom_vline(xintercept = 1,                      
             color = "red",                       
             linetype = "dashed",                
             linewidth = 1) +                          
  labs(title = "Natal", 
       subtitle = "77.78% dos bairros com área maior ou igual a 1km²",
       x = "",                          
       y = "",                           
       caption = "Total de 36 bairros") +  
  theme_minimal() +                               
  theme(plot.title = element_text(size = 16,      
                                  face = "bold"), 
        axis.text = element_text(size = 12),      
        axis.title = element_text(size = 13))     

#Rio de Janeiro
dados_rj_novo <- dados_rj %>%
  select(nome, area_km2) %>%
  mutate(area_km2 = as.numeric(gsub(",", ".", area_km2)))

n_bairros_rj<-length(unique(dados_rj_novo$nome))
bairros_rj_maior_1km2<-length(which(dados_rj_novo$area_km2>=1))
porcentagem_rj<-(bairros_rj_maior_1km2/n_bairros_rj)*100

plot_rj<-ggplot(dados_rj_novo, aes(x = area_km2)) +
  geom_histogram(binwidth = 0.5,                  
                 fill = "lightblue",             
                 color = "black",                 
                 alpha = 0.7) +
  geom_vline(xintercept = 1,                      
             color = "red",                       
             linetype = "dashed",                
             linewidth = 1) +                          
  labs(title = "Rio de Janeiro", 
       subtitle = "82.32% dos bairros com área maior ou igual a 1km²",
       x = "",                          
       y = "",                           
       caption = "Total de 164 bairros") +  
  theme_minimal() +                               
  theme(plot.title = element_text(size = 16,      
                                  face = "bold"), 
        axis.text = element_text(size = 12),      
        axis.title = element_text(size = 13))  

#Salvador
dados_ssa_novo <- dados_ssa %>%
  select(NM_BAIRROS, area_km2) %>%
  mutate(area_km2 = as.numeric(gsub(",", ".", area_km2)))

n_bairros_ssa<-length(unique(dados_ssa_novo$NM_BAIRROS))
bairros_ssa_maior_1km2<-length(which(dados_ssa_novo$area_km2>=1))
porcentagem_ssa<-(bairros_ssa_maior_1km2/n_bairros_ssa)*100

plot_ssa<-ggplot(dados_ssa_novo, aes(x = area_km2)) +
  geom_histogram(binwidth = 0.5,                  
                 fill = "lightblue",             
                 color = "black",                 
                 alpha = 0.7) +
  geom_vline(xintercept = 1,                      
             color = "red",                       
             linetype = "dashed",                
             linewidth = 1) +                          
  labs(title = "Salvador", 
       subtitle = "45.96% dos bairros com área maior ou igual a 1km²",
       x = "Área (km²)",                          
       y = "Frequência",                           
       caption = "Total de 161 bairros") +  
  theme_minimal() +                               
  theme(plot.title = element_text(size = 16,      
                                  face = "bold"), 
        axis.text = element_text(size = 12),      
        axis.title = element_text(size = 13))  

#Sao Paulo
dados_sp_novo <- dados_sp %>%
  select(ï..sp_nome, sp_areakmt) %>%
  mutate(sp_areakmt = as.numeric(gsub(",", ".", sp_areakmt)))

n_bairros_sp<-length(unique(dados_sp_novo$ï..sp_nome))
bairros_sp_maior_1km2<-length(which(dados_sp_novo$sp_areakmt>=1))
porcentagem_mc<-(bairros_sp_maior_1km2/n_bairros_sp)*100

plot_sp<-ggplot(dados_sp_novo, aes(x = sp_areakmt)) +
  geom_histogram(binwidth = 0.5,                  
                 fill = "lightblue",             
                 color = "black",                 
                 alpha = 0.7) +
  geom_vline(xintercept = 1,                      
             color = "red",                       
             linetype = "dashed",                
             linewidth = 1) +                          
  labs(title = "São Paulo", 
       subtitle = "100% dos distritos com área maior ou igual a 1km²",
       x = "Área (km²)",                          
       y = "",                           
       caption = "Total de 32 distritos") +  
  theme_minimal() +                               
  theme(plot.title = element_text(size = 16,      
                                  face = "bold"), 
        axis.text = element_text(size = 12),      
        axis.title = element_text(size = 13))   

#Vitoria
dados_vt_novo <- dados_vt %>%
  select(ï..nome, area_km2) %>%
  mutate(area_km2 = as.numeric(gsub(",", ".", area_km2)))

n_bairros_vt<-length(unique(dados_vt_novo$ï..nome))
bairros_vt_maior_1km2<-length(which(dados_vt_novo$area_km2>=1))
porcentagem_vt<-(bairros_vt_maior_1km2/n_bairros_vt)*100

plot_vt<-ggplot(dados_vt_novo, aes(x = area_km2)) +
  geom_histogram(binwidth = 0.5,                  
                 fill = "lightblue",             
                 color = "black",                 
                 alpha = 0.7) +
  geom_vline(xintercept = 1,                      
             color = "red",                       
             linetype = "dashed",                
             linewidth = 1) +                          
  labs(title = "Vitória", 
       subtitle = "13.75% dos bairros com área maior ou igual a 1km²",
       x = "Área (km²)",                          
       y = "",                           
       caption = "Total de 80 bairros") +  
  theme_minimal() +                               
  theme(plot.title = element_text(size = 16,      
                                  face = "bold"), 
        axis.text = element_text(size = 12),      
        axis.title = element_text(size = 13))    

#CRIANDO IMAGEM UNICA
combined_plot <- (plot_bh | plot_ct | plot_jp) /  
  (plot_mc | plot_nt | plot_rj) /  
  (plot_ssa | plot_sp | plot_vt)   

combined_plot
ggsave("Histogramas bairros.png", plot = combined_plot, width = 15, height = 8, dpi = 300)
