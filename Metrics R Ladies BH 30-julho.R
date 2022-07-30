
# ------------------------------------------------------------------------------
# 
# Workshop: Como Plotar Métricas e Entregar Valor para Times Ágeis
#
# Como plotar Métricas de Fluxo, interpretar entregando valor para o seu time!
# Usando o RStudio para visualizar e interpretar métricas
# 
# Facilitadora | Rosana Santos
#
# R Ladies Belo Horizonte
# 30/julho/2022, sábado
# ------------------------------------------------------------------------------

# limpa objetos da memória
rm (list = ls(all = T))    

# Instala e carrega pacotes caso necessário
if (!require('ggplot2'))   install.packages('ggplot2');   library('ggplot2')
if (!require('gridExtra')) install.packages('gridExtra'); library('gridExtra')
if (!require('lubridate')) install.packages('lubridate'); library('lubridate')
if (!require('scales'))    install.packages('scales');   library('scales')
if (!require('dplyr'))    install.packages('dplyr');    library('dplyr')

# carregando a base de dados de fluxo de desenvolvimento de software
df <- read.csv(file.choose(), header = TRUE, sep =";")

View (df)     # invoca um visualizador de dados estilo planilha
nrow (df)     # retorna o número de linhas
ncol (df)     # retorna o número de colunas
colnames(df)  # retorna os nomes das colunas
head (df, 4)  # retorna as 4 primeiras linhas do dataframe
tail (df, 2)  # retorna as 2 ultimas linhas do dataframe
summary(df)   # retorna sumário do dataframe: classe e comprimento


# ------------------------------------------------------------------------------
# Lead Time, Idade do Item e Tempo Ciclo = cálculo por Item
# ------------------------------------------------------------------------------
df$data_criacao <- format(as.Date(df$data_criacao, format="%d/%m/%Y"), "%Y/%m/%d")
df$data_inicio  <- format(as.Date(df$data_inicio, format="%d/%m/%Y"), "%Y/%m/%d")
df$data_fim     <- format(as.Date(df$data_fim, format="%d/%m/%Y"), "%Y/%m/%d")

df$tempo_ciclo  <- (difftime(df$data_fim,df$data_inicio, units="days")+1)
df$tempo_ciclo

df$lead_time    <- difftime(df$data_fim,df$data_criacao, units="days")
df$lead_time

df$idade        <- difftime(df$data_inicio,df$data_criacao, units="days")
df$idade

# ------------------------------------------------------------------------------
# Distribuição do Tempo Ciclo por Sprint
# ------------------------------------------------------------------------------
p1 <- ggplot (df, aes(sprint_origem, tempo_ciclo)) +
  geom_point (shape=15, fill="blue", color="blue", size=2) +
  geom_text(aes(label=tempo_ciclo), position = position_dodge(width = 1))+
   labs (title    = "Tempo Ciclo por Sprint",
         subtitle = "RLadies BH, Projeto Beta Tera",
         x        = "Sprint",
         y        = "tempo em dias") +
    scale_y_continuous (limits=c(0, 80)) +
  theme_grey()
p1

p11 <- ggplot (df, aes(sprint_origem, tempo_ciclo)) +
  geom_boxplot() +
  geom_text(aes(label=tempo_ciclo), position = position_dodge(width = 1))+
    labs (title    = "Tempo Ciclo por Sprint",
        subtitle = "RLadies BH, Projeto Beta Tera",
        x        = "Sprint",
        y        = "tempo em dias") +
  scale_y_continuous (limits=c(0, 80)) +
  theme_grey()
p11

# ------------------------------------------------------------------------------
# Distribuição do Lead Time por Sprint
# ------------------------------------------------------------------------------
p2 <- ggplot (df, aes(sprint_origem, lead_time)) +
  geom_point (shape=23, fill="darkorange", color="darkorange", size=2) +
  geom_text(aes(label=tempo_ciclo), position = position_dodge(width = 1))+
  labs (title    = "Lead Time por Sprint",
        subtitle = "RLadies BH, Projeto Beta Tera",
        x        = "Sprint",
        y        = "tempo em dias") +
  scale_y_continuous (limits=c(0, 80)) +
  theme_grey()
p2

p21 <- ggplot (df, aes(sprint_origem, lead_time)) +
  geom_boxplot() +
  geom_text(aes(label=tempo_ciclo), position = position_dodge(width = 1))+
  labs (title    = "Lead Time por Sprint",
        subtitle = "RLadies BH, Projeto Beta Tera",
        x        = "Sprint",
        y        = "tempo em dias") +
  scale_y_continuous (limits=c(0, 80)) +
  theme_grey()
p21

# ------------------------------------------------------------------------------
# Distribuição da Idade do Item por Sprint
# ------------------------------------------------------------------------------
p3 <- ggplot (df, aes(sprint_origem, idade)) +
  geom_point (shape=21, fill="magenta", color="magenta", size=2) +
  geom_text(aes(label=idade), position = position_dodge(width = 1))+
  labs (title    = "Idade do Item por Sprint",
        subtitle = "RLadies BH, Projeto Beta Tera",
        x        = "Sprint",
        y        = "tempo em dias") +
  scale_y_continuous (limits=c(0, 80)) +
  theme_grey()
p3

p31 <- ggplot (df, aes(sprint_origem, lead_time)) +
  geom_boxplot() +
  geom_text(aes(label=lead_time), position = position_dodge(width = 1))+
  labs (title    = "Lead Time por Sprint",
        subtitle = "RLadies BH, Projeto Beta Tera",
        x        = "Sprint",
        y        = "tempo em dias") +
  scale_y_continuous (limits=c(0, 80)) +
  theme_grey()
p31

# ------------------------------------------------------------------------------
# Visualizando Tempo Ciclo, Lead Time e Idade do Item por Sprint na mesma Tela
# ------------------------------------------------------------------------------
grid.arrange(p1, p2, p3, ncol = 3)

# ------------------------------------------------------------------------------
# Lead Time, Idade do Item e Tempo Ciclo = cálculo da média por Sprint
# ------------------------------------------------------------------------------

a <- aggregate (df$tempo_ciclo, list(df$sprint_origem), FUN=mean, na.rm=TRUE)
b <- aggregate (df$lead_time,   list(df$sprint_origem), FUN=mean, na.rm=TRUE)
c <- aggregate (df$idade,       list(df$sprint_origem), FUN=mean, na.rm=TRUE)


p4 <- ggplot (a, aes(Group.1, x)) +
    geom_point (shape=15, fill="blue", color="blue", size=2) +
  geom_text(aes(label=x), position = position_dodge(width = 1))+
  labs (title    = "Tempo Ciclo médio por Sprint",
        subtitle = "RLadies BH, Projeto Beta Tera",
        x        = "Sprint",
        y        = "tempo em dias") +
  scale_y_continuous (limits=c(0, 80)) +
  theme_grey()
p4

p5 <- ggplot (b, aes(Group.1, x)) +
  geom_point (shape=23, fill="darkorange", color="darkorange", size=2) +
  geom_text(aes(label=x), position = position_dodge(width = 1))+
  labs (title    = "Lead Time médio por Sprint",
        subtitle = "RLadies BH, Projeto Beta Tera",
        x        = "Sprint",
        y        = "tempo em dias") +
  scale_y_continuous (limits=c(0, 80)) +
  theme_grey()
p5

p6 <- ggplot (c, aes(Group.1, x)) +
  geom_point (shape=21, fill="magenta", color="magenta", size=2) +
  geom_text(aes(label=x), position = position_dodge(width = 1))+
  labs (title    = "Idade do Item média por Sprint",
        subtitle = "RLadies BH, Projeto Beta Tera",
        x        = "Sprint",
        y        = "tempo em dias") +
  scale_y_continuous (limits=c(0, 80)) +
  theme_grey()
p6

# ------------------------------------------------------------------------------
# Visualizando a Vaz�o di�ria
# ------------------------------------------------------------------------------
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)

colnames(df)
df$data_fim
d <- as.Date(df$data_fim, format = "%Y/%m/%d")


class(d)

p7 <- ggplot(df) +
  geom_histogram(aes(x = d)) +
  scale_x_date(date_breaks = "7 days") +
  labs (title    = "Vaz�o di�ia",
        subtitle = "RLadies BH, Projeto Beta Tera",
        x        = "Data",
        y        = "quantidade") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))
p7


grid.arrange(p7, p1, p3, nrow = 3)

# ------------------------------------------------------------------------------
# Visualizando o WIP
# ------------------------------------------------------------------------------

datas_trab <- c(min(df$data_inicio[!is.na(df$data_inicio)]), 
                max(df$data_fim[!is.na(df$data_fim)]) )

seq_datas <- data.frame(datas = seq(from=as.Date(datas_trab[1]), 
                                    to=as.Date(datas_trab[2]), by="day"),
                        WIP = rep(0))


for(i in 1:dim(seq_datas)[1]){
  
  if(!is.na(df$data_fim[i])){ #pulando cart�o que n�o tem data de fim
    datas1 <- seq(from=as.Date(df$data_inicio[i]), 
                  to=as.Date(df$data_fim[i]), by="day")
    
    for(j in 1:length(datas1)){
      seq_datas$WIP <- seq_datas$WIP + 
        ifelse(seq_datas$datas == datas1[j] ,1,0)
      
    }
  }
}

seq_datas

p8 <-plot(seq_datas, 
     main="WIP", sub="RLadies BH, Projeto Beta Tera")


grid.arrange(p7, p1, p8, p3, ncol=2, nrow = 2)
