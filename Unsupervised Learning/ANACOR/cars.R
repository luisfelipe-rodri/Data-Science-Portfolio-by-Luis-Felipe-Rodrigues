# Pacotes a serem instalados e carregados

pacotes <- c("plotly","tidyverse","ggrepel","sjPlot","knitr","kableExtra",
             "FactoMineR","cabootcrs","factoextra", "reshape2")

suppressPackageStartupMessages(sapply(pacotes, require, character = T))


# Carregando a base de dados

car <- read.csv(file = 'imports-85.data.csv')[, c(3,7)]

# Nomeando colunas

colnames(car) <- c('make', 'style')

#Observado os dados carregados

car[1:15,] %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

#Criando uma tabela de contingências

tab <- table(car$make,
             car$style)

tab

#Teste Qui-Quadrado

qui2_car <- chisq.test(tab)
qui2_car

#Mapa de calor dos resíduos padronizados ajustados

data.frame(qui2_car$stdres) %>%
  rename(make = 1,
         style = 2) %>% 
  ggplot(aes(x = make, y = style, fill = Freq, label = round(Freq,3))) +
  geom_tile() +
  geom_text(size = 3, angle = 90) +
  scale_fill_gradient2(low = "#440154FF", 
                       mid = "white", 
                       high = "#FDE725FF",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text(angle = 90))

#Decomposicao da inercia principal total

It <- qui2_car$statistic/nrow(car)
It

#Construindo a matriz P

P <- 1/nrow(car) * tab

#Column profile

data.frame(tab) %>% 
  group_by(Var2) %>% 
  summarise(Var1 = Var1,
            Massas = Freq / sum(Freq)) %>% 
  dcast(Var1 ~ Var2) %>% 
  column_to_rownames("Var1") %>% 
  round(., digits = 3)

column_profile <- apply(tab, MARGIN = 1, FUN = sum) / nrow(car)
column_profile

#Row profiles

data.frame(tab) %>% 
  group_by(Var1) %>% 
  summarise(Var2 = Var2,
            Massas = Freq / sum(Freq)) %>% 
  dcast(Var1 ~ Var2) %>% 
  column_to_rownames("Var1") %>% 
  round(., digits = 3) 

row_profile <- apply(tab, MARGIN = 2, FUN = sum) / nrow(car)
row_profile

#Matriz Dl
Dl <- diag(column_profile)

#Matriz Dc
Dc <- diag(row_profile)

#Matriz lc'
lc <- column_profile %o% row_profile

#Matriz A
A <- diag(diag(Dl) ^ (-1/2)) %*% (P - lc) %*% diag(diag(Dc) ^ (-1/2))

#Curiosidade:
A_matriz <- qui2_car$residuals / sqrt(nrow(car))

#Matriz W
W_matriz <- t(A_matriz) %*% A_matriz

#Extraindo os eigenvalues da matriz W
eigenvalues <- eigen(W_matriz)
eigenvalues

sum(eigenvalues$values) #It

#Dimensionalidade dos dados
dimensoes <- min(nrow(A_matriz) - 1, ncol(A_matriz) - 1)
dimensoes

#Percentual da Inercia Total explicada
It_explicada <- eigenvalues$values[1:2] / It
It_explicada

# Elaborando a ANACOR:

anacor <- CA(tab)

# Capturando todas as coordenadas num só objeto

ca_coordenadas <- rbind(anacor$row$coord, anacor$col$coord)
ca_coordenadas

# Capturando a quantidade de categorias por variável

id_var <- apply(car,
                MARGIN =  2,
                FUN = function(x) nlevels(as.factor(x)))
id_var

# Juntando as coordenadas e as categorias capturadas anteriormente

ca_coordenadas_final <- data.frame(ca_coordenadas, 
                                   Variable = rep(names(id_var), id_var))

ca_coordenadas_final

# Mapa perceptual elegante:

ca_coordenadas_final %>% 
  rownames_to_column() %>% 
  rename(Category = 1) %>% 
  ggplot(aes(x = Dim.1, 
             y = Dim.2, 
             label = Category, 
             color = Variable, 
             shape = Variable)) +
  geom_point(size = 2) +
  geom_text_repel(max.overlaps = 100,
                  size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = paste("Dimension 1:", paste0(round(anacor$eig[1,2], digits = 2), "%")),
       y = paste("Dimension 2:", paste0(round(anacor$eig[2,2], digits = 2), "%"))) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")

# Estabelecendo a Clusterização

cluster_cars <- kmeans(ca_coordenadas, centers = 2)

# Observando os resultados

fviz_cluster(cluster_cars, data = ca_coordenadas)



