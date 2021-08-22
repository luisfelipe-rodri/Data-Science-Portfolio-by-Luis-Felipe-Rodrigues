# carregando as bibliotecas 
suppressPackageStartupMessages(library("plotly"))
suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("knitr"))
suppressPackageStartupMessages(library("kableExtra"))
suppressPackageStartupMessages(library("car"))
suppressPackageStartupMessages(library("rgl"))
suppressPackageStartupMessages(library("gridExtra"))
suppressPackageStartupMessages(library("PerformanceAnalytics"))
suppressPackageStartupMessages(library("reshape2"))
suppressPackageStartupMessages(library("rayshader"))
suppressPackageStartupMessages(library("psych"))
suppressPackageStartupMessages(library("ggrepel"))
suppressPackageStartupMessages(library("factoextra"))
suppressPackageStartupMessages(library("sp"))
suppressPackageStartupMessages(library("gridExtra"))

# Carregar dados
pizza <- as.data.frame(read_csv("Pizza.csv"))

# Apresentando a base de dados:
pizza %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Analisando as correlações entre variáveis da base de dados
chart.Correlation(pizza[, 2:8], histogram = TRUE, pch = "+")

# Salvando a Matriz de Correlações
rho_pizza <- cor(pizza[,2:8])

# Construindo um mapa de calor a partir das correlações
rho_pizza %>% 
  melt() %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(x = Var1, y = Var2, label = round(x = value, digits = 3)),
            size = 4) +
  labs(x = NULL,
       y = NULL,
       fill = "Correlações") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))

# O teste de efericidade de Bartlett
cortest.bartlett(R = rho_pizza)

# O algoritmo prcomp(), do pacote psych, EXIGE que a a matriz de dados fornecida
# a ele já esteja padronizada pelo procedimento zscores:
pizza_std <- pizza %>% 
  select(-brand) %>% 
  select(-id) %>% 
  scale() %>% 
  data.frame()

# Rodando a PCA
afpc_pizza <- prcomp(pizza_std)
summary(afpc_pizza)

# Sumarizando pontos importantes:
data.frame(eigenvalue = afpc_pizza$sdev ^ 2,
           var_compartilhada = summary(afpc_pizza)$importance[2,],
           var_cumulativa = summary(afpc_pizza)$importance[3,]) -> relatorio

relatorio %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Visualizando os pesos que cada variável tem em cada componente principal 
# obtido pela PCA
ggplotly(
  data.frame(afpc_pizza$rotation) %>%
    mutate(var = names(pizza[2:8])) %>%
    melt(id.vars = "var") %>%
    mutate(var = factor(var)) %>%
    ggplot(aes(x = var, y = value, fill = var)) +
    geom_bar(stat = "identity", color = "black") +
    facet_wrap(~variable) +
    labs(x = NULL, y = NULL, fill = "Legenda:") +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))
)

# Scree Plot - apenas ignorar os warnings
ggplotly(
  fviz_eig(X = afpc_pizza,
           ggtheme = theme_bw(), 
           barcolor = "black", 
           barfill = "dodgerblue4",
           linecolor = "darkgoldenrod4")
)

#Extraindo as Cargas Fatoriais
k <- sum((afpc_pizza$sdev ^ 2) > 1) 
cargas_fatoriais <- afpc_pizza$rotation[, 1:k] %*% diag(afpc_pizza$sdev[1:k])

# Visualizando as cargas fatoriais
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Visualizando as Comunalidades
data.frame(rowSums(cargas_fatoriais ^ 2)) %>%
  rename(comunalidades = 1) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Relatório das cargas fatoriais e das comunalidades
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2) %>%
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Plotagem das Cargas Fatoriais
data.frame(cargas_fatoriais) %>%
  ggplot(aes(x = X1, y = X2)) +
  geom_point(color = "orange") +
  geom_hline(yintercept = 0, color = "darkorchid") +
  geom_vline(xintercept = 0, color = "darkorchid") +
  geom_text_repel(label = row.names(cargas_fatoriais)) +
  labs(x = "F1",
       y = "F2") +
  theme_bw()

# Scores Fatoriais
scores_fatoriais <- t(afpc_pizza$rotation)/afpc_pizza$sdev 
colnames(scores_fatoriais) <- colnames(pizza_std)

scores_fatoriais

scores_fatoriais %>%
  t() %>%
  data.frame() %>%
  rename(PC1 = 1,
         PC2 = 2) %>%
  select(PC1, PC2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Proposta da construção de um ranking ------------------------------------

#Assumindo-se apenas o F1 como indicador, calculam-se os scores fatoriais:
score_D1 <- scores_fatoriais[1,]
score_D1

#Estabelecendo o ranking dos indicadores assumido
F1 <- t(apply(pizza_std, 1, function(x) x * score_D1))

F1 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Na construção de rankings no R, devemos efetuar a multiplicação por -1, 
#visto que os scores fatoriais das observações mais fortes são, por padrão, 
#apresentados acompanhados do sinal de menos.
F1 <- data.frame(F1) %>%
  mutate(fator1 = rowSums(.) * -1)

F1 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Importando a coluna referente ao fator F1:
pizza["fator1"] <- F1$fator1

#Criando um ranking pela soma ponderada dos fatores por sua variância
#compartilhada
pizza %>%
  mutate(pontuacao = fator1 * 
           relatorio$var_compartilhada[1]) -> pizza

#Visualizando o ranking final
pizza %>%
  arrange(desc(pontuacao)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

