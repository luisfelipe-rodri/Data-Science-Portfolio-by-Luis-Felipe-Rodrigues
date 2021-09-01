# Carregar pacotes

pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp", 'parallel', 'Rcpp', 'DiagrammeR',
             'xgboost', 'caret', 'method')

suppressPackageStartupMessages(sapply(pacotes, require, character = T))

options(warn=-1)

#Carregando a base de dados

houses <- as.data.frame(read_csv("houses_to_rent.csv"), col_types = cols())

# OBSERVANDO OS DADOS CARREGADOS DO DATASET 

head(houses, n=15) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 16)

#Estatasticas univariadas

summary(houses)


# Transformando colunas para forma numerica

houses$floor <- as.numeric(houses$floor) %>% replace_na(0)


for(i in 10:ncol(houses)) {
  x <- as.numeric(
            gsub('\\,', '.',
                 gsub('([a-zA-Z]\\$)', '', houses[ , i])))
  
  houses[ , i] <- x %>% replace_na(0)
}

# Estatisticas univariadas novamente

summary(houses)

# Selecionando apenas variaveis numericas

df = houses %>% 
      dplyr::select(where(is.numeric)) %>%
      select(-X1) %>%
      data.frame()

# ESTUDO DAS CORRELACOES  

df %>%
  correlation(method = "pearson") %>%
  plot()


chart.Correlation((df[1:10]), histogram = TRUE)


#Estimando a Regressão Multipla das variaveis numericas

modelo_houses <- lm(formula = total ~ . -X1 -city -animal -furniture,
                    data = houses)

#Parametros do modelo

summary(modelo_houses)

# intervalo de confianca
summ(modelo_houses, confint = T, digits = 3, ci.width = .95)
export_summs(modelo_houses, scale = F, digits = 5)

df$totalfit <- modelo_houses$fitted.values

# Adicionando Dummies

house_dummies <- dummy_columns(.data = houses,
                                   select_columns = c("animal", "furniture", "city"),
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = T)

#Observando os dados com dummies
head(house_dummies,n=15) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

glimpse(house_dummies)

#Estimando a Regressao Multipla com variaveis dummies

modelo_houses_dummies <- lm(formula = total ~ . - X1,
                           data = house_dummies)

#Parametros do modelo

summary(modelo_houses_dummies)

# Intervalo de Confianca
confint(modelo_houses_dummies, level = 0.95)

#TRANSFORMAcaO DE BOX-COX
#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(house_dummies$total) #funcao powerTransform do pacote car#
lambda_BC

#Inserindo o lambda de Box-Cox na base de dados para a estimaco de um novo modelo
house_dummies$bc_total <- (((house_dummies$total ^ lambda_BC$lambda) - 1) / 
                           lambda_BC$lambda)

modelo_bc <- lm(formula = bc_total ~ . - total - X1,
                data = house_dummies)

#Parametros do modelo
summary(modelo_bc)


#Comparando os parametros do modelo_linear com os do modelo_bc
export_summs(modelo_houses_dummies, modelo_bc, scale = F, digits = 4)

#Repare que ha um salto na qualidade do ajuste para o modelo nao linear (R2)
data.frame("R2OLS" = round(summary(modelo_houses_dummies)$r.squared, 4),
           "R2BoxCox" = round(summary(modelo_bc)$r.squared, 4)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)

# PROCEDIMENTO STEPWISE
#Aplicando o procedimento Stepwise, temos o seguinte código:
step_houses <- step(modelo_bc, k = 3.841459)

#Parametros do modelo

summary(step_houses)

#algumas variaveis sao excluidas apos o procedimento Stepwise, nesta
#forma funcional linear!

export_summs(step_houses, scale = F, digits = 5)

#Parametros reais do modelo com procedimento Stepwise
confint(step_houses, level = 0.95) # siginificancia 5%
plot_summs(step_houses, colors = "#440154FF") #funcao plot_summs do pacote ggstance

#Parametros padronizados
plot_summs(step_houses, scale = TRUE, colors = "#440154FF")

#Comparando os ICs dos betas dos modelos sem e com procedimento Stepwise
plot_summs(modelo_bc, step_houses, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("#FDE725FF", "#440154FF"))

# Separando em dados de teste e treino

set.seed(12)

indexes = createDataPartition(house_dummies$total, p = .85, list = F)

house_dummies$X1 <- NULL
house_dummies$bc_total <- NULL

m <- as.matrix(house_dummies)

train = m[indexes, ]
test = m[-indexes, ]

train_x = train[, -10]
train_x = scale(train_x)[,]
train_y = train[,10]

test_x = test[, -10]
test_x = scale(test[,-10])[,]
test_y = test[,10]

#Estimando Regressao por meio do algoritmo XGBOOST

m1_xgb <-
  xgboost(
    data = train_x,
    label = train_y,
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
  )

#valores previstos

pred_xgb <- predict(m1_xgb, test_x)

# Avaliando 
yhat <- pred_xgb
y <- test_y
postResample(yhat, y)

#Comparando com outros modelos

data.frame("R2OLS" = round(summary(modelo_houses_dummies)$r.squared, 4),
           "R2BoxCox" = round(summary(modelo_bc)$r.squared, 4),
           "R2XGB" = round(postResample(yhat, y)["Rsquared"],4)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 20)

# plotando os residuos 
r <- y - yhat
plot(r)

# relacao entre o previsto e o original
plot(y,
     yhat)
abline(lm(yhat ~ y))

# Arvores de decisao do modelo
xgb.plot.tree(model = m1_xgb, trees = 0:2)

# importancia da variaveis

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")



