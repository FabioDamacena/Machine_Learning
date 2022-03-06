## Machine Learning heart Disease

# Carrega pacotes úteis 

pacotes <- c(
  'tidyverse',  # Pacote básico de datawrangling
  'rpart',      # Biblioteca de árvores
  'rpart.plot', # Conjunto com Rpart, plota a parvore
  'gtools',     # funções auxiliares como quantcut,
  'Rmisc',      # carrega a função sumarySE para a descritiva
  'scales',     # importa paletas de cores
  'viridis',    # Escalas 'viridis' para o ggplot2
  'caret',       # Funções úteis para machine learning
  'AMR',
  'randomForest',
  'fastDummies',
  'rattle',
  'xgboost'
  
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
library(Rmisc)

# Após carregar a base de dados, muda nome das variáveis

cleveland = processed.cleveland %>% select(Age = V1,
                                           Sex = V2,
                                           ChestPainType = V3,
                                           Trestbps = V4,
                                           Chol = V5,
                                           Fbs = V6,
                                           Restecg = V7,
                                           Thalach= V8,
                                           Exang = V9,
                                           Oldpeak = V10,
                                           Slope = V11,
                                           Ca = V12,
                                           Thal = V13,
                                           Num = V14)

# muda categorias numéricas para o nome da categoria para facilitar entendimento

cleveland = mutate(cleveland, Sex = recode(Sex, "1" = "Male", "0" = "Female"))
cleveland = mutate(cleveland, ChestPainType = recode(ChestPainType, "1" = "typical angina",
                                                     "2" = "atypical angina",
                                                     "3" = "non-anginal pain",
                                                     "4" = "asymptomatic"))
cleveland = mutate(cleveland, Exang = recode(Exang, "1" = "yes", "0" = "no"))
cleveland = mutate(cleveland, Slope = recode(Slope, "1" = "upsloping",
                                             "2" = "flat",
                                             "3" = "downsloping"))
cleveland = mutate(cleveland, Thal = recode(Thal, "3.0" = "normal",
                                             "6.0" = "fixed defect",
                                             "7.0" = "reversable defect"))

cleveland = mutate(cleveland, Restecg = recode(Restecg, "0" = "normal",
                                            "1" = "ST-T wave abnormalityt",
                                            "2" = "left ventricular hypertrophy"))


# Análise descritiva

unique(cleveland$Num)
unique(cleveland$Fbs)

tmp <- cleveland # cópia da base cleveland

ggplot(data = tmp) +
  geom_bar(aes(x = Sex), color = "black", fill = "steelblue4") +
  labs(title = "Cleveland dataset",
       subtitle = "descriptive analysys",
       x = "Sex",
       y = "Quant",
       caption = "Período: 2021") +
  theme_classic()

# Existem cerca de 200 homens e 100 mulheres no dataset


ggplot(data = tmp) +
  geom_histogram(aes(x = Age), fill = "orange", color = "black") +
  labs(x = "Age",
       y = "Freq") +
  theme_minimal()

# A maioria das observações tem idade superior a 50 anos
mean(tmp$Age)

ggplot(data = tmp) +
  geom_bar(aes(x = ChestPainType), color = "black", fill = "steelblue4") +
  labs(title = "Cleveland dataset",
       subtitle = "descriptive analysys",
       x = "cp",
       y = "Quant",
       caption = "Período: 2021") +
  theme_classic()


ggplot(data = tmp) +
  geom_histogram(aes(x = Trestbps), fill = "springgreen4", color = "black") +
  labs(x = "Trestbps",
       y = "Freq") +
  theme_minimal()

ggplot(data = tmp) +
  geom_histogram(aes(x = Chol), fill = "springgreen4", color = "black") +
  labs(x = "chol",
       y = "Freq") +
  theme_minimal()

ggplot(data = tmp) +
  geom_bar(aes(x = Fbs), color = "black", fill = "steelblue4") +
  labs(title = "Cleveland dataset",
       subtitle = "descriptive analysys",
       x = "fbs",
       y = "Quant",
       caption = "Período: 2021") +
  theme_classic()

ggplot(data = tmp) +
  geom_bar(aes(x = Restecg), color = "black", fill = "steelblue4") +
  labs(title = "Cleveland dataset",
       subtitle = "descriptive analysys",
       x = "restecg",
       y = "Quant",
       caption = "Período: 2021") +
  theme_classic()

ggplot(data = tmp) +
  geom_histogram(aes(x = Thalach), fill = "springgreen4", color = "black") +
  labs(x = "thalach",
       y = "Freq") +
  theme_minimal()

ggplot(data = tmp) +
  geom_bar(aes(x = Exang), color = "black", fill = "steelblue4") +
  labs(title = "Cleveland dataset",
       subtitle = "descriptive analysys",
       x = "exang",
       y = "Quant",
       caption = "Período: 2021") +
  theme_classic()


ggplot(data = tmp) +
  geom_histogram(aes(x = Oldpeak), fill = "springgreen4", color = "black") +
  labs(x = "oldpeack",
       y = "Freq") +
  theme_minimal()

ggplot(data = tmp) +
  geom_bar(aes(x = Slope), color = "black", fill = "steelblue4") +
  labs(title = "Cleveland dataset",
       subtitle = "descriptive analysys",
       x = "slope",
       y = "Quant",
       caption = "Período: 2021") +
  theme_classic()

ggplot(data = tmp) +
  geom_bar(aes(x = Ca), color = "black", fill = "steelblue4") +
  labs(title = "Cleveland dataset",
       subtitle = "descriptive analysys",
       x = "ca",
       y = "Quant",
       caption = "Período: 2021") +
  theme_classic()

ggplot(data = tmp) +
  geom_bar(aes(x = Thal), color = "black", fill = "steelblue4") +
  labs(title = "Cleveland dataset",
       subtitle = "descriptive analysys",
       x = "thal",
       y = "Quant",
       caption = "Período: 2021") +
  theme_classic()

ggplot(data = tmp) +
  geom_bar(aes(x = Num), color = "black", fill = "steelblue4") +
  labs(title = "Cleveland dataset",
       subtitle = "descriptive analysys",
       x = "num",
       y = "Quant",
       caption = "Período: 2021") +
  theme_classic()


## Exercício de machine learning

#############################################
# construir a árvore de classificação #
arvore <- rpart(Num ~ .,
                data=tmp,
                parms = list(split = 'gini'), # podemos trocar para  'information'
                method='class' # Essa opção indica que a resposta é qualitativa
)

#########################
# Visualizando a árvore #

# Plotando a árvore
rpart.plot::rpart.plot(arvore,
                       box.palette = "auto") # Paleta de cores

rpart.plot::rpart.plot(arvore)


##############################
# Avaliação básica da árvore #

# Predizendo com a árvore

# Probabilidade de cada diagnóstico
prob = predict(arvore, tmp)
prob # indica a probabilidade de classificar a observação em cada tipo de diagnóstico

# Classificação dos diagnósticos
classification = prob[,1:5]>=.5 # devolve TRUE se a prob de cada diagnóstico >= 50%
classification
summary(classification)

# Matriz de confusão
# ver mais sobre matriz de confusão para esta predição com múltiplas categorias

?confusionMatrix()

confusionMatrix(factor(classification[,1]), factor(tmp$Num))


