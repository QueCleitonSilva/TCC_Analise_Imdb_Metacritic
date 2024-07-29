
################################################################################
#                      DESCRIÇÃO E EXPLORAÇÃO DO DATASET                       #
################################################################################


# Carregando a biblioteca para manipulação de dados
library(dplyr)

# Carregando a base de dados
Dadosfiltrados <- read.csv("DadosFiltrados_filmesBase_15setembro.csv", encoding = "UTF-8")

write.csv(Dadosfiltrados, "dados_filtrados_25agosto.csv", row.names = FALSE)


#######################################################

# Sumário estatístico das variáveis 
summary(Dadosfiltrados)


########################################
# Variaveis e seus detalhes #
# Genero #

Dadosfiltrados$primeira_palavra_genero <- as.factor(Dadosfiltrados$primeira_palavra_genero)

# Criar uma tabela de frequência para a nova variável "primeira_palavra_genero"
tabela_frequencia_primeira_palavra <- table(Dadosfiltrados$primeira_palavra_genero)

# Calcular as proporções (porcentagens) das frequências
proporcoes <- prop.table(tabela_frequencia_primeira_palavra) * 100

# Ordenar a tabela de frequência por frequência decrescente
tabela_frequencia_primeira_palavra <- sort(tabela_frequencia_primeira_palavra, decreasing = TRUE)

# Converter a tabela ordenada em um data frame
tabela_frequencia_df <- as.data.frame(tabela_frequencia_primeira_palavra)

# Adicionar a coluna de proporções (porcentagens) ao data frame
tabela_frequencia_df$Porcentagem <- proporcoes

# Renomear as colunas do data frame
colnames(tabela_frequencia_df) <- c("Primeira_Palavra", "Frequencia", "Porcentagem")

# Salvar o data frame em um arquivo CSV
write.csv(tabela_frequencia_df, "tabela_frequencia_genero_primeirapalavra_porcentagem.csv", row.names = FALSE)

##########################################

# Variavel Duração #

# Realizar análise descritiva da variável "Duração"
descritiva_duração <- summary(Dadosfiltrados$Duração)

# Imprimir os resultados
print(descritiva_duração)

# Converter o objeto summary em um dataframe
descritiva_df <- as.data.frame(t(descritiva_duração))

write.csv(descritiva_df, "analise_descritiva_duracao.csv", row.names = TRUE)
###################################

# Variavel Lingua #

# Calcular a tabela de frequência para a variável "Lingua"
tabela_frequencia_lingua <- table(Dadosfiltrados$Lingua)

# Calcular as proporções (porcentagens) das frequências
proporcoes_lingua <- prop.table(tabela_frequencia_lingua) * 100

# Criar um dataframe com os resultados
tabela_com_proporcoes_lingua <- data.frame(Lingua = names(tabela_frequencia_lingua),
                                           Frequencia = as.vector(tabela_frequencia_lingua),
                                           Porcentagem = proporcoes_lingua)

# Ordenar o dataframe por frequência decrescente
tabela_com_proporcoes_lingua <- tabela_com_proporcoes_lingua[order(tabela_com_proporcoes_lingua$Frequencia, decreasing = TRUE),]

# Salvar o dataframe com as frequências e proporções em um arquivo CSV
write.csv(tabela_com_proporcoes_lingua, "tabela_frequencia_lingua.csv", row.names = FALSE)

Dadosfiltrados$Lingua <- factor(Dadosfiltrados$Lingua)

#############################################################

# Variavel Diretor #

# Calcular a tabela de frequência para a variável "Diretor"
tabela_frequencia_diretor <- table(Dadosfiltrados$Diretor)

# Calcular as proporções (porcentagens) das frequências
proporcoes_diretor <- prop.table(tabela_frequencia_diretor) * 100

# Criar um dataframe com os resultados
tabela_com_proporcoes_diretor <- data.frame(Diretor = names(tabela_frequencia_diretor),
                                            Frequencia = as.vector(tabela_frequencia_diretor),
                                            Porcentagem = proporcoes_diretor)

# Ordenar o dataframe por frequência decrescente
tabela_com_proporcoes_diretor <- tabela_com_proporcoes_diretor[order(tabela_com_proporcoes_diretor$Frequencia, decreasing = TRUE),]

# Imprimir a tabela com frequências e proporções
print(tabela_com_proporcoes_diretor)

# Salvar o dataframe com as frequências e proporções em um arquivo CSV
write.csv(tabela_com_proporcoes_diretor, "tabela_frequencia_diretor.csv", row.names = FALSE)

Dadosfiltrados$Diretor <- as.factor(Dadosfiltrados$Diretor)

#############################################################

# Variavel Escritor #

# Calcular a tabela de frequência para a variável "Escritor"
tabela_frequencia_escritor <- table(Dadosfiltrados$Escritor)

# Calcular as proporções (porcentagens) das frequências
proporcoes_escritor <- prop.table(tabela_frequencia_escritor) * 100

# Criar um dataframe com os resultados
tabela_com_proporcoes_escritor <- data.frame(Diretor = names(tabela_frequencia_escritor),
                                            Frequencia = as.vector(tabela_frequencia_escritor),
                                            Porcentagem = proporcoes_escritor)

# Ordenar o dataframe por frequência decrescente
tabela_com_proporcoes_escritor <- tabela_com_proporcoes_escritor[order(tabela_com_proporcoes_escritor$Frequencia, decreasing = TRUE),]

# Imprimir a tabela com frequências e proporções
print(tabela_com_proporcoes_escritor)

# Salvar o dataframe com as frequências e proporções em um arquivo CSV
write.csv(tabela_com_proporcoes_escritor, "tabela_frequencia_escritor.csv", row.names = FALSE)

#Transformando em fator
Dadosfiltrados$Escritor <- as.factor(Dadosfiltrados$Escritor)
#############################################################

# Variavel Atores ############################

# Preparação da variavel#

# Dividir a coluna Atores em diferentes atores usando a função strsplit
atores_divididos <- strsplit(Dadosfiltrados$Atores, ",")

# Criar as novas colunas no dataframe
Dadosfiltrados$Ator_principal <- sapply(atores_divididos, `[`, 1)
Dadosfiltrados$Ator_coadjuvante <- sapply(atores_divididos, `[`, 2)
Dadosfiltrados$Ator_coadjuvante2 <- sapply(atores_divididos, `[`, 3)
Dadosfiltrados$Ator_coadjuvante3 <- sapply(atores_divididos, `[`, 4)

####################################################

# Criar a tabela de frequência
tabela_frequencia <- table(Dadosfiltrados$Ator_principal)

# Calcular as porcentagens
porcentagens <- prop.table(tabela_frequencia) * 100

# Criar um dataframe com os resultados
resultado <- data.frame(Ator_principal = names(tabela_frequencia),
                        Frequencia = as.vector(tabela_frequencia),
                        Porcentagem = as.vector(porcentagens))

write.csv(resultado, "tabela_frequencia.csv", row.names = FALSE)

#Transformando em fator
Dadosfiltrados$Ator_principal <- factor(Dadosfiltrados$Ator_principal)
###############################################

# Atores coadjuvantes #

# Criar a tabela de frequência
tabela_frequencia_Coadjuvantes <- table(Dadosfiltrados$Ator_coadjuvante)

# Calcular as porcentagens
porcentagens <- prop.table(tabela_frequencia_Coadjuvantes) * 100

# Criar um dataframe com os resultados
resultado_Coadjuvantes <- data.frame(Ator_coadjuvante = names(tabela_frequencia_Coadjuvantes),
                                     Frequencia = as.vector(tabela_frequencia_Coadjuvantes),
                                     Porcentagem = as.vector(porcentagens))

# Ordenar o dataframe pela frequência (opcional)
resultado_Coadjuvantes <- resultado_Coadjuvantes[order(resultado_Coadjuvantes$Frequencia, decreasing = TRUE), ]

#salvando em csv
write.csv(resultado_Coadjuvantes, "resultado_Coadjuvantes.csv", row.names = FALSE)


##########################################################################
#Distribuição da variavel ROI_percentual por genero principla#

resultados <- aggregate(ROI_percentual ~ genero_principal, data = Dadosfiltrados, 
          FUN = function(x) c(max = max(x), mean = mean(x), min = min(x), Q1 = quantile(x, 0.25), Q3 = quantile(x, 0.75)))

write.csv(resultados, "resultados_Roi_por_genero.csv", row.names = FALSE)

##########################################################################


resultados_variaveis_numericas <- sapply(Dadosfiltrados[, c("Nota_média_Imdb", "Número_votos_IMDB", "Orçamento_Usd_USA", "Renda_bruta_mundial", "ROI_percentual", "Nota_média_metascore", "Número_avaliações_Metacritics", "Número_avaliações_criticoscinema", "Ano")], 
                     function(x) c(max = max(x), mean = mean(x), min = min(x), Q1 = quantile(x, 0.25), Q3 = quantile(x, 0.75)))

write.csv(resultados_variaveis_numericas, file = "resultados_variaveis_numericas.csv", row.names = FALSE)


################################################
# Carregue o pacote dplyr

install.packages("rlang")
library(rlang)

install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

install.packages("GGally")
library(GGally)

# Se Dadosfiltrados não for um data frame, transforme-o em um data frame
if (!is.data.frame(Dadosfiltrados)) {
  Dadosfiltrados <- as.data.frame(Dadosfiltrados)
}

Dadosfiltrados %>%
  select(
    ROI_percentual,
    Duração,
    Nota_média_Imdb,
    Número_votos_IMDB,
    Orçamento_Usd_USA,
    Renda_bruta_mundial,
    Nota_média_metascore,
    Número_avaliações_Metacritics,
    Número_avaliações_criticoscinema
  ) %>%
  ggpairs()

  ###Ano, Duração, Nota_média_Imdb, Número_votos_IMDB, Orçamento_Usd_USA, Renda_bruta_mundial, X.ROI_rendabrutamundialUSD_vs_orçamentoUSD , Nota_média_metascore, Número_avaliações_Metacritics, Número_avaliações_criticoscinema

str(Dadosfiltrados)
########################################################

# Ajuste do modelo de regressão linear
modelo <- lm(Nota_média_Imdb ~ Nota_média_metascore, data = Dadosfiltrados)

# Sumário do modelo
summary(modelo)

# Calcular os valores previstos pelo modelo
predicoes <- predict(modelo, newdata = Dadosfiltrados)

# Calcular o RMSE
rmse <- sqrt(mean((Dadosfiltrados$Nota_média_Imdb - predicoes)^2))

# Calcular o MAE
mae <- mean(abs(Dadosfiltrados$Nota_média_Imdb - predicoes))

# Calcular o erro percentual em relação à variável de resposta real
erro_percentual <- (mae / mean(Dadosfiltrados$Nota_média_Imdb)) * 100

#########################################################################

# Carregue as bibliotecas necessárias
install.packages("caret")
library(caret)

# Defina uma semente para garantir a reprodutibilidade
set.seed(123)

# Divida os dados em treinamento (70%) e teste (30%)
amostra <- createDataPartition(Dadosfiltrados$Nota_média_Imdb, p = 0.7, list = FALSE)
dados_treinamento <- Dadosfiltrados[amostra, ]
dados_teste <- Dadosfiltrados[-amostra, ]

# Ajuste o modelo de regressão linear aos dados de treinamento
modelo <- lm(Nota_média_Imdb ~ Nota_média_metascore, data = dados_treinamento)

# Faça previsões usando o modelo nos dados de teste
previsoes <- predict(modelo, newdata = dados_teste)

# Avalie o desempenho do modelo
erro_percentual <- mean(abs(dados_teste$Nota_média_Imdb - previsoes) / dados_teste$Nota_média_Imdb) * 100

# Visualize o erro percentual
print(paste("Erro Percentual Médio: ", round(erro_percentual, 2), "%"))

#dados do modelo 
summary(erro_percentual)

#########################################################

# Valor estimado
valor_estimado <- 7.0

# MAPE (Mean Absolute Percentage Error) de 9.23%
mape <- 9.23 / 100  # Convertendo a porcentagem para decimal

# Valor Real (aqui, usaremos o valor estimado como o valor real para fins de ilustração)
valor_real <- valor_estimado

# Calculando o MAE
mae <- mape * valor_real

# Calculando a margem de erro (metade do MAE)
margem_de_erro <- mae / 2

# Visualizando a margem de erro
print(paste("A margem de erro é de +/-", round(margem_de_erro, 2)))

#########################################################

# Calcule os quartis da variável ROI_percentual
quartis <- quantile(Dadosfiltrados$ROI_percentual, probs = c(0.25, 0.75))

# Filtre as observações dentro do primeiro até o terceiro quartil
dados_filtrados <- Dadosfiltrados %>% 
  filter(ROI_percentual >= quartis[1] & ROI_percentual <= quartis[2])

# Crie um gráfico de dispersão
library(ggplot2)

ggplot(dados_filtrados, aes(x = ROI_percentual, y = Nota_média_Imdb)) +
  geom_point() +
  labs(x = "ROI_percentual", y = "Nota_média_IMDB") +
  ggtitle("Gráfico de Dispersão de ROI_percentual vs. Nota_média_Imdb") +
  theme_minimal()

###########################################################

# Calcule os quartis da variável ROI_percentual
quartis <- quantile(Dadosfiltrados$ROI_percentual, probs = c(0.25, 0.75))

# Filtre as observações dentro do primeiro até o terceiro quartil
dados_filtrados <- Dadosfiltrados %>% 
  filter(ROI_percentual >= quartis[1] & ROI_percentual <= quartis[2])

# Crie um gráfico de dispersão com a variável "Nota_média_metascore" no eixo y
library(ggplot2)

ggplot(dados_filtrados, aes(x = ROI_percentual, y = Nota_média_metascore)) +
  geom_point() +
  labs(x = "ROI_percentual", y = "Nota_média_metascore") +
  ggtitle("Gráfico de Dispersão de ROI_percentual vs. Nota_média_metascore") +
  theme_minimal()

##############################################################

# Carregue a biblioteca ggplot2 se ainda não estiver carregada
library(ggplot2)

# Crie um gráfico de dispersão comparando as variáveis
ggplot(Dadosfiltrados, aes(x = Nota_média_metascore, y = Nota_média_Imdb)) +
  geom_point() +
  labs(x = "Nota_média_metascore", y = "Nota_média_Imdb") +
  ggtitle("Gráfico de Dispersão de Nota_média_metascore vs. Nota_média_Imdb") +
  theme_minimal()

##############################################################

# Nível de confiança (em decimal)
confianca <- 0.95

# Margem de erro desejada
margem_erro <- 0.05

# Valor crítico da distribuição normal padrão
z <- qnorm((1 + confianca) / 2)

# Desvio padrão estimado (substitua pelo valor real)
desvio_padrao <- sd(Dadosfiltrados$Nota_média_metascore)

# Tamanho da amostra necessário
tamanho_amostra <- (z^2 * desvio_padrao^2) / margem_erro^2

# Arredonde para o próximo número inteiro
tamanho_amostra <- ceiling(tamanho_amostra)

# Exibir o tamanho da amostra necessário
print(paste("Tamanho da amostra necessário:", tamanho_amostra))


#########################################################

# Suponha que você tenha um dataframe chamado Dadosfiltrados
# com as variáveis Nota_média_Imdb e ROI_percentual

# Criar as faixas desejadas para a variável Nota_média_Imdb
faixas <- cut(Dadosfiltrados$Nota_média_Imdb, 
              breaks = c(0, 1.9, 3.9, 6.9, 8.9, 10), 
              labels = c("0-1.9", "2-3.9", "4-6.9", "7-8.9", "9-10"))

# Calcular a média de ROI_percentual por faixa
resultado <- aggregate(Dadosfiltrados$ROI_percentual, by = list(faixas), FUN = mean)

# Renomear as colunas do resultado
colnames(resultado) <- c("Faixa_Nota_Imdb", "Média_ROI_percentual")

# Exibir o resultado
print(resultado)

##############################################

## Média Orçamento USD ##

# Calcular a média de Orçamento_usd_USA por faixa
resultado_orcamento <- aggregate(Dadosfiltrados$Orçamento_Usd_USA, by = list(faixas), FUN = mean)

# Renomear as colunas do resultado
colnames(resultado_orcamento) <- c("Faixa_Nota_Imdb", "Média_Orçamento_usd_USA")

# Exibir o resultado
print(resultado_orcamento)

###################################################

# Contar o número de observações em cada faixa
contagem_por_faixa <- table(faixas)

# Exibir o resultado
print(contagem_por_faixa)

#########################################################

## Arvore de decisão ##

install.packages("rpart")

library(rpart)

# Suponha que você tenha uma base de dados chamada Dadosfiltrados
set.seed(123)  # Defina uma semente para reproducibilidade
percentagem_treinamento <- 0.7  # Por exemplo, 70% para treinamento
indices_treinamento <- sample(1:nrow(Dadosfiltrados), 
                              size = percentagem_treinamento * nrow(Dadosfiltrados))
dados_treinamento <- Dadosfiltrados[indices_treinamento, ]
dados_teste <- Dadosfiltrados[-indices_treinamento, ]

modelo_arvore <- rpart(Nota_média_Imdb ~ Nota_média_metascore + genero_principal
                       + Duração + Orçamento_Usd_USA, data = Dadosfiltrados, xval=10,
                       control = rpart.control(cp = 0, 
                                               minsplit = 2,
                                               maxdepth = 30))
######################################
# 2.3 Complexidade dos caminhos
######################################

tab_cp <- rpart::printcp(modelo_arvore)
rpart::plotcp(modelo_arvore)

######################################
# 2.4 Escolher o caminho que otimiza a impureza no cross validation
######################################
tab_cp <- rpart::printcp(modelo_arvore)
rpart::plotcp(modelo_arvore)

tab_cp[which.min(tab_cp[,'xerror']),]

cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']
cp_min

#############################################################

previsoes <- predict(modelo_arvore, newdata = Dadosfiltrados)

# Calcula o SQE (Soma dos Quadrados de Erro)
SQE <- sum((Dadosfiltrados$Nota_média_Imdb - previsoes)^2)

# Calcula a SST (Soma dos Quadrados Total)
SST <- sum((Dadosfiltrados$Nota_média_Imdb - mean(Dadosfiltrados$Nota_média_Imdb))^2)

# Calcula o QMT (Quadrado Médio do Tratamento)
QMT <- (SST - SQE) / (length(Dadosfiltrados$Nota_média_Imdb) - 1)

# Calcula o R-quadrado
R_quadrado <- 1 - (SQE / SST)

# Imprime os resultados
cat("SQE:", SQE, "\n")
cat("SST:", SST, "\n")
cat("QMT:", QMT, "\n")
cat("R-quadrado:", R_quadrado, "\n")

# Crie um gráfico de dispersão dos dados observados vs. dados esperados
plot(Dadosfiltrados$Nota_média_Imdb, previsoes, 
     xlab = "Dados Observados", ylab = "Dados Esperados",
     main = "Gráfico de Dados Observados vs. Dados Esperados")

# Adicione uma linha de referência de 45 graus (para verificar a igualdade perfeita)
abline(a = 0, b = 1, col = "red")

# Adicione uma legenda
legend("topleft", legend = "Igualdade Perfeita", col = "red", lty = 1)

##Arvore tunada após CP minimo ##

modelo_arvore <- rpart(Nota_média_Imdb ~ Nota_média_metascore + genero_principal
                       + Duração + Orçamento_Usd_USA, data = Dadosfiltrados, xval=10,
                       control = rpart.control(cp = cp_min, maxdepth = 30))

summary(modelo_arvore)
#####################################################################################

# Extrair os valores reais da base de teste
valores_reais <- dados_teste$Nota_média_Imdb


# Calcular o erro absoluto médio (MAE)
mae <- mean(abs(previsoes - valores_reais))

# Calcular a porcentagem de erro em relação aos valores reais
porcentagem_erro <- (mae / mean(valores_reais)) * 100

print(paste("Erro Absoluto Médio (MAE):", mae))
print(paste("Porcentagem de Erro:", porcentagem_erro, "%"))


# Calcular o MSE para a base de teste
mse_teste <- mean((previsoes - dados_teste$Nota_média_Imdb)^2)

# Calcular o MAE para a base de teste
mae_teste <- mean(abs(previsoes - dados_teste$Nota_média_Imdb))

# Visualizar o MSE e o MAE para a base de teste
print(paste("MSE para a base de teste:", round(mse_teste, 3)))
print(paste("MAE para a base de teste:", round(mae_teste, 3)))

# Carregue a biblioteca "rpart.plot" para plotar a árvore
library(rpart.plot)
install.packages("rpart.plot")

# Crie um gráfico das previsões
plot(previsoes, main = "Gráfico das Previsões da Árvore de Decisão")

# Plotar a árvore de decisão
prp(modelo_arvore, main = "Árvore de regressão")

#####################################################################################

######################################################################################
# Ajuste a árvore de decisão e defina um valor menor para cp (ajuste conforme necessário)
modelo_arvore <- rpart(formula = Nota_média_Imdb ~ Nota_média_metascore + genero_principal + 
                         Duração + Orçamento_Usd_USA, data = dados_treinamento, control = rpart.control(cp = 0.0008))

# Crie a árvore de regressão com um valor mínimo de observações em um nó folha (ajuste conforme necessário)
mincut_value <- 20  # Defina o valor mínimo desejado de observações em um nó folha

arvore_poda <- tree(Nota_média_Imdb ~ Nota_média_metascore + genero_principal, data = dados_treinamento, mincut = mincut_value)


# Visualize a árvore de regressão podada
plot(modelo_arvore)
text(modelo_arvore)

summary(arvore_poda)

plot(arvore_poda)
text(arvore_poda)

install.packages("tree")
library(tree)

# Plotar a árvore de decisão
prp(modelo_arvore, main = "Árvore de Regressão")

#####################################################################################
#####################################################################################

# Crie um novo conjunto de dados para prever (substitua os valores pelas suas observações)
MissaoImp_2023_filme <- data.frame(
  Nota_média_metascore = 81, # Substitua pelo valor real
  genero_principal = "Action", # Substitua pelo valor real
  Duração = 164, # Substitua pelo valor real
  Orçamento_Usd_USA = 29100000 # Substitua pelo valor real
)

# Faça a previsão
previsao <- predict(modelo_arvore, MissaoImp_2023_filme)
# A variável 'previsao' agora contém a previsão da Nota_média_Imdb

######################################################################################

# Crie um novo conjunto de dados para prever (substitua os valores pelas suas observações)
Barbie_2023_filme <- data.frame(
  Nota_média_metascore = 80, # Substitua pelo valor real
  genero_principal = "Comedy", # Substitua pelo valor real
  Duração = 114, # Substitua pelo valor real
  Orçamento_Usd_USA = 145000000 # Substitua pelo valor real
)

# Faça a previsão
previsao <- predict(modelo_arvore, Barbie_2023_filme)

# A variável 'previsao' agora contém a previsão da Nota_média_Imdb

#####################################################################################

######################################################################################

# Crie um novo conjunto de dados para prever (substitua os valores pelas suas observações)
######################################################################################

# Crie um novo conjunto de dados para prever (substitua os valores pelas suas observações)
Barbie_2023_filme <- data.frame(
  Nota_média_metascore = 80, # Substitua pelo valor real
  genero_principal = "Comedy", # Substitua pelo valor real
  Duração = 114, # Substitua pelo valor real
  Orçamento_Usd_USA = 145000000 # Substitua pelo valor real
)

# Faça a previsão
previsao <- predict(modelo_arvore, Barbie_2023_filme)

# A variável 'previsao' agora contém a previsão da Nota_média_Imdb

#####################################################################################
ToyStory_2019_filme <- data.frame(
  Nota_média_metascore = 84, # Substitua pelo valor real
  genero_principal = "Animation", # Substitua pelo valor real
  Duração = 100, # Substitua pelo valor real
  Orçamento_Usd_USA = 200000000 # Substitua pelo valor real
)

# Faça a previsão
previsao <- predict(modelo_arvore, ToyStory_2019_filme)

# A variável 'previsao' agora contém a previsão da Nota_média_Imdb

#####################################################################################

#####################################################################################

## Arvore de decisão após KMeans##

install.packages("rpart")

library(rpart)

# Suponha que você tenha uma base de dados chamada Dadosfiltrados
set.seed(123)  # Defina uma semente para reproducibilidade
percentagem_treinamento <- 0.7  # Por exemplo, 70% para treinamento
indices_treinamento <- sample(1:nrow(Dadosfiltrados), 
                              size = percentagem_treinamento * nrow(Dadosfiltrados))
dados_treinamento <- Dadosfiltrados[indices_treinamento, ]
dados_teste <- Dadosfiltrados[-indices_treinamento, ]

# Aumente a profundidade máxima da árvore para, por exemplo, 5 (ajuste conforme necessário)
modelo_arvore_Kmeans <- rpart(Nota_média_Imdb ~ Nota_média_metascore + genero_principal
                              + Duração + Orçamento_Usd_USA + Pais_reduz + Lingua_reduz + Diretor_reduz + Escritor_reduz + Produtora_reduz + Ator_principal_reduz + Ator_coadjuvante_reduz, 
                              data = dados_treinamento, control = rpart.control(cp = 0.0008))

previsoes <- predict(modelo_arvore_Kmeans, newdata = dados_teste)

# Extrair os valores reais da base de teste
valores_reais <- dados_teste$Nota_média_Imdb

# Calcular o erro absoluto médio (MAE)
mae <- mean(abs(previsoes - valores_reais))

# Calcular a porcentagem de erro em relação aos valores reais
porcentagem_erro <- (mae / mean(valores_reais)) * 100

print(paste("Erro Absoluto Médio (MAE):", mae))
print(paste("Porcentagem de Erro:", porcentagem_erro, "%"))

# Portanto, a margem de erro para mais e para menos em uma previsão de nota 7.0 da 
# variável "Nota_média_Imdb" é de aproximadamente 0.0038 (ou 0.38%) em ambas as direções. 
# Isso significa que a previsão pode estar dentro de +/- 0.0038 da nota 7.0, com base
# no MAE e na porcentagem de erro calculados

summary(modelo_arvore_Kmeans)

# Calcular o MSE para a base de teste
mse_teste <- mean((previsoes - dados_teste$Nota_média_Imdb)^2)

# Calcular o MAE para a base de teste
mae_teste <- mean(abs(previsoes - dados_teste$Nota_média_Imdb))

# Visualizar o MSE e o MAE para a base de teste
print(paste("MSE para a base de teste:", round(mse_teste, 3)))
print(paste("MAE para a base de teste:", round(mae_teste, 3)))

# Carregue a biblioteca "rpart.plot" para plotar a árvore
library(rpart.plot)
install.packages("rpart.plot")

# Crie um gráfico das previsões
plot(previsoes, main = "Gráfico das Previsões da Árvore de Decisão")

# Plotar a árvore de decisão
install.packages("rpart.plot")
library(rpart.plot)

prp(modelo_arvore_Kmeans, main = "Árvore de Decisão utilizando Kmeans", box.palette = "BuGn")


#####################################################################################

# Substitua os valores abaixo pelos valores reais que deseja prever
novo_dado <- data.frame(
  Nota_média_metascore = 75, # Substitua pelo valor real
  genero_principal = "Ação", # Substitua pelo valor real
  Duração = 120, # Substitua pelo valor real
  Orçamento_Usd_USA = 50000000, # Substitua pelo valor real
  Pais_reduz = "EUA", # Substitua pelo valor real
  Lingua_reduz = "Inglês", # Substitua pelo valor real
  Diretor_reduz = "Diretor_A", # Substitua pelo valor real
  Escritor_reduz = "Escritor_B", # Substitua pelo valor real
  Produtora_reduz = "Produtora_C", # Substitua pelo valor real
  Ator_principal_reduz = "Ator_X", # Substitua pelo valor real
  Ator_coadjuvante_reduz = "Ator_Y" # Substitua pelo valor real
)

# Realize a previsão
previsao <- predict(modelo_arvore_Kmeans, novo_dado)

# A variável 'previsao' agora contém o resultado da previsão

#####################################################################################

## Calcular formado das categorias criadas ###

# Carregue a biblioteca dplyr (se ainda não estiver carregada)
library(dplyr)

# Crie uma tabela de correspondência entre 'Pais' e 'Pais_reduz'
tabela_correspondencia <- Dadosfiltrados %>%
  select(Pais, Pais_reduz) %>%
  distinct()  # Remove duplicatas


# Salve os resultados em um arquivo CSV
write.csv(resultados, "resultados.csv", row.names = FALSE)


##################################################################################

# Use a função sapply para calcular o número de categorias únicas para cada variável
num_categorias <- sapply(Dadosfiltrados, function(x) length(unique(x)))

# Exiba o número de categorias para cada variável
print(num_categorias)

#################################################################################

## Redução categorias variavel Pais ##

# Carregue as bibliotecas necessárias
library(tidyverse)

# Carregue seus dados (substitua "seuarquivo.csv" pelo caminho correto do seu arquivo)
Dadosfiltrados <- read.csv("seuarquivo.csv")

# Selecione apenas as variáveis relevantes para a análise
dados_analise <- Dadosfiltrados %>%
  select(Pais, Nota_média_Imdb)

# Defina o número mínimo e máximo de clusters que deseja testar
num_clusters_min <- 5
num_clusters_max <- 50

# Crie uma lista vazia para armazenar a inércia total
inercia_total <- vector("double", length = num_clusters_max - num_clusters_min + 1)

# Loop para ajustar o K-Means para diferentes números de clusters e calcular a inércia total
for (k in num_clusters_min:num_clusters_max) {
  modelo_kmeans <- kmeans(dados_analise[, 2], centers = k)
  inercia_total[k - num_clusters_min + 1] <- modelo_kmeans$tot.withinss
}

# Crie um gráfico de linha para visualizar a inércia total em relação ao número de clusters
plot(num_clusters_min:num_clusters_max, inercia_total, type = "b", 
     xlab = "Número de Clusters", ylab = "Inércia Total",
     main = "Elbow Method para Escolher o Número de Clusters")

# Adicione uma linha vertical para indicar o ponto de inflexão (cotovelo)
abline(v = which(diff(inercia_total) < mean(diff(inercia_total))) + num_clusters_min - 1, col = "red")

# Legenda
legend("topright", legend = "Ponto de Inflexão", col = "red", lty = 1, cex = 0.8)

# Defina os rótulos no eixo x para mostrar todos os números de clusters
axis(1, at = num_clusters_min:num_clusters_max)

######################################################################################

# Ajuste o modelo K-Means com o número escolhido de clusters
n_clusters <- 21  # Substitua pelo número de clusters escolhido
modelo_final <- kmeans(dados_analise[, 2], centers = n_clusters)

# Adicione a nova variável categórica ao Dadosfiltrados
Dadosfiltrados$Pais_reduz <- as.factor(modelo_final$cluster)

######################################################################################


#################################################################################

## Redução categorias variavel Lingua ##


# Selecione apenas as variáveis relevantes para a análise (Pais e Nota_média_Imdb)
dados_analise <- Dadosfiltrados %>%
  select(Lingua, Nota_média_Imdb)

# Defina o número mínimo e máximo de clusters que deseja testar
num_clusters_min <- 5
num_clusters_max <- 50

# Crie uma lista vazia para armazenar a inércia total
inercia_total <- vector("double", length = num_clusters_max - num_clusters_min + 1)

# Loop para ajustar o K-Means para diferentes números de clusters e calcular a inércia total
for (k in num_clusters_min:num_clusters_max) {
  modelo_kmeans <- kmeans(dados_analise[, 2], centers = k)
  inercia_total[k - num_clusters_min + 1] <- modelo_kmeans$tot.withinss
}

# Crie um gráfico de linha para visualizar a inércia total em relação ao número de clusters
plot(num_clusters_min:num_clusters_max, inercia_total, type = "b", 
     xlab = "Número de Clusters", ylab = "Inércia Total",
     main = "Elbow Method para Escolher o Número de Clusters (Lingua)")

# Adicione uma linha vertical para indicar o ponto de inflexão (cotovelo)
abline(v = which(diff(inercia_total) < mean(diff(inercia_total))) + num_clusters_min - 1, col = "red")


# Legenda
legend("topright", legend = "Ponto de Inflexão", col = "red", lty = 1, cex = 0.8)

# Defina os rótulos no eixo x para mostrar todos os números de clusters
axis(1, at = num_clusters_min:num_clusters_max)

######################################################################################

# Ajuste o modelo K-Means com o número escolhido de clusters
n_clusters <- 23  # Substitua pelo número de clusters escolhido
modelo_final <- kmeans(dados_analise[, 2], centers = n_clusters)

# Adicione a nova variável categórica ao Dadosfiltrados
Dadosfiltrados$Lingua_reduz <- as.factor(modelo_final$cluster)

######################################################################################

#################################################################################

## Redução categorias variavel Diretor ##


# Selecione apenas as variáveis relevantes para a análise (Diretor e Nota_média_Imdb)
dados_analise <- Dadosfiltrados %>%
  select(Diretor, Nota_média_Imdb)

# Defina o número mínimo e máximo de clusters que deseja testar
num_clusters_min <- 20
num_clusters_max <- 40

# Crie uma lista vazia para armazenar a inércia total
inercia_total <- vector("double", length = num_clusters_max - num_clusters_min + 1)

# Loop para ajustar o K-Means para diferentes números de clusters e calcular a inércia total
for (k in num_clusters_min:num_clusters_max) {
  modelo_kmeans <- kmeans(dados_analise[, 2], centers = k)
  inercia_total[k - num_clusters_min + 1] <- modelo_kmeans$tot.withinss
}

# Crie um gráfico de linha para visualizar a inércia total em relação ao número de clusters
plot(num_clusters_min:num_clusters_max, inercia_total, type = "b", 
     xlab = "Número de Clusters", ylab = "Inércia Total",
     main = "Elbow Method para Escolher o Número de Clusters (Diretor)")

# Adicione uma linha vertical para indicar o ponto de inflexão (cotovelo)
abline(v = which(diff(inercia_total) < mean(diff(inercia_total))) + num_clusters_min - 1, col = "red")

# Legenda
legend("topright", legend = "Ponto de Inflexão", col = "red", lty = 1, cex = 0.8)

# Defina os rótulos no eixo x para mostrar todos os números de clusters
axis(1, at = num_clusters_min:num_clusters_max)

######################################################################################

# Ajuste o modelo K-Means com o número escolhido de clusters
n_clusters <- 23  # Substitua pelo número de clusters escolhido
modelo_final <- kmeans(dados_analise[, 2], centers = n_clusters)

# Adicione a nova variável categórica ao Dadosfiltrados
Dadosfiltrados$Diretor_reduz <- as.factor(modelo_final$cluster)

######################################################################################


######################################################################################

# Selecione apenas as variáveis relevantes para a análise (Escritor e Nota_média_Imdb)
dados_analise <- Dadosfiltrados %>%
  select(Escritor, Nota_média_Imdb)

# Defina o número mínimo e máximo de clusters que deseja testar
num_clusters_min <- 20
num_clusters_max <- 40

# Crie uma lista vazia para armazenar a inércia total
inercia_total <- vector("double", length = num_clusters_max - num_clusters_min + 1)

# Loop para ajustar o K-Means para diferentes números de clusters e calcular a inércia total
for (k in num_clusters_min:num_clusters_max) {
  modelo_kmeans <- kmeans(dados_analise[, 2], centers = k)
  inercia_total[k - num_clusters_min + 1] <- modelo_kmeans$tot.withinss
}

# Crie um gráfico de linha para visualizar a inércia total em relação ao número de clusters
plot(num_clusters_min:num_clusters_max, inercia_total, type = "b", 
     xlab = "Número de Clusters", ylab = "Inércia Total",
     main = "Elbow Method para Escolher o Número de Clusters (Escritor)")

# Adicione uma linha vertical para indicar o ponto de inflexão (cotovelo)
abline(v = which(diff(inercia_total) < mean(diff(inercia_total))) + num_clusters_min - 1, col = "red")

# Legenda
legend("topright", legend = "Ponto de Inflexão", col = "red", lty = 1, cex = 0.8)

# Defina os rótulos no eixo x para mostrar todos os números de clusters
axis(1, at = num_clusters_min:num_clusters_max)

######################################################################################

# Ajuste o modelo K-Means com o número escolhido de clusters
n_clusters <- 21  # Substitua pelo número de clusters escolhido
modelo_final <- kmeans(dados_analise[, 2], centers = n_clusters)

# Adicione a nova variável categórica ao Dadosfiltrados
Dadosfiltrados$Escritor_reduz <- as.factor(modelo_final$cluster)

######################################################################################

######################################################################################

# Selecione apenas as variáveis relevantes para a análise (Produtora e Nota_média_Imdb)
dados_analise <- Dadosfiltrados %>%
  select(Produtora, Nota_média_Imdb)

# Defina o número mínimo e máximo de clusters que deseja testar
num_clusters_min <- 20
num_clusters_max <- 40

# Crie uma lista vazia para armazenar a inércia total
inercia_total <- vector("double", length = num_clusters_max - num_clusters_min + 1)

# Loop para ajustar o K-Means para diferentes números de clusters e calcular a inércia total
for (k in num_clusters_min:num_clusters_max) {
  modelo_kmeans <- kmeans(dados_analise[, 2], centers = k)
  inercia_total[k - num_clusters_min + 1] <- modelo_kmeans$tot.withinss
}

# Crie um gráfico de linha para visualizar a inércia total em relação ao número de clusters
plot(num_clusters_min:num_clusters_max, inercia_total, type = "b", 
     xlab = "Número de Clusters", ylab = "Inércia Total",
     main = "Elbow Method para Escolher o Número de Clusters (Produtora)")

# Adicione uma linha vertical para indicar o ponto de inflexão (cotovelo)
abline(v = which(diff(inercia_total) < mean(diff(inercia_total))) + num_clusters_min - 1, col = "red")

# Legenda
legend("topright", legend = "Ponto de Inflexão", col = "red", lty = 1, cex = 0.8)

# Defina os rótulos no eixo x para mostrar todos os números de clusters
axis(1, at = num_clusters_min:num_clusters_max)

######################################################################################

# Ajuste o modelo K-Means com o número escolhido de clusters
n_clusters <- 31  # Substitua pelo número de clusters escolhido
modelo_final <- kmeans(dados_analise[, 2], centers = n_clusters)

# Adicione a nova variável categórica ao Dadosfiltrados
Dadosfiltrados$Produtora_reduz <- as.factor(modelo_final$cluster)

######################################################################################

######################################################################################


# Selecione apenas as variáveis relevantes para a análise (Ator_principal e Nota_média_Imdb)
dados_analise <- Dadosfiltrados %>%
  select(Ator_principal, Nota_média_Imdb)

# Defina o número mínimo e máximo de clusters que deseja testar
num_clusters_min <- 20
num_clusters_max <- 40

# Crie uma lista vazia para armazenar a inércia total
inercia_total <- vector("double", length = num_clusters_max - num_clusters_min + 1)

# Loop para ajustar o K-Means para diferentes números de clusters e calcular a inércia total
for (k in num_clusters_min:num_clusters_max) {
  modelo_kmeans <- kmeans(dados_analise[, 2], centers = k)
  inercia_total[k - num_clusters_min + 1] <- modelo_kmeans$tot.withinss
}

# Crie um gráfico de linha para visualizar a inércia total em relação ao número de clusters
plot(num_clusters_min:num_clusters_max, inercia_total, type = "b", 
     xlab = "Número de Clusters", ylab = "Inércia Total",
     main = "Elbow Method para Escolher o Número de Clusters (Ator_principal)")

# Adicione uma linha vertical para indicar o ponto de inflexão (cotovelo)
abline(v = which(diff(inercia_total) < mean(diff(inercia_total))) + num_clusters_min - 1, col = "red")

# Legenda
legend("topright", legend = "Ponto de Inflexão", col = "red", lty = 1, cex = 0.8)

# Defina os rótulos no eixo x para mostrar todos os números de clusters
axis(1, at = num_clusters_min:num_clusters_max)

######################################################################################

# Ajuste o modelo K-Means com o número escolhido de clusters
n_clusters <- 30  # Substitua pelo número de clusters escolhido
modelo_final <- kmeans(dados_analise[, 2], centers = n_clusters)

# Adicione a nova variável categórica ao Dadosfiltrados
Dadosfiltrados$Ator_principal_reduz <- as.factor(modelo_final$cluster)

######################################################################################

######################################################################################

# Carregue as bibliotecas necessárias (se já carregou no código anterior, não precisa carregar novamente)
install.packages("tidyverse")
library(tidyverse)

# Selecione apenas as variáveis relevantes para a análise (Ator_coadjuvante e Nota_média_Imdb)
dados_analise <- Dadosfiltrados %>%
  select(Ator_coadjuvante, Nota_média_Imdb)

# Defina o número mínimo e máximo de clusters que deseja testar
num_clusters_min <- 5
num_clusters_max <- 50

# Ajuste o modelo K-Means uma vez fora do loop
modelo_kmeans <- kmeans(dados_analise[, 2], centers = num_clusters_max)

# Crie uma lista vazia para armazenar a inércia total
inercia_total <- vector("double", length = num_clusters_max - num_clusters_min + 1)

# Loop para ajustar o K-Means para diferentes números de clusters e calcular a inércia total
for (k in num_clusters_min:num_clusters_max) {
  modelo_kmeans <- kmeans(dados_analise[, 2], centers = k)
  inercia_total[k - num_clusters_min + 1] <- modelo_kmeans$tot.withinss
}

# Crie um gráfico de linha para visualizar a inércia total em relação ao número de clusters
plot(num_clusters_min:num_clusters_max, inercia_total, type = "b", 
     xlab = "Número de Clusters", ylab = "Inércia Total",
     main = "Elbow Method para Escolher o Número de Clusters (Ator_coadjuvante)")

# Adicione uma linha vertical para indicar o ponto de inflexão (cotovelo)
abline(v = which(diff(inercia_total) < mean(diff(inercia_total))) + num_clusters_min - 1, col = "red")

# Legenda
legend("topright", legend = "Ponto de Inflexão", col = "red", lty = 1, cex = 0.8)

# Defina os rótulos no eixo x para mostrar todos os números de clusters
axis(1, at = num_clusters_min:num_clusters_max)

######################################################################################

# Ajuste o modelo K-Means com o número escolhido de clusters
n_clusters <- 30  # Substitua pelo número de clusters escolhido
modelo_final <- kmeans(dados_analise[, 2], centers = n_clusters)

# Adicione a nova variável categórica ao Dadosfiltrados
Dadosfiltrados$Ator_coadjuvante_reduz <- as.factor(modelo_final$cluster)

######################################################################################

# Sumário estatístico das variáveis numéricas
summary(Dadosfiltrados[, c("Ano", "Duração", "Nota_média_Imdb", "Número_votos_IMDB", "Orçamento_Usd_USA", "Renda_bruta_mundial","X.ROI_rendabrutamundialUSD_vs_orçamentoUSD" , "Nota_média_metascore", "Número_avaliações_Metacritics", "Número_avaliações_criticoscinema")])

#salvando arquivo dados filtrados 8 de agosto
write.csv(Dadosfiltrados, file = "DadosFiltrados_filmesBase_15setembro.csv", row.names = FALSE)

#####################################################################################
#####################################################################################
#####################################################################################

install.packages("rpart")
# Carregue a biblioteca rpart
library(rpart)

install.packages("rpart.plot")
# Carregue a biblioteca rpart
library(rpart.plot)

# Crie a árvore de decisão
modelo_arvore_ROI <- rpart(ROI_percentual ~ Ator_principal + Ator_coadjuvante + Escritor + Produtora + genero_principal + Duração + Pais + Lingua + Diretor + Nota_média_Imdb + Orçamento_Usd_USA + Nota_média_metascore, data = Dadosfiltrados, xval=10,
                           control = rpart.control(cp = 0, 
                                                   minsplit = 2,
                                                   maxdepth = 30))

tab_cp <- rpart::printcp(modelo_arvore_ROI)
rpart::plotcp(modelo_arvore_ROI)

tab_cp[which.min(tab_cp[,'xerror']),]

cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']
cp_min

previsoes <- predict(modelo_arvore_ROI, newdata = Dadosfiltrados)

# Calcula o SQE (Soma dos Quadrados de Erro)
SQE <- sum((Dadosfiltrados$Nota_média_Imdb - previsoes)^2)

# Calcula a SST (Soma dos Quadrados Total)
SST <- sum((Dadosfiltrados$Nota_média_Imdb - mean(Dadosfiltrados$Nota_média_Imdb))^2)

# Calcula o QMT (Quadrado Médio do Tratamento)
QMT <- (SST - SQE) / (length(Dadosfiltrados$Nota_média_Imdb) - 1)

# Calcula o R-quadrado
R_quadrado <- 1 - (SQE / SST)

# Imprime os resultados
cat("SQE:", SQE, "\n")
cat("SST:", SST, "\n")
cat("QMT:", QMT, "\n")
cat("R-quadrado:", R_quadrado, "\n")

# Crie um gráfico de dispersão dos dados observados vs. dados esperados
plot(Dadosfiltrados$Nota_média_Imdb, previsoes, 
     xlab = "Dados Observados", ylab = "Dados Esperados",
     main = "Gráfico de Dados Observados vs. Dados Esperados")

# Adicione uma linha de referência de 45 graus (para verificar a igualdade perfeita)
abline(a = 0, b = 1, col = "red")

# Adicione uma legenda
legend("topleft", legend = "Igualdade Perfeita", col = "red", lty = 1)

##Arvore tunada após CP minimo ##

modelo_arvore_ROI <- rpart(ROI_percentual ~ Ator_principal + Ator_coadjuvante + Escritor + Produtora + genero_principal + Duração + Pais + Lingua + Diretor + Nota_média_Imdb + Orçamento_Usd_USA + Nota_média_metascore, data = Dadosfiltrados, xval=10,
                           control = rpart.control(cp = cp_min, 
                                                   minsplit = 2,
                                                   maxdepth = 30))

summary(modelo_arvore_ROI)


prp(modelo_arvore_ROI, main = "Árvore de Decisão ROI", box.palette = "BuGn")

summary(Dadosfiltrados)

#############################################################################

## Teste final
## Arvore preditora Nota IMDB

head(Dadosfiltrados)

install.packages("rpart")
# Carregue a biblioteca rpart
library(rpart)

install.packages("rpart.plot")
# Carregue a biblioteca rpart
library(rpart.plot)

modelo_arvore_IMDB_categoricas <- rpart(Nota_média_Imdb ~ Ator_principal + Ator_coadjuvante + Escritor + Produtora + genero_principal + Duração + Pais + Lingua + Diretor + Orçamento_Usd_USA, data = Dadosfiltrados, xval=10,
                           control = rpart.control(cp = 0, 
                                                   minsplit = 2,
                                                   maxdepth = 5))

prp(modelo_arvore_IMDB_categoricas, main = "Árvore de Decisão ROI", box.palette = "BuGn")

summary(modelo_arvore_IMDB_categoricas)

tab_cp <- rpart::printcp(modelo_arvore_IMDB_categoricas)
rpart::plotcp(modelo_arvore_IMDB_categoricas)

tab_cp[which.min(tab_cp[,'xerror']),]

cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']
cp_min

previsoes <- predict(modelo_arvore_IMDB_categoricas, newdata = Dadosfiltrados)

# Calcula o SQE (Soma dos Quadrados de Erro)
SQE <- sum((Dadosfiltrados$Nota_média_Imdb - previsoes)^2)

# Calcula a SST (Soma dos Quadrados Total)
SST <- sum((Dadosfiltrados$Nota_média_Imdb - mean(Dadosfiltrados$Nota_média_Imdb))^2)

# Calcula o QMT (Quadrado Médio do Tratamento)
QMT <- (SST - SQE) / (length(Dadosfiltrados$Nota_média_Imdb) - 1)

# Calcula o R-quadrado
R_quadrado <- 1 - (SQE / SST)

# Imprime os resultados
cat("SQE:", SQE, "\n")
cat("SST:", SST, "\n")
cat("QMT:", QMT, "\n")
cat("R-quadrado:", R_quadrado, "\n")

# Crie um gráfico de dispersão dos dados observados vs. dados esperados
plot(Dadosfiltrados$Nota_média_Imdb, previsoes, 
     xlab = "Dados Observados", ylab = "Dados Esperados",
     main = "Gráfico de Dados Observados vs. Dados Esperados")

##############################################################################

## teste final 2

modelo_arvore_Metascore_categoricas <- rpart(Nota_média_metascore ~ Ator_principal + Ator_coadjuvante + Escritor + Produtora + genero_principal + Duração + Pais + Lingua + Diretor + Orçamento_Usd_USA, data = Dadosfiltrados, xval=10,
                                        control = rpart.control(cp = 0, 
                                                                minsplit = 2,
                                                                maxdepth = 3))
summary(modelo_arvore_Metascore_categoricas)
