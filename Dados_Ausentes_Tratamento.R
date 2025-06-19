####### Tratando Valores Ausentes NA ########

# Função que mostra uma tabela bruta com valores lógicos identificando células vazias
is.na(Objeto))

# Função que responde se determidado tem valores ausentes ou não
any(is.na(Objeto))

# Função que mostra as linhas e colunas com valores ausentes
which(is.na(Objeto), arr.ind = TRUE)

# Função que mostra quantos valores ausentes cada coluna apresenta
colSums(is.na(Objeto))


# Curadoria de dados com valores ausentes!

# Se tiver poucos NA podemos substituir esses valores pela média, mediana ou valor fixo

## Média
for (col in colnames(SeuDataFrame)) {
  if (is.numeric(SeuDataFrame[[col]])) {
    SeuDataFrame[[col]][is.na(SeuDataFrame[[col]])] <- mean(SeuDataFrame[[col]], na.rm = TRUE)
  }
}

## Valor Fixo
SeuDataFrame[is.na(SeuDataFrame)] <- 0 #Valor 0


# Se tiver muitos NA em poucas colunas, podemos remover as colunas

SeuDataFrame <- SeuDataFrame[, colSums(is.na(SeuDataFrame)) == 0]  # Remove todas as colunas com qualquer NA

SeuDataFrame <- SeuDataFrame[, colMeans(is.na(SeuDataFrame)) < 0.5] # se quiser remover apenas colunas com mais de 50% de NA:


# Muitos NA dispersos em várias linhas

## Remover as linhas
SeuDataFrame <- na.omit(SeuDataFrame)  # Remove todas as linhas que tenham pelo menos um NA

# Para dados muito importantes podemos substituir o NA pelo valor anterior (ISSO PARA SÉRIES TEMPORAIS) ou podemos utilizar Modelos de Imputação (INTERPOLAÇÃO OU MACHINE LEARNING)

## Séries Temporais
library(zoo)
SeuDataFrame <- na.locf(SeuDataFrame)  # Preenche com o último valor não-NA

## Modelos de Imputação: prever dados usando técnicas como regressão, KNN ou algoritmos avançados como o missForest


# missForest - Funciona para Dados Categóricos e Mantém Relações entre Variáveis

library(missForest)
SeuDataFrame <- missForest(SeuDataFrame)$ximp #Salva um novo data.frame com dados estimados
SeuDataFrame$OOBerror # estimativa do erro do resultado final
SeuDataFrame$error #o erro verdadeiro

# Essa técnica usa o Random Forest para prever os valores ausentes com base nas demais variáveis do conjunto de dados

# Identificação dos NA → A função detecta quais variáveis possuem valores ausentes.

# Inicialização → Substitui temporariamente os NA por uma estimativa inicial, geralmente a média (para variáveis numéricas) ou a moda (para categóricas).

# Predição com Random Forest:
  # Para cada variável com NA, ela é tratada como variável dependente (y).
  # As outras colunas sem NA são usadas como preditores (X).
  # Um modelo de Random Forest é treinado para prever os valores ausentes.
  # Os valores ausentes são substituídos pelas previsões do modelo.

# Iteração → O processo se repete várias vezes até que as diferenças entre iterações sejam mínimas.
 
# O NRMSE (Normalized Root Mean Square) Erro quadrático médio normalizado é uma métrica usada para avaliar a qualidade da imputação de valores ausentes quando utilizamos métodos como missForest(). Ele mede o erro médio da imputação normalizado pelo intervalo ou desvio padrão dos valores verdadeiros.

# Como interpretar o NRMSE?
## NRMSE próximo de 0 → A imputação foi excelente (os valores imputados estão muito próximos dos valores reais).
## NRMSE entre 0.1 e 0.5 → A imputação é razoável, mas pode haver erros consideráveis.
## NRMSE > 0.5 → A imputação tem alto erro e pode não ser confiável.

##### Em geral, quanto menor o NRMSE, melhor a imputação!

install.packages("missForest")  # Instalar (se ainda não tiver)
library(missForest)             # Carregar o pacote

data(iris)

set.seed(81)
iris.ausentes <- prodNA(iris, noNA = 0.2) #Adicionar valores ausentes aleatórios, o valor 0.2 produzmuitos valores ausentes, tente reduzir esse valor.

any(is.na(iris.ausentes))
which(is.na(iris.ausentes), arr.ind = TRUE)
colSums(is.na(iris.ausentes))

iris.estimadosT <- missForest(iris.ausentes, xtrue = iris, verbose = TRUE) # verbose mostra o que a função está fazendo

## Estimativa do erro
iris.estimadosT$OOBerror

## Estimativa do erro verdadeiro
iris.estimadosT$error

# Se as estimativas dos erros estiverem confortáveis (O PFC mostra a porcentagem das variáveis que não foram corretamente estimadas) salve os dados em outro objeto
iris.estimados <- iris.estimados$ximp


# Comparar as duas planilhas


# Criar uma máscara dos valores que foram imputados
mascara_NA <- is.na(iris.ausentes)

# Extrair apenas os valores imputados e seus valores reais correspondentes
valores_imputados <- iris.estimados[mascara_NA]
valores_reais <- iris[mascara_NA]

# Criar um data frame para visualizar as comparações
comparacao <- data.frame(
  Valor_Real = valores_reais,
  Valor_Imputado = valores_imputados
)

# Mostrar a tabela de comparação
print(comparacao)

# Filtrar apenas os valores numéricos (caso tenha variáveis categóricas)
valores_reais_num <- as.numeric(valores_reais)  
valores_imputados_num <- as.numeric(valores_imputados)

# Remover possíveis NAs que possam surgir ao converter os dados
valores_reais_num <- valores_reais_num[!is.na(valores_reais_num)]
valores_imputados_num <- valores_imputados_num[!is.na(valores_imputados_num)]

# Calcular variância e desvio padrão para os valores reais
var_real <- var(valores_reais_num)
sd_real <- sd(valores_reais_num)

# Calcular variância e desvio padrão para os valores imputados
var_imputado <- var(valores_imputados_num)
sd_imputado <- sd(valores_imputados_num)

# Criar um data frame com os resultados
resultado_variancia_desvio <- data.frame(
  Estatistica = c("Variância", "Desvio Padrão"),
  Valores_Reais = c(var_real, sd_real),
  Valores_Imputados = c(var_imputado, sd_imputado)
)

# Mostrar os resultados
print(resultado_variancia_desvio)

# Verificar a normalidade dos dados:
shapiro_real <- shapiro.test(valores_reais_num)
shapiro_imputado <- shapiro.test(valores_imputados_num)

# Criar data frame com os resultados
resultado <- data.frame(
  Conjunto = c("Valores Reais", "Valores Imputados"),
  W_Statistic = c(shapiro_real$statistic, shapiro_imputado$statistic),
  p_Value = c(shapiro_real$p.value, shapiro_imputado$p.value)
)
return(resultado)
}

# A função shapiro retorna uma tabela com os valores do teste estatístico W e o p-valor associado ao teste. 

# A hipótese nula do teste shapiro é que os dados seguem uma distribuição normal, logo, se o p-valor for maior que o alfa (nível de significância) não rejeitamos a hipótese nula, logo os dados são normais

# Se o p-valor for menor que o alfa, nós rejeitamos a hipótese nula e os dados não são normais.

# O W (estatística do teste) mede o quanto os dados se aproximam de uma distribuição normal. Quanto mais próximo de 1, mais próximo os dados estão de uma distribuição normal

# Valores altos de W pode indicar a presença de outliers, ou até mesmo assimetria e curtose nos dados. 

# O W dos dados imputados foram 0.9487308, logo, vamos entender por que esse resultado é tão próximo de um se o p-valor é tão baixo.

# Visualizar a distribuição dos dados com HISTOGRAMA e Q-Q PLOT
hist(valores_reais_num, main = "Histograma dos Dados", col = "lightblue")
qqnorm(valores_reais_num)
qqline(valores_reais_num, col = "red")

# verificar outliers
boxplot(valores_reais_num, main = "Boxplot dos Dados", col = "lightgreen")

# Testando estatísticamente os dados estimados com os dados reais

# Teste de Wilcoxon pareado para comparar os valores reais e imputados
wilcox_test <- wilcox.test(valores_reais_num, valores_imputados_num, paired = TRUE)

# Exibir o p-valor
print(wilcox_test$p.value)

# Se p < 0.05 → Diferença significativa entre os grupos (rejeitamos a hipótese nula de que as distribuições são iguais).

# Seu p-valor = 0.059 está ligeiramente acima de 0.05, indicando que não há uma diferença estatisticamente significativa entre os valores imputados e os valores reais, mas está bem próximo do limiar.

# Comparar as variâncias Teste de Levene para dados não normais
library(car)
leveneTest(c(valores_reais_num, valores_imputados_num), 
           group = rep(c("Reais", "Imputados"), 
                       c(length(valores_reais_num), length(valores_imputados_num))))
# p-value = 0.9582: O p-valor é muito maior que 0.05, o que indica que não há diferença significativa nas variâncias entre os grupos comparados.

# Tamanho do Efeito
library(rstatix)
library(coin)
dados <- data.frame(
  valores = c(valores_reais_num, valores_imputados_num),
  grupo = rep(c("Reais", "Imputados"), 
              c(length(valores_reais_num), length(valores_imputados_num)))
)

wilcox_effsize(dados, formula = valores ~ grupo)

# O valor de efeito (0.0187) está muito próximo de zero e a classificação como "small" sugere que, embora exista uma diferença entre os grupos Imputados e Reais, essa diferença é muito pequena.



############## Imputação po KNN ##########################


# Instale o pacote DMwR, se necessário
install.packages("VIM")


# Carregue o pacote
library(VIM)
library(missForest)

# Carregar o conjunto de dados iris
data(iris)

# Definir uma semente para reprodutibilidade
set.seed(123)

# Introduzir valores ausentes aleatórios (10% de valores ausentes)
iris_ausentes <- prodNA(iris, noNA = 0.2) # Função para adicionar valores ausentes em posições aleatórias, o valor 0.2 produz muitos valores ausentes, tente reduzir esse valor para testar.
# Imputação com KNN
iris_imputados <- kNN(iris_ausentes, k = 5)  # k=5 significa que vamos considerar os 5 vizinhos mais próximos

# Exibir os dados com ausentes e imputados

# Criar uma máscara dos valores que foram imputados
mascara_NA <- is.na(iris_ausentes)

# Extrair apenas os valores imputados e seus valores reais correspondente
valores_imputados <- iris_imputados[mascara_NA]
valores_reais <- iris[mascara_NA]

# Criar um data frame para visualizar as comparações
comparacao <- data.frame(
  Valor_Real = valores_reais,
  Valor_Imputado = valores_imputados
)

# Mostrar a tabela de comparação
print(comparacao)


valores_reais_num <- as.numeric(valores_reais)  
valores_imputados_num <- as.numeric(valores_imputados)

# Remover possíveis NAs que possam surgir ao converter os dados
valores_reais_num <- valores_reais_num[!is.na(valores_reais_num)]
valores_imputados_num <- valores_imputados_num[!is.na(valores_imputados_num)]
 
wilcox_test <- wilcox.test(valores_reais_num, valores_imputados_num, paired = TRUE)

# Exibir o p-valor
print(wilcox_test$p.value)



####################### Impultação Múltipla ###########################

# Instalar o pacote mice, se necessário
install.packages("mice")

# Carregar o pacote
library(mice)

# Realizar imputação múltipla com 5 imputações
imputacoes <- mice(iris_ausentes, m = 5, method = 'pmm', seed = 123)

# Ver o primeiro conjunto imputado
head(complete(imputacoes, 1))

iris_imputado_1 <- complete(imputacoes, 1)

# Extrair apenas os valores imputados e seus valores reais correspondente
valores_imputados_mult <- iris_imputado_1[mascara_NA]

comparacao_mult <- data.frame(
  Valor_Real = valores_reais,
  Valor_Imputado = valores_imputados_mult
)

# Mostrar a tabela de comparação
print(comparacao_mult)



############# Redes Neurais -H2O ####################

# Carregar o pacote H2O
library(h2o)

# Inicializar o H2O
h2o.init()

# Converter para H2OFrame
iris_h2o <- as.h2o(iris_ausentes)

# Treinar uma rede neural para imputação de dados ausentes
# Usaremos todas as variáveis preditoras
modelo_nn <- h2o.deeplearning(
  training_frame = iris_h2o,  # Dados de treinamento
  x = 1:(ncol(iris_h2o)-1),   # Todas as colunas, exceto a coluna de resposta (não temos uma variável de resposta aqui)
  y = "Species",  # Especificar a variável alvo (se houver)
  activation = "Rectifier",  # Função de ativação
  hidden = c(10, 10),  # Camadas ocultas (exemplo)
  epochs = 10  # Número de épocas
)

# Imputação dos valores ausentes
iris_imputado <- h2o.predict(modelo_nn, iris_h2o)

# Verificar se os valores ausentes foram imputados
colSums(is.na(as.data.frame(iris_imputado)))

# Converter o H2OFrame de volta para um data.frame
iris_imputado_df <- as.data.frame(iris_imputado)

# Visualizar os dados imputados
head(iris_imputado_df, 3)

# Finalizar o H2O
h2o.shutdown(prompt = FALSE)

