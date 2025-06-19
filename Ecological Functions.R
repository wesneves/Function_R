# Funções Ecologia ----

# teste-t - var iguais ----
# Teste utilizado para saber se existe diferença entre as médias de dois grupos ou tratamentos.Para responder a esta pergunta, William Sealy Gosset, químico da cervejaria Guinness, desenvolveu em 1908 o Teste T que é uma estatística que segue uma distribuição t de Student para rejeitar ou não uma hipótese nula de médias iguais entre dois grupos.

# Premissas do Teste t
# As amostras devem ser independentes
# As unidades amostrais são selecionadas aleatoriamente
# Distribuição normal (gaussiana) dos resíduos
# Homogeneidade da variância

# Pergunta - O CRC (comprimento rostro-cloacal) dos machos de P. nattereri (Physalaemus nattereri (Anura:Leptodactylidae)) é maior na estação chuvosa do que na estação seca?

# Dados
CRC_PN_macho <- ecodados::teste_t_var_igual

# Teste de normalidade
residuos <- lm(CRC ~ Estacao, data = CRC_PN_macho)
car::qqPlot(residuos)

## Teste de Shapiro-Wilk
residuos_modelo <- residuals(residuos)
shapiro.test(residuos_modelo)

## Teste de homogeneidade de variância
car::leveneTest(CRC ~ as.factor(Estacao), data = CRC_PN_macho)

## Análise Teste T
t.test(CRC ~ Estacao, data = CRC_PN_macho, var.equal = TRUE)

## Gráfico
ggplot(data = CRC_PN_macho, aes(
  x = Estacao,
  y = CRC, color = Estacao
)) +
  labs(
    x = "Estações",
    y = expression(paste("CRC (mm) - ", italic("P. nattereri")))
  ) +
  geom_boxplot(
    fill = c("darkorange", "cyan4"), color = "black",
    outlier.shape = NA
  ) +
  geom_jitter(
    shape = 16, position = position_jitter(0.1),
    cex = 5, alpha = 0.7
  ) +
  scale_color_manual(values = c("black", "black")) +
  theme(legend.position = "none")

# Neste exemplo, rejeitamos a hipótese nula de que as médias do CRC dos machos entre as estações seca e chuvosa são iguais. Os resultados mostram que os machos de P. nattereri coletados na estação chuvosa foram em média 0,43 mm maiores do que os machos coletados na estação seca (t49 = 4,15, P < 0,001).


# teste-t - var diferentes ----

# Pergunta - O CRC das fêmeas de L. podicipinus (Leptodactylus podicipinus) é maior na estação chuvosa do que na estação seca?

# Dados
CRC_LP_femea <- ecodados::teste_t_var_diferente

## Teste de normalidade usando QQ-plot
residuos_LP <- lm(CRC ~ Estacao, data = CRC_LP_femea)
car::qqPlot(residuos_LP)

## Teste de Shapiro-Wilk
residuos_modelo_LP <- residuals(residuos_LP)
shapiro.test(residuos_modelo_LP)

## Teste de homogeneidade da variância - Os resíduos não apresentam homogeneidade da variância. Portanto, vamos realizar o Teste T com variâncias diferentes. Para isso, use o argumento var.equal = FALSE.
car::leveneTest(CRC ~ as.factor(Estacao), data = CRC_LP_femea)

## Teste T
t.test(CRC ~ Estacao, data = CRC_LP_femea, var.equal = FALSE)


# teste-t Pareado ----
# O Teste T Pareado é uma estatística que usa dados medidos duas vezes na mesma unidade amostral, resultando em pares de observações para cada amostra (amostras pareadas).

# Premissas do Teste t para amostras pareadas
# As unidades amostrais são selecionadas aleatoriamente
# As observações não são independentes
# Distribuição normal (gaussiana) dos valores da diferença para cada par
# Pergunta - A riqueza de espécies de artrópodes é prejudicada pelas queimadas?

# Dados
Pareado <- ecodados::teste_t_pareado
Riqueza_Pre_Queimada <- Pareado$Riqueza[Pareado$Estado == "Pre-Queimada"]
Riqueza_Pos_Queimada <- Pareado$Riqueza[Pareado$Estado == "Pos-Queimada"]

## Análise Teste T Pareado
t.test(Riqueza_Pre_Queimada, Riqueza_Pos_Queimada, paired = TRUE)

## Gráfico
library(ggpubr)
ggpaired(Pareado,
  x = "Estado", y = "Riqueza",
  color = "Estado", line.color = "gray", line.size = 0.8,
  palette = c("darkorange", "cyan4"), width = 0.5,
  point.size = 4, xlab = "Estado das localidades",
  ylab = "Riqueza de Espécies"
) +
  expand_limits(y = c(0, 150))


# Correlação Pearson ----
# É um teste que mede a força relativa da relação linear entre duas variáveis contínuas (X e Y). Importante ressaltar que a análise de correlação não assume que a variável X influencie a variável Y, ou que exista uma relação de causa e efeito entre elas.

# Premissas da Correlação de Person
# As amostras devem ser independentes e pareadas (i.e., as duas variáveis devem ser medidas na mesma unidade amostral)
# As unidades amostrais são selecionadas aleatoriamente
# A relação entre as variáveis tem que ser linear.

# Pergunta - Existe correlação entre a altura do tronco e o tamanho da raiz dos arbustos?

# Dados
correlacao_arbustos <- ecodados::correlacao

## Correlação de Pearson
cor.test(correlacao_arbustos$Tamanho_raiz,
  correlacao_arbustos$Tamanho_tronco,
  method = "pearson"
)

## Gráfico
ggplot(data = correlacao_arbustos, aes(
  x = Tamanho_raiz,
  y = Tamanho_tronco
)) +
  labs(x = "Tamanho da raiz (m)", y = "Altura do tronco (m)") +
  geom_point(size = 4, shape = 21, fill = "darkorange", alpha = 0.7) +
  geom_text(
    x = 14, y = 14, label = "r = 0.89, P < 0.001",
    color = "black", size = 5
  ) +
  geom_smooth(
    method = lm, se = FALSE, color = "black",
    linetype = "dashed"
  ) +
  theme(legend.position = "none")


# Regressão Linear ----
# A regressão linear simples é usada para analisar a relação entre uma variável preditora (plotada no eixo-X) e uma variável resposta (plotada no eixo-Y). As duas variáveis devem ser contínuas. Diferente das correlações, a regressão assume uma relação de causa e efeito entre as variáveis. O valor da variável preditora (X) causa, direta ou indiretamente, o valor da variável resposta (Y).

# Pergunta - A temperatura afeta o tamanho do CRC de populações de Dendropsophus minutus?

# Dados
dados_regressao <- ecodados::regressoes

## Verificar as premissas do teste
lm(CRC ~ Temperatura, data = dados_regressao) |>
  plot()

## regressão simples
modelo_regressao <- lm(CRC ~ Temperatura, data = dados_regressao)

## Resultados usando a função anova
anova(modelo_regressao)

## Resultados usando a função summary
summary(modelo_regressao)

## Gráfico
ggplot(data = dados_regressao, aes(x = Temperatura, y = CRC)) +
  labs(
    x = "Temperatura média anual (°C)",
    y = "Comprimento rostro-cloacal (mm)"
  ) +
  geom_point(size = 4, shape = 21, fill = "darkorange", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme(legend.position = "none")

# Verificar os coeficientes
coef(modelo_regressao)

# Calcular o Valor Predito pelo Modelo
predict(modelo_regressao, newdata = data.frame(Temperatura = 16))


# Valor do Intercepto
predict(modelo_regressao, newdata = data.frame(Temperatura = 0))

# Valor da inclinação
predict(modelo_regressao, newdata = data.frame(Temperatura = 17)) - predict(modelo_regressao, newdata = data.frame(Temperatura = 16))


# Regressão Linear Múltipla ----
# A regressão linear múltipla é uma extensão da regressão linear simples. Ela é usada quando queremos determinar o valor da variável resposta (Y) com base nos valores de duas ou mais variáveis preditoras (X1, X2, Xn).

# Premissas da Regressão Linear Múltipla
# As amostras devem ser independentes
# As unidades amostrais são selecionadas aleatoriamente
# Distribuição normal (gaussiana) dos resíduos
# Homogeneidade da variância dos resíduos.

# Pergunta - O tamanho do CRC das populações de D. minutus é influenciado pela temperatura e precipitação das localidades onde os indivíduos ocorrem?

# Multicolinearidade -  Multicolinearidade ocorre quando as variáveis preditoras são correlacionadas. A multicolinearidade aumentam o erro padrão associado aos coeficientes produzindo resultados menos confiáveis.

lm(CRC ~ Temperatura + Precipitacao,
  data = dados_regressao
) |>
  car::vif()

# VIF (Fator de Inflação da Variância) - Alguns autores consideram valores de VIF acima de 10 como fortemente correlacionadas, outros mais conservadores consideram o valor de 5, 3 ou até mesmo 2.

## Normalidade e homogeneidade das variâncias
cowplot::plot_grid(plotlist = sjPlot::plot_model(lm(CRC ~ Temperatura + Precipitacao, data = dados_regressao), type = "diag"))

# Os resíduos apresentam distribuição normal e variâncias homogêneas.

## regressão múltipla
modelo_regressao_mult <- lm(CRC ~ Temperatura + Precipitacao,
  data = dados_regressao
)
summary(modelo_regressao_mul)

# Likelihood-ratio test (LRT) - compara dois modelos aninhados, testando se os parâmetros do modelo mais complexo diferem significativamente do modelo mais simples. Em outras palavras, ele testa se há necessidade de se incluir uma variável extra no modelo para explicar os dados.

## Criando os modelos aninhados
modelo_regressao_mul <- lm(CRC ~ Temperatura + Precipitacao,
  data = dados_regressao
)
modelo_regressao <- lm(CRC ~ Temperatura, data = dados_regressao)

## Likelihood-ratio test (LRT)
lmtest::lrtest(modelo_regressao_mul, modelo_regressao)

# A Hipótese Nula (H0) do teste Likelihood-ratio test (LRT) é de que o modelo mais simples é o melhor.

# Valor de p < 0.05 rejeita a hipótese nula e o modelo mais complexo é o melhor

# Valor de p > 0.05 não rejeita a hipótese nula e o modelo mais simples é o melhor

## Comparando com o modelo somente com o intercepto

# Criando um modelo sem variáveis, só o intercepto.
modelo_intercepto <- lm(CRC ~ 1, data = dados_regressao)

lmtest::lrtest(modelo_regressao, modelo_intercepto)


# ANOVA ----
# ANOVA refere-se a uma variedade de delineamentos experimentais nos quais a variável preditora é categórica e a variável resposta é contínua.

# Premissas da ANOVA
# As amostras devem ser independentes
# As unidades amostrais são selecionadas aleatoriamente
# Distribuição normal (gaussiana) dos resíduos
# Homogeneidade da variância.

# ANOVA de um fator ----
# Este teste considera delineamentos experimentais com apenas um fator (ou tratamento) que pode ser composto por três ou mais grupos (ou níveis).

# Pergunta O crescimento dos indivíduos de C. arabica é melhorado pela adição do adubo X-2020?

# Dados
dados_anova_simples <- ecodados::anova_simples

dados_anova_simples <- dados_anova_simples |>
  mutate(Tratamento = stringr::str_replace(Tratamento, "Adubo_X-2020", "Adubo_X_2020"))


## Modelo ANOVA de udados_anova_simples## Modelo ANOVA de um fator
Modelo_anova <- aov(Crescimento ~ Tratamento, data = dados_anova_simples)

## Normalidade
shapiro.test(residuals(Modelo_anova))

## Homogeneidade da variância
bartlett.test(Crescimento ~ Tratamento, data = dados_anova_simples)

## Resultados da anova
anova(Modelo_anova)

# Testes post-hoc
## Diferenças entre os tratamentos
TukeyHSD(Modelo_anova)

## Reorganizando a ordem que os grupos irão aparecer no gráfico
dados_anova_simples$Tratamento <- factor(dados_anova_simples$Tratamento,
  levels = c("Controle", "Adubo_Tradicional", "Adubo_X_2020")
)

## Gráfico
ggplot(
  data = dados_anova_simples,
  aes(x = Tratamento, y = Crescimento, color = Tratamento)
) +
  geom_boxplot(
    fill = c("darkorange", "darkorchid", "cyan4"),
    color = "black", show.legend = FALSE, alpha = 0.4
  ) +
  geom_jitter(
    shape = 16, position = position_jitter(0.1),
    cex = 4, alpha = 0.7
  ) +
  scale_color_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  scale_y_continuous(limits = c(0, 20), breaks = c(0, 5, 10, 15, 20)) +
  geom_text(x = 1, y = 12, label = "ab", color = "black", size = 5) +
  geom_text(x = 2, y = 17, label = "a", color = "black", size = 5) +
  geom_text(x = 3, y = 17, label = "b", color = "black", size = 5) +
  scale_x_discrete(labels = c("Sem adubo", "Tradicional", "X-2020")) +
  labs(
    x = "Adubação", y = "Crescimento Coffea arabica (cm)",
    size = 20
  ) +
  theme(legend.position = "none")


# ANOVA de dois fatores ou ANOVA fatorial ----
# Este teste considera delineamentos amostrais com dois fatores (ou tratamentos) que podem ser compostos por dois ou mais grupos (ou níveis). Esta análise tem uma vantagem, pois permite avaliar o efeito da interação entre os fatores na variável resposta. Quando a interação está presente, o impacto de um fator depende do nível (ou grupo) do outro fator.

# Pergunta - O tempo de eliminação da droga é dependente do sistema XY de determinação do sexo e idade dos pacientes?

# Dados
dados_dois_fatores <- ecodados::anova_dois_fatores

## Análise Anova de dois fatores
# A interação entre os fatores é representada por *
Modelo1 <- aov(Tempo ~ Pessoas * Idade, data = dados_dois_fatores)

# Olhando os resultados
anova(Modelo1)

# Sem significância na interação, vamos utilizar o LRT
# Criando modelo sem interação.
Modelo2 <- aov(Tempo ~ Pessoas + Idade, data = dados_dois_fatores)

## LRT
lmtest::lrtest(Modelo1, Modelo2)
# a interação não é importante. Então podemos seguir com o modelo mais simples.

## Normalidade e homogeneidade das variâncias
cowplot::plot_grid(plotlist = sjPlot::plot_model(Modelo2, type = "diag"))

# Resultados do modelo
anova(Modelo2)

# não precisamos realizar testes de comparações múltiplas post-hoc porque os fatores apresentam apenas dois níveis.

## Gráfico
ggplot(
  data = Modelo2,
  aes(y = Tempo, x = Pessoas, color = Idade)
) +
  geom_boxplot() +
  stat_summary(
    fun = mean, geom = "point",
    aes(group = Idade, x = Pessoas),
    color = "black",
    position = position_dodge(0.7), size = 4
  ) +
  ggforce::geom_link(aes(x = 0.8, y = 31, xend = 1.8, yend = 40),
    color = "darkorange",
    lwd = 1.3, linetype = 2
  ) +
  ggforce::geom_link(aes(x = 1.2, y = 19, xend = 2.2, yend = 26.5),
    color = "cyan4", lwd = 1.3, linetype = 2
  ) +
  labs(
    x = "Sistema XY de determinação do sexo",
    y = "Tempo (horas) para eliminar a droga"
  ) +
  scale_color_manual(values = c(
    "darkorange", "cyan4",
    "darkorange", "cyan4"
  )) +
  scale_y_continuous(
    limits = c(10, 50),
    breaks = c(10, 20, 30, 40, 50)
  )

# ANOVA de dois fatores com efeito da interação

# Dados
dados_dois_fatores_interacao2 <- ecodados::anova_dois_fatores_interacao2

## Análise anova de dois fatores
Modelo_interacao2 <- aov(Tempo ~ Pessoas * Idade,
  data = dados_dois_fatores_interacao2
)

## Olhando os resultados
anova(Modelo_interacao2)

## Gráfico
ggplot(
  data = dados_dois_fatores_interacao2,
  aes(y = Tempo, x = Pessoas, color = Idade)
) +
  geom_boxplot() +
  stat_summary(
    fun = mean, geom = "point",
    aes(group = Idade, x = Pessoas),
    color = "black", position = position_dodge(0.7),
    size = 4
  ) +
  ggforce::geom_link(aes(x = 0.8, y = 31, xend = 1.8, yend = 27),
    color = "darkorange",
    lwd = 1.3, linetype = 2
  ) +
  ggforce::geom_link(aes(x = 1.2, y = 19, xend = 2.2, yend = 41),
    color = "cyan4",
    lwd = 1.3, linetype = 2
  ) +
  labs(
    x = "Sistema XY de determinação do sexo",
    y = "Tempo (horas) para eliminar a droga"
  ) +
  scale_color_manual(values = c(
    "darkorange", "cyan4",
    "darkorange", "cyan4"
  )) +
  scale_y_continuous(
    limits = c(10, 50),
    breaks = c(10, 20, 30, 40, 50)
  )

# Quando as linhas se cruzam é um exemplo clássico de interação! Jovens são mais rápidos para eliminarem a droga em pessoas XX, enquanto os idosos são mais rápidos para eliminarem a droga nas pessoas XY.


# ANOVA em blocos aleatorizados ----
# No delineamento experimental com blocos aleatorizados, cada fator é agrupado em blocos, com réplicas de cada nível do fator representado em cada bloco.

# O bloco é uma área ou período de tempo dentro do qual as condições ambientais são relativamente homogêneas. O objetivo do uso dos blocos é controlar fontes de variações indesejadas na variável dependente que não são de interesse do pesquisador. Desta maneira, podemos retirar dos resíduos os efeitos das variações indesejadas que não são do nosso interesse, e testar com maior poder estatístico os efeitos dos tratamentos de interesse.

# Pergunta - A distância da poça artificial ao fragmento florestal influencia a riqueza de espécies de anuros?

# Dados
dados_bloco <- ecodados::anova_bloco

## Análise Anova em blocos aleatorizados
model_bloco <- aov(Riqueza ~ Pocas + Error(Blocos), data = dados_bloco)
summary(model_bloco)

## Forma errada de análisar Anova em blocos
modelo_errado <- aov(Riqueza ~ Pocas, data = dados_bloco)
anova(modelo_errado)

## Teste de Tuckey's honest significant difference
pairs(lsmeans::lsmeans(model_bloco, "Pocas"), adjust = "tukey")

# Reordenando a ordem que os grupos irão aparecer no gráfico.
dados_bloco$Pocas <- factor(dados_bloco$Pocas,
  levels = c(
    "Int-100m", "Int-50m", "Borda",
    "Mat-50m", "Mat-100m"
  )
)

## Gráfico
ggplot(data = dados_bloco, aes(x = Pocas, y = Riqueza)) +
  labs(x = "Poças artificiais", y = "Riqueza de espécies de anuros") +
  geom_boxplot(color = "black", show.legend = FALSE, alpha = 0.4) +
  geom_jitter(
    shape = 16, position = position_jitter(0.1), cex = 4,
    alpha = 0.7
  ) +
  scale_x_discrete(labels = c("-100m", "-50m", "Borda", "50m", "100m")) +
  theme(legend.position = "none")

# Neste exemplo, rejeitamos a hipótese nula de que a distância das poças artificiais até as bordas dos fragmentos florestais não influência a riqueza de espécies de anuros. As poças artificiais instaladas nas bordas dos fragmentos florestais apresentaram maior riqueza de espécies do que as poças distantes.

# Análise de Covariância (ANCOVA) ----
# A ANCOVA pode ser compreendida como uma extensão da ANOVA com a adição de uma variável contínua (covariável) medida em todas as unidades amostrais.

# A ideia é que a covariável também afete os valores da variável resposta. Não incluir a covariável irá fazer com que a variação não explicada pelo modelo se concentre nos resíduos. Incluindo a covariável, o tamanho do resíduo é menor e o teste para avaliar as diferenças nos tratamentos, que é o interesse do pesquisador, terá mais poder estatístico.

# Pergunta - A herbivoria diminui a biomassa dos frutos?

# Dados
dados_ancova <- ecodados::ancova

## Ancova
modelo_ancova <- lm(Biomassa ~ Herbivoria * Raiz, data = dados_ancova)

# Verificando as premissas da Ancova
cowplot::plot_grid(plotlist = sjPlot::plot_model(modelo_ancova, type = "diag"))

## Resultados do modelo
anova(modelo_ancova)

## Criando modelo sem interação
modelo_ancova2 <- lm(Biomassa ~ Herbivoria + Raiz, data = dados_ancova)

## Likelihood-ratio test
lmtest::lrtest(modelo_ancova, modelo_ancova2)

## Gráfico
ggplot(data = dados_ancova, aes(
  x = Raiz, y = Biomassa,
  fill = Herbivoria
)) +
  labs(x = "Tamanho da raiz (cm)", y = "Biomassa dos frutos (g)") +
  geom_point(size = 4, shape = 21, alpha = 0.7) +
  scale_colour_manual(values = c("darkorange", "cyan4")) +
  scale_fill_manual(
    values = c("darkorange", "cyan4"),
    labels = c("Com herbivoria", "Sem herbivoria")
  ) +
  geom_smooth(aes(color = Herbivoria),
    method = "lm",
    show.legend = FALSE
  )


# GLS - Mínimos Quadrados Generalizados (Generalized Least Squares)  ----
#  As técnicas de Generalized Least Squares (GLS) são utilizadas principalmente para lidar com violações dos pressupostos da regressão linear clássica (OLS) — especialmente quando há heterocedasticidade (variância não constante dos resíduos) e/ou correlação entre os erros (como em dados espaciais, temporais ou repetidos).

# Este modelo basicamente assume que a estrutura de covariância é uma função da distância entre as localidades.Existem diferentes funções de covariância como a Esférica: corSpher(form=\~lat+long); · Exponencial: corExp(form=\~lat+long); Gaussiana: corGaus(form=\~lat+long); Linear: corLin(form=\~lat+long); Razão quadrática: corRatio(form=\~lat+long).

# Dados
library(vegan)
data("mite")

## Calcular a riqueza de espécies em cada comunidade
riqueza <- vegan::specnumber(mite)

## Selecionar a variável ambiental - quantidade de água no substrato
data("mite.env")
agua <- mite.env[, 2]

## Criar um data.frame com riqueza, quantidade de água no substrato e coordenadas geográficas
data("mite.xy")
coords <- mite.xy
colnames(coords) <- c("long", "lat")

mite_dat <- data.frame(riqueza, agua, coords)

# Modelo linear sem incorporar a estrutura espacial
## Modelo
linear_model <- lm(riqueza ~ agua, mite_dat)

## Resíduos
par(mfrow = c(2, 2))
plot(linear_model, which = 1:4)

## Resultados do modelo
res_lm <- summary(linear_model)

## Coeficiente de determinação e coeficientes
res_lm$adj.r.squared

res_lm$coefficients

## Modelo gls sem estrutura espacial (Igual ao linear_model)
no_spat_gls <- nlme::gls(riqueza ~ agua, mite_dat, method = "REML")

# Uma maneira de identificar se os resíduos do modelo linear apresentam estrutura espacial é fazendo uma figura chamada - variograma--. Se houver estrutura espacial nos resíduos, isso pode afetar: A validez estatística dos testes do modelo (p-values, ICs, etc.); A eficiência das estimativas dos parâmetros; A precisão das predições.

# O variograma é uma ferramenta da geoestatística que mede a semelhança entre valores (neste caso, os resíduos) em função da distância entre os pontos.

# Se os resíduos não têm estrutura espacial, o variograma será aproximadamente plano (sem aumento da semivariância com a distância).

# Se os resíduos têm estrutura espacial, a semivariância aumenta com a distância, indicando que resíduos próximos são mais parecidos entre si do que os distantes.

# O variograma possui três parâmetros: i) nugget, ii) range e iii) sill

# O nugget é utilizado para quantificar a variabilidade observada nos valores menores (ou seja, em pequenas distâncias).

# O range, por sua vez, é usado para identificar a distância máxima em que a autocorrelação espacial está presente

# A posição limiar que representa claramente a “pausa” no crescimento da curva (range) indica os pontos não correlacionados e representa o sill.

## Variograma
variog_mod1 <- nlme::Variogram(no_spat_gls,
  form = ~ lat + long,
  resType = "normalized"
)

## Gráfico
plot(variog_mod1)

## Índice I de Moran
## Primeiro precisamos calcular uma matriz de distâncias geográficas entre as comunidades
dat_dist <- rdist::pdist(coords) # matriz de distância
ape::Moran.I(x = mite_dat$riqueza, w = dat_dist)

# $observed =  # Valor observado do Moran's I
# $expected =  # Valor esperado sob aleatoriedade
# $sd       =  # Desvio padrão do valor esperado
# $p.value  =  # Valor p associado.

# há evidência muito forte de que os dados de riqueza apresentam estrutura espacial negativa, ou seja, valores próximos tendem a ser diferentes entre si — um padrão que pode estar relacionado a gradientes espaciais fortes ou processos de exclusão ecológica local

# O resultado do índice de moran I e o variograma mostra que não podemos utilizar uma análise de regressão simples, pois ele assume independência entre as observações.

# Ajuste do modelo para o melhor variograma
## Covariância esférica
espher_model <- nlme::gls(
  riqueza ~ agua, mite_dat,
  nlme::corSpher(form = ~ lat + long, nugget = TRUE)
)
## Covariância exponencial
expon_model <- nlme::gls(
  riqueza ~ agua, mite_dat,
  nlme::corExp(form = ~ lat + long, nugget = TRUE)
)
## Covariância Gaussiana
gauss_model <- nlme::gls(
  riqueza ~ agua, mite_dat,
  nlme::corGaus(form = ~ lat + long, nugget = TRUE)
)
## Covariância linear
cor_linear_model <- nlme::gls(
  riqueza ~ agua, mite_dat,
  nlme::corLin(form = ~ lat + long, nugget = TRUE)
)
## Covariância razão quadrática
ratio_model <- nlme::gls(
  riqueza ~ agua, mite_dat,
  nlme::corRatio(form = ~ lat + long, nugget = TRUE)
)


# AIC - AIC é um método estatístico que compara os modelos criados na sua pesquisa e seleciona o melhor entre eles.

# Um modelo só será considerado superior a outro quando a diferença entre os seus valores de AIC (i.e, delta) forem maiores do que 2.

aic_fit <- AIC(
  no_spat_gls, espher_model, expon_model, gauss_model,
  cor_linear_model, ratio_model
)

aic_fit |>
  dplyr::arrange(AIC)

## Gráfico
plot(residuals(ratio_model, type = "normalized") ~ fitted(ratio_model))

## Varigrama
ratio_variog <- nlme::Variogram(ratio_model,
  form = ~ lat + long,
  resType = "normalized"
)

## Resumo dos modelos
summary(ratio_model)$tTable

summary(no_spat_gls)$tTable

## Gráficos
plot(ratio_variog, main = "Variograma como Modelo Ratio")

plot(variog_mod1, main = "Variograma Modelo Normal")


# GLM ----
# Diferentemente do modelo linear, um GLM estima os parâmetros por meio de Máxima Verossimilhança (ML) ao invés dos Mínimos Quadrados Comuns, também chamados de Mínimos Quadrados Ordinários (OLS).

# Um GLM relaciona a distribuição da variável resposta aos preditores lineares por meio de uma função de ligação.

## Dados Contínuos

## Gaussiana (Normal) -   Y é uma variável contínua

## Gamma - Y é uma variável contínua, mas só aceita valores contínuos positivos.

## Beta - Y é uma variável de proporção que varia continuamente entre 0 e 1 ou 0 e 100, mas não inclui 0 nem 1.

## Dados de Contagem

## Binomial - Y é binário (e.g., vivo ou morto). a variância deve ser igual à média e o parâmetro de dispersão é sempre 1.

## Poisson - Y é uma contagem (e.g., abundância ou riqueza de espécies). a variância deve ser igual à média e o parâmetro de dispersão é sempre 1.

## Bionomial Negativa - Y é uma contagem (e.g., abundância ou riqueza de espécies). Quando a variância > média (ou seja, há superdispersão).

## quasi-Poisson ou quasi-Negative binomial - não permite o uso do AIC

## Zero-inflated Poisson ou Zero-inflated NB - Se além da sobredispersão houver muitos zeros nos dados.

# Poisson ----
# Descrita apenas por um parâmetro livre (λ), dessa forma a variância tem que ser igual a média.

# Pergunta - A distância linear para o corpo d’água mais próximo influencia a abundância total de espécies de anuros?

# Dados
fragmentos <- ecodados::fragmentos

## Explorar os dados
dplyr::glimpse(fragmentos)

## Gráfico
ggplot(fragmentos, aes(dfrag, Riqueza_obs)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_smooth(method = "lm") +
  labs(
    x = "Distância para o fragmento mais próximo",
    y = "Riqueza observada"
  )


## Modelo
mod_pois <- glm(Riqueza_obs ~ dfrag,
  family = poisson(link = "log"),
  data = fragmentos
)

## Diagnose avançada # Testando Superdispersão - Superdispersão ocorre quando a variância observada é muito maior do que aquela predita pelo modelo.
simulationOutput <- DHARMa::simulateResiduals(
  fittedModel = mod_pois,
  plot = TRUE
)

# KS test: p = 0.52 - O Teste de Kolmogorov-Smirnov (KS) verifica se a distribuição geral dos resíduos simulados se desvia significativamente de uma distribuição uniforme (o que é esperado para um modelo bem ajustado).

# Com um p-valor de 0.52 (que é muito maior que 0.05), não rejeitamos a hipótese nula. Isso significa que não há evidência estatística de que o ajuste geral do seu modelo seja ruim. A forma da distribuição dos resíduos está de acordo com o esperado.

# Dispersion test: p = 0 - verifica se a variância dos seus dados é compatível com a variância esperada pelo modelo (para um modelo de Poisson, por exemplo, a variância deveria ser igual à média).

# Com um p-valor de 0 (ou muito próximo de zero), rejeitamos fortemente a hipótese nula. Isso indica que há superdispersão (overdispersion) significativa no seu modelo. Em outras palavras, a variabilidade (dispersão) observada nos seus dados é muito maior do que o seu modelo atual consegue explicar.

# Outlier test: p = 0.54 -  Verifica se há mais outliers (pontos de dados que o modelo prevê muito mal) do que o esperado por acaso.

# Com um p-valor de 0.54 (maior que 0.05), não rejeitamos a hipótese nula. Isso significa que o número de outliers no seu modelo não é estatisticamente preocupante.

## Overdispersion
par(mfrow = c(1, 1))
DHARMa::testDispersion(mod_pois)

# usa a estatística qui-quadrado de Pearson para calcular a dispersão. - Melhor visualização.
performance::check_overdispersion(mod_pois)

# Outra maneira de calcular com base na deviance -  Esta é a forma "clássica" e mais direta de estimar o parâmetro de dispersão a partir da saída padrão de um GLM no R (summary(mod_pois)). A "deviance" é uma medida de quão bem o seu modelo ajustado se compara a um "modelo saturado" (um modelo que se ajusta perfeitamente aos dados).

mod_pois$deviance / mod_pois$df.residual
# Quando esse valor é próximo de 1, isso sugere que não há overdispersion. No entanto, se ele for maior que 1.5, isso sugere que o modelo sofre de overdispersion

## Resumo do modelo
summary(mod_pois)

# Inflação de Zeros ----
# Qualquer das formas mostradas acima de diagnosticar overdispersion pode ser usada na maioria das vezes, com exceção de dados com muitos zeros (pouca variância).

## Inflação de zeros - performanace
performance::check_zeroinflation(mod_pois)

DHARMa::testZeroInflation(mod_pois)

# O modelo tem problemas com overdispersion, mas isso não é causado pelo excesso de zeros.

## Calculando o R2 do modelo
MuMIn::r.squaredGLMM(mod_pois)

piecewiseSEM::rsquared(mod_pois)

performance::r2(mod_pois)

# Plot do Modelo - Caso fosse Confiável, esse modelo tem superdispersão de dados

library(MASS)
a1 <- ggplot(fragmentos, aes(dfrag, Riqueza_obs)) +
  geom_point(cex = 4, alpha = 0.7) +
  geom_smooth(
    method = "glm", formula = y ~ x,
    method.args = list(family = "poisson"), se = TRUE
  ) +
  labs(
    x = "Distância para o fragmento mais próximo",
    y = "Riqueza observada"
  )

ggExtra::ggMarginal(a1, fill = "red")


# O que causa Overdispersion? ----
# As causas aparentes são geradas pela má especificação do modelo, tais como: não inclusão de covariáveis ou interações no modelo; presença de outliers na variável resposta; efeitos não lineares da covariável (X2, X3…); escolha errada da função de ligação (link function).

# As causas reais incluem: variância maior que a média, muitos zeros, agregação de observações, correlação entre observações (não independência)

## O que fazer se seu modelo tiver overdispersion?

# Usar outra distribuição, tal como Binomial Negativa, caso o dispersion parameter seja maior que 15 ou 20 ou Usar um modelo com correção de erro da sobredispersão, caso 1.5 < dispersion > 15

# Binomial Negativa ----
# A binomial negativa pode ser entendida como uma mistura das distribuições Poisson e Gamma, ou seja, ela aceita dados de contagem que sejam positivos, mas sem zero.

## Ajuste do modelo
mod_nb <- MASS::glm.nb(Riqueza_obs ~ dfrag, data = fragmentos)

## Diagnose avançada
simulationOutput <- DHARMa::simulateResiduals(fittedModel = mod_nb, plot = TRUE)

par(mfrow = c(1, 1))
DHARMa::testDispersion(mod_nb)

performance::check_overdispersion(mod_nb)

mod_nb$deviance / mod_nb$df.residual

## Coeficiente de determinação
piecewiseSEM::rsquared(mod_nb)

summary(mod_nb)

## Gráfico
ggplot(fragmentos, aes(dfrag, Riqueza_obs)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_smooth(method = "glm.nb", formula = y ~ x, se = TRUE) +
  labs(
    x = "Distância para o fragmento mais próximo",
    y = "Riqueza observada"
  )


# Quasi-Poisson ----
# Não dá para utilizar o AIC.

## Modelo
mod_quasipois <- glm(Riqueza_obs ~ dfrag,
  family = quasipoisson(link = "log"),
  data = fragmentos
)


# Distribuição Binomial ----
# Quando temos dados de proporção (e.g., número de doentes por 1000 habitantes) ou incidência (i.e., presença ou ausência), a distribuição mais adequada para modelar os dados é a distribuição binomial.

# Pergunta - A melanina protege girinos contra os efeitos da radiação ultravioleta?

# Dados
uv_cells <- ecodados::uv_cells

## Traduzir nomes das colunas e níveis de pigmentação
colnames(uv_cells) <- c(
  "UV", "Pigmentacao", "n_celulas", "linfocito",
  "neutrofilo", "basofilo", "monocito",
  "eosinofilo"
)

uv_cells <- uv_cells |>
  dplyr::mutate(Pigmentacao = stringr::str_replace_all(
    Pigmentacao,
    c("Yes" = "Sim", "No" = "Não")
  ))

## Gráfico
# Calcular média e intervalo de confiança
eosinofilo <- Rmisc::summarySE(uv_cells,
  measurevar = "eosinofilo",
  groupvars = c("UV", "Pigmentacao")
)

# Definir posição de linhas e pontos no gráfico
pd <- position_dodge(0.1)

eosinofilo |>
  ggplot(aes(
    x = UV, y = eosinofilo, colour = Pigmentacao,
    group = Pigmentacao, fill = Pigmentacao
  )) +
  geom_errorbar(aes(ymin = eosinofilo - se, ymax = eosinofilo + se),
    width = .1, size = 1.1, position = pd
  ) +
  geom_line(position = pd, size = 1.1) +
  geom_point(pch = 21, colour = "black", position = pd, size = 3.5) +
  scale_colour_manual(values = c("darkorange", "cyan4")) +
  scale_fill_manual(values = c("darkorange", "cyan4")) +
  labs(
    x = "UV", y = "Eosinófilo", fill = "Pigmentação",
    colour = "Pigmentação"
  )

## Modelo
mod1 <- glm(cbind(eosinofilo, n_celulas) ~ UV * Pigmentacao,
  family = binomial, data = uv_cells
)

## Diagnose avançada
simulationBion <- DHARMa::simulateResiduals(fittedModel = mod1, plot = TRUE)

performance::binned_residuals(mod1)

## Coeficientes estimados pelo modelo
summary(mod1)

anova(mod1)

## Parâmetros
pairs(emmeans::emmeans(mod1, ~ UV | Pigmentacao))

ggplot(uv_cells, aes(UV, eosinofilo)) +
  geom_violin(aes(color = Pigmentacao)) +
  geom_jitter(
    shape = 16, position = position_jitter(0.1),
    cex = 4, alpha = 0.7
  ) +
  scale_colour_manual(values = c("darkorange", "cyan4"))


## Dados de Incidência - Presença ou Ausência

# Pergunta - A probabilidade de lagartos da espécie Coleodactylus meridionalis perderem (autotomizarem) a cauda aumenta com o tamanho do corpo e de acordo com o sexo dos lagartos?

# Dados
lagartos <- ecodados::lagartos

colnames(lagartos) <- c(
  "numero", "sexo", "SVL", "comprimento_cauda",
  "cauda_autotomizada", "estado_cauda"
)

## Dados faltantes
visdat::vis_miss(lagartos, cluster = TRUE)

## Removendo dados faltantes
dados_semNA <- remove_missing(lagartos, vars = "sexo")

## Visualizar
visdat::vis_miss(dados_semNA)

## Gráfico
ggplot(dados_semNA, aes(SVL, estado_cauda)) +
  geom_point(aes(shape = sexo, colour = sexo), size = 4, alpha = 0.4) +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial")
  ) +
  labs(
    y = "Estado da Cauda", x = "Comprimento Rostro-Cloacal (mm)",
    shape = "Sexo", colour = "Sexo"
  )

# Logit ----
## Modelos
mod_log <- glm(estado_cauda ~ SVL * sexo,
  data = dados_semNA,
  family = binomial(link = "logit")
)

# Probit ----
mod_pro <- glm(estado_cauda ~ SVL * sexo,
  data = dados_semNA,
  family = binomial(link = "probit")
)

# Seleção de modelos
bbmle::AICctab(mod_log, mod_pro, nobs = 139)
# Existe pouca diferença entre o modelo probit e logit. Como o modelo logit é mais simples vamos interpretá-lo apenas.

## Diagnóse avançada
simulationBion <- DHARMa::simulateResiduals(fittedModel = mod_log, plot = T)

performance::binned_residuals(mod_log, show_dots = TRUE)

# Para modelos com parâmetro de dispersão conhecida (e.g., binomial e Poisson), o chi-quadrado é a estatística mais apropriada.
## Coeficientes estimados pelo modelo
summary(mod_log)

anova(mod_log, test = "Chisq")

# A interpretação dos resultados é que o tamanho de corpo (SVL) afeta negativamente a probabilidade da cauda estar intacta, i.e., com o aumento do tamanho, a probabilidade da cauda permanecer intacta diminui. A interação não foi significativa, então o efeito é independente do sexo dos lagartos.

# Zero-inflated ----
# Quando se analisa a abundância ou riqueza de espécies é comum que tenhamos dados com muitos zeros. Esse fenômeno pode ser causado por vários processos ecológicos, tais como locais fora do nicho da espécie, falha na detecção, amostras feitas fora do habitat ou em locais onde não se espera encontrar a espécie. Esses modelos são chamados de zero-inflated e hurdle models

# Esse tipo de dado é problemático porque rompe com os pressupostos da distribuição Poisson e binomial negativa, podendo inclusive ser uma das causas da overdispersion.

# Hurdle Models
# modelam os dados dividindo-os em dois subconjuntos: um no qual reduzimos os dados à presença-ausência, ou seja, todos os dados maiores que 1 são transformados em 1 e usamos, por exemplo, uma distribuição binomial; e uma outra parte que só considera os valores positivos sem zero, utilizando uma Poisson ou binomial negativa truncadas. Ao fazer isso, a distribuição truncada assume que os zeros são gerados tanto por processos ecológicos, quanto erros de amostragem (ou seja, é impossível distinguir entre essas duas fontes). Portanto, esses zeros são excluídos da distribuição com dados de contagem.

# Zero-Inflated
# Modelos com zero inflados funcionam de maneira similar, mas permitem que a distribuição Poisson contenha zeros, ou seja, não é utilizada uma distribuição truncada. Ao fazer isso, está distribuição de Poisson pressupõe que os zeros foram gerados por um processo ecológico real, tal como, ausência de habitat adequado.

# Pergunta - Quais atributos de história de vida dos lagartos são relacionados com o volume (load) de infecção, tais como tamanho e sexo?

# Dados
parasitas <- ecodados::parasitas

## Gráficos
ggplot(parasitas, aes(Raillietiella_mottae, fill = Especie)) +
  geom_density(alpha = 0.4) +
  facet_grid(Especie ~ Sexo) +
  scale_fill_manual(values = c("darkorange", "cyan4")) +
  theme(legend.position = "none")

ggplot(parasitas, aes(CRC, Raillietiella_mottae, fill = Especie)) +
  geom_point(size = 4, alpha = 0.4, shape = 21) +
  facet_grid(Sexo ~ Especie) +
  scale_fill_manual(values = c("darkorange", "cyan4")) +
  theme(legend.position = "none") +
  labs(
    x = "Comprimento Rostro-Cloacal",
    y = expression(italic("Raillietiella mottae"))
  )

# Quando nos deparamos com dados complexos assim, a estratégia é sempre começar com um modelo simples e depois adicionar mais parâmetros. Portanto, vamos iniciar com um modelo Poisson, mesmo sabendo que ele muito provavelmente não será adequado para modelar estes dados.

# ## Modelo
pois_plain <- glm(Raillietiella_mottae ~ CRC + Sexo * Especie,
  data = parasitas, family = "poisson"
)

## Diagnose avançada
# Verificar zero inflation
performance::check_zeroinflation(pois_plain)

# Verificar overdispersion
performance::check_overdispersion(pois_plain)

## Modelos 3 modelos
library(glmmTMB)
# Hurdle model
hur_NB <- glmmTMB(Raillietiella_mottae ~ CRC + Sexo * Especie,
  zi = ~., data = parasitas, family = truncated_nbinom2
)

# zero-inflated Poisson
ziNB_mod2 <- glmmTMB(Raillietiella_mottae ~ CRC + Sexo * Especie,
  zi = ~., data = parasitas, family = nbinom2
)

# zero-inflated Negative binomial
ziP_mod2 <- glmmTMB(Raillietiella_mottae ~ CRC + Sexo * Especie,
  zi = ~., data = parasitas, family = poisson
)

## Diagnose de inflação de zeros
performance::check_zeroinflation(hur_NB) # Aqui vemos que o modelo zero-altered (Hurdle Model) conseguiu predizer exatamente a quantidade de zeros observada, fazendo com que o modelo seja suficiente para usarmos com esses dados.

performance::check_zeroinflation(ziP_mod2)

performance::check_zeroinflation(ziNB_mod2)

## Seleção de modelos
bbmle::ICtab(pois_plain, hur_NB, ziP_mod2, ziNB_mod2,
  type = c("AICc"),
  weights = TRUE
)
# Não podemos distinguir entre os dois modelos com zeroinflated porque o dAICc < 2, ou seja, o ajuste deles aos dados são praticamente iguais.

## Diagnoses
simulationOutput <- DHARMa::simulateResiduals(fittedModel = hur_NB, plot = T)

simulationOutput <- DHARMa::simulateResiduals(fittedModel = ziP_mod2, plot = T)

simulationOutput <- DHARMa::simulateResiduals(fittedModel = ziNB_mod2, plot = T)

## Coeficientes estimados pelo modelo
summary(hur_NB)

## Gráfico
parasitas$phat <- predict(hur_NB, type = "response")
parasitas <- parasitas[with(parasitas, order(Sexo, Especie)), ]
ggplot(parasitas, aes(
  x = CRC, y = phat, colour = Especie,
  shape = Sexo, linetype = Sexo
)) +
  geom_point(aes(y = Raillietiella_mottae),
    size = 4,
    alpha = .7, position = position_jitter(h = .2)
  ) +
  geom_line(size = 1) +
  scale_fill_manual(values = c("darkorange", "cyan4")) +
  scale_colour_manual(values = c("darkorange", "cyan4")) +
  labs(
    x = "Comprimento Rostro-Cloacal",
    y = expression(paste(
      "Abundância de ",
      italic("Raillietiella mottae")
    ))
  )


# Dados Contínuos - Distribuição Beta ----
# a família Beta é utilizada para modelar variáveis de resposta contínuas restritas ao intervalo (0, 1), como proporções ou taxas. Diferente da binomial, que lida com contagens de sucessos, o modelo Beta é adequado quando a variável é realmente contínua entre 0 e 1, como proporção de cobertura de coral, taxa de germinação, ou eficiência de um processo.


# Dados
fish <- ecodados::fish

## Tradução dos nomes das colunas
colnames(fish) <- c(
  "animal", "tratamento", "tempo", "sexo",
  "preto", "vermelho"
)

# Pergunta - A YOH aumenta a coloração escura no olho e mandíbula dos peixes via dispersão dos pigmentos?

## Filtrando os dados
fish$animal <- factor(fish$animal)
fish$sexo <- factor(fish$sexo)

macho_preto <- dplyr::filter(fish, sexo == "M")

## Gráfico
ggplot(macho_preto, aes(preto / 100)) +
  geom_density(colour = "cyan4", fill = "cyan4", alpha = 0.4) +
  theme(legend.position = "none") +
  labs(x = "Índice de escuridão do corpo")

# No histograma podemos ver que os dados de fato variam continuamente no intervalo entre 0 e 1, tendo uma distribuição notadamente bimodal.

## Modelo
mod2 <- glmmTMB::glmmTMB(preto / 100 ~ tratamento * tempo + (1 | animal),
  family = glmmTMB::beta_family, data = macho_preto
)

## Diagnose
simulationOutput <- DHARMa::simulateResiduals(fittedModel = mod2, plot = TRUE)

## Coeficientes estimados pelo modelo
car::Anova(mod2)

# Aqui vemos que a interação é significativa. Portanto, temos de interpretar os níveis do fator da combinação, fazemos isso no pacote emmeans colocando a barra |.

# níveis do fator da combinação
pairs(emmeans::emmeans(mod2, ~ tratamento | tempo))

## Gráfico
escuridao <- Rmisc::summarySE(macho_preto,
  measurevar = "preto",
  groupvars = c("tempo", "tratamento")
)

# Definir posição de linhas e pontos no gráfico
pd <- position_dodge(0.1)

escuridao |>
  ggplot(aes(x = tempo, y = preto, colour = tratamento,
    group = tratamento, fill = tratamento)) +
  geom_errorbar(aes(ymin = preto - se, ymax = preto + se),
    width = .1, size = 1.1, position = pd) +
  geom_line(position = pd, size = 1.1) +
  geom_point(pch = 21, colour = "black", position = pd, 
             size = 3.5) +
  scale_colour_manual(values = c("darkorange", "cyan4")) +
  scale_fill_manual(values = c("darkorange", "cyan4")) +
  xlab("Tempo de experimento (horas)") +
  ylab("Índice de escuridão do corpo")


# Análise Multidimensional ----

# Análises multivariadas podem ser divididas, grosseiramente, em dois tipos: agrupamento e ordenação.

# Análises de agrupamento, em geral, tentam agrupar objetos (observações) ou descritores em grupos de maneira que objetos do mesmo grupo sejam mais semelhantes entre si do que objetos de outros grupos (Legendre & Legendre 2012). 

# Análise de ordenação, é uma operação pela qual os objetos (ou descritores) são posicionados num espaço que contém menos dimensões que o conjunto de dados original.

# Modo Q e Modo R ----

# A análise no Modo Q serve para verificar a relação entre objetos. Coeficientes de associação do modo Q são medidas de (dis)similaridade ou distância

# Análise no Modo R serve para verificar a relação entre descritores. Os coeficientes de associação para o modo R são covariância ou correlação.

# Índices de Similaridade (S) e Distância (1 - S) ----

# A Similaridade é máxima (S=1) quando dois objetos são idênticos - Existem ao menos 26 índices de similaridade que podem ser agrupados de acordo com o tipo de dado (qualitativos ou quantitativos) ou a maneira com que lidam com duplos zeros (simétricos ou assimétricos).

# Distância é o contrário da similaridade (D=1-S) e não têm limites superiores (dependem da unidade de medida). as distâncias só se aplicam a dados quantitativos e têm como características serem métricas, semi-métricas ou não-métricas.

# Coeficientes Simétricos para Variáveis Contínuas ----
# Euclidiana, Canberra (variação da Distância Euclidiana), Mahalanobis (calcula a distância entre dois pontos num espaço não ortogonal, levando em consideração a covariância entre descritores), Manhattan (variação da Distância Euclidiana), Chord (elimina diferenças entre abundância total de espécies), 𝜒2 (dá peso maior para espécies raras) e Hellinger (não dá peso para espécies raras).

# Como essas medidas de distância apresentadas acima assumem que os dados são quantitativos e não de contagem, elas não são adequadas para lidar com dados de bundância ou incidência de espécies, porque atribuem um grau de parecença a pares de locais que compartilham zeros (Legendre & Legendre 2012). Por esse motivo, precisamos de coeficientes que desconsiderem os duplos zeros. Eles são chamados de assimétricos.

# Coeficientes Assimétricos Binários para objetos ----
#  Esses coeficientes são apropriados para dados de incidência de espécies (presença ausência) e desconsideram as duplas ausências. Os índices deste tipo mais comuns utilizados em ecologia são Jaccard, Sørensen e Ochiai.

# A diferença entre os índices de Jaccard e Sørensen é que o índice de Sørensen dá peso dobrado para duplas presenças. Por conta dessas características, estes índices são adequados para quantificar diversidade beta (Anderson et al. 2011, Legendre & De Cáceres 2013). Esses índices variam entre 0 (nenhuma espécie é compartilhada entre o par de locais) a 1 (todas as espécies são compartilhadas entre o par de locais).

# Coeficientes Binários para descritores (R mode) ----
# Se o objetivo for calcular a similaridade entre descritores binários (e.g., presença ou ausência de características ambientais) de pares de locais, geralmente o coeficiente recomendado é o de Sokal & Michener. Este índice está implementado na função dist.binary() do pacote ade4.

# Coeficientes Assimétricos Quantitativos para objetos ----
# Estes são os coeficientes utilizados para dados de contagem (e.g., abundância) e quantitativos (e.g., frequência, biomassa, porcentagem de cobertura). Diferentemente das distâncias, estes coeficientes são assimétricos, ou seja, não consideram duplas ausências e, portanto, são adequados para analisar dados de composição de espécies. Além disso, uma outra característica deles é serem semi-métricos. Os índices mais comuns deste tipo são Bray-Curtis (conhecido como percentage difference, em inglês), Chord, log-Chord, Hellinger, chi-quadrado e Morisita-Horn. - Todos os índices discutidos até aqui estão implementados nas funções ade4::dist.ktab(), adespatial::dist.ldc() e vegan::vegdist().

# Coeficientes para descritores (R mode) que incluem mistura de tipos de dados ----
# Gower ade4::dist.ktab()

# Dica ----
# Padronize as variáveis (normalmente elas possuem unidades diferentes), para diminuir o erro do tipo 1, padronize-as utilizando a distribuição Z
# Transforme os dados com muitos zeros e alta abundância - hellinger ou chord.


# Análise de Agrupamento ----
# O objetivo da análise de agrupamento é agrupar objetos admitindo que haja um grau de similaridade entre eles. - existem cinco tipos de métodos: i) sequenciais ou simultâneos, ii) aglomerativo ou divisivo, iii) monotéticos ou politéticos, iv) hierárquico ou não hierárquicos e v) probabilístico.

#### Para avaliar o quão bem uma análise de agrupamento representa os dados originais existe uma métrica — o Coeficiente de Correlação Cofenético.

# Agrupamento Hierárquico ----
# Métodos hierárquicos podem ser divididos naqueles que consideram o centroide ou a média aritmética entre os grupos.

# UPGMA (Agrupamento pelas médias aritméticas não ponderadas)----
# É o principal método hierárquico que utiliza a média aritmética. - Ele funciona da seguinte forma: a maior similaridade (ou menor distância) identifica os próximos agrupamentos a serem formados. Após esse evento, o método calcula a média aritmética das similaridades ou distâncias entre um objeto e cada um dos membros do grupo ou, no caso de um grupo previamente formado, entre todos os membros dos dois grupos. Todos os objetos recebem pesos iguais no cálculo.

# WARD ----
# É o principal método que utiliza centroides. Ele é baseado no critério de quadrados mínimos (OLS), o mesmo utilizado para ajustar um modelo linear, o objetivo é definir os grupos de maneira que a soma de quadrado dentro dos grupos seja minimizada. 
 
# Nível de Corte ----
# Para interpretar os resultados precisamos antes definir um nível de corte, que vai nos dizer quantos grupos existem. Os principais é pela Reamostragem (bootstrap) e pelo método heurístico

# Dados de Abundância ----
# Se os valores não forem discrepantes utilizar transformação de Hellinger, se não log1p()
# Variáveis com diferentes escalas, transformar em Zscore para ter a média 0 e sd 1.

# Pergunta - Existem grupos de espécies de anfíbios anuros com padrões de ocorrência similar ao longo das poças?

# Dados Composição de espécies
sp_compos <- ecodados::bocaina

## Coeficiente de Morisita-Horn ----
distBocaina <- vegan::vegdist(x = sp_compos, method = "horn")

## Agrupamento com a função hclust e o método UPGMA
dendro <- hclust(d = distBocaina, method = "average")

## Visualizar os resultados - objetos hclust funcionam muito bem com o plot()
plot(dendro, hang=-1, main = "Dendrograma",
     ylab = "Similaridade (índice de Horn)",
     xlab="", sub="")

## Coeficiente de correlação cofenética
cofresult <- cophenetic(dendro)
cor(cofresult, distBocaina)

# Um coeficiente de correlação cofenética > .7 indica uma boa representação.

# Gap Statistic (método para escolher o número ótimo de clusters)

library(cluster)
library(factoextra)
fviz_nbclust(as.matrix(distBocaina), FUN = hcut, method = "gap_stat")

## Gráfico
plot(dendro, main = "Dendrograma",
     ylab = "Similaridade (índice de Horn)",
     xlab="", sub="")
k <- 10
n <- ncol(sp_compos)
MidPoint <- (dendro$height[n-k] + dendro$height[n-k+1]) / 2
abline(h = MidPoint, lty=2)

# Coeficiente de Distância de Chord ----
# É indicada para dados de composição de espécies. Se transformarmos uma matriz usando a transformação Chord e depois calcularmos a distância Euclidiana, isso equivale a calcular diretamente a distância de Chord. 

dist_chord <- vegan::vegdist(sp_compos, method = "chord")
bocaina_transf <- disttransform(t(sp_compos), "chord")


# Agrupamento hierárquico
hc <- hclust(dist_chord, method = "average")

plot(hc, hang=-1, main = "Dendrograma com valores de P",
     ylab = "Distância Euclideana",
     xlab="", sub="")
pvclust::pvrect(hc)
