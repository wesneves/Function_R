# Visualização Gráfica ----

# ggplot2: pacote integrado ao tidyverse, possui uma sintaxe própria baseada na gramática de gráficos por camadas (layers), necessitando de funções específicas para objetos de classes diferentes, demandando geralmente mais tempo para a construção dos códigos. Possui funções como ggplot(), aes(), geom_*(), facet_*(), stats_*(), coord_*() e theme_*(), que são conectadas pelo operador +
 
#· ggplot2 extentions: conjunto de pacotes que adicionam diversas expansões ao pacote ggplot2. Exemplos: gganimate, GGally, patchwork e esquisse

# visdat: cria visualizações preliminares de dados exploratórios de um conjunto de dados inteiro para identificar problemas ou recursos inesperados usando pacote ggplot2. Possui diversas funções específicas: vis_dat() para visão geral dos dados, vis_miss() para visão de dados faltantes (NA) e vis_compare() para visualizar a diferença entre dados
# ggpubr: pacote que fornece funções simplificadas para criar e personalizar gráficos para publicação, baseados no ggplot2. Possui funções específicas: gghistogram(), ggdensity(), ggboxplot(), ggviolin(), ggbarplot() e ggscatter()
# lattice: pacote para visualização de dados inspirado nos gráficos treliça (do inglês Trellis, geralmente para dados com muitas variáveis que geram uma matriz retangular de gráficos). Também possui funções específicas: xyplot(), histogram(), densityplot(), barchart(), bwplot() e dotplot(). O pacote latticeExtra disponibiliza algumas possibilidades a mais para esse pacote
# · plotly: pacote para criar gráficos interativos da web por meio da biblioteca gráfica de JavaScript de código aberto plotly.js. Também possui funções específicas: plot_ly(), add_histogram(), add_bars(), add_boxplot(), add_markers(), add_paths(), add_lines() e add_polygons().


# ggplot2() ----

# Camada 1 - dados ggplot(): são as informações no formato data.frame que serão usadas nas diferentes camadas nas funções aes(), geom_*(), stat_*(), facet_*() e scale_*()
# Camada 2 - mapeamento aes(): atributos estéticos, determina que colunas do data. frame serão usadas para as representações geométricas, assim como tamanho, forma, cor, preenchimento e transparência
# Camada 3 - definição da geometria geom_*(): define o tipo de gráfico, como pontos, boxplots, violino, linhas, polígonos, entre outros
# Camada 4 - transformações estatísticas stat_*(): modificam, quando necessário, os dados que serão incluídos no gráfico, além de produzir estatísticas como regressões
# Camada 5 - sistema de coordenadas coords_*(): descreve como as coordenadas dos dados são mapeadas para o plano do gráfico
# Camada 6 - facetas facets_*(): especifica como a visualização dos elementos aes() são divididos em diferentes “janelas gráficas”
# Camada 7 - escala scale_*(): permite o controle das características visuais (cor, forma e tamanho) dos elementos declarados em aes()
# Camada 8 - temas theme*(): controla a aparência visual dos elementos do gráfico, como fontes, cores e legenda.


# Estrutura ----
'ggplot(data = <DATA>) +
  <GEOM_FUNCTION>(
    mapping = aes(<MAPPINGS>),
    stat = <STAT>,
    position = <POSITION>
  ) +
  <COORDINATE_FUNCTION> +
  <FACET_FUNCTION> +
  <SCALE_FUNCTION> +
  <THEME_FUNCTION>'
  
# Tipos de Gráficos
# 
# Histograma (histogram): distribuição de frequência de uma coluna para dados contínuos (cores diferentes podem representar espécies, populações ou grupos distintos)
# 
# Gráfico de densidade (density plot): distribuição da densidade de uma coluna para dados contínuos (assim como no histograma, cores diferentes podem ser utilizadas para representar espécies, populações ou grupos distintos)
# 
#  Gráfico de dispersão (scatter plot) e gráfico de linha: relação entre valores de duas colunas para dados contínuos (X e Y)
#  
# Diagrama de pontos (dot plot): distribuição da quantidade de valores agrupados de uma coluna para dados contínuos
# 
# Gráfico de setores (pie chart e donut chart): representação da quantidade de valores de uma coluna para dados categóricos, geralmente em proporção ou porcentagem
# 
# Gráfico de barras (bar plot): representação da quantidade de valores de uma ou mais colunas para dados categóricos
# 
# Gráfico de caixa (box plot e violin plot): distribuição de valores contínuos de uma coluna (Y) para dois ou mais fatores categóricos de outra coluna (X) no formato de caixas e também no formato de “violinos” (considerando a variação e distribuição)
# 
# Gráfico pareado (pairs plot): relação entre valores de duas colunas para dados contínuos (X e Y), para colunas par-a-par.
  
# geom_histogram() ----
## Dados
dist_normal <- data.frame(x = rnorm(10000, mean = 0, sd = 1))

## Histograma de uma variável contínua
library(ggplot2)

ggplot(data = dist_normal, aes(x = x)) +
  geom_histogram()

# Dados dos pinguins em português
pinguins <- penguins

## Edição dos nomes das colunas para português
colnames(pinguins) <- c("especies", "ilha", "comprimento_bico",
                        "profundidade_bico", "comprimento_nadadeira",
                        "massa_corporal", "sexo", "ano")

## Histograma da coluna flipper_length_mm
ggplot(data = pinguins, aes(x = comprimento_nadadeira)) +
  geom_histogram()

## Histograma com cores para diferentes categorias com sobreposição
ggplot(data = pinguins, aes(x = comprimento_nadadeira,
                            fill = especies)) +
  geom_histogram(alpha = .4) +
  labs(title = "Com sobreposiçao")

## Histograma com cores para diferentes categorias sem sobreposição
ggplot(data = pinguins, aes(x = comprimento_nadadeira,
                            fill = especies)) +
  geom_histogram(position = "dodge") +
  labs(title = "Sem sobreposiçao")

## Histograma exemplo
ggplot(data = pinguins, aes(x = comprimento_nadadeira,
                            fill = especies)) +
  geom_histogram(alpha = .4, position = "identity") +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  theme_bw(base_size = 14) +
  labs(x = "Comprimento da nadadeira (mm)",
       y = "Frequência absoluta", fill = "Espécies")


# geom_density() ----
## Gráfico de densidade
ggplot(data = pinguins, aes(x = comprimento_nadadeira)) +
  geom_density()

## Argumento fill
ggplot(data = pinguins, aes(x = comprimento_nadadeira)) +
  geom_density(fill = "cyan4")

## Argumento fill, color e alpha
ggplot(data = pinguins, aes(x = comprimento_nadadeira)) +
  geom_density(fill = "cyan4", color = "black", alpha = .4)

## O argumento fill preenche cada nível da coluna "especies" (sem transparência: alpha = 1)
ggplot(data = pinguins, aes(x = comprimento_nadadeira,
                            fill = especies)) +
  geom_density() +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  labs(title = "Sem transparência")

## Gráfico de densidade com cores para diferentes categorias com sobreposição
ggplot(data = pinguins, aes(x = comprimento_nadadeira,
                            fill = especies)) +
  geom_density(alpha = .4) +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  labs(title = "Com transparência")

## Gráfico de densidade exemplo
ggplot(data = pinguins, aes(x = comprimento_nadadeira,
                            fill = especies)) +
  geom_density(alpha = .4) +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  scale_x_continuous(breaks = seq(from = 160, to = 240, by = 10),
                     limits = c(160, 240)) +
  scale_y_continuous(breaks = seq(from = 0, to = .07, by = .01)) +
  theme_bw(base_size = 14) +
  labs(x = "Comprimento da nadadeira (mm)", y = "Densidade",
       fill = "Espécies")


# geom_dotplot() ----
## Gráfico de pontos
ggplot(data = pinguins, aes(x = comprimento_nadadeira)) +
  geom_dotplot(dotsize = .6)

## O argumento fill preenche cada nível da coluna "especies" (sem transparência: alpha = 1)
ggplot(data = pinguins, aes(x = comprimento_nadadeira,
                            fill = especies)) +
  geom_dotplot(dotsize = .9)

## Diagrama de pontos com cores para diferentes categorias com sobreposição
ggplot(data = pinguins, aes(x = comprimento_nadadeira,
                            fill = especies)) +
  geom_dotplot(dotsize = .7, color = "black", alpha = .4)

## Diagrama de pontos exemplo
ggplot(data = pinguins, aes(x = comprimento_nadadeira,
                            fill = especies)) +
  geom_dotplot(color = "black", alpha = .7, position = "dodge") +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  scale_x_continuous(breaks = seq(from = 170, to = 240, by = 10),
                     limits = c(170, 240)) +
  scale_y_continuous(breaks = seq(from = 0, to = 1.4, by = .2),
                     limits = c(0, 1.4)) +
  theme_bw(base_size = 14) +
  labs(x = "Comprimento da nadadeira (mm)", y = "Frequência",
       fill = "Espécies")

# geom_bar() ----
## Número de indivíduos coletados
pinguins_count <- pinguins |> 
  dplyr::count(especies)
pinguins_count

## Gráfico de barras
ggplot(data = pinguins_count, aes(x = especies, y = n)) +
  geom_bar(stat = "identity")

## Modificando o preenchimento
ggplot(data = pinguins_count, aes(x = especies, y = n)) +
  geom_bar(stat = "identity", fill = "cyan4")

## Modificando a cor e o preenchimento
ggplot(data = pinguins_count, aes(x = especies, y = n)) +
  geom_bar(stat = "identity", color = "cyan4", fill = "white")

## Modificando a largura da barra = .75
ggplot(data = pinguins_count, aes(x = especies, y = n)) +
  geom_bar(stat = "identity", width = .75) +
  labs(title = "Largura = .75")

## Modificando a largura da barra = .25
ggplot(data = pinguins_count, aes(x = especies, y = n)) +
  geom_bar(stat = "identity", width = .25) +
  labs(title = "Largura = .25")

## Barras vertical
ggplot(data = pinguins_count, aes(x = especies, y = n)) +
  geom_bar(stat = "identity", width = .6)

## Barras horizontal
ggplot(data = pinguins_count, aes(x = especies, y = n)) +
  geom_bar(stat = "identity", width = .6) +
  coord_flip()

## Gráfico de barras com preenchimento colorido
ggplot(data = pinguins_count, aes(x = especies, y = n,
                                  fill = especies)) +
  geom_bar(stat = "identity")


## Calcular o desvio padrão por espécie
pinguins_media <- pinguins |> 
  dplyr::group_by(especies) |> 
  dplyr::summarise(media = mean(comprimento_nadadeira, na.rm = TRUE),
                   desvio = sd(comprimento_nadadeira, na.rm = TRUE))

## Gráfico de barras com desvio padrão
ggplot(data = pinguins_media, aes(x = especies, y = media,
                                  fill = especies)) +
  geom_bar(stat = "identity", alpha = .4) +
  geom_errorbar(aes(ymin = media-desvio, ymax = media+desvio),
                width = .1) +
  geom_point()

## Gráfico de barra exemplo
ggplot(data = pinguins_count, aes(x = especies, y = n,
                                  fill = especies)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = n), fill = "white") +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 14) +
  labs(x = "Espécies", y = "Número de indivíduos", fill = "Espécies")

# geom_bar - (pie chart) coord_polar() ----
## Cálculo da proporção - pie
pinguins_prop <- pinguins |> 
  dplyr::count(especies) |> 
  dplyr::mutate(prop = round(n/sum(n), 4)*100) 

## Gráfico de setores
ggplot(data = pinguins_prop, aes(x = "", y = prop, fill = especies)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = paste0(prop, "%")), color = "white",
            position = position_stack(vjust = .5), size = 6) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(fill = "Espécies")

# geom_bar - (donut chart) coord_polar() ----

## Gráfico de setores - donut
ggplot(data = pinguins_prop, aes(x = 2, y = prop,
                                 fill = especies)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(prop, "%")), color = "white",
            position = position_stack(vjust = .5), size = 4) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  xlim(0, 2.5) +
  coord_polar(theta = "y", start = 0) +
  theme_void() +
  theme(legend.position = c(.5, .5),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15)) +
  labs(fill = "Espécies")

## Gráfico de barras - vertical
g_bar_v <- ggplot(data = pinguins_prop, aes(x = especies, y = prop,
                                            fill = especies)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = prop), fill = "white") +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Gráfico de Barras (Vertical)", x = "Espécies",
       y = "Número de indivíduos", fill = "Espécies")

## Gráfico de barras - horizontal
g_bar_h <- ggplot(data = pinguins_prop, aes(x = especies, y = prop,
                                            fill = especies)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = prop), fill = "white") +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Gráfico de Barras (Horizonal)", x = "Espécies",
       y = "Número de indivíduos", fill = "Espécies")

## Gráfico de setores - pie
g_pie <- ggplot(data = pinguins_prop, aes(x = "", y = prop,
                                          fill = especies)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = paste0(prop, "%")), color = "white",
            position = position_stack(vjust = .5), size = 3) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Pie chart", fill = "Espécies")

## Gráfico de setores - donut
g_donut <- ggplot(data = pinguins_prop, aes(x = 2, y = prop,
                                            fill = especies)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(prop, "%")), color = "white",
            position = position_stack(vjust = .5), size = 2) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  coord_polar(theta = "y", start = 0) +
  xlim(0, 2.5) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Donut chart", fill = "Espécies")

## Combinação dos gráfigos
gridExtra::grid.arrange(g_bar_v, g_bar_h, g_pie, g_donut, nrow = 2)


# geom_boxplot() ----
## Gráfico de caixas das coluna comprimento_nadadeira e espécies
ggplot(data = pinguins, aes(y = comprimento_nadadeira, x = especies)) +
  geom_boxplot()

## Destaque dos outliers
ggplot(data = pinguins, aes(y = comprimento_nadadeira, x = especies)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Outliers vermelhos")

## Remoção dos outliers
ggplot(data = pinguins, aes(y = comprimento_nadadeira, x = especies)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Outliers removidos")

## Gráfico com caixa entalhadas - O entalhe representa um intervalo de confiança aproximado de 95% ao redor da mediana. O propósito principal desse entalhe é fornecer um guia visual aproximado para comparar se as medianas de diferentes grupos são estatisticamente diferentes.

## Se os entalhes de dois boxplots não se sobrepõem, há forte evidência de que suas medianas são diferentes.
ggplot(data = pinguins, aes(y = comprimento_nadadeira, x = especies,
                            fill = especies)) +
  geom_boxplot(notch = TRUE) +
  labs(title = "Caixas entalhadas")

## Boxplot com jitters
ggplot(data = pinguins, aes(y = comprimento_nadadeira,
                            x = especies,
                            fill = especies)) +
  geom_boxplot() +
  geom_jitter(size = .5)

# geom_violin() ----
ggplot(data = pinguins, aes(y = comprimento_nadadeira, x = especies,
                            fill = especies)) +
  geom_violin() +
  geom_jitter(size = .5)

## Combinando o gráfico de violino com o de caixas
ggplot(data = pinguins, aes(y = comprimento_nadadeira, x = especies,
                            fill = especies)) +
  geom_violin() +
  geom_boxplot(width = .1, fill = "gray")


## Gráfico de caixas exemplo
ggplot(data = pinguins, aes(x = especies, y = comprimento_nadadeira,
                            fill = especies)) +
  geom_boxplot(width = .5, show.legend = FALSE) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 14) +
  labs(title = "Boxplot sem pontos", x = "Espécies",
       y = "Comprimento da nadadeira (mm)")

## Gráfico de violino exemplo
ggplot(data = pinguins, aes(x = especies, y = comprimento_nadadeira,
                            fill = especies)) +
  geom_violin(width = .5, show.legend = FALSE) +
  geom_jitter(alpha = .4, show.legend = FALSE,
              position = position_jitter(width = .15, seed = 0)) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 14) +
  labs(title = "Gráfico de violino com jitter", x = "Espécies",
       y = "Comprimento da nadadeira (mm)")

## Gráfico de caixas e violino exemplo
ggplot(data = pinguins, aes(x = especies, y = comprimento_nadadeira,
                            fill = especies)) +
  geom_violin(width = .5, show.legend = FALSE) +
  geom_boxplot(width = .3, fill = "gray", show.legend = FALSE) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 14) +
  labs(title = "Gráfico de violino com boxplot", x = "Espécies",
       y = "Comprimento da nadadeira (mm)")

# geom_point() e shape ----

shapes_df <- data.frame(
  id = 0:25
)

# Criar o gráfico de referência
ggplot(shapes_df, aes(x = id %% 6, y = -floor(id / 6))) +
  geom_point(aes(shape = id), size = 6, fill = "red", stroke = 1.5) +
  geom_text(aes(label = id), nudge_y = -0.4, size = 4) +
  scale_shape_identity() + # Usa os valores da coluna 'id' diretamente como shapes
  theme_void() +           # Remove eixos e fundo para um visual mais limpo
  labs(title = "Guia de Shapes do ggplot2 (0-25)")

## Gráfico de dispersão das coluna comprimento_nadadeira e comprimento_bico
ggplot(data = pinguins, aes(x = comprimento_bico,
                            y = comprimento_nadadeira)) +
  geom_point()

## Cor e tamanho dos pontos
ggplot(data = pinguins, aes(x = comprimento_bico,
                            y = comprimento_nadadeira)) +
  geom_point(color = "cyan4", size = 4) +
  labs(title = "Sem transparência")

## Cor, tamanho dos pontos e transparência
ggplot(data = pinguins, aes(x = comprimento_bico,
                            y = comprimento_nadadeira)) +
geom_point(color = "cyan4", size = 4, alpha = .4) +
  labs(title = "Com transparência")

## Formato e tamanho
ggplot(data = pinguins, aes(x = comprimento_bico, 
                            y = comprimento_nadadeira)) +
  geom_point(shape = 1, size = 4)

## Formato e tamanho para espécies
ggplot(data = pinguins, aes(x = comprimento_bico,
                            y = comprimento_nadadeira, color = especies)) +
  geom_point(shape = 19, size = 4)

## Formato e tamanho e cor
ggplot(data = pinguins, aes(x = comprimento_bico,
                            y = comprimento_nadadeira, fill = especies)) +
  geom_point(shape = 21, size = 4, color = "black")

## Linha de ajuste
ggplot(data = pinguins, aes(x = comprimento_bico,
                            y = comprimento_nadadeira)) +
  geom_point(shape = 20, size = 4, color = "black") +
  geom_smooth(method = lm)


## Gráfico de dispersão exemplo
ggplot(data = pinguins, aes(x = comprimento_bico,
                            y = comprimento_nadadeira,
                            color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 16) +
  labs(x = "Comprimento do bico (mm)",
       y = "Comprimento da nadadeira (mm)",
       color = "Espécies", shape = "Espécies")
  

# GGally::ggpairs() ----
# A função ggpairs() do pacote GGally permite criar múltiplos gráficos pareados comparando as variáveis contínuas no seu conjunto de dados. Além de demonstrar gráficos de dispersão de cada par de variáveis, ela apresenta gráficos de densidade de cada variável individualmente.

pinguins |> 
  dplyr::select(massa_corporal, comprimento_bico,
                profundidade_bico, comprimento_nadadeira) |> 
  GGally::ggpairs(aes(color = pinguins$especies)) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw()

pinguins |> 
  dplyr::select(sexo, massa_corporal, comprimento_bico,
                profundidade_bico, comprimento_nadadeira) |> 
  GGally::ggpairs(aes(color = sexo)) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw()

# Legendas ----

## Legenda acima
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "top") +
  labs(title = "Legenda acima do gráfico",
       x = "Comprimento do bico (mm)",
       y = "Profundidade do bico (mm)", color = "Espécies",
       shape = "Espécies")

## Leganda abaixo
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  labs(title = "Legenda abaixo do gráfico",
       x = "Comprimento do bico (mm)", y = "Profundidade do bico (mm)", color = "Espécies",
       shape = "Espécies")
## Sem legenda
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  labs(title = "Sem legenda", x = "Comprimento do bico (mm)",
       y = "Profundidade do bico (mm)", color = "Espécies",
       shape = "Espécies")

## Legenda personalizada
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "right",
        legend.text = element_text(size = 14, colour = "red"),
        legend.title = element_text(face = "bold"),
        legend.box.background = element_rect(color="red", size=2),
        legend.margin = margin(6, 6, 6, 6)) +
  labs(title = "Legenda personalizada", x = "Comprimento do bico (mm)",
       y = "Profundidade do bico (mm)", color = "Espécies",
       shape = "Espécies")

## Legenda interna
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 16) +
  theme(legend.position = c(.2, .8),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(), legend.text = element_text(size = 12, face = "bold")) +
  labs(title = "Legenda interna", x = "Comprimento do bico (mm)",
       y = "Profundidade do bico (mm)", color = "Espécies",
       shape = "Espécies")


# Eixo, fonte, gride ----
## Nome dos eixos
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .4) +
  ylim(170, 230) +
  xlim(30, 60) +
  labs(title = "Nome dos eixos", x = "Eixo X", y = "Eixo Y")

## Intervalo dos eixos
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .4) +
  scale_x_continuous(limits = c(30, 60), breaks = seq(30, 60, 2)) +
  labs(title = "Intervalo dos eixos", x = "Eixo X", y = "Eixo Y")

## Cor e fonte dos eixos
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .4) +
  theme(axis.title.x = element_text(face = "bold", size = 20,
                                    colour = "cyan4"),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(face = "bold", size = 20,
                                    colour = "cyan4"),
        axis.text.y = element_text(size = 14)) +
  labs(title = "Cor e fonte dos eixos", x = "Eixo X", y = "Eixo Y")

## Intervalo e ângulos do texto dos eixos
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .4) +
  scale_x_continuous(limits = c(20, 60), breaks = seq(20, 60, 2)) +
  theme(axis.title.x = element_text(face = "bold", size = 20,
                                    colour = "cyan4"),
        axis.text.x = element_text(size = 14, angle = 45),
        axis.title.y = element_text(face = "bold", size = 20,
                                    colour = "cyan4"),
        axis.text.y = element_text(size = 14)) +
  labs(title = "Intervalo e ângulos do texto dos eixos",
       x = "Eixo X", y = "Eixo Y")

## Linhas de grade principais
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .4) +
  scale_x_continuous(limits = c(30, 60), breaks = seq(30, 60, 5)) +
  theme(axis.title.x = element_text(face = "bold", size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(face = "bold", size = 16),
        axis.text.y = element_text(size = 12),
        panel.grid.minor = element_blank()) +
  labs(title = "Linhas de grade principais", x = "Eixo X",
       y = "Eixo Y")

## Retirar linhas de grade
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .4) +
  scale_x_continuous(limits = c(30, 60), breaks = seq(30, 60, 5)) +
  theme(axis.title.x = element_text(face = "bold", size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(face = "bold", size = 16),
        axis.text.y = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  labs(title = "Retirar linhas de grade", x = "Eixo X", y = "Eixo Y")

## Borda do gráfico
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = 0.5) +
  scale_x_continuous(limits = c(30, 60), breaks = seq(30, 60, 5)) +
  theme(axis.title.x = element_text(face = "bold", size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(face = "bold", size = 16),
        axis.text.y = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(size = 2, colour = "black", fill = NA)) +
  labs(title = "Borda do gráfico", x = "Eixo X", y = "Eixo Y")

## Borda do gráfico
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .4) +
  scale_x_continuous(limits = c(30, 60), breaks = seq(30, 60, 5)) +
  theme(axis.title.x = element_text(face = "bold", size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(face = "bold", size = 16),
        axis.text.y = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(size = 1)) +
  labs(title = "Borda do gráfico", x = "Eixo X", y = "Eixo Y")


# ggtheme() ----
# Existem vários temas criados dentro do universo ggtheme() que podem facilitar a escolha de um modelo com ótima qualidade para publicação.

## theme_gray
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_gray(base_size = 16) +
  labs(title = "theme_gray()", x = "Comprimento do bico (mm)",
       y = "Profundidade do bico (mm)", color = "Espécies",
       shape = "Espécies")

## theme_bw()
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 16) +
  labs(title = "theme_bw()", x = "Comprimento do bico (mm)",
       y = "Profundidade do bico (mm)", color = "Espécies",
       shape = "Espécies")

## theme_classic()
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_classic(base_size = 16) +
  labs(title = "theme_classic()", x = "Comprimento do bico (mm)",
       y = "Profundidade do bico (mm)", color = "Espécies",
       shape = "Espécies")

# theme_custom() ----

## Criar uma função com os ajustes finos
tema_personalizado <- function(){
  # Defina uma fonte
  font <- "Times" # Digite names(pdfFonts()) no console do R para ver a lista de fontes disponíveis
  theme(
    # Defina elementos do gride
    panel.grid.major = element_line(colour = "#d3d3d3"),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = .5),
    # Defina elementos textuais
    # Título
    plot.title = element_text(
      family = font, # Fonte
      size = 20, # Tamanho da fonte
      face = 'bold', # Tipo de fonte
      hjust = 0, # Alinhamento horizontal
      vjust = 2), # Alinhamento vertical
    # Subtítulo
    plot.subtitle = element_text(
      family = font, # Fonte
      size = 14), # Tamanho da fonte
    # Rúbrica
    plot.caption = element_text(
      family = font, # Fonte
      size = 10, # Tamanho da fonte
      hjust = 1), # Alinhamento horizontal
    # Título dos eixos
    axis.title = element_text(
      family = font, # Fonte
      size = 14), # Tamanho da fonte
    # Texto dos eixos
    axis.text = element_text(
      family = font, # Fonte
      size = 14) # Tamanho da fonte
  )}
## Gráfico usando a função de tema criada
ggplot(data = pinguins,
       aes(x = comprimento_bico, y = comprimento_nadadeira,
           color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  tema_personalizado() +
  labs(title = "Tema personalizado", x = "Comprimento do bico (mm)",
       y = "Profundidade do bico (mm)", color = "Espécies",
       shape = "Espécies", caption = "Fonte = palmerpinguins")

# ggplot2::ggsave() ----
## Gráfico
g1 <- ggplot(data = pinguins,
             aes(x = comprimento_bico, y = comprimento_nadadeira,
                 color = especies, shape = especies)) +
  geom_point(size = 4, alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_manual(values = c(19, 15, 17)) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  theme(legend.position = c(.1, .1),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank()) +
  tema_personalizado() +
labs(x = "Comprimento do bico (mm)", y = "Profundidade do bico (mm)",
     color = "Espécies", shape = "Espécies")

## Exportar no formato PDF
ggsave(filename = "g1.pdf",
       plot = g1,
       width = 15,
       height = 15,
       dpi =300,
       units = "cm")

## Exportar no formato PNG
ggsave(filename = "g1.png",
       plot = g1,
       width = 15,
       height = 15,
       dpi =300,
       units = "cm")
