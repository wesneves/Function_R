# Testar Linearidade ----

# A forma mais poderosa e intuitiva de testar a linearidade é através da inspeção visual, combinando um gráfico de dispersão com diferentes linhas de tendência. O pacote ggplot2 é a ferramenta perfeita para isso.

# A Estratégia: Compare uma Linha Reta com uma Curva Flexível

# Pacotes necessários
library(tidyverse)
library(mgcv) # Para GAM

# Semente para reprodutibilidade
set.seed(42)

# Cenário 1: Dados com relação claramente LINEAR
dados_lineares <- tibble(
  x = 1:100,
  y = 2 * x + rnorm(100, mean = 0, sd = 20) # Relação linear + ruído
)

# Cenário 2: Dados com relação claramente NÃO-LINEAR (curva)
dados_nao_lineares <- tibble(
  x = 1:100,
  y = 50 * sin(x / 20) + (x / 2) + rnorm(100, mean = 0, sd = 10) # Relação senoidal
)

# Dados Lineares ----
ggplot(dados_lineares, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", aes(color = "Modelo Linear (lm)"), se = FALSE) +
  # 'loess' é um bom padrão para exploração visual
  geom_smooth(method = "loess", aes(color = "Modelo Flexível (loess)"), se = FALSE) +
  labs(
    title = "Cenário 1: Relação Linear",
    subtitle = "A linha reta (lm) e a curva (loess) são quase idênticas",
    color = "Tipo de Modelo"
  ) +
  theme_minimal()

# No gráfico, a linha vermelha (loess) e a linha azul (lm) estão praticamente sobrepostas. Isso é um forte indicativo de que a relação entre x e y é, de fato, linear. A linha reta descreve a tendência dos dados tão bem quanto a curva flexível.


# Dados não lineares ----
ggplot(dados_nao_lineares, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", aes(color = "Modelo Linear (lm)"), se = FALSE) +
  geom_smooth(method = "loess", aes(color = "Modelo Flexível (loess)"), se = FALSE) +
  
  labs(
    title = "Cenário 2: Relação Não-Linear",
    subtitle = "A curva (loess) descreve os dados muito melhor que a reta (lm)",
    color = "Tipo de Modelo"
  ) +
  theme_minimal()
