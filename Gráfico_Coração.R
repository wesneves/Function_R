# Carregar a biblioteca necessária
library(ggplot2)

# Definir a sequência de t para os parâmetros
t <- seq(0, 2*pi, length.out = 1000)

# Equações paramétricas para o coração
x <- 16 * sin(t)^3
y <- 13 * cos(t) - 5 * cos(2*t) - 2 * cos(3*t) - cos(4*t)

# Criar um data frame com os valores de x e y
data <- data.frame(x = x, y = y)

# Plotar o gráfico usando ggplot2
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "red") +
  labs(title = "Gráfico de um Coração", x = "x", y = "y") +
  theme_minimal() +
  coord_fixed(ratio = 1) +  # Garantir que o gráfico tenha uma proporção igual
  theme(panel.grid.major = element_line(color = "gray", size = 0.2))
