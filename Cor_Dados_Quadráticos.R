# Dados com relação em "U" perfeita (não-linear)
x_quad <- -10:10
y_quad <- x_quad^2

# Visualização
plot(x_quad, y_quad)

# Teste de correlação de Pearson
cor.test(x_quad, y_quad, method = "pearson")


# Usando os dados não-lineares do exemplo anterior

# 1. Ajuste o modelo linear
modelo_lm <- lm(y ~ x, data = dados_nao_lineares)

# 2. Ajuste o modelo aditivo (GAM)
modelo_gam <- gam(y ~ s(x), data = dados_nao_lineares)

# 3. Compare os dois modelos com um teste de análise de variância (ANOVA)
# H₀: O modelo mais simples (lm) é suficiente.
# Hₐ: O modelo mais complexo (gam) é significativamente melhor.
anova(modelo_lm, modelo_gam, test = "F")
