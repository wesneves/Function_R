# Certifique-se de que os pacotes necessários estão instalados

library(vegan)
library(lmtest)

#' @title Sugere um modelo estatístico para dados ecológicos
#' @description Avalia um conjunto de dados e sugere o modelo mais apropriado
#'     baseado no número e tipo de variáveis resposta e preditoras.
#' @param df O seu dataframe.
#' @param respostas Um vetor de strings com os nomes das suas variáveis resposta.
#' @param preditores Um vetor de strings com os nomes das suas variáveis preditoras.
#' @param efeito_aleatorio Uma string opcional com o nome da coluna do efeito aleatório.
#' @return Mensagens no console sugerindo o modelo estatístico.

qual_modelo <- function(df, respostas, preditores, efeito_aleatorio = NULL) {
  
  cat("--- Iniciando Análise Sugestiva de Modelo ---\n\n")
  
  # --- Passo 1: Checagem Multivariada vs. Univariada ---
  if (length(respostas) > 1) {
    message("✅ Detectado: Análise Multivariada (", length(respostas), " respostas).")
    message("➡️ Sugestão: Use PERMANOVA (com a função 'adonis2' do pacote 'vegan').")
    message("   Lembre-se de checar a homogeneidade da dispersão multivariada com 'betadisper'.\n")
    
    # Exemplo de como rodar (apenas para ilustração)
    cat("   Exemplo de código:\n")
    cat("   distancias <- vegdist(df[, respostas], method = 'bray')\n")
    cat("   permanova_resultado <- adonis2(distancias ~ ", paste(preditores, collapse = " + "), ", data = df)\n")
    cat("   print(permanova_resultado)\n")
    
    return(invisible(NULL)) # Encerra a função aqui
  }
  
  # --- Passo 2: Análise Univariada floor arredonda os valores ---
  message("✅ Detectado: Análise Univariada (resposta: '", respostas, "').")
  resposta_vec <- df[[respostas]]
  is_continuous <- is.numeric(resposta_vec) && !all(floor(resposta_vec) == resposta_vec)
  is_count <- all(floor(resposta_vec) == resposta_vec) && all(resposta_vec >= 0)
  is_binary <- all(resposta_vec %in% c(0, 1))
  
  # --- Passo 3: Avaliar tipo de variável resposta ---
  if (is_count) {
    message("ℹ️ Info: Sua resposta '", respostas, "' parece ser de CONTAGEM (números inteiros).")
    message("➡️ Sugestão: Modelos Lineares Generalizados (GLM/GLMM) com família 'poisson' ou 'quasipoisson'.")
  } else if (is_binary) {
    message("ℹ️ Info: Sua resposta '", respostas, "' parece ser BINÁRIA (0/1).")
    message("➡️ Sugestão: Modelos Lineares Generalizados (GLM/GLMM) com família 'binomial'.")
  } else if (is_continuous) {
    message("ℹ️ Info: Sua resposta '", respostas, "' parece ser CONTÍNUA.")
    message("➡️ Sugestão: Testar pressupostos para modelos lineares (LM/LMM) ou usar modelos aditivos (GAM).")
  } else {
    warning("AVISO: Não foi possível determinar o tipo da variável resposta. Verifique seus dados.")
    return(invisible(NULL))
  }
  
  cat("\n--- Checando Pressupostos para Dados Contínuos ---\n")
  # Construir uma fórmula simples para o teste
  formula_teste <- as.formula(paste(respostas, "~", preditores[1]))
  modelo_teste <- lm(formula_teste, data = df)
  
  # Checar normalidade dos resíduos
  shapiro_test <- shapiro.test(residuals(modelo_teste))
  if (shapiro_test$p.value < 0.05) {
    message("❌ Pressuposto Falhou: Os resíduos NÃO são normalmente distribuídos (p < 0.05).\n")
    message("   Considere usar GLM (se souber a distribuição) ou GAM (se a relação for não-linear).\n")
  } else {
    message("✅ Pressuposto Atendido: Os resíduos parecem ter distribuição normal.")
  }
  
  # Checar homocedasticidade
  # (performance::check_model imprime o resultado do teste de Breusch-Pagan) - A homocedasticidade é a pressuposição de que a variância dos resíduos é constante ao longo dos valores preditos. Se os resíduos formam um "funil", a variância não é constante (heterocedasticidade).
  
  Breusch_Pagantes_check <- lmtest::bptest(modelo_teste)
  if (Breusch_Pagantes_check$p.value < 0.05) {
    message("❌ Pressuposto Falhou: Os resíduos não têm variância constante (heterocedasticidade) - (p < 0.05). \n")
    message("   Considere transformar sua variável resposta (modelo_log <- lm(log(y) ~ x1 + x2, data = dados)) ou usar um modelo que aceite variância não constante - GLM (se souber a distribuição) Para contagens ou dados com overdispersion: glm(y ~ x1 + x2, family = quasipoisson, data = dados) \n
Ou usar GLS com estrutura de variância (pacote nlme) se quiser modelar diretamente a heterocedasticidade.).")
  } else {
    message("✅ Pressuposto Atendido: Os resíduos têm variância constante (homocedasticidade) - (p > 0.05).")
  }
  
  cat("\n--- Sugestão Final com Base nos Efeitos ---\n")
  
  if (!is.null(efeito_aleatorio)) {
    message("✅ Efeito Aleatório ('", efeito_aleatorio, "') foi fornecido.")
    message("➡️ Sugestão Principal: Modelo Misto (GLMM ou LMM).")
    message("   - Se os resíduos não foram normais: GLMM (ex: `glmer` do pacote 'lme4').")
    message("   - Se os resíduos foram normais: LMM (ex: `lmer` do pacote 'lme4').\n")
    cat("   Exemplo GLMM: glmer(", respostas, " ~ ", paste(preditores, collapse = " + "), " + (1|", efeito_aleatorio, "), data = df, family = ...)\n")
    
  } else {
    message("ℹ️ Nenhum efeito aleatório foi fornecido.")
    message("➡️ Sugestão Principal: Modelo Padrão (GLM, LM ou GAM).")
    message("   - Se os pressupostos lineares falharam: GAM (ex: `gam` do pacote 'mgcv') é uma ótima alternativa.")
    message("   - Se os dados são de contagem/binários: GLM (ex: `glm`).")
    message("   - Se os pressupostos lineares foram atendidos: LM/ANOVA/ANCOVA (ex: `lm`).\n")
    cat("   Exemplo GAM: gam(", respostas, " ~ s(", preditores[1], ") + ..., data = df, family = ...)\n")
  }
  
  cat("\n--- Fim da Análise ---\n")
  cat("Lembre-se: esta é uma sugestão automática. Sempre explore seus dados visualmente e valide os pressupostos do modelo final escolhido!\n")
}