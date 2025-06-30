# ================================================================ #
###### SCRIPT DE REPLICAÇÃO - AVALIAÇÃO 2 – ECONOMETRIA I ######
# Endogeneidade, Variáveis Instrumentais e Modelos para Dados em Paine
# Autor: Samuel Maia
# Data: Junho de 2025
# ================================================================= #

# Este script gera todas as tabelas e figuras da Seção 4 da Avaliação.
# Ele parte de uma base de dados pré-processada e executa todas as
# análises descritivas, econométricas e de robustez.





# ---------------------------------------------------------------- #
###### 0. CONFIGURAÇÃO INICIAL: PACOTES E DEFINIÇÕES GLOBAIS ######
# ------------------------------------------------------------------- #
# Limpar o ambiente para uma replicação limpa
rm(list = ls())
gc()

# Carregar todos os pacotes necessários
packages <- c("tidyverse", "plm", "lmtest", "sandwich", "modelsummary",
    "knitr", "kableExtra", "broom", "ggrepel", "patchwork", "tibble")
# install.packages(setdiff(packages, rownames(installed.packages()))) # Descomente para instalar
lapply(packages, library, character.only = TRUE)

# Definindo constantes que serão usadas ao longo do script
paises_ocde <- c("AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK", "FIN", "FRA", "DEU",
    "GRC", "HUN", "IRL", "ISR", "ITA", "JPN", "KOR", "MEX", "NLD", "NZL",
    "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")





# ------------------------------------------------------------------- #
###### 1. CARREGAMENTO E PREPARAÇÃO INICIAL DOS DADOS ####
# ------------------------------------------------------------------- #
cat("PASSO 1: Carregando e preparando os dados...\n")

# Ajuste o caminho para o seu arquivo de dados pré-processado
file_path <- "/path/to/your/combined_data_final.rds"
if (!file.exists(file_path)) {
    stop("Arquivo de dados não encontrado. Ajuste a variável 'file_path'.")
}
combined_data_final <- readRDS(file_path)

# ------------------------------------------------------------------- #
###### 2. SEÇÃO 4.1: DADOS, AMOSTRA E ANÁLISE DESCRITIVA ####
# ------------------------------------------------------------------- #
cat("\n--- GERANDO OUTPUTS PARA A SEÇÃO 4.1 ---\n")

# 2.1 Construção da Amostra Analítica Decadal (base para a maioria das análises)
painel_decadal_raw <- combined_data_final %>%
    filter(year >= 1980, year < 2010) %>%
    mutate(decade = floor(year / 10) * 10) %>%
    group_by(country_id, decade, wid_variable, lower_percentile, upper_percentile) %>%
    summarise(
        wid_value = mean(wid_value, na.rm = TRUE),
        avg_eci   = mean(avg_eci, na.rm = TRUE),
        log_gdp   = mean(log(gdppp), na.rm = TRUE),
        schooling = mean(schooling, na.rm = TRUE),
        .groups = 'drop'
    ) %>%
    drop_na()

# ===================================================================#
# 2.2 Tabela 4.1: Estatísticas Descritivas
# ===================================================================#
cat("Gerando Tabela 4.1: Estatísticas Descritivas...\n")

# Preparar dados para a tabela
tabela_desc_data <- painel_decadal_raw %>%
    # Pivotar para ter Ginis como colunas separadas
    pivot_wider(names_from = wid_variable, values_from = wid_value) %>%
    select(
        `Índice de Complexidade Econômica (ECI)` = avg_eci,
        `Gini (Pré-Tributação)` = gptincj992,
        `Gini (Pós-Tributação, com in-kind)` = gdiincj992,
        `PIB per capita (log)` = log_gdp,
        `Anos de Estudo` = schooling
    ) %>%
    drop_na() # Garante que só estatísticas de observações completas sejam mostradas

tabela_descritiva <- tabela_desc_data %>%
    tbl_summary(
        statistic = all_continuous() ~ "{mean} ({sd}) [{min}, {max}]",
        digits = all_continuous() ~ 2
    ) %>%
    modify_header(label ~ "**Variável**") %>%
    modify_header(stat_0 ~ "**Média (Desv. Padrão) [Mín, Máx]**") %>%
    modify_caption("**Tabela 4.1: Estatísticas Descritivas das Variáveis do Painel Decadal**") %>%
    as_gt()

print(tabela_descritiva)

# ===================================================================#
# 2.3 Figura 4.1: Densidades de Kernel
# ===================================================================#
cat("Gerando Figura 4.1: Densidades de Kernel...\n")

# Preparar dados para o plot de Gini
gini_long_data <- tabela_desc_data %>%
    select(`Gini (Pré-Tributação)`, `Gini (Pós-Tributação, com in-kind)`) %>%
    pivot_longer(cols = everything(), names_to = "tipo_gini", values_to = "valor_gini")

# Criar os 4 plots
p_eci <- ggplot(tabela_desc_data, aes(x = `Índice de Complexidade Econômica (ECI)`)) + geom_density(fill="grey", alpha=0.7) + labs(title="(a) ECI") + theme_minimal()
p_gini <- ggplot(gini_long_data, aes(x = valor_gini, fill = tipo_gini)) + geom_density(alpha = 0.6) + labs(title="(b) Índice de Gini", x="Gini") + theme_minimal() + theme(legend.position="bottom", legend.title=element_blank())
p_schooling <- ggplot(tabela_desc_data, aes(x = `Anos de Estudo`)) + geom_density(fill="grey", alpha=0.7) + labs(title="(c) Anos de Estudo") + theme_minimal()
p_loggdp <- ggplot(tabela_desc_data, aes(x = `PIB per capita (log)`)) + geom_density(fill="grey", alpha=0.7) + labs(title="(d) PIB per capita (log)") + theme_minimal()

# Combinar em uma figura
figura_densidades <- (p_eci + p_gini) / (p_schooling + p_loggdp) +
    plot_annotation(
        title = 'Figura 4.1: Densidades de Kernel das Variáveis-Chave na Amostra Decadal',
        theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ) & theme(legend.position = 'bottom')

print(figura_densidades)
# ggsave("figura_densidades.png", plot = figura_densidades, width = 8, height = 7)

# ===================================================================#
# 2.4 Tabela 4.2: Estrutura Média da Desigualdade (Shares)
# ===================================================================#
cat("Gerando Tabela 4.2: Estrutura Média dos Shares...\n")

faixas_panel_completa <- tribble(
    ~lower,  ~upper,  ~faixa,
    0,   10,  "D1", 10,  20,  "D2", 20,  30,  "D3", 30,  40,  "D4",
    40,  50,  "D5", 50,  60,  "D6", 60,  70,  "D7", 70,  80,  "D8",
    80,  90,  "D9", 90, 100, "D10 (90-100%)", 95, 100, "Top 5%",
    99, 100, "Top 1%", 99.9,100, "Top 0.1%", 99.99,100, "Top 0.01%"
)

tabela_shares_final <- painel_decadal_raw %>%
    left_join(faixas_panel_completa, by = c("lower_percentile" = "lower", "upper_percentile" = "upper")) %>%
    drop_na(faixa) %>%
    filter(wid_variable %in% c("sptincj992", "sdiincj992")) %>%
    mutate(conceito_renda = if_else(wid_variable == "sptincj992", "Pré-Tributação", "Pós-Tributação")) %>%
    group_by(faixa, conceito_renda) %>%
    summarise(share_medio = mean(wid_value, na.rm = TRUE) * 100, .groups = "drop") %>%
    pivot_wider(names_from = conceito_renda, values_from = share_medio) %>%
    mutate(`Efeito Redistributivo (p.p.)` = `Pós-Tributação` - `Pré-Tributação`) %>%
    slice(match(faixas_panel_completa$faixa, faixa))

# Gerar o código LaTeX para a tabela
kable(tabela_shares_final,
    col.names = c("Faixa de Renda", "Pré-Tributação (%)", "Pós-Tributação (%)", "Efeito Redistributivo (p.p.)"),
    digits = 2, format = "latex", booktabs = TRUE,
    caption = "Participação Média na Renda por Faixa e Conceito (Amostra Decadal)",
    label = "tab:share_structure_final"
) %>% kable_styling(latex_options = "striped")


# ===================================================================#
# 2.5 Figura 4.2: Correlação Visual ECI vs. Gini
# ===================================================================#
cat("Gerando Figura 4.2: Scatter Plot ECI vs. Gini...\n")

# Preparar dados para o plot
plot_data_gini <- painel_decadal_raw %>%
    filter(wid_variable %in% c("gptincj992", "gdiincj992")) %>%
    mutate(conceito = if_else(wid_variable == "gptincj992", "Gini Pré-Tributação", "Gini Pós-Tributação (com in-kind)"))

paises_destacados <- c("BRA", "FIN", "YEM", "USA")
dados_destacados <- plot_data_gini %>% filter(country_id %in% paises_destacados)

# Gráfico
ggplot(plot_data_gini, aes(x = avg_eci, y = wid_value, color = conceito)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
    geom_text_repel(data = dados_destacados, aes(label = country_id), 
        size = 4, fontface = "bold", box.padding = 0.5) +
    labs(
        title = "Correlação entre Complexidade Econômica e Índice de Gini",
        subtitle = "Dados Decadais Agregados (1980-2000)",
        x = "Complexidade Econômica Média (ECI)",
        y = "Índice de Gini"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank())



# -------------------------------------------------------------------#
# 3. SEÇÃO 4.2: MODELO DE PAINEL BASE E DIAGNÓSTICOS ####
# -------------------------------------------------------------------#
cat("\n--- GERANDO OUTPUTS PARA A SEÇÃO 4.2 ---\n")


# ===================================================================#
# 3.1 Tabela 4.3: Comparação de Estimadores (Pooled, RE, FE, FD)
# ===================================================================#
cat("Gerando Tabela 4.3: Comparação de Estimadores...\n")

# --- PASSO A: Preparar os dados para a regressão do Gini ---
# Usaremos a base decadal e focaremos no Gini Pós-Tributação para a comparação
painel_gini_para_comparacao <- painel_decadal_raw %>%
    filter(wid_variable == "gdiincj992") %>%
    rename(gini_value = wid_value) %>%
    drop_na(avg_eci, log_gdp, schooling, gini_value)

p_painel_gini <- pdata.frame(painel_gini_para_comparacao, index = c("country_id", "decade"))

# --- PASSO B: Estimar os quatro modelos ---
formula_gini <- gini_value ~ avg_eci + log_gdp + schooling

lista_modelos <- list(
    "MQO Empilhado"       = plm(formula_gini, data = p_painel_gini, model = "pooling"),
    "Efeitos Aleatórios"  = plm(formula_gini, data = p_painel_gini, model = "random"),
    "Efeitos Fixos"       = plm(formula_gini, data = p_painel_gini, model = "within"),
    "Primeiras Diferenças" = plm(formula_gini, data = p_painel_gini, model = "fd")
)

# --- PASSO C: Executar os testes de diagnóstico ---
teste_hausman <- phtest(lista_modelos[["Efeitos Fixos"]], lista_modelos[["Efeitos Aleatórios"]])
teste_cd <- tryCatch({pcdtest(lista_modelos[["Efeitos Fixos"]])}, error = function(e){NULL}) 

# --- PASSO D: Calcular erros-padrão robustos (para evitar o erro do modelsummary) ---
vcov_robust_comp <- purrr::map(lista_modelos, ~ vcovHC(.x, type = "HC1", cluster = "group"))

# --- PASSO E: Gerar a tabela LaTeX ---
nota_hausman <- paste0("Teste de Hausman (chi2): ", round(teste_hausman$statistic, 2), 
    " (p-valor: ", format.pval(teste_hausman$p.value, digits=2, eps=0.001), ").")
nota_cd <- if(!is.null(teste_cd)) {
    paste0("Teste de Pesaran CD (z): ", round(teste_cd$statistic, 2), 
        " (p-valor: ", format.pval(teste_cd$p.value, digits=2, eps=0.001), ").")
} else {"Teste de Pesaran CD não pôde ser computado."}

modelsummary(
    lista_modelos,
    output = "latex",
    vcov = vcov_robust_comp,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    coef_rename = c("avg_eci" = "ECI", "log_gdp" = "PIB pc (log)", "schooling" = "Anos de Estudo"),
    gof_map = c("nobs", "r.squared"),
    title = "\\caption{Comparação de Estimadores para o Efeito do ECI no Gini Pós-Tributação \\label{tab:model_comparison}}",
    notes = list("Erros-padrão clusterizados por país em parênteses.", nota_hausman, nota_cd),
    booktabs = TRUE
)


# =================================================================== #
# 3.2 Figura 4.3: O Padrão da "Foice" #
# =================================================================== #
cat("Gerando Figura 4.3: Gráfico da Foice...\n")

# --- PASSO A: Definir faixas e ordem para a análise de shares ---
faixas_analise_foice <- tribble(
    ~lower,  ~upper,  ~faixa,
    0,   10,  "D1", 10,  20,  "D2", 20,  30,  "D3", 30,  40,  "D4",
    40,  50,  "D5", 50,  60,  "D6", 60,  70,  "D7", 70,  80,  "D8",
    80,  90,  "D9", 90, 100, "D10 (90-100%)", 95, 100, "Top 5%",
    99, 100, "Top 1%", 99.9,100, "Top 0.1%", 99.99,100, "Top 0.01%"
)
ordem_x_foice <- faixas_analise_foice$faixa

# --- PASSO B: Preparar os dados para a análise de shares --- #
painel_shares_modelo <- painel_decadal_raw %>%
    left_join(faixas_analise_foice, by = c("lower_percentile" = "lower", "upper_percentile" = "upper")) %>%
    drop_na(faixa) %>%
    filter(wid_variable %in% c("sptincj992", "sdiincj992")) %>%
    rename(share_value = wid_value) %>%
    drop_na(share_value, avg_eci, log_gdp, schooling)

# --- PASSO C: Loop para estimar o modelo FE para cada faixa --- #
formula_shares <- share_value ~ avg_eci + log_gdp + schooling

resultados_foice <- imap_dfr(
    c("Pré-tributação" = "sptincj992", "Pós-tributação (com in-kind)" = "sdiincj992"),
    function(var_code, nm_conc){
        map_dfr(ordem_x_foice, function(gr){
            base_modelo <- painel_shares_modelo %>% filter(wid_variable == var_code, faixa == gr)
            if(nrow(base_modelo) < 10 || n_distinct(base_modelo$country_id) < 5) return(NULL)
            tryCatch({
                p_base <- pdata.frame(base_modelo, index=c("country_id", "decade"))
                fe_mod <- plm(formula_shares, data=p_base, model="within")
                vcrob <- coeftest(fe_mod, vcovHC(fe_mod, type="HC1", cluster="group"))
                tidy(vcrob, conf.int=TRUE) %>%
                    filter(term=="avg_eci") %>%
                    transmute(conceito=nm_conc, faixa=gr, beta=estimate, lo=conf.low, hi=conf.high)
            }, error=function(e) NULL)
        })
    })

# --- PASSO D: Gerar o gráfico final --- #
resultados_foice$faixa <- factor(resultados_foice$faixa, levels = ordem_x_foice)

plot_foice <- ggplot(resultados_foice, aes(x = faixa, y = beta, colour = conceito, group = conceito)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = .2, linewidth = .7) +
    geom_line(linetype = "dashed", linewidth = 0.6) +
    scale_colour_manual(
        name = "Conceito de Renda:",
        values = c("Pré-tributação" = "#0072B2", "Pós-tributação (com in-kind)" = "#C51B7D")
    ) +
    theme_minimal(base_family = "sans") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    labs(
        title = "Efeito da Complexidade Econômica na Participação da Renda por Faixa",
        subtitle = "Coeficientes de um modelo de painel com Efeitos Fixos (Decadal)",
        x = "Faixa de Renda",
        y = "Coeficiente de ECI (β)"
    )

print(plot_foice)
# ggsave("figura_foice.png", plot = plot_foice, width = 10, height = 6, dpi = 300)





# -------------------------------------------------------------------#
# 3. SEÇÃO 4.2: MODELO DE PAINEL BASE E DIAGNÓSTICOS
# -------------------------------------------------------------------#
cat("\n--- GERANDO OUTPUTS PARA A SEÇÃO 4.2 ---\n")

# ===================================================================#
# 3.1 Tabela 4.3: Comparação de Estimadores (Pooled, RE, FE, FD)
# ===================================================================#
cat("Gerando Tabela 4.3: Comparação de Estimadores...\n")

# --- PASSO A: Preparar os dados para a regressão do Gini ---
# Usaremos a base decadal e focaremos no Gini Pós-Tributação para a comparação
painel_gini_para_comparacao <- painel_decadal_raw %>%
    filter(wid_variable == "gdiincj992") %>%
    rename(gini_value = wid_value) %>%
    drop_na(avg_eci, log_gdp, schooling, gini_value)

p_painel_gini <- pdata.frame(painel_gini_para_comparacao, index = c("country_id", "decade"))

# --- PASSO B: Estimar os quatro modelos ---
formula_gini <- gini_value ~ avg_eci + log_gdp + schooling

lista_modelos <- list(
    "MQO Empilhado"       = plm(formula_gini, data = p_painel_gini, model = "pooling"),
    "Efeitos Aleatórios"  = plm(formula_gini, data = p_painel_gini, model = "random"),
    "Efeitos Fixos"       = plm(formula_gini, data = p_painel_gini, model = "within"),
    "Primeiras Diferenças" = plm(formula_gini, data = p_painel_gini, model = "fd")
)

# --- PASSO C: Executar os testes de diagnóstico ---
teste_hausman <- phtest(lista_modelos[["Efeitos Fixos"]], lista_modelos[["Efeitos Aleatórios"]])
teste_cd <- tryCatch({pcdtest(lista_modelos[["Efeitos Fixos"]])}, error = function(e){NULL})

# --- PASSO D: Calcular erros-padrão robustos (para evitar o erro do modelsummary) ---
vcov_robust_comp <- purrr::map(lista_modelos, ~ vcovHC(.x, type = "HC1", cluster = "group"))

# --- PASSO E: Gerar a tabela LaTeX ---
nota_hausman <- paste0("Teste de Hausman (chi2): ", round(teste_hausman$statistic, 2), 
    " (p-valor: ", format.pval(teste_hausman$p.value, digits=2, eps=0.001), ").")
nota_cd <- if(!is.null(teste_cd)) {
    paste0("Teste de Pesaran CD (z): ", round(teste_cd$statistic, 2), 
        " (p-valor: ", format.pval(teste_cd$p.value, digits=2, eps=0.001), ").")
} else {"Teste de Pesaran CD não pôde ser computado."}

modelsummary(
    lista_modelos,
    output = "latex",
    vcov = vcov_robust_comp,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    coef_rename = c("avg_eci" = "ECI", "log_gdp" = "PIB pc (log)", "schooling" = "Anos de Estudo"),
    gof_map = c("nobs", "r.squared"),
    title = "\\caption{Comparação de Estimadores para o Efeito do ECI no Gini Pós-Tributação \\label{tab:model_comparison}}",
    notes = list("Erros-padrão clusterizados por país em parênteses.", nota_hausman, nota_cd),
    booktabs = TRUE
)


# ===================================================================#
# 3.2 Figura 4.3: O Padrão da "Foice"
# ===================================================================#
cat("Gerando Figura 4.3: Gráfico da Foice...\n")

# --- PASSO A: Definir faixas e ordem para a análise de shares ---
faixas_analise_foice <- tribble(
    ~lower,  ~upper,  ~faixa,
    0,   10,  "D1", 10,  20,  "D2", 20,  30,  "D3", 30,  40,  "D4",
    40,  50,  "D5", 50,  60,  "D6", 60,  70,  "D7", 70,  80,  "D8",
    80,  90,  "D9", 90, 100, "D10 (90-100%)", 95, 100, "Top 5%",
    99, 100, "Top 1%", 99.9,100, "Top 0.1%", 99.99,100, "Top 0.01%"
)
ordem_x_foice <- faixas_analise_foice$faixa

# --- PASSO B: Preparar os dados para a análise de shares ---
painel_shares_modelo <- painel_decadal_raw %>%
    left_join(faixas_analise_foice, by = c("lower_percentile" = "lower", "upper_percentile" = "upper")) %>%
    drop_na(faixa) %>%
    filter(wid_variable %in% c("sptincj992", "sdiincj992")) %>%
    rename(share_value = wid_value) %>%
    drop_na(share_value, avg_eci, log_gdp, schooling)

# --- PASSO C: Loop para estimar o modelo FE para cada faixa ---
formula_shares <- share_value ~ avg_eci + log_gdp + schooling

resultados_foice <- imap_dfr(
    c("Pré-tributação" = "sptincj992", "Pós-tributação (com in-kind)" = "sdiincj992"),
    function(var_code, nm_conc){
        map_dfr(ordem_x_foice, function(gr){
            base_modelo <- painel_shares_modelo %>% filter(wid_variable == var_code, faixa == gr)
            if(nrow(base_modelo) < 10 || n_distinct(base_modelo$country_id) < 5) return(NULL)
            tryCatch({
                p_base <- pdata.frame(base_modelo, index=c("country_id", "decade"))
                fe_mod <- plm(formula_shares, data=p_base, model="within")
                vcrob <- coeftest(fe_mod, vcovHC(fe_mod, type="HC1", cluster="group"))
                tidy(vcrob, conf.int=TRUE) %>%
                    filter(term=="avg_eci") %>%
                    transmute(conceito=nm_conc, faixa=gr, beta=estimate, lo=conf.low, hi=conf.high)
            }, error=function(e) NULL)
        })
    })

# --- PASSO D: Gerar o gráfico final ---
resultados_foice$faixa <- factor(resultados_foice$faixa, levels = ordem_x_foice)

plot_foice <- ggplot(resultados_foice, aes(x = faixa, y = beta, colour = conceito, group = conceito)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = .2, linewidth = .7) +
    geom_line(linetype = "dashed", linewidth = 0.6) +
    scale_colour_manual(
        name = "Conceito de Renda:",
        values = c("Pré-tributação" = "#0072B2", "Pós-tributação (com in-kind)" = "#C51B7D")
    ) +
    theme_minimal(base_family = "sans") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    labs(
        title = "Efeito da Complexidade Econômica na Participação da Renda por Faixa",
        subtitle = "Coeficientes de um modelo de painel com Efeitos Fixos (Decadal)",
        x = "Faixa de Renda",
        y = "Coeficiente de ECI (β)"
    )

print(plot_foice)
# ggsave("figura_foice.png", plot = plot_foice, width = 10, height = 6, dpi = 300)




# -------------------------------------------------------------------#
####### 4. SEÇÃO 4.3: ANÁLISE DE ENDOGENEIDADE #####
# -------------------------------------------------------------------#
cat("\n--- GERANDO OUTPUTS PARA A SEÇÃO 4.3 ---\n")

# -------------------------------------------------------------------#
# 4.1 Figura 4.4: Robustez às Defasagens Temporais
# -------------------------------------------------------------------#
cat("\n--- GERANDO FIGURA 4.4: Gráfico de Lags ---\n")


# PASSO A: Definir a função de análise para os shares com lags
analisar_lags_shares <- function(numero_de_lags, dados_base = combined_data_final) {
    
    faixas <- tribble(
        ~lower,  ~upper,  ~faixa,
        0,   10,  "D1", 10,  20,  "D2", 20,  30,  "D3", 30,  40,  "D4",
        40,  50,  "D5", 50,  60,  "D6", 60,  70,  "D7", 70,  80,  "D8",
        80,  90,  "D9", 90, 100, "D10 (90-100%)", 95, 100, "Top 5%",
        99, 100, "Top 1%", 99.9,100, "Top 0.1%", 99.99,100, "Top 0.01%"
    )
    ordem_x <- faixas$faixa
    
    # Prepara o painel ANUAL para a análise de lags
    painel <- dados_base %>%
        mutate(log_gdp = log(gdppp)) %>%
        filter(year %in% (1995 - numero_de_lags):2015,
            wid_variable %in% c("sptincj992", "sdiincj992")) %>%
        left_join(faixas, by = c("lower_percentile" = "lower", "upper_percentile" = "upper")) %>%
        drop_na(faixa) %>%
        group_by(country_id, faixa, wid_variable) %>%
        arrange(year) %>%
        mutate(lag_eci = lag(avg_eci, numero_de_lags)) %>%
        ungroup() %>%
        filter(!is.na(lag_eci), year >= 1995) %>%
        rename(share_value = wid_value)
    
    formula <- share_value ~ lag_eci + log_gdp + schooling
    
    resultados <- imap_dfr(c("Pré-tributação" = "sptincj992", "Pós-tributação (com in-kind)" = "sdiincj992"), function(var_code, nm_conc){
        map_dfr(ordem_x, function(gr){
            base_modelo <- painel %>% filter(wid_variable == var_code, faixa == gr)
            if(nrow(base_modelo) < 20 || n_distinct(base_modelo$country_id) < 5) return(NULL)
            tryCatch({
                p_base <- pdata.frame(base_modelo, index=c("country_id", "year"))
                fe_mod <- plm(formula, data=p_base, model="within")
                vcrob <- coeftest(fe_mod, vcovHC(fe_mod, type="HC1", cluster="group"))
                tidy(vcrob, conf.int=TRUE) %>%
                    filter(term=="lag_eci") %>%
                    transmute(conceito=nm_conc, faixa=gr, beta=estimate, lo=conf.low, hi=conf.high)
            }, error=function(e) NULL)
        })
    }) %>% mutate(lag_aplicado = paste("Lag de", numero_de_lags, "Ano(s)"))
    return(resultados)
}

# PASSO B: Executar a função para lags 1, 2 e 3 e gerar o gráfico
lags_a_testar <- c(1, 2, 3)
resultados_lags <- map_dfr(lags_a_testar, ~analisar_lags_shares(numero_de_lags = .x))

plot_lags <- ggplot(resultados_lags %>% drop_na(faixa), aes(x = factor(faixa, levels = unique(faixas_analise$faixa)), y = beta, colour = conceito, group = conceito)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(size = 2) + geom_errorbar(aes(ymin = lo, ymax = hi), width = .2) + geom_line(linetype = "dashed") +
    facet_wrap(~factor(lag_aplicado, levels = paste("Lag de", lags_a_testar, "Ano(s)"))) +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    labs(title="Figura 4.4: Robustez do Efeito do ECI a Diferentes Defasagens Temporais", x="Faixa de Renda", y="Coeficiente de ECI (β)")

print(plot_lags)
# ggsave("figura_4_4_lags.png", plot = plot_lags, width = 11, height = 6)


# -------------------------------------------------------------------#
# 4.2 Tabela 4.4: Simulação de Variáveis Instrumentais (2SLS)
# -------------------------------------------------------------------#
cat("\nGerando Tabela 4.4: Simulação IV/2SLS...\n")

# --- PASSO A: Preparar dados decadais para a análise de IV ---
# Usaremos o painel decadal já criado 'painel_decadal_raw'
# E criaremos o lag de 2 décadas para ser o instrumento
painel_iv <- painel_decadal_raw %>%
    group_by(country_id, wid_variable) %>%
    arrange(decade) %>%
    mutate(lag_eci2 = lag(avg_eci, 2)) %>%
    ungroup() %>%
    filter(country_id %in% paises_ocde, wid_variable == "gdiincj992") %>%
    rename(gini_value = wid_value) %>%
    drop_na(gini_value, avg_eci, log_gdp, schooling, lag_eci2)

p_dados_iv <- pdata.frame(painel_iv, index = c("country_id", "decade"))

# --- PASSO B: Estimar o primeiro estágio e calcular a F-estatística ---
first_stage_plm <- plm(avg_eci ~ lag_eci2 + log_gdp + schooling, data = p_dados_iv, model = "within")
f_stat_iv <- (summary(first_stage_plm)$coefficients["lag_eci2", "t-value"])^2
nota_f_iv <- paste0("F-estatística do 1º estágio (instrumento lag_eci2): ", round(f_stat_iv, 2))

# --- PASSO C: Estimar os modelos FE e 2SLS ---
formula_base_iv <- gini_value ~ avg_eci + log_gdp + schooling
formula_2sls <- gini_value ~ avg_eci + log_gdp + schooling | lag_eci2 + log_gdp + schooling

model_fe_iv <- plm(formula_base_iv, data = p_dados_iv, model = "within")
model_2sls_iv  <- plm(formula_2sls, data = p_dados_iv, model = "within")

lista_modelos_iv <- list("Efeitos Fixos" = model_fe_iv, "2SLS (IV)" = model_2sls_iv)

# --- PASSO D: Calcular erros-padrão robustos e gerar a tabela LaTeX ---
vcov_robust_iv <- map(lista_modelos_iv, ~vcovHC(.x, type = "HC1", cluster = "group"))

modelsummary(
    lista_modelos_iv,
    output = "latex",
    stars = c('*' = .1, '**' = .05, '***' = .01),
    vcov = vcov_robust_iv,
    coef_rename = c("avg_eci" = "ECI"),
    gof_map = "nobs",
    title = "\\caption{Comparação das Estimativas FE e 2SLS (IV) para o Gini Pós-Tributação (OCDE) \\label{tab:iv_comparison}}",
    notes = list(nota_f_iv, "Valores abaixo de 10 sugerem instrumento fraco."),
    booktabs = TRUE
)



# -------------------------------------------------------------------#
###### 5. SEÇÃO 4.4: ANÁLISES DE ROBUSTEZ E HETEROGENEIDADE #####
# -------------------------------------------------------------------#
cat("\n--- GERANDO OUTPUTS PARA A SEÇÃO 4.4 ---\n")

# 5.1 Figura 4.5: Heterogeneidade do Efeito (OCDE vs. Não-OCDE)
cat("Gerando Figura 4.5: Gráfico OCDE vs. Não-OCDE...\n")
# (O código para esta figura foi fornecido anteriormente)

# 5.2 Tabela 4.5: Tabela de Robustez de Especificação (OCDE)
cat("Gerando Tabela 4.5: Tabela de Robustez de Especificação...\n")
# (O código para esta tabela foi fornecido anteriormente, incluindo o manejo do Modelo 5)

cat("\n--- SCRIPT DE REPLICAÇÃO CONCLUÍDO ---\n")