#--------------------------------------------------------------------------#
#
# Nome do Script: Análise de dados das Cooperativas de Crédito Brasileiras
#                 que enviaram informação de seus auditores para o BACEN
#                 em 2021
#
# Objetivo: Importar dados das Coop. Créd. que enviaram informações de seus
#           auditores independentes para o BACEN em 2021, tratar e gerar info.
#
# Autor: Ricardo Theodoro
# Email: rtheodoro@usp.br
# Data da criação: 2022-10-13
#
#--------------------------------------------------------------------------#
#
# Notas: Este script contém o que está no arquivo analise_auditores_coopcred.Rmd
#
#--------------------------------------------------------------------------#
options(scipen = 6, digits = 4)
#--------------------------------------------------------------------------#




# Pacotes utilizados ------------------------------------------------------

install.packages("dplyr")
install.packages("glue")
install.packages("janitor")
install.packages("kableExtra")
install.packages("tidyselect")



# Importando a base -------------------------------------------------------

coop_cred_2021_auditores <-
  read.csv("data_raw/coop_cred_2021_auditores.csv") |>
  janitor::clean_names()


# Resumo

dplyr::glimpse(coop_cred_2021_auditores)


# Selecionando colunas ----------------------------------------------------

# Exportando uma tabela em formato latex
coop_cred_2021_auditores <- coop_cred_2021_auditores |>
  dplyr::select(
    cnpj,
    auditor_independente,
    big_four,
    uf,
    numero_agencias,
    ativo_total,
    patrimonio_liquido,
    filiacao
  )

coop_cred_2021_auditores |>
  dplyr::select(-cnpj) |>
  stargazer::stargazer(
    title = "Estatístias Descritivas das variáveis numéricas - pré-tratamento",
    summary = TRUE,
    iqr = TRUE,
    flip = TRUE,
    header = FALSE
  )



# Transformando colunas em numéricas --------------------------------------

# Primeira forma
coop_cred_2021_auditores$big_four <-
  as.numeric(coop_cred_2021_auditores$big_four)

# Segunda forma
coop_cred_2021_auditores <- coop_cred_2021_auditores |>
  dplyr::mutate(numero_agencias = as.numeric(numero_agencias))



# Visualizando variáveis categóricas --------------------------------------


# Função
resumo_categorica <- function(data, var) {
  teste <- tidyselect::enquo(var) |> rlang::as_name()
  
  data |>
    dplyr::group_by({{ var }}) |>
    dplyr::count() |>
    dplyr::arrange(-n) |>
    janitor::adorn_totals() 
}

resumo_categorica(coop_cred_2021_auditores, auditor_independente) 

resumo_categorica(coop_cred_2021_auditores, filiacao)

resumo_categorica(coop_cred_2021_auditores, uf)


coop_cred_2021_auditores |> 
  janitor::tabyl(big_four)




#  Associação entre variáveis qualitativas ( $\chi^2$ e Pearson) ----------

chisq_filiacao <- chisq.test(
  coop_cred_2021_auditores$big_four,
  coop_cred_2021_auditores$filiacao
)

print(chisq_filiacao)

chisq_uf <- chisq.test(
  coop_cred_2021_auditores$big_four,
  coop_cred_2021_auditores$uf
)

print(chisq_uf)



# Assocoação entre variáveis quantitativas (covariância e correlaç --------


chisq.test(
  coop_cred_2021_auditores$big_four,
  coop_cred_2021_auditores$patrimonio_liquido
)

chisq.test(
  coop_cred_2021_auditores$big_four,
  coop_cred_2021_auditores$ativo_total
)

# Correlação
cor(
  coop_cred_2021_auditores |> dplyr::filter(big_four == 0) |> dplyr::select(patrimonio_liquido),
  coop_cred_2021_auditores |> dplyr::filter(big_four == 0) |> dplyr::select(ativo_total),
  method = "pearson"
)

cor(
  coop_cred_2021_auditores |> dplyr::filter(big_four == 1) |> dplyr::select(patrimonio_liquido),
  coop_cred_2021_auditores |> dplyr::filter(big_four == 1) |> dplyr::select(ativo_total),
  method = "pearson"
)

# Covariância

cov(
  coop_cred_2021_auditores$patrimonio_liquido,
  coop_cred_2021_auditores$ativo_total
)


#  Associação entre variáveis qualitativas e quantitativas ($R^2$) --------

r2 <- lm(patrimonio_liquido ~ big_four, coop_cred_2021_auditores) |> summary()

r2$r.squared
