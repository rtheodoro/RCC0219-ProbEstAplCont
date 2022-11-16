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
options(scipen = 6, digits = 2)
# transforma notação científica em n inteiro
# arredondo duas cadas decimais
#--------------------------------------------------------------------------#


# Pacotes utilizados ------------------------------------------------------

# install.packages("dplyr")
# install.packages("glue")
# install.packages("janitor")
# install.packages("kableExtra")
# install.packages("tidyselect")

library(tidyselect)

# Importando a base -------------------------------------------------------

coop_cred_2021_auditores <-
   read.csv("data_raw/coop_cred_2021_auditores.csv") |>
   janitor::clean_names()


# Resumo
dplyr::glimpse(coop_cred_2021_auditores)

# Selecionando colunas ----------------------------------------------------

# Exportando uma tabela em formato latex
coop_cred_2021_auditores <- coop_cred_2021_auditores |>
   dplyr::select(cnpj,
                 big_four,
                 uf,
                 numero_agencias,
                 ativo_total,
                 patrimonio_liquido,
                 filiacao)

coop_cred_2021_auditores |>
   dplyr::select(-cnpj, -big_four) |>
   dplyr::select(where(is.numeric)) |>
   summary()

# Os números estão muito grandes, fica difívil visualiar

coop_cred_2021_auditores <-
   coop_cred_2021_auditores |>
   dplyr::mutate(dplyr::across(
      where(is.numeric) & !c(cnpj, big_four, numero_agencias),
      .fns = ~ . / 100000000
   ))

# Estatísticas descritivas

coop_cred_2021_auditores |>
   dplyr::select(-cnpj, -big_four) |>
   dplyr::select(where(is.numeric)) |>
   summary()


# Visualizando variáveis categóricas --------------------------------------


# Função
resumo_categorica <- function(data, var) {
   teste <- tidyselect::enquo(var) |> rlang::as_name()

   data |>
      dplyr::group_by({
         {
            var
         }
      }) |>
      dplyr::count() |>
      dplyr::arrange(-n) |>
      janitor::adorn_totals()
}


resumo_categorica(coop_cred_2021_auditores, filiacao)

resumo_categorica(coop_cred_2021_auditores, uf)


coop_cred_2021_auditores |>
   janitor::tabyl(big_four)


# Agrupando valores -------------------------------------------------------

# Agrupando valores de colunas ----

coop_cred_2021_auditores |>
   dplyr::group_by(big_four, uf) |>
   dplyr::count() |>
   print(n = Inf)

# Filtrando valores ----

coop_cred_2021_auditores |>
   dplyr::filter(uf == "SP" | uf == "SC") |>
   dplyr::group_by(big_four, uf) |>
   dplyr::count() |>
   print(n = Inf)


coop_cred_2021_auditores |>
   dplyr::filter(uf == "SP" & big_four == 1) |>
   dplyr::group_by(uf) |>
   dplyr::count()


## Boxplot - visualização dos quartis ------------------------------------

boxplot(
   coop_cred_2021_auditores$ativo_total,
   main = "Boxplot do Ativo Total",
   ylab = "Ativo Total",
   xlab = "Distribuição",
   col = "darkgreen"
)

quantile(coop_cred_2021_auditores$ativo_total)

boxplot(
   coop_cred_2021_auditores$patrimonio_liquido,
   main = "Boxplot do Patrimônio Líquido",
   ylab = "Patrimônio Líquido",
   xlab = "Distribuição",
   col = "darkgreen"
)


quantile(coop_cred_2021_auditores$patrimonio_liquido)


# Removendo outliers

coop_cred_2021_auditores <- coop_cred_2021_auditores |>
   dplyr::filter(ativo_total <= 1000 &
                    patrimonio_liquido <= 100)



coop_cred_2021_auditores |>
   janitor::tabyl(big_four)


boxplot(
   coop_cred_2021_auditores$ativo_total,
   main = "Boxplot do Ativo Total",
   ylab = "Ativo Total",
   xlab = "Distribuição",
   col = "darkgreen"
)


boxplot(
   coop_cred_2021_auditores$patrimonio_liquido,
   main = "Boxplot do Patrimônio Líquido",
   ylab = "Patrimônio Líquido",
   xlab = "Distribuição",
   col = "darkgreen"
)


boxplot(
   ativo_total ~ big_four,
   coop_cred_2021_auditores,
   main = "Boxplot do Ativo Total Por Grupos",
   ylab = "Ativo Total",
   xlab = "Big Four",
   col = "darkgreen"
)


## Histograma - gráfico de densidade/frequência ---------------------------

hist(
   coop_cred_2021_auditores$ativo_total,
   freq = FALSE,
   main = "Distribuição do Ativo Total",
   ylab = "Densidade",
   xlab = "Valores do Ativo Total"
)

# Teste de normalidade
shapiro.test(coop_cred_2021_auditores$ativo_total) # p-valor < 0,05 -> A distribuição não é normal

hist(
   coop_cred_2021_auditores$patrimonio_liquido,
   freq = FALSE,
   main = "Distribuição do Patrimônio Líquido",
   ylab = "Densidade",
   xlab = "Valores do Patrimônio Líquido"
)

# Teste de normalidade
shapiro.test(coop_cred_2021_auditores$patrimonio_liquido) # p-valor < 0,05 -> A distribuição não é normal


# Normalizando variáveis
coop_cred_2021_auditores <- coop_cred_2021_auditores |>
   dplyr::mutate(ativo_log = log(ativo_total),
                 pl_log = log(patrimonio_liquido))


hist(
   coop_cred_2021_auditores$ativo_log,
   freq = FALSE,
   main = "Distribuição do log(Ativo Total)",
   ylab = "Densidade",
   xlab = "Valores do log(Ativo Total)"
)

# Teste de normalidade
shapiro.test(coop_cred_2021_auditores$ativo_log) # p-valor < 0,05 -> A distribuição não é normal, mas se aproxima

hist(
   coop_cred_2021_auditores$pl_log,
   freq = FALSE,
   main = "Distribuição do log(Patrimônio Líquido)",
   ylab = "Densidade",
   xlab = "Valores do log(Patrimônio Líquido)"
)

# Teste de normalidade
shapiro.test(coop_cred_2021_auditores$pl_log) # p-valor < 0,05 -> A distribuição não é normal, mas se aproxima

# Associação entre variáveis qualitativas (chi2 e Pearson) ----------

chisq.test(coop_cred_2021_auditores$big_four,
           coop_cred_2021_auditores$filiacao)


chisq.test(coop_cred_2021_auditores$big_four,
           coop_cred_2021_auditores$uf)

# p-valor < 0,05 grupos iguais

# Assocoação entre variáveis quantitativas (covariância e correlaç --------

chisq.test(coop_cred_2021_auditores$big_four,
           coop_cred_2021_auditores$pl_log)

chisq.test(coop_cred_2021_auditores$big_four,
           coop_cred_2021_auditores$ativo_log)

# p-valor = 0,05, grupos diferentes

# Correlação
cor(
   coop_cred_2021_auditores |> dplyr::filter(big_four == 0) |> dplyr::select(pl_log),
   coop_cred_2021_auditores |> dplyr::filter(big_four == 0) |> dplyr::select(ativo_log),
   method = "pearson"
)

cor(
   coop_cred_2021_auditores |> dplyr::filter(big_four == 1) |> dplyr::select(pl_log),
   coop_cred_2021_auditores |> dplyr::filter(big_four == 1) |> dplyr::select(ativo_log),
   method = "pearson"
)

# Próximo de 1, são autamente correlacionadas


# Covariância

cov(coop_cred_2021_auditores$pl_log,
    coop_cred_2021_auditores$ativo_log)

# Auta covariância


# Associação entre variáveis qualitativas e quantitativas (R^2) --------

r2 <-
   lm(pl_log ~ ativo_log + big_four, coop_cred_2021_auditores) |> summary()

r2$r.squared # quanto maior, melhor.


# Gráficos ----------------------------------------------------------------

coop_cred_2021_auditores |>
   ggplot2::ggplot() +
   ggplot2::aes(x = pl_log, y = ativo_log) +
   ggplot2::geom_point(colour = "#011e5a") +
   ggplot2::xlab("Patrimônio Líquido") +
   ggplot2::ylab("Ativo Total") +
   ggplot2::labs(title = "Relação Patrimônio Líquido X Ativo Total") +
   ggplot2::geom_smooth(method = "lm")

# Distribuições de Probabilidade -------------------------------------------

# Ver: https://bookdown.org/matheusogonzaga/apostila_r2/distribuicoes-de-probabilidade.html

# Exemplos de uso

# Binomial ----------------------------------------------------------------


# Suponha que na nossa amostra de 392 cooperativas, n = 392, estejamos interessados na
# probabilidade de encontrarmos 10 cooperativas auditadas por big four, k = 10, com
# p = 0.05.
# Calculamos a probabilidade P(X = 8 ) da seguinte maneira:

dbinom(x = 10, size = 392, prob = 0.05)

# Desta vez, queremos encontrar a probabilidade de no máximo 10 cooperativas da
# nossa amostra serem auditadas por big four, ou seja, estamos interessados na
# probabilidade acumulada da binomial com n = 10 e p = 0.05 avaliada no caso em que X = 10.

# Buscamos a probabilidade da ocorrência de 10, 9, 8, 7,…, 2, 1 e 0 sucessos, P(X ≤ 10)
pbinom(q = 10, size = 392, prob = 0.05)

# Para ser >10, basta subtrair 1 do resultado ou adicionar o parâmetro lower.tail = FALSE


#  Se queremos o quantil referente ao percentil de ordem 0.95, estamos
# interessados num determinado valor de sucessos que seja maior do que 95% dos
# valores de sucesso possíveis.
qbinom(p = 0.95, size = 392, prob = 0.05)




