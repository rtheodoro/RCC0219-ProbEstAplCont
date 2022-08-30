# -------------------------------------------------------------------------#
#                   Importando e tratando base
#
#  Script criado para importar e tratar os dados da base
#  SurvAlun - EstProb I.xlsx
#
# -------------------------------------------------------------------------#


# Pacotes utilizados ------------------------------------------------------

# Instalar antes:
install.packages(c("dplyr", "janitor", "lubridate", "readxl", "stringr"))

library(dplyr)
library(janitor)
library(lubridate)
library(readxl)
library(stringr)

# Importando --------------------------------------------------------------

survAluno <- readxl::read_xlsx("Aula-RCC0219/data_input/SurvAlun - EstProb I.xlsx")

# Resumo
dplyr::glimpse(survAluno)

# Tratando ----------------------------------------------------------------

## Limpando nomes ----
# Se os nomes fossem curtos, poderiamos usar a função
# janitor::clean_names(), mas nesse caso precisamos reduzir o tamanho

names(survAluno) <- c(
  "data_hora", "altura", "n_calcado", "peso", "cirurgia", "estetica",
  "estetica_outros", "cor", "oculos", "sexo", "estado_civil",
  "n_pessoas_moradia", "entidade_estudantil", "tipo_entidade_estudantil",
  "religiao", "animais_domesticos", "outro_animais_domesticos", "n_filhos",
  "idade", "doa_sangue", "frequencia_esporte", "n_livros_ano", "uf_antes_fearp",
  "n_pessoas_whatsapp", "renda_em_sm", "auxilio_usp", "tipo_auxilio_usp",
  "bandejao", "n_refeicoes_bandejao", "fundo_de_investimento", "cartao_de_credito",
  "conta_em_bancos", "previdencia_privada", "ano_ingresso_usp",
  "cotas", "divide_residencia", "n_tv", "viagem_exterior", "n_paises",
  "idioma_fluente", "outro_idioma", "plano_saude", "escola_colegial",
  "casa_pais", "escolaridade_pai", "escolaridade_mae", "ajuda_financeira_pais",
  "transporte_em_ribeirao", "graduacao_anterior", "curso_graduacao_anterior",
  "empregado", "modalidade_emprego", "profissao", "trabalho_contabil",
  "cidade_empresa", "horas_trabalho_semana", "renda_complementar"
)

dplyr::glimpse(survAluno)

str(survAluno)

# Selecionando colunas ----

# Existem duas formas de selecionar variáveis de um `data.frame`

# Primeira, com pacote base do R

survAluno[, 1:25]
survAluno[, c("bandejao","ano_ingresso_usp")]

# Segunda, com o pacote `dplyr`

survAluno |>
   dplyr::select(1:25)

survAluno |>
   dplyr::select(bandejao:ano_ingresso_usp)

# Filtrando valores ----

survAluno |>
   dplyr::filter(trabalho_contabil == "Sim") |>
   dplyr::group_by(horas_trabalho_semana) |>
   dplyr::count() |>
   janitor::adorn_totals()

# Agrupando valores de colunas ----

survAluno |>
   dplyr::group_by(trabalho_contabil, horas_trabalho_semana) |>
   dplyr::count()

# Separando colunas
survAluno <- survAluno |>
  dplyr::mutate(
    data = lubridate::ymd(as.Date(data_hora)),
    hora = stringr::str_sub(data_hora, start = 12),
    hora = lubridate::hms(hora)
  ) |>
  dplyr::select(data_hora, data, hora, everything())

max(survAluno$data_hora) - min(survAluno$data_hora)
max(survAluno$data) - min(survAluno$data)
max(lubridate::hour(survAluno$hora)) - min(lubridate::hour(survAluno$hora))

# Resumo de respostas ----

# Estatística descritiva das variáveis numéricas
library(tidyselect)
survAluno |>
  dplyr::select(where(is.double)) |>
  summary()

# Resumo das respostas categóricas
survAluno |>
  dplyr::group_by(animais_domesticos) |>
  dplyr::count() |>
  dplyr::arrange(n) |>
  janitor::adorn_totals()


### Criando função ----
resumo_categorica <- function(base, variavel) {
  base |>
    dplyr::group_by({{ variavel }}) |>
    dplyr::count() |>
    dplyr::arrange(n) |>
    janitor::adorn_totals()
}

# Criando uma iteração para todas as colunsa de vuma vez
for (i in 1:ncol(survAluno)) {
   resumo_categorica(survAluno, survAluno[, i]) |>
    print()
}




