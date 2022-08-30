# -------------------------------------------------------------------------
#                   Importando e tratando base
#
#  Script criado para importar e tratar os dados da base
#  SurvAlun - EstProb I.xlsx
#
# -------------------------------------------------------------------------


# Pacotes utilizados ------------------------------------------------------

# Instalar antes:
install.packages(c("dplyr", "janitor", "lubridate", "readxl", "stringr"))

library(dplyr)
library(janitor)
library(lubridate)
library(readxl)
library(stringr)

# Importando --------------------------------------------------------------

survAlun <- readxl::read_xlsx("Aula-RCC0219/data_input/SurvAlun - EstProb I.xlsx")

# Tratando ----------------------------------------------------------------

# Limpando nomes
names(survAlun) <- c("data_hora", "altura", "n_calcado", "peso", "cirurgia", "estetica",
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

# Separando colunas

survAlun <- survAlun |>
   dplyr::mutate(
      data = lubridate::ymd(as.Date(data_hora)),
      hora = stringr::str_sub(data_hora, start = 12),
      hora = lubridate::hms(hora)
   ) |>
   dplyr::select(data_hora, data, hora, everything())

max(survAlun$data_hora) - min(survAlun$data_hora)
max(survAlun$data) - min(survAlun$data)
max(lubridate::hour(survAlun$hora)) - min(lubridate::hour(survAlun$hora))


