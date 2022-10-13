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
# Notas:
#
#--------------------------------------------------------------------------#
options(scipen = 6, digits = 4)
#--------------------------------------------------------------------------#


# Pacotes utilizados ------------------------------------------------------

# Instalando pacotes
install.packages("janitor")

# Carregando pacotes
library(janitor)


# *Está comentado pois usarei no formato pacote::funcao
# *Assim vocês irão saber de qual pacote vem a função utilizada


# Carregando bases --------------------------------------------------------

coop_cred <- read.csv("Aula-RCC0219/data_raw/coop_cred_2021_auditores.csv") |>
   janitor::clean_names()

# Tratando dados ----------------------------------------------------------

# Selecionando variáveis de interesse

coop_cred <- coop_cred |>
   dplyr::select(cnpj, auditor_independente, uf, ativo_total, pecld, passivo_total)

coop_cred |>
   dplyr::count(uf) |>
   dplyr::arrange(-n) |>
   janitor::adorn_totals()

coop_cred |>
   dplyr::count(auditor_independente) |>
   dplyr::arrange(-n) |>
   janitor::adorn_totals()


# Estatísticas descritivas ------------------------------------------------



# Plotando Gráficos -------------------------------------------------------




