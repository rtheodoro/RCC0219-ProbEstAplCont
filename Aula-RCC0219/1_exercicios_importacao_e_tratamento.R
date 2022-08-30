# -------------------------------------------------------------------------#
#                   Exercicios
#
#  Script criado para resolução de exercícios
#  Sobre importação e tratamento de dados
#
# -------------------------------------------------------------------------#

# Instalar pacote basesCursoR
# A instalação deste pacote é diferente pois está no github, não no CRAN
install.packages("devtools")
devtools::install_github("curso-r/basesCursoR")

# Escolher base disponível
basesCursoR::bases_disponiveis()

# Importar a base escolhida, ex:
pokemon <- basesCursoR::pegar_base("pokemon")


# Depois de selecionada a base de dados, aplique os conhecimentos adquiridos em
# aula nesta nova base
