# -------------------------------------------------------------------------#
#                   Exercícios
#
#  Script criado para resolução de exercícios
#  Sobre importação e tratamento de dados
#
# -------------------------------------------------------------------------#

# 1 - Importe o questionário survAluno ------------------------------------

survAluno <- readxl::read_xlsx("Aula-RCC0219/data_raw/SurvAlun - EstProb I.xlsx")

# 2 - Selecione as colunas relacionadas a: --------------------------------
# idade, altura, calcado, peso, cirurgia, oculos, quantidade de refeicoes no
# badeijao, estado civil, animais domésticos, quantidade de livros por ano,
# ano de ingresso na usp, outros idiomas e sobre doaçao de sangue

survAluno <- janitor::clean_names(survAluno)

survAluno_alterado <- survAluno |>
   dplyr::select(qual_a_sua_altura,
                 qual_o_numero_do_seu_calcado,
                 em_relacao_ao_seu_peso_aponte_abaixo_por_favor_em_qual_o_grupo_que_voce_se_encaixa,
                 ja_fez_alguma_cirurgia,
                 voce_usa_oculos,
                 regularmente_voce_faz_quantas_refeicoes_diarias_no_bandeijao,
                 qual_seu_estado_civil,
                 qual_dos_animais_domesticos_de_estimacao_abaixo_voce_possui,
                 quantos_livros_por_ano_voce_diria_que_le,
                 em_qual_ano_voce_ingressou_como_aluno_de_graduacao_pela_primeira_vez_na_usp,
                 caso_tenha_marcado_outro_por_favor_informe_no_campo_abaixo,
                 voce_doa_sangue
                 )

###########################################################################
#
# É importante selecionar as colunas porque as vezes trabalhamos com
# data.frames muito grandes, então diminuir a quantidade de colunas utilizadas
# facilita a visualização e deixa o processo mais rápido
#
###########################################################################

# 3 - Padronize o nome destas colunas -------------------------------------

# survAluno <- janitor::clean_names(survAluno)
# Esta função irá remover acentos, espaços e outros caracteres especiais
# isso irá evitar muitos problemas na horas de trabalharmos com estas colunas

# Reduzir o nome destas colunas também irá facilitar na hora de trabalharmos
# com elas

names(survAluno_alterado) <- c("altura", "n_calcado", "peso",
                               "cirurgia", "oculos", "bandejao",
                               "estado_civil", "animais_domesticos",
                               "n_livros_ano", "ano_ingresso", "idioma",
                               "doacao_sangue")


# 4 - Calcule os quartis, média e mediana das colunas altura e calçado ----

# Duas formas de fazer isso.

# 1)
quantile(survAluno_alterado$altura)
mean(survAluno_alterado$altura)


# 2)
survAluno_alterado |>
   dplyr::select(n_calcado) |>
   summary()


# 5 - Apresente um resumo das respostas sobre animais domésticos,  --------
# estado civil e óculos

survAluno_alterado |>
   dplyr::group_by(animais_domesticos) |>
   dplyr::count() |>
   dplyr::arrange(n) |>
   janitor::adorn_totals()

survAluno_alterado |>
   dplyr::group_by(estado_civil) |>
   dplyr::count() |>
   dplyr::arrange(n) |>
   janitor::adorn_totals()

survAluno_alterado |>
   dplyr::group_by(oculos) |>
   dplyr::count() |>
   dplyr::arrange(n) |>
   janitor::adorn_totals()

# 6 - Sobre o uso de óculos: substitua "Sim" por 1 e "Não" por 0 ---------

survAluno_alterado <- survAluno_alterado |>
   dplyr::mutate(oculos = ifelse(oculos == "Sim", 1, 0))

# 7 - Sobre as pessoas que falam outros idiomas ---------------------------
# Substitua os valores NA por "nenhum" e deixe todas as respostas em letra
# minúscula

survAluno_alterado <- survAluno_alterado |>
   dplyr::mutate(idioma = tolower(idioma),
                 idioma = ifelse(is.na(idioma), "nenhum", idioma))

str(survAluno_alterado$idioma) # verificando

# 8 - Filtre a base e veja quantas pessoas usam óculos e possuem   --------
# cachorro

survAluno_alterado |>
   dplyr::filter(oculos == 1 & animais_domesticos == "Cachorro") |>
   dplyr::count()

# 9 - Das pessoas que doam sangue, quantas já passaram por cirurgia? ------

survAluno_alterado |>
   dplyr::filter(doacao_sangue == "Sim" & cirurgia == "Sim") |>
   dplyr::count()

# 10 - Qual a quantidade média de livros lidos por ano?   ------------------
# Para isso, selecione apenas os números das respostas e transforme a coluna
# em numérica - use a função as.numeric()

survAluno_alterado <- survAluno_alterado |>
   dplyr::mutate(
      n_livros_ano = stringr::str_extract(n_livros_ano, "[0-9]+"),
      n_livros_ano = ifelse(is.na(n_livros_ano), 0, n_livros_ano),
      n_livros_ano = as.numeric(n_livros_ano)
   )

survAluno_alterado |>
   dplyr::summarise(media_livros_ano = mean(n_livros_ano))

# 11 - Das pessoas que fazem duas refeições no bandejão --------------------
# Qual a média de livros lidos por ano?
# Dica:
# base |>
#   filter() |>
#   group_by() |>
#   summarise(valor_medio_coluna_var = mean(var))

survAluno_alterado |>
   dplyr::filter(bandejao == "2 refeições") |>
   dplyr::summarise(media_livros_ano = mean(n_livros_ano))


# 12 - Das pessoas que não fazem refeições no bandejão e leem mais --------
# de quatro livros por ano: quantas usam óculos?

survAluno_alterado |>
   dplyr::filter(bandejao == "0 refeições" & n_livros_ano > 4 & oculos == 1) |>
   dplyr::count()

# 13 - Salve essa tratada no formato .csv ---------------------------------

write.csv(survAluno_alterado, "Aula-RCC0219/exercicios/1_exercicio.csv")

# 14 - Quais informações tiramos dessa base? ------------------------------
# Quais outras informações você acha interessante tirar?

# Tiramos informações sobre altura e n de calçados. Verificamos a quantidade
# de pessoas que doaram sangue e já fizeram cirurgia.
# Também verificamos que média de livros lidos no ano por pessoas que fazem
# duas refeições no bandeijão é menor que a média total. Entre outras.

# Poderiamos ver a quantidade de livros lidos ao ano por pessoas que usam
# óculos e compará-la com as que não usam óculos.


# Extra -------------------------------------------------------------------

# Importe uma base de dados de sua preferência e aplique o que foi ensinado
# e utilizado nos exercícios acima.
# Selecione algumas colunas que você julgue que sejam interessantes para análise,
# filtre seus valores, agrupe valores e relate os resultados encontrados

## Exemplos de base de dados ----

# Instale pacote basesCursoR
# A instalação deste pacote é diferente pois está no github, não no CRAN
install.packages("devtools")
devtools::install_github("curso-r/basesCursoR")

# Visualize as bases disponíveis
basesCursoR::bases_disponiveis()

# Importe a base escolhida, como no meu exemplo, a base pokemon:
pokemon <- basesCursoR::pegar_base("pokemon")

