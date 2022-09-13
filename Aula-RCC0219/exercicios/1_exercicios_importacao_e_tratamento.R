# -------------------------------------------------------------------------#
#                   Exercícios
#
#  Script criado para resolução de exercícios
#  Sobre importação e tratamento de dados
#
# -------------------------------------------------------------------------#

# 1 - Importe o questionário survAluno ------------------------------------

# 2 - Selecione as colunas relacionadas a: --------------------------------
# idade, altura, calcado, peso, cirurgia, oculos, quantidade de refeicoes no
# badeijao, estado civil, animais domésticos, quantidade de livros por ano,
# ano de ingresso na usp, outros idiomas e sobre doaçao de sangue

# 3 - Padronize o nome destas colunas -------------------------------------

# 4 - Calcule os quartis, média e mediana das colunas altura e calçado ----

# 5 - Apresente um resumo das respostas sobre animais domésticos,  --------
# estado civil e óculos

# 6 - Sobre o uso de óculos: substitua "Sim" por 1 e "Não" por 0 ---------

# 7 - Sobre as pessoas que falam outros idiomas ---------------------------
# Substitua os valores NA por "nenhum" e deixe todas as respostas em letra
# minúscula

# 8 - Filtre a base e veja quantas pessoas usam óculos e possuem   --------
# cachorro

# 9 - Das pessoas que doam sangue, quantas já passaram por cirurgia? ------

# 10 - Qual a quantidade média de livros lidos por ano?   ------------------
# Para isso, selecione apenas os números das respostas e transforme a coluna
# em numérica - use a função as.numeric()

# 11 - Das pessoas que fazem duas refeições no bandejão --------------------
# Qual a média de livros lidos por ano?
# Dica:
# base |>
#   filter() |>
#   group_by() |>
#   summarise(valor_medio_coluna_var = mean(var))

# 12 - Das pessoas que não fazem refeições no bandejão e leem mais --------
# de quatro livros por ano: quantas usam óculos?

# 13 - Salve essa tratada no formato .csv ---------------------------------

# 14 - Quais informações tiramos dessa base? ------------------------------
# Quais outras informações você acha interessante tirar?

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

