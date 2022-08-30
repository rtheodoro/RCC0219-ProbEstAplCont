
# -------------------------------------------------------------------------
#                   Projetos, Objetos, Funções e Pacotes
#
#  Script criado par armazenar os códigos utilizados no slide
#  3_slides_projetos_objetos_funcoes_pacotes.html
#
#
#
# -------------------------------------------------------------------------


## Objetos ----

# São letras ou palavras criadas para receberem valor

# - Variáveis

# Salvando `1` em `a`
a <- 1
# Avaliando o objeto `a`
a


# **ATENÇÃO**:
#
# - o R diferencia letras maiúsculas de minísculas
#
# - Não utilizem caracteres especiais, apenas _
#
# - O nome deve representas sua utilidade


## Vetores ----
#
# Vetores são apenas conjuntos indexados de valores.
#
# Cada coluna de um data frame será representada como um vetor.

vetor_num <- c(1, 2, 3)
class(vetor_num)

vetor_letras <- c("a", "b", "c")
class(vetor_letras)

vetor_num_letras <- c("a", 3, "c")
class(vetor_num_letras)

## Trabalhando com objetos e vetores

# - Criando sequência

vetor_1a10 <- 1:10

vetor_10a1 <- 10:1

vetor_10a1[3]

## Pertence ----

# Como verificar se um valor está dentro de um vetor?

vetor_num %in% vetor_1a10

# Funções ----
## Funções

# São nomes que guardam um código de R.
# Sempre que você precisar repetir um trecho de código para realizar uma tarefa,
# você deve criar uma função e utilizá-lá.
#
# Exemplo: função soma

sum(a+A)

# Os valores dentro do parênteses da função são chamados de argumentos e serão
#utilizados como parâmetros para cálculo do resultado.

## Criando Funções

# Podemos criar nosas próprias funções

soma_3_divide_2 <- function(valor1){

   (valor1 + 3) / 2

}

soma_3_divide_2(7)

# Como chamar pacotes ----

# Duas formas:

#   1. `library(dplyr)` -> carrega todas as funções presentes no pacote

install.packages("dplyr")

library(dplyr)
mtcars |>
   select(cyl, mpg) |>
   filter(mpg > 33)


# 2. `dplyr::select()` -> carrega apenas a função chamada, durante este uso

mtcars |>
   dplyr::select(cyl, mpg) |>
   dplyr::filter(mpg > 33)






