# -------------------------------------------------------------------------
#                   R e RStudio
#
#  Script criado par armazenar os códigos utilizados no slide
#  2_slides_rrstudio.html
#
#
#
# -------------------------------------------------------------------------

## Operações básicas com o R


# Soma
1 + 1

# Subtração
1 - 1

# Multiplicação
1 * 1

# Divisão
1 / 1 # um dividido por um


## Criando variáveis

# Numérica
numero <- 1
numero

class(numero)

letra <- "a"
letra



## Criando vetores


# Vetor numérico
numeros <- c(1, 2, 3)
numeros

class(numeros)

# Vetor de caracteres
letras <- c("asdffsdf", "b", "c")
letras

mistura <- c("a", 1, "b", 2)

## Criando um data.frame

tabela <- data.frame(letras = LETTERS[1:10], numeros = seq(1:10))
tabela

names(tabela)

arquivo <- read.csv()
