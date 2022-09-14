# -------------------------------------------------------------------------#
#        Calculando estatíticas e gerando gráficos
#
#  Script criado para importar e tratar os dados da base
#  SurvAlun - EstProb I.xlsx
#
# -------------------------------------------------------------------------#

# Ler https://statsandr.com/blog/descriptive-statistics-in-r/#coefficient-of-variation
# Ler https://medium.com/psicodata/entenda-a-media-pelo-menos-1-desvio-padrao-acima-da-media-145e9edb6a8f


# Pacotes utilizados ------------------------------------------------------

# Instalar antes:
install.packages(c("dplyr", "summarytools", "ggplot2", "psych", "tidyselect"))

library(dplyr)
library(summarytools)
library(ggplot2) # Procurem pelo pacote esquisse
library(psych)
library(tidyselect)

# Carregando base ---------------------------------------------------------

survAluno_alterado <-
   read.csv("Aula-RCC0219/data/survAluno_alterado.csv")

# Estatísticas ------------------------------------------------------------

# Variáveis numéricas

# Média
mean(survAluno_alterado$n_livros_ano)

# Desvio Padrão
sd(survAluno_alterado$n_livros_ano)

# Mediana
median(survAluno_alterado$n_livros_ano)

# Quartil
quantile(survAluno_alterado$n_livros_ano)

# Variáveis categóricas
library(tidyselect)

psych::describeBy(
   survAluno_alterado |> dplyr::select(where(is.numeric)),
   survAluno_alterado$religiao
)

# Frequência
survAluno_alterado$animais_domesticos |>
   as.factor() |>
   summary()

# Proporções
survAluno_alterado$animais_domesticos |>
   table() |>
   proportions() |>
   round(2)

# Mais de uma variável
# Por linha
table(survAluno_alterado$animais_domesticos,
      survAluno_alterado$casa_pais) |>
   proportions() |>
   round(2)

# Por coluna
table(survAluno_alterado$animais_domesticos,
      survAluno_alterado$casa_pais) |>
   proportions(2) |>
   round(2)

summarytools::ctable(
   x = survAluno_alterado$animais_domesticos,
   y = survAluno_alterado$casa_pais,
   prop = "t" # Exibe o total
)

# Estatística descritva

survAluno_alterado |>
   dplyr::select(where(is.numeric)) |> # rodar library(tidyselect)
   psych::describe(quant=c(0, .25, .5, .75, 1))


# Gráficos ----------------------------------------------------------------


## Histograma - gráfico de frequência ------------------------------------

histograma_altura <- hist(
   survAluno_alterado$altura,
   main = "Distribuição da Altura dos Alunos",
   ylab = "Frequência",
   xlab = "Valores da altura"
)

plot(density(survAluno_alterado$altura),
     lwd = 2,
     col = "red")

# Curva de densidade
desidade_altura <- density(survAluno_alterado$altura)

lines(
   x = desidade_altura$x,
   y = desidade_altura$y * length(survAluno_alterado$altura) * diff(histograma_altura$breaks)[1],
   lwd = 2,
   col = "red"
)

# Distribuição simétrica em torno da média.
# Nessa situação, temos a presença de uma distribuição próxima da normal.
# Ou seja, Média = Mediana = Moda

# Ver summary(survAluno_alterado$altura) , os valores são próximos

# Teste de normalidade
shapiro.test(survAluno_alterado$altura) # p-valor > 0,05 -> A distribuição é normal


## Boxplot - visualização dos quartis ------------------------------------

boxplot(
   survAluno_alterado$altura,
   main = "Boxplot da Altura dos Alunos",
   ylab = "Altura",
   xlab = "Distribuição",
   col = "darkgreen"
)


quantile(survAluno_alterado$altura)

## Exportando gráficos ---------------------------------------------------

# Em .png
png("Aula-RCC0219/graficos/boxplot_n_livros.png")

survAluno_alterado |>
   ggplot2::ggplot() +
   ggplot2::aes(x = n_livros_ano) +
   ggplot2::geom_boxplot(
      fill = "lightblue",
      outlier.colour = "red",
      outlier.shape = 25,
      outlier.size = 2,
      notch = FALSE
   ) +
   ggplot2::coord_flip() +
   ggplot2::labs(title = 'Boxplot Quantidade de Livros por Ano') +
   ggplot2::xlab("Frequência na Quantidade de Livros")

dev.off()

# Em .pdf
pdf("Aula-RCC0219/graficos/hist_n_livros.pdf")

# Criando histograma com {ggplot2}
survAluno_alterado |>
   ggplot2::ggplot(ggplot2::aes(n_livros_ano)) +
   ggplot2::geom_histogram(
      ggplot2::aes(y = ..density..),
      bins = 13,
      colour = "#011e5a",
      fill = "lightblue"
   ) +
   ggplot2::stat_function(fun = dnorm,
                          args = list(
                             mean = mean(survAluno_alterado$n_livros_ano, na.rm = T),
                             sd = sd(survAluno_alterado$n_livros_ano, na.rm = T)
                          ),
                          colour = "#011e5a") +
   ggplot2::scale_x_continuous(breaks = seq(from = 0, to = 26, by = 2)) +
   # Rótulos
   ggplot2::xlab("Quantidade de Livros Lidos por Ano") +
   ggplot2::ylab("Frequência na Quantidade de Livros") +
   ggplot2::labs(title = "Histograma da Quantidade de Livros Lidos por Ano")

# Distribuição simétrica positiva.
# Nessa situação, a variável possui valores positivos extremos.
# Ou seja, uma cauda positiva longa. Aqui, Média > Mediana > Moda.
# Ver summary(survAluno_alterado$n_livros_ano)
dev.off()


## Outros gráficos -------------------------------------------------------

# Gráfico de barras


