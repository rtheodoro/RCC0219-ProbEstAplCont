# -------------------------------------------------------------------------#
#        Calculando estatíticas e gerando gráficos
#
#  Script criado para importar e tratar os dados da base
#  SurvAlun - EstProb I.xlsx
#
# -------------------------------------------------------------------------#

# Ler https://statsandr.com/blog/descriptive-statistics-in-r/#coefficient-of-variation
# Ler https://medium.com/psicodata/entenda-a-media-pelo-menos-1-desvio-padrao-acima-da-media-145e9edb6a8f
# Ler https://exts.ggplot2.tidyverse.org/gallery/
# Ler https://r-graph-gallery.com/
# Resumo do ggplot2: https://nyu-cdsc.github.io/learningr/assets/data-visualization-2.1.pdf

# Alternativa ao ggplot2, pacote plotly

# Qual gráfico escolher? https://www.data-to-viz.com/

# Pacotes utilizados ------------------------------------------------------

# Instalar antes:
install.packages(c(
   "dplyr",
   "summarytools",
   "ggplot2",
   "ggthemes",
   "psych",
   "tidyselect"
))

library(dplyr)
library(summarytools)
library(ggplot2) # Procurem pelo pacote esquisse
library(ggthemes)
library(psych)
library(tidyselect)

# Carregando base ---------------------------------------------------------

survAluno_alterado <-
   read.csv("data/survAluno_alterado.csv")

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

# Estatística descritva

survAluno_alterado |>
   dplyr::select(where(is.numeric)) |> # rodar library(tidyselect)
   psych::describe(quant = c(0, .25, .5, .75, 1))

# Variáveis categóricas
library(tidyselect)

psych::describeBy(survAluno_alterado |>
                     dplyr::select(where(is.numeric)),
                  survAluno_alterado$religiao)

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


# Gráficos ----------------------------------------------------------------


## Histograma - gráfico de densidade/frequência ---------------------------

hist(
   survAluno_alterado$altura,
   freq = FALSE,
   main = "Distribuição da Altura dos Alunos",
   ylab = "Densidade",
   xlab = "Valores da altura"
)

# Curva de densidade
lines(density(survAluno_alterado$altura),
      lwd = 2,
      col = "red")

# Curva de densidade sobre frequência
# desidade_altura <- density(survAluno_alterado$altura)
#
# lines(
#    x = desidade_altura$x,
#    y = desidade_altura$y * length(survAluno_alterado$altura) * diff(histograma_altura$breaks)[1],
#    lwd = 2,
#    col = "red"
# )

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
png("graficos/g_boxplot_n_livros.png")

survAluno_alterado |>
   ggplot2::ggplot() +
   ggplot2::aes(x = n_livros_ano) +
   ggplot2::geom_boxplot(
      fill = "lightblue",
      outlier.colour = "red",
      outlier.shape = 25,
      outlier.size = 2
   ) +
   ggplot2::coord_flip() +
   ggplot2::labs(title = 'Boxplot Quantidade de Livros por Ano') +
   ggplot2::xlab("Frequência na Quantidade de Livros")

dev.off()

# Em .pdf
pdf("graficos/g_hist_n_livros.pdf")

# Criando histograma com {ggplot2}
survAluno_alterado |>
   ggplot2::ggplot() +
   ggplot2::aes(n_livros_ano) +
   ggplot2::geom_histogram(
      ggplot2::aes(y = ..density..),
      bins = 13,
      colour = "#011e5a",
      fill = "lightblue"
   ) +
   # Curva de densidade
   ggplot2::stat_function(fun = dnorm,
                          args = list(
                             mean = mean(survAluno_alterado$n_livros_ano, na.rm = T),
                             sd = sd(survAluno_alterado$n_livros_ano, na.rm = T)
                          ),
                          colour = "red") +
   ggplot2::scale_x_continuous(breaks = seq(from = 0, to = 26, by = 2)) +
   # Rótulos
   ggplot2::xlab("Quantidade de Livros Lidos por Ano") +
   ggplot2::ylab("Densidade da Quantidade de Livros") +
   ggplot2::labs(title = "Histograma da Quantidade de Livros Lidos por Ano")

# Distribuição simétrica positiva.
# Nessa situação, a variável possui valores positivos extremos.
# Ou seja, uma cauda positiva longa. Aqui, Média > Mediana > Moda.
# Ver summary(survAluno_alterado$n_livros_ano)
dev.off()


## Outros gráficos -------------------------------------------------------


# Gráfico de Colunas
survAluno_alterado |>
  dplyr::count(idade, oculos) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = idade, y = n, fill = oculos) +
  ggplot2::geom_col(position = 'dodge') +
  ggplot2::xlab("Idade") +
  ggplot2::ylab("Quantidade de Alunos") +
  ggplot2::labs(title = "Relação Idade X Óculos")


# Gráfico de pizza
survAluno_alterado |>
   dplyr::select(oculos) |>
   dplyr::group_by(oculos) |>
   dplyr::count() |>
   ggplot2::ggplot() +
   ggplot2::aes(x = "", y = n, fill = oculos) +
   ggplot2::geom_col() +
   ggplot2::coord_polar("y") +
   ggplot2::geom_text(
      ggplot2::aes(label = n),
      col = "white",
      size = 8,
      position = ggplot2::position_stack(vjust = 0.5)
   ) +
   ggplot2::labs(title = "Gráfico de pizza sobre usuários de óculos") +
   ggplot2::xlab("") +
   ggplot2::ylab("") +
   ggplot2::theme_classic() +
   ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
   ) +
   ggthemes::scale_fill_colorblind()

# Gráfico de pontos
survAluno_alterado |>
   ggplot2::ggplot() +
   ggplot2::aes(x = altura, y = peso) +
   ggplot2::geom_point(colour = "#011e5a",
                       fill = "lightblue") +
   ggplot2::xlab("Altura") +
   ggplot2::ylab("Peso") +
   ggplot2::labs(title = "Relação Peso X Altura")

# Gráfico de pontos
survAluno_alterado |>
   ggplot2::ggplot() +
   ggplot2::aes(x = altura, y = n_calcado) +
   ggplot2::geom_point(colour = "#011e5a") +
   ggplot2::xlab("Altura") +
   ggplot2::ylab("Nº Calçado") +
   ggplot2::labs(title = "Relação Nº de Calçado X Altura") +
   ggplot2::geom_smooth(method = "lm", se = FALSE)
