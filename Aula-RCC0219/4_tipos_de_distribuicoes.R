# As distribuições a serem usadas serão:
# Bernoulli
# Binomial
# Poisson
# logistica ##### FAZER

# Função de probabilidade e distribuição acumulado ####
# Valor esperado e variância ####

# Falar da média e variância dos tipos de distribuição

# Normal padrão
# chi quadrado #### FAZER


# Ver: https://bookdown.org/matheusogonzaga/apostila_r2/distribuicoes-de-probabilidade.html

# Visualização das distribuições ------------------------------------------

# Apenas rodar essa parte, não precisa entrar em detalhes do código aqui

## Função para gerar os gráficos ----

# size: número de tentativas (n > = 0)
# prob: probabilidade de sucesso por tentativa
# lb: cauda de baixo
# ub: cauda de cima
# col: cor
# lwd: grossura da linha
binom_sum <- function(size, prob, lb, ub, col = 4, lwd = 1, ...) {
   x <- 0:size
   
   if (missing(lb)) {
      lb <- min(x)
   }
   if (missing(ub)) {
      ub <- max(x)
   }
   
   plot(dbinom(x, size = size, prob = prob), type = "h", lwd = lwd, ...)
   
   if(lb == min(x) & ub == max(x)) {
      color <- col
   } else {
      color <- rep(1, length(x))
      color[(lb + 1):ub ] <- col
   }
   
   lines(dbinom(x, size = size, prob = prob), type = "h",
         col =  color, lwd = lwd, ...)
}


# lambda: média
# lb: cauda inferior
# ub: cauda superior
# col: cor
# lwd: grossura da linha
pois_sum <- function(lambda, lb, ub, col = 4, lwd = 1, ...) {
   x <- 0:(lambda + lambda * 2)
   
   if (missing(lb)) {
      lb <- min(x)
   }
   if (missing(ub)) {
      ub <- max(x)
   }
   
   plot(dpois(x, lambda = lambda), type = "h", lwd = lwd, ...)
   
   if(lb == min(x) & ub == max(x)) {
      color <- col
   } else {
      color <- rep(1, length(x))
      color[(lb + 1):ub ] <- col
   }
   
   lines(dpois(x, lambda = lambda), type = "h",
         col =  color, lwd = lwd, ...)
}


## Bernoulli --------------------------------------------------------------

# https://statisticsglobe.com/bernoulli-distribution-in-r-dbern-pbern-qbern-rbern

install.packages("Rlab")
library("Rlab")

# Bernoulli Função Densidade de Probabilidade (dbern)
x_dbern <- seq(0, 10, by = 1)                    # Especificar os valores de x
y_dbern <- dbern(x_dbern, prob = 0.7)            # aplicar os valores na funcao dbern
plot(y_dbern, type = "o")                        # Plota os valores 

# Bernoulli Função de Distribuição Cumulativa (pbern)

x_pbern <- seq(0, 10, by = 1)                    # Especificar os valores de x
y_pbern <- pbern(x_pbern, prob = 0.7)            # aplicar os valores na funcao pbern
plot(y_pbern, type = "o")                        # Plota os valores

# Bernoulli Função Quantilica (qbern)

x_qbern <- seq(0, 1, by = 0.1)                   # Especificar os valores de x
y_qbern <- qbern(x_qbern, prob = 0.7)            # aplicar os valores na funcao qbern
plot(y_qbern, type = "o")                        # Plota os valores


# Gerando números aleatórios para Bernoulli (rbern)

set.seed(98989)                                  # Semente (para sempre ter o mesmo resultado)
N <- 10000                                       # Tamanho da amostra

y_rbern <- rbern(N, prob = 0.7)                  # Pega N valores aleatórios
y_rbern                                          # Mostra os valores obtidos
hist(y_rbern,                                    # Plota gráfico de densidade
     breaks = 5,
     main = "")


## Binomial ----------------------------------------------------------------

# https://www.tutorialspoint.com/r/r_binomial_distribution.htm
# https://r-coder.com/binomial-distribution-r/

# funções utiliadas
# dbinom(x, size, prob)
# pbinom(x, size, prob)
# qbinom(p, size, prob)
# rbinom(n, size, prob)

x <- seq(0, 50, by = 1) # Cria amostra de 50 valores, um a um
y <- dbinom(x, 50, 0.5) # Cria uma distribuição binomial.
plot(x, y) # Plot the graph for this sample.

# A função dbinom
dbinom(x,           # Valores de x (x = 0, 1, 2, ..., n)
       size,        # Quantidade de tentativas (n > = 0)
       prob,        # Probabilidade de sucesso por tentativa
       log = FALSE) # Se TRUE, transforma a probabilidade em log


# Probabilidade de obter 26 ou menos caras em 51 lançamentos de moedas
x <- pbinom(26, 51, 0.5)
print(x)

# A função pbinom 
pbinom(q,                 # Vetor de quantils
       size,              # Número de tentativas (n > = 0)
       prob,              # Probabilidade de sucesso
       lower.tail = TRUE, # Se TRUE, as probabilidades são P(X <= x), ou P(X > x) 
       log.p = FALSE)     # Se TRUE, as probabilidades são dadas em log

pbinom(6, size = 20, prob = 0.4) # 0.2500107 ou 25%
1 - pbinom(6, size = 20, prob = 0.4, lower.tail = FALSE) # Equivalente

binom_sum(size = 20, prob = 0.4, ub = 6, lwd = 2,
          ylab = "P(X = x)", xlab = "Número de sucesso")

# Quantas caras irão ter a probabilidade de aparecem em 0.25 em 51 lançamentos de moeda

x <- qbinom(0.25, 51, 1 / 2)
print(x)

# A função qbinom 

qbinom(p,                 # Probabilidade ou vetor de probabilidades
       size,              # Número de tentativas (n > = 0)
       prob,              # Probabilidade de sucesso por tentativas
       lower.tail = TRUE, # Se TRUE, as probabilidadas são P(X <= x), ou P(X > x)
       log.p = FALSE)     # Se TRUE, as probabilidadas são dadas em log

qbinom(p = 0.4, size = 5, prob = 0.7) # 3

x <- 1:80

# size = 80, prob = 0.2
plot(qbinom(seq(0, 1, 0.001), size = 80, prob = 0.2),
     main = "Função Quantilica Binomial",
     ylab = "Q(p)", xlab = "p",
     type = "s", col = 3, xaxt = "n")
axis(1, labels = seq(0, 1, 0.1), at = 0:10 * 100)

# size = 80, prob = 0.3
lines(qbinom(seq(0, 1, 0.001), size = 80, prob = 0.3), type = "s", col = 2)

# size = 80, prob = 0.4
lines(qbinom(seq(0, 1, 0.001), size = 80, prob = 0.4), type = "s", col = 1)

# Adiciona legenda
legend("topleft", legend = c("80  0.2", "80  0.3", "80  0.4"),
       title = "size  prob", title.adj = 0.95,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)

# Encontre 8 valores aleatórios de uma amostra de 150 números com probabildiade de 0.4
x <- rbinom(8, 150, .4)
print(x)


# A função rbinom 
rbinom(n,    # Número de observações aleatórias a serem geradas
       size, # Número de tentativas (> = 0)
       prob) # A probabilidade de sucesso em cada tentativa

set.seed(2)
rbinom(n = 15, size = 30, prob = 0.1)

# Plota os valores da função de probabilidade binomial

# Eixo x
x <- 1:80

# size = 80, prob = 0.2
plot(dbinom(x, size = 80, prob = 0.2), type = "h", lwd = 2,
     main = "Função de Probabildiade Binomial",
     ylab = "P(X = x)", xlab = "Número de sucesso")

# size = 80, prob = 0.3
lines(dbinom(x, size = 80, prob = 0.3), type = "h",
      lwd = 2, col = rgb(1,0,0, 0.7))

# size = 80, prob = 0.4
lines(dbinom(x, size = 80, prob = 0.4), type = "h",
      lwd = 2, col = rgb(0, 1, 0, 0.7))

# Adiciona legenda
legend("topright", legend = c("80  0.2", "80  0.3", "80  0.4"),
       title = "size  prob", title.adj = 0.95,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)


# Plota uma função de distribuição binomial cumulativa

# Eixo X
x <- 1:80

# size = 80, prob = 0.2
plot(pbinom(x, size = 80, prob = 0.2), type = "s", lwd = 2,
     main = "Função distribuião cumulativa binomial",
     xlab = "Number of successes", ylab = "F(x)")

# size = 80, prob = 0.3
lines(pbinom(x, size = 80, prob = 0.3), type = "s",
      lwd = 2, col = 2)

# size = 80, prob = 0.4
lines(pbinom(x, size = 80, prob = 0.4), type = "s",
      lwd = 2, col = 3)

# Adiciona legenda
legend("bottomright", legend = c("80  0.2", "80  0.3", "80  0.4"),
       title = "size  prob", title.adj = 0.95,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)


## Poisson -----------------------------------------------------------------

# https://r-coder.com/poisson-distribution-r/

# A função dpois

dpois(x,           # Valores de x(x = 0, 1, 2, ...)
      lambda,      # Número médio de eventos ocorridos no intervalo
      log = FALSE) # Se TRUE, mostra a probabilidade como log

# Plota a função Probabilidade Poisson
# Eixo X
x <- 0:50

lambda <- 5
plot(dpois(x, lambda), type = "h", lwd = 2,
     main = "Função de probabilidade de Poisson",
     ylab = "P(X = x)", xlab = "Número de eventos")

lambda <- 10
lines(dpois(x, lambda), type = "h", lwd = 2, col = rgb(1,0,0, 0.7))

lambda <- 20
lines(dpois(x, lambda), type = "h", lwd = 2, col = rgb(0, 1, 0, 0.7))

# Legenda
legend("topright", legend = c("5", "10", "20"),
       title = expression(lambda), title.adj = 0.75,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)

# A função ppois 
ppois(q,                 # Quantil ou vetor de quantil
      lambda,            # Média ou vetor de médias
      lower.tail = TRUE, # Se TRUE, as probabilidades são P(X <= x), ou P(X > x)
      log.p = FALSE)     # Se TRUE, as probabilidades são em log

ppois(5, lambda = 10) # 0.06708596

# Considere que um site recebe em média 15 visitar por hora, em uma distribuiçã Poisson
# lambda = 15

# A probabilidade obter 10 ou menos visitas por hora
ppois(10, lambda = 15) # 0.1184644 or 11.8%
1 - ppois(10, lambda = 15, lower.tail = FALSE) # Equivalente
sum(dpois(0:10, lambda = 15)) # Equivalente

# plot
pois_sum(lambda = 15, ub = 10, lwd = 2, #ub = cauda de cima
         ylab = "P(X = x)", xlab = "Visitas por hora")

pois_sum(lambda = 15, lb = 20, lwd = 2, #lb = cauda de baixo
         ylab = "P(X = x)", xlab = "Visitas por hora")

# A probabilidade de receber entre 10 e 20 visitas

ppois(20, lambda = 15) - ppois(10, lambda = 15) # 0.7985647 ou 79.86%
sum(dpois(11:20, lambda = 15)) # Equivalente

pois_sum(lambda = 15, lb = 10, ub = 20, lwd = 2,
         ylab = "P(X = x)", xlab = "Visitas por hora")
