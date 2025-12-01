library(dplyr)
library(ggplot2)

# neste primeiro laboratório, vamos estimar o tamanho da população de ursos pardos de uma determinada área

# no experimento de marcação e recaptura, na primeira sessão, foram capturados (marcados) 23 ursos pardos; na segunda sessão, foram capturados 19 ursos, sendo que, destes 19, 4 foram observados também na primeira sessão.

# admitindo, num primeiro momento que a população N de ursos pardos é igual a 100, qual a probabilidade de observarmos 4 ursos marcados na segunda sessão? A resposta para esta pergunta é dada por uma distribuição hipergeométrica.

?dhyper
dhyper(x = 4,
       m = 23,
       n = 77,
       k = 19)

# podemos calcular a probabilidade de observarmos diferentes números de ursos marcados na segunda sessão, variando o valor de x entre 0 e 19, isto é, a densidade de probabilidade da variável aleatória "número de ursos marcados recapturados na segunda sessão", quando N = 100 é:
densidade <- dhyper(x = 0:19,
                    m = 23,
                    n = 77,
                    k = 19)

# vamos visualizar essa distribuição de probabilidade:
df <- data.frame(capturados = 0:19,
                 densidade)
head(df)

df |>
  ggplot(aes(x = capturados, y = densidade)) +
  geom_col() +
  scale_x_continuous(breaks = 0:19) +
  theme_bw()

# do gráfico acima, observe que a maior probabilidade é observarmos 4 ursos marcados na segunda sessão, quando N = 100.

# para os cálculos acima, admitimos que N = 100. Mas, na verdade, não sabemos o valor de N. Podemos então estimá-lo a partir dos dados observados (4 ursos marcados recapturados na segunda sessão), utilizando a abordagem bayesiana.

# neste primeiro momento, vamos considerar que o valor de N pode variar entre 50 e 500, e que todos esses valores são igualmente prováveis a priori (distribuição uniforme a priori).

intervalo <- 50:500
length(intervalo)

# a distribuição a posteriori de N sem padronização, dado que observamos 4 ursos marcados na segunda sessão, é dada por:
posteriori_sem_padronizacao <- (1/451)*dhyper(x = 4,
                             m = 23,
                             n = intervalo - 23,
                             k = 19)

posteriori <- posteriori_sem_padronizacao/(sum(posteriori_sem_padronizacao))

df2 <- data.frame(N = 50:500,
                  posteriori)


df2 |>
  ggplot(aes(x = N, y = posteriori))+
  geom_line() +
  theme_bw()

sum(df2$posteriori[df2$N < 70]) # probabilidade de N ser menor que 70 a partir da posteriori

