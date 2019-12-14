# T-test

sample_1 <- rnorm(40, 0, 1)
sample_2 <- rnorm(40, 12, 12)
hist(c(sample_1, sample_2), breaks=40)

# Statistique t qui nous sert aussi comme
# statistique de test quand on cherche a
# detecter une difference en tendance generale
# entre deux groupes d'observations
t_stat <- function(x1, x2) {
  diff_in_means <- mean(x1) - mean(x2)
  n1 <- length(x1)
  n2 <- length(x2)
  variance_term <- sqrt(var(x1)/n1 +
                          var(x2)/n2)
  diff_in_means/variance_term
}

sample_1b <- rnorm(5, 0, 10)
sample_2b <- rnorm(7, 4, 20)

hist(c(sample_1b, sample_2b), breaks=40)

# Faire des simulations de deux groupes
# qui ne different pas systematiquement en leur
# tendance centrale

position_globale <- mean(c(sample_1b, sample_2b))
N_ITER <- 9999
statistiques <- c()
for (i in 1:N_ITER) {
  sample_1_h0 <- rnorm(5, position_globale, 10)
  sample_2_h0 <- rnorm(7, position_globale, 20)
  stat <- t_stat(sample_1_h0, sample_2_h0)
  statistiques[i] <- stat
}
hist(statistiques)
abline(v=t_stat(sample_1b, sample_2b), col="red")
# Si tout ce que je cherche a falsifier est l'hypothese
# qu'il n'y a pas de difference systematique dans la
# position des groupes - peu importe si le groupe 1
# ou le groupe 2 est le plus eleve - 
# j'aimerais compter le nombre de fois que j'arrive
# a une statistique aussi extreme dans l'un des deux
# sens - positif ou negatif - sous l'hypothese
# de manque de difference systematique en position
abline(v=-t_stat(sample_1b, sample_2b), col="red")

# Je calcule le pourcentage de tels cas dans mon 
# echantillon,
(sum(abs(statistiques) > abs(t_stat(sample_1b, sample_2b))) + 1)/(N_ITER + 1)
# Cette version en R se base sur la loi theorique t,
# qui suppose que les deux groupes suivent vraiment
# une loi normale. Mais ce n'est pas generalement tres grave;
# en cas de doute, on peut faire autre chose (e.g., un
# test de permutation), mais pour des differences de position
# entre deux groupes, le test t est robuste a la supposition
# de normalite
t.test(sample_1b, sample_2b, var.equal = FALSE)

# Le resultat montre la meme chose qu'on avait
# constate visuellement, ce qui est que les deux
# groupes, en vue de leurs variances qui sont larges,
# sont pas tres distincts numeriquement

# Si je fais la meme chose sur le premier echantillon,
position_globale_a <- mean(c(sample_1, sample_2))
N_ITER <- 9999
statistiques_a <- c()
for (i in 1:N_ITER) {
  sample_1a_h0 <- rnorm(40, position_globale_a, 1)
  sample_2a_h0 <- rnorm(40, position_globale_a, 12)
  stat <- t_stat(sample_1a_h0, sample_2a_h0)
  statistiques_a[i] <- stat
}
hist(statistiques_a, xlim=c(-6,6))
abline(v=t_stat(sample_1, sample_2), col="red")

(sum(abs(statistiques_a) > abs(t_stat(sample_1, sample_2))) + 1)/(N_ITER + 1)
t.test(sample_1, sample_2, var.equal = FALSE)


# Relations lineaires

# J'avais une difference entre deux conditions
library(magrittr)
spr_summary <- spr %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarize(meanLogRT=mean(logRT),
                   medianLogRT=median(logRT))
ggplot2::ggplot(spr, ggplot2::aes(x=logRT)) +
  ggplot2::geom_histogram(fill="lightgrey",
                          colour="black",
                          binwidth = 0.2) +
  ggplot2::geom_vline(data=spr_summary,
                      ggplot2::aes(xintercept=meanLogRT)) +
  ggplot2::geom_vline(data=spr_summary,
                      ggplot2::aes(xintercept=medianLogRT),
                      lty="dashed") +
  ggplot2::facet_grid(Condition ~ .)


# On cherche a visualiser un effet theorique
# d'une variable numerique, telle que frequence
# de mot, sur nos donnees, plutot qu'une variable
# comme "groupe". Un type d'effet qui fait
# la generalisation de ce qu'on voit ici est
# un effet dit "lineaire" qui aurait un effet,
# par exemple, de diminuer la tendance centrale de
# 0.2 sur l'echelle log, a chaque fois que la
# frequence de mot augmente de, par exemple
# 10 sur 1000000 de mots.

# On peut generer un cas un peu plus artificiel
# qui se comporte de cette facon la.
linear <- NULL
for (frequence in 2:9) {
  new_response <- sample(spr_ungram$logRT, replace=TRUE) -
          frequence*0.5
  new_data <- tibble::tibble(logRT=new_response,
                             freq=frequence)
  linear <- dplyr::bind_rows(linear, new_data)
}

# Je fais le meme type de graphique
linear_summary <- linear %>%
  dplyr::group_by(freq) %>%
  dplyr::summarize(meanLogRT=mean(logRT),
                   medianLogRT=median(logRT))
ggplot2::ggplot(linear, ggplot2::aes(x=logRT)) +
  ggplot2::geom_histogram(fill="lightgrey",
                          colour="black",
                          binwidth = 0.2) +
  ggplot2::geom_vline(data=linear_summary,
                      ggplot2::aes(xintercept=meanLogRT)) +
  ggplot2::geom_vline(data=linear_summary,
                      ggplot2::aes(xintercept=medianLogRT),
                      lty="dashed") +
  ggplot2::facet_grid(freq ~ .)


# Il est plus convenable de voir les
# choses d'une autre facon, notamment de
# visualiser non pas par facet mais sur deux
# axes, RT vs frequence
ggplot2::ggplot(linear, ggplot2::aes(y=logRT,
                                     x=freq)) +
  ggplot2::geom_point()

# La pente de la ligne que j'ai construite
# est de -0.5. Pour plotter la ligne il suffit
# de trouver l'intercept, qui va etre de
intercept <- 6.314393 + 0.5
ggplot2::ggplot(linear, ggplot2::aes(y=logRT,
                                     x=freq)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept=intercept,
                       slope=-0.5)










