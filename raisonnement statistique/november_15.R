# Test de signe

arbuthnot <- readr::read_csv("arbuthnot.csv")

# On constate que, de maniere systematique, il y a plus
# de garcons qui naissent que de filles
plot(boys ~ year, data=arbuthnot, ylim=c(2000, 9000))
plot(girls ~ year, data=arbuthnot, ylim=c(2000, 9000))

# Plotter garcons et filles sur le meme graphique
arbuthnot_long <- tidyr::gather(arbuthnot,
                                value="births",
                                key="sex",
                                boys, girls)
ggplot2::ggplot(arbuthnot_long,
                ggplot2::aes(y=births, x=year,
                             colour=sex)) +
  ggplot2::geom_point() +
  ggplot2::scale_color_brewer(type="qual", palette="Set1")


# Compter nombre d'annees qu'il y a plus de garcons
# que de filles
n_male_years <- sum(arbuthnot$boys > arbuthnot$girls)

# On pourrait generer un dataset artificiel sous l'hypothese que
# les naissances sont equiprobable 
# Le principe d'un test de signe est qu'on peut simplement
# reflechir au resume suivant d'un tel dataset:
# le nombre d'annees qu'il y a plus de garcons que de filles
rbinom(1, 82, 0.5)
rbinom(1, 82, 0.5)
rbinom(1, 82, 0.5)
rbinom(1, 82, 0.5)

# Si on echantillonne plusieurs fois sous cette hypothese
# nous pouvons voir les predictions en terme de nombre
# d'annees "garcon" (i.e. plus de garcon que de filles)
stats_hypothese_nulle <- rbinom(1000, 82, 0.5)
hist(stats_hypothese_nulle, xlim=c(24,82))

# On voit que notre resultat est tres loin des predictions
# de cette hypothese
abline(v=n_male_years)

# Notion de p-value

# Probabilite d'avoir exactement 82 annees garcons
# sous cette hypothese
dbinom(n_male_years, 82, 0.5)

# Pour calculer ce qu'on appelle une p-value,
# on fait presque cette demarche. L'idee etant que
# un resultat de 82 serait tres inattendu sous 
# l'hypothese d'equiprobabilite, on calcule la probabilite
# d'un resultat comme celui qu'on a vu OU PIRE
# pour cette hypothese. Donc, ce qu'on ferait si
# on essayait d'evaluer l'hypothese avec cette demarche
# mais on avait constate 80 annees garcon (et non pas 82)
# est qu'on calculerait la probabilite d'au moins
# 80 annees garcon (vu que 81, 82 sont encore plus
# douteux comme resultat)
1 - pbinom(81, 82, 0.5)
# Ici on nous dit que la probabilite est de zero, on
# sait que ce n'est pas le cas. Ca doit etre un probleme
# numerique. Voila de la magie pour regler le probleme.
pbinom(81, 82, 0.5, lower.tail = FALSE)
pbinom(81, 82, 0.5, lower.tail = FALSE, log=TRUE)

# C'est une probabilite qui est tres proche a zero.
# Ca veut dire que ce resultat est tres tres inattendu
# sous l'hypothese que les naissances sont equiprobables.

# Il existe des seuils standards pour passer a une
# p-value a la certitude totale, ce qui sont, nautrellement,
# artificielles, parce qu'il n'y a pas de certitude.
# Mais les gens sont "contents" de supposer qu'une p-value
# en dessous de 0.05 (1/20) est raisonnablement improbable
# (meme si, si on prend le cas actuel, on arrive a ca
# deja ayant juste 49 annees masculines, ce qui est quand
# meme pas si improbable que ca)
pbinom(48, 82, 0.5, lower.tail = FALSE)

# Exemples de loi probabilistes
sample_bernoulli <- function(p) {
  return(sample(c("FACE", "PILE"), 1, prob=c(p, 1-p)))
}

sample_binomial <- function(n, p) {
  successes <- 0
  for (i in 1:n) {
    coin_flip <- sample_bernoulli(p)
    if (coin_flip == "FACE") {
      successes <- successes + 1
    }
  }
  return(successes)
}
sample_binomial(82, 0.5)
sample_binomial(82, 0.5)
sample_binomial(82, 0.5)

# Loi Poisson-binomiale est comme une loi binomiale
# mais avec des probabilites potentiellement differente
# sur chaque essai de la sequence d'essais Bernoullis
sample_poisson_binomial <- function(p_vec) {
  n <- length(p_vec)
  successes <- 0
  for (i in 1:n) {
    coin_flip <- sample_bernoulli(p_vec[i])
    if (coin_flip == "FACE") {
      successes <- successes + 1
    }
  }
  return(successes)  
}

# Pour montrer ce que ca donne je vais generer
# des probabilites aleatoirement
# Juste pour voir ce qui se passe, je genere
# des probabilites <= 0.50, donc, la piece a toujours
# une probabilite plus elevee de tomber PILE
probabilites <- runif(60, min=0, max=0.50)
poisson_binomial_samples <- rep(0, 10000)
for (i in 1:10000) {
  poisson_binomial_samples[i] <- sample_poisson_binomial(probabilites)
}
hist(poisson_binomial_samples)




