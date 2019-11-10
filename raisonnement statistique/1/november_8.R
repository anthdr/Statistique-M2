load("spr_english.rda")
spr <- spr_english
spr$logRT <- log(spr$RT)
hist(spr$RT)
hist(spr$logRT)
# L'objectif d'une transformation log
# sur des donnees RT est de les rendre
# un peu plus symetriques

# Creer des data frames separes pour
# chaque condition
spr_gram <- dplyr::filter(spr, Condition == "Grammatical")
spr_ungram <- dplyr::filter(spr, Condition == "Ungrammatical")

# Visualiser donnees des deux conditions separement
hist(spr_gram$logRT)
abline(v=median(spr_gram$logRT), col="red")
hist(spr_ungram$logRT)
abline(v=median(spr_ungram$logRT), col="red")

# Je veux savoir si la mediane des deux groupes
# est differente parce que je constate que
# les deux histogrammes ont une forme differente
# telle que "gram" a plus de donnees "longues"
# que "ungram"
median(spr_gram$logRT) - median(spr_ungram$logRT)


# Ce que je cherche a faire pour generer
# les predictions de l'hypothese que, malgre
# tout, la difference numerique entre les observations
# dans les conditions ne soit due qu'au hasard
# (e.g. je n'ai jamais exactement le meme temps
# de reponse), est d'avoir des donnees qui sont
# des donnees comme des donnees de RT que j'ai
# mais ou j'ai fait en sorte de casser le lien
# entre Condition et logRT

# On peut arriver a des telles fausses donnees
# en faisant une permutation d'une des
# variables concernees, c'est a dire, en
# changeant l'ordre de logRT dans mon data
# frame aleatoirement pour qu'il n'y ait
# plus de lien avec Condition
spr_fake <- spr
spr_fake$logRT <- sample(spr$logRT)
spr_fake_gram <- dplyr::filter(spr_fake, Condition == "Grammatical")
spr_fake_ungram <- dplyr::filter(spr_fake, Condition == "Ungrammatical")

hist(spr_fake_gram$logRT)
abline(v=median(spr_fake_gram$logRT), col="red")
hist(spr_fake_ungram$logRT)
abline(v=median(spr_fake_ungram$logRT), col="red")

# Les medianes maintenant sont assez proches
median(spr_fake_gram$logRT) - median(spr_fake_ungram$logRT)

# Exemples de boucles
chiffre <- 10
max_chiffre <- 10000
while (chiffre < max_chiffre) {
  chiffre <- chiffre + 10
  print(chiffre)
}


for (v in c(3, 4, 6, 7)) {
  w <- v*10
  print(w)
}

# Faire 1000 fois notre demarche de generer des
# fausses donnêes
N_ITER <- 1000
statistiques <- c()
for (i in 1:N_ITER) {
  spr_fake <- spr
  spr_fake$logRT <- sample(spr$logRT)
  spr_fake_gram <- dplyr::filter(spr_fake, Condition == "Grammatical")
  spr_fake_ungram <- dplyr::filter(spr_fake, Condition == "Ungrammatical")
  stat <- median(spr_fake_gram$logRT) - median(spr_fake_ungram$logRT)
  statistiques[i] <- stat
}

stat_obs <- median(spr_gram$logRT) - median(spr_ungram$logRT)
hist(statistiques, col="yellow", xlim=c(-0.2, 0.2))
abline(v=stat_obs, col="blue")

# Prend en entrêe un data frame et le nom d'une variable dans 
# ce data frame (codê en chaine de caracteres), et retourne
# un data frame identique sauf avec l'ordre de 'var' permute
randomiser_var <- function(d, var) {
  result <- d
  result[[var]] <- sample(d[[var]])
  return(result)
}

# Prend en entrêe un data frame, le nom d'une variable dans
# ce data frame qui contient deux valeurs sur laquelle je veux
# splitter les donnêes, ainsi que les deux valeurs de cette variable,
# et le nom d'une deuxieme variable.
# Retourne la mediane de la deuxieme variable pour le sous-groupe
# d'observations correspondantes a la premiere valeur de la premiere
# variable moins la mediane correspondante pour le deuxieme groupe
diff_medianes <- function(d, split_var, group1, group2, dep_var) {
  g1 <- dplyr::filter(d, d[[split_var]] == group1)
  g2 <- dplyr::filter(d, d[[split_var]] == group2)
  stat <- median(g1[[dep_var]]) - median(g2[[dep_var]])
  return(stat)
}

# Prend en entrêe un data frame, le nom d'une variable dans
# ce data frame qui contient deux valeurs sur laquelle je veux
# splitter les donnêes, ainsi que les deux valeurs de cette variable,
# et le nom d'une deuxieme variable.
# Retourne la moyenne de la deuxieme variable pour le sous-groupe
# d'observations correspondantes a la premiere valeur de la premiere
# variable moins la moyenne correspondante pour le deuxieme groupe
diff_moyennes <- function(d, split_var, group1, group2, dep_var) {
  g1 <- dplyr::filter(d, d[[split_var]] == group1)
  g2 <- dplyr::filter(d, d[[split_var]] == group2)
  stat <- mean(g1[[dep_var]]) - mean(g2[[dep_var]])
  return(stat)
}

# Version plus generique
N_ITER <- 1000
statistiques <- c()
for (i in 1:N_ITER) {
  # Etape 1: je genere un tableau tout comme mon tableau de base
  # SAUF que les donnês representent l'hypothese que il n'y
  # a rien que le pur hasard qui donne lieu aux differences
  # constatêes
  spr_fake <- randomiser_var(spr, "logRT")
  # Etape 2: je genere une statistique de test
  # pour ces donnêes inventês
  #stat <- diff_medianes(spr_fake, "Condition", "Grammatical", "Ungrammatical", "logRT")
  stat <- diff_moyennes(spr_fake, "Condition", "Grammatical", "Ungrammatical", "logRT")
  statistiques[i] <- stat
}
stat_obs <- mean(spr_gram$logRT) - mean(spr_ungram$logRT)
hist(statistiques, col="yellow", xlim=c(-0.2, 0.2))
abline(v=stat_obs, col="blue")
