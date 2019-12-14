# Ce n'est pas le cas tous les effets
# d'une variable sur une autre sont lineaires.
# On peut facilement avoir des effets
# non-lineaires.
nonlinear_1 <- NULL
for (explanatory in 1:9) {
  new_response <- sample(spr_ungram$logRT,
                         replace=TRUE) +
    cos(explanatory)*0.5
  new_table <- tibble::tibble(Explanatory=explanatory,
                              Response=new_response)
  nonlinear_1 <- dplyr::bind_rows(nonlinear_1,
                                  new_table)
}
nonlinear_1_summary <- nonlinear_1 %>%
  dplyr::group_by(Explanatory) %>%
  dplyr::summarize(meanResponse=mean(Response),
                   medianResponse=median(Response)) %>%
  dplyr::ungroup()
ggplot2::ggplot(nonlinear_1,
                ggplot2::aes(x=Response)) +
  ggplot2::geom_histogram(fill="lightgrey", 
                          colour="black",
                          binwidth=0.2) +
  ggplot2::geom_vline(data=nonlinear_1_summary,
                      ggplot2::aes(xintercept=meanResponse)) +
  ggplot2::geom_vline(data=nonlinear_1_summary, 
                      ggplot2::aes(xintercept=medianResponse),
                      lty="dashed") +
  ggplot2::facet_grid(Explanatory ~ .)


# Critere de squared error: l'idee est qu'on cherche
# une ligne (m = pente, b = intercepte) qui est
# la plus proche que possible aux donnees. Concretement,
# on va minimiser la somme des erreurs carrees, c'est
# a dire \sum_i (y_i - ychapeau_i)^2 , ou ychapeau_i
# est m*x_i + b, i.e. la prediction de la ligne proposee.
squared_error <- function(theta, y, x) {
  intercept <- theta[1]
  slope <- theta[2]
  y_pred <- intercept + slope*x
  result <- sum((y - y_pred)^2)
  return(result)
}

# En R, je peux faire des optimisations avec
# une fonction qui s'appelle optim() qui prend
# en entree une initialisation (les valeurs de
# depart pour, dans notre cas, b et m), un critere
# forme de fonction a MINIMISER (i.e. la "loss"),
# et des arguments supplementaires, et qui nous
# retourne des parametres qui, selon optim(), sont
# optimaux, dans notre cas, la ligne la plus "proche"
# a l'ensemble de donnees.
best_linear <- function(y, x) {
  o <- optim(c(0,0), squared_error, y=y, x=x,
             method="BFGS")
  return(o$par)
}

# Je teste
best_linear(linear$logRT, linear$freq)


# Avant, nous avons un jeu de donnees
# simules qui etait "realiste" dans le
# sens que, meme si les points suivaient
# la meme distribution pour chaque valeur
# de x, il ne s'agissait pas des memes valeurs
# de y. Ca veut dire qu'il y a des differences
# dans la position de y (entre valeurs de x)
# qui sont dues a l'effet lineaire, et il y
# a aussi des differences dues a l'echantillonnage.
# Notre optimisateur ne recupere pas
# la "verite" de -0.5 parce qu'il n'est
# possible de faire la vraie separation
# entre l'effet lineaire et l'effet de bruit/
# echantillonnage. Mais on peut montrer que,
# dans la limite, avec de plus en plus de
# donnees, on s'approche a la bonne reponse
# avec le critere erreur carree.
#
# On peut montrer aussi que s'il y a zero
# effet d'echantillonnage, on trouve
# exactement la bonne reponse.
linear_2 <- NULL
for (frequence in 2:9) {
  new_response <- spr_ungram$logRT - frequence*0.5
  new_data <- tibble::tibble(logRT=new_response,
                             freq=frequence)
  linear_2 <- dplyr::bind_rows(linear_2, new_data)
}
best_linear(linear_2$logRT, linear_2$freq)

# R contient une fonction qui resolut le
# meme probleme (d'une autre maniere car
# il existe pour le modele lineaire simple
# une solution analytique)
lm(logRT ~ freq, linear)
lm(logRT ~ freq, linear_2)
lm(Response ~ Explanatory, nonlinear_1)


# On peut avoir deux ou plusieurs variables
# qui ont un effet lineaire sur nos donnees,
# qui (selon le modele lineaire) s'ajoutent, c'est
# a dire, par addition. 
linear_multiple <- NULL
for (expl_1 in 2:9) {
  for (expl_2 in seq(-0.7, 0.4, by=0.1)) {
    new_responses <- sample(spr_ungram$logRT,
                            12, replace=T) +
      expl_1*0.5 + expl_2*(-2)
    new_table <- tibble::tibble(Explanatory_1=expl_1,
                                Explanatory_2=expl_2,
                                Response=new_responses)
    linear_multiple <- dplyr::bind_rows(
      linear_multiple, new_table)
  }
}
ggplot2::ggplot(linear_multiple,
                ggplot2::aes(x=Response)) +
  ggplot2::geom_histogram() +
  ggplot2::facet_grid(Explanatory_1 ~ Explanatory_2)

lm(Response ~ Explanatory_1 + Explanatory_2, data=linear_multiple)


# Si je plotte les deux effets individuellement,
# on peut voir concretement le sens de ces deux
# coefficients
plot_linear <- function(d, resp_var, expl_var) {
  linear_parameters <- best_linear(d[[resp_var]], d[[expl_var]])
  d_summary <- d %>%
    dplyr::group_by(get(expl_var)) %>%
    dplyr::summarize(meanResponse=mean(get(resp_var)),
                     medianResponse=median(get(resp_var))) %>%
    dplyr::ungroup()
  names(d_summary)[1] <- expl_var
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y=resp_var, x=expl_var)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(slope=linear_parameters[2], intercept=linear_parameters[1],
                colour="blue") +
    ggplot2::geom_point(data=d_summary, ggplot2::aes(y=meanResponse), size=3.5) +
    ggplot2::geom_point(data=d_summary, ggplot2::aes(y=meanResponse),
               colour="red", size=3)   
  return(p)
}

plot_linear(linear_multiple,
            "Response", "Explanatory_1")
plot_linear(linear_multiple,
            "Response", "Explanatory_2")


# Si je "code" les variables categoriques avec
# des valeurs numeriques, je peux souvent profiter
# de l'ensemble d'outils et de raisonnement relies
# aux modeles lineaires, pour poser des questions
# telles que "est-ce qu'il y a une difference
# de position entre ces deux groupes" (i.e. question
# t-test)
sprd <- spr
sprd$ConditionCode <- ifelse(sprd$Condition == "Grammatical",
                             0, 1)
dplyr::group_by(sprd, Condition) %>%
  dplyr::summarize(meanLogRT=mean(logRT)) %>%
  dplyr::ungroup()
m_opt <- best_linear(sprd$logRT, sprd$ConditionCode)

