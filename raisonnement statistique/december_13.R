## Logistic regression

logit <- function(p) {
  return(log(p/(1-p)))
}

ilogit <- function(z) {
  return(1/(1+exp(-z)))
}

logit_loss <- function(theta, y, x) {
  intercept <- theta[1]
  slope <- theta[2]
  z_pred <- intercept + slope*x
  p_pred <- ilogit(z_pred)
  ll <- sum(log((p_pred^y)*((1-p_pred)^(1-y))))
  return(-ll)
}

best_linear <- function(y, x, cost) {
  o <- optim(c(0,0), cost, y=y, x=x, method="BFGS")
  return(o$par)
}

asd <- readr::read_csv("https://bit.ly/2zJh19E")
asd_summary <- dplyr::group_by(asd, Age_Month, Group) %>%
  dplyr::summarize(percent_correct=mean(Correct))
ggplot2::ggplot(asd_summary,
                ggplot2::aes(y=percent_correct, x=Age_Month)) +
  ggplot2::geom_point() +
  ggplot2::facet_grid(Group ~ .)

curve(ilogit, from=-9, to=9)

fit_asd <- best_linear(as.numeric(asd$Correct), asd$Age_Month,
                       logit_loss)


asd_summary_logit <- dplyr::group_by(asd, Age_Month) %>%
  dplyr::summarize(logit_percent_correct=logit(mean(Correct)))
ggplot2::ggplot(asd_summary_logit,
                ggplot2::aes(y=logit_percent_correct,
                             x=Age_Month)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept=fit_asd[1],
                       slope=fit_asd[2])


fit_asd_lm <- glm(Correct ~ Age_Month, family="binomial", data=asd)

asd$Group_c <- ifelse(asd$Group == "Typically developing children", 0, 1)
fit_asd_mult <- glm(Correct ~ Age_Month + Group_c, family="binomial", data=asd)

asd_summary_logit_bygroup <-
  dplyr::group_by(asd, Age_Month, Group) %>%
  dplyr::summarize(logit_percent_correct=logit(mean(Correct)))

asd_coefs <- tibble::tibble(
  Group=c("Typically developing children",
          "ASD subjects"),
  Intercept=c(coef(fit_asd_mult)[1],
              coef(fit_asd_mult)[1] + coef(fit_asd_mult)[3]),
  Age_Effect=c(coef(fit_asd_mult)[2])
)

ggplot2::ggplot(asd_summary_logit_bygroup,
                ggplot2::aes(y=logit_percent_correct,
                             x=Age_Month)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(data=asd_coefs,
                       ggplot2::aes(intercept=Intercept, slope=Age_Effect)) +
  ggplot2::facet_grid(Group ~ .)


# Test statistics for logistic regression

z_h0 <- ifelse(
  asd$Group == "ASD subjects",
  coef(fit_asd_mult)[1] + coef(fit_asd_mult)[3],
  coef(fit_asd_mult)[1]
)
p_h0 <- ilogit(z_h0)

N_SAMPLES <- 9999
statistics <- NULL
for (i in 1:N_SAMPLES) {
  y_fake <- NULL
  for (j in 1:nrow(asd)) {
    y_fake[j] <- sample(c(0, 1), 1, prob=c(1-p_h0[j], p_h0[j]))
  }
  fit_fake <- glm(y_fake ~  asd$Age_Month + asd$Group_c, family="binomial")
  statistics[i] <- coef(fit_fake)[2]
}



lm(RT ~ Condition, spr)
library(ggplot2)
ggplot(data = spr) + 
  geom_point(mapping = aes(x = Condition, y = RT), stat = "summary")
  geom_hline(intercept = 733.49, slope = -60.44)






