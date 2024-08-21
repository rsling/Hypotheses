load("nullsub.RData")

require(lme4)
require(performance)
require(MuMIn)
require(vioplot)
require(LaplacesDemon)

## Orality vs. pronouns LM and plot
## Remove two outliers, try again.

orality.rs <- orality[-c(16,25),]

xmdl.rs <- lm(OVERT_RATE ~ ORSCORE, data = orality.rs)
summary(xmdl.rs)


orality_plot_regression.rs <- ggplot(data = orality.rs, aes(x = ORSCORE, y = OVERT_RATE)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x="ORSCORE", y="Proportion of overt pronouns") +
  geom_point(size=3.0, aes(pch = Country, color = Country)) +
  theme_bw() + scale_color_npg()
orality_plot_regression.rs

res_orality.rs <- simulateResiduals(xmdl.rs, plot = T)

## Hierarchical modelling of the RAW data

# Data prep as in Rmd for paper.

subj.rs <- within(mydata, Country <- relevel(factor(Country), ref = "Spain"))
subj.rs <- within(subj.rs, Macro_Region <- relevel(factor(Macro_Region), ref = "Spain"))
#subj.rs <- within(subj.rs, Genre <- relevel(factor(Genre), ref = "SUPP"))
subj.rs %>% select(Region, Genre, sub_POS, Country, Year, ORSCORE, docID, sentenceID, Century, Macro_Region) %>%
  na.omit() %>%
  mutate(Century = as.factor(Century)) -> subj.rs
subj.rs <- subset(subj.rs, subj.rs$docID != "CPMTpoSP")

subj.data.rs <- transform(subj.rs,
                          sub_POS = as.factor(sub_POS),
                          docID = as.factor(docID),
                          Country = as.factor(Country),
                          Region = as.factor(Region),
                          Genre = as.factor(Genre),
                          ORScoreZ = scale(ORSCORE),
                          YearZ = scale(Year)
                          )

save(list=c("orality.rs","subj.data.rs"), file = "nullsubjects.RData")

SubjPaper <- glmer(sub_POS~ORScoreZ*YearZ+Macro_Region+(1|docID), data = subj.data.rs, family="binomial")
summary(SubjPaper)
r2(SubjPaper)
r.squaredGLMM(SubjPaper)
plot(SubjPaper)

# Prediction accuracy. Oops!
p.baseline <- as.numeric(predict(SubjPaper, type="response")>0.5)
mean(p.baseline==as.numeric(subj.data.rs$sub_POS)-1)
table(p.baseline,as.numeric(subj.data.rs$sub_POS)-1)
plot(hist(predict(SubjPaper, type="response")))

# Diverse checks.
check_collinearity(SubjPaper)
check_autocorrelation(SubjPaper) # Oops!
plot(check_model(SubjPaper))
check_heterogeneity_bias(SubjPaper)
check_singularity(SubjPaper)
check_outliers(SubjPaper)
check_overdispersion(SubjPaper)
check_residuals(SubjPaper)
check_predictions(SubjPaper)
check_distribution(SubjPaper)

# Null model only with docID
Subj0 <- glmer(sub_POS~1+(1|docID), data = subj.data.rs, family="binomial")
summary(Subj0)
r2(Subj0)
compare_performance(Subj0, SubjPaper)

# Now extend model from paper / use Country and Genre instead of Year and MacroRegion
Subj1 <- glmer(sub_POS~Country+Genre+(1|docID), data = subj.data.rs, family="binomial")
summary(Subj1)
r2(Subj1)
compare_performance(Subj1, SubjPaper)

# Now extend model from paper / use Country and Genre instead of MacroRegion
Subj2 <- glmer(sub_POS~ORScoreZ*YearZ+Country+Genre+(1|docID), data = subj.data.rs, family="binomial")
summary(Subj2)
r2(Subj2)
compare_performance(Subj2, SubjPaper)

# Now extend model from paper / use Country instead of MacroRegion, no Genre
Subj3 <- glmer(sub_POS~ORScoreZ*YearZ+Country+(1|docID), data = subj.data.rs, family="binomial")
summary(Subj3)
r2(Subj3)
compare_performance(Subj3, SubjPaper)

# Now add a noise predictor to the model from the paper
subj.data.rs.noise <- transform(subj.data.rs, Noise = runif(nrow(subj.data.rs), -2, 2))
Subj4 <- glmer(sub_POS~ORScoreZ*YearZ+Macro_Region+Noise+(1|docID), data = subj.data.rs.noise, family="binomial")
summary(Subj4)
r2(Subj4)
compare_performance(Subj4, SubjPaper)

par(mfrow=c(1,2))
vioplot(predict(SubjPaper, type="response")~subj.data.rs$sub_POS, ylim=c(0,0.3))
vioplot(predict(Subj0, type="response")~subj.data.rs$sub_POS, ylim=c(0,0.3))
par(mfrow=c(1,1))

# Get jitter coords
jitterx <- runif(length(predict(Subj0, type="response")), 0, 0.005)
jittery <- runif(length(predict(Subj0, type="response")), 0, 0.005)

# No jitter if desired
jitterx <- 0
jittery <- 0

# Get color corresponding to outcome in data.
colos <- ifelse(subj.data.rs$sub_POS=="NULL", "orange", "darkgreen")
pichy <- ifelse(subj.data.rs$sub_POS=="NULL", 3, 4)

plot(predict(Subj0, type="response")+jitterx, predict(SubjPaper, type="response")+jittery,
     pch=pichy, bty ="n",
     xlim = c(0,0.3), ylim = c(0,0.3),
     col = colos,
     xlab = "Prediction of baseline model with only a random intercept for document",
     ylab = "Predictions of models with more complex structure"
)

# Compare null model predictions with model from paper
plot(predict(Subj0, type="response"), predict(SubjPaper, type="response"),
     pch=20, bty ="n",
     xlim = c(0,0.3), ylim = c(0,0.3),
     col = 1,
     xlab = "Prediction of baseline model with only a random intercept for document",
     ylab = "Predictions of models with more complex structure"
     )
points(predict(Subj0, type="response"), predict(Subj1, type="response")+jitterx, pch=2, col=2)
points(predict(Subj0, type="response"), predict(Subj2, type="response"), pch=3, col=3)
points(predict(Subj0, type="response"), predict(Subj3, type="response"), pch=4, col=4)
legend("topleft",
       legend = c("ORScore*Year+MacroRegion (paper)", "Country+Genre", "ORScoreZ*Year+Country+Genre", "ORScore*Year+Country"),
       bty ="n",
       cex=0.75,
       pch=c(20,2,3,4),
       col=1:4)


mean(abs(predict(Subj0, type="response")-predict(SubjPaper, type="response")))
max(abs(predict(Subj0, type="response")-predict(SubjPaper, type="response")))
plot(coef(SubjPaper)[["docID"]][,1])

mean(abs(predict(Subj1, type="response")-predict(SubjPaper, type="response")))
max(abs(predict(Subj1, type="response")-predict(SubjPaper, type="response")))

mean(abs(predict(Subj2, type="response")-predict(SubjPaper, type="response")))
max(abs(predict(Subj2, type="response")-predict(SubjPaper, type="response")))

mean(abs(predict(Subj3, type="response")-predict(SubjPaper, type="response")))
max(abs(predict(Subj3, type="response")-predict(SubjPaper, type="response")))

logi <- function(x) {1/(1+exp(-x))}

B00 <- coef(summary(Subj0))[1,1]
CM00 <- as.numeric(unlist(coef(Subj0)["docID"]))
plot(logi(CM00+B00), logi(CM00+B00), pch=4, col=3)

B0 <- coef(summary(SubjPaper))[1,1]
CM0 <- as.numeric(unlist(as.data.frame(coef(SubjPaper)["docID"])[,1]))
plot(logi(CM0+B0), logi(CM+B), pch=4, col=3)

# Simulate significance of noise factor

# Now add a noise predictor to the model from the paper
# simruns <- 1000
# r2m <- (rep(0,simruns))
# r2c <- (rep(0,simruns))
# pvals <- (rep(0,simruns))
# for (i in 1:simruns){
#   .newnoise = rnorm(nrow(subj.data.rs), mean = runif(1,-5,5), sd = 3)
#   .subj4 <- glmer(sub_POS~ORScoreZ*YearZ+Macro_Region+.newnoise+(1|docID), data = subj.data.rs, family="binomial")
#   r2m[i] <- unlist(unname(r2(.subj4)[["R2_marginal"]]))
#   r2c[i] <- unlist(unname(r2(.subj4)[["R2_conditional"]]))
#   pvals[i] <- summary(.subj4)[["coefficients"]][".newnoise", "Pr(>|z|)"]
# }
# plot(pvals)
# plot(r2m)
# plot(r2c)
# length(which(pvals<0.05))
#
# length(which(r2m > unlist(unname(r2(SubjPaper)[["R2_marginal"]]))))
# length(which(r2c > unlist(unname(r2(SubjPaper)[["R2_conditional"]]))))
#
# plot(density(r2m - unlist(unname(r2(SubjPaper)[["R2_marginal"]]))))
