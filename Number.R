load("number.RData")

#ex.dataadj$agreement <- ex.dataadj$agreement_new
Adj <- data.frame(
                 Type=paste0("Adj_", ex.dataadj$position),
                 POS="Adj",
                 Agreement=as.numeric(ex.dataadj$agreement_new),
                 Text = as.factor(ex.dataadj$Text),
                 Year = as.integer(ex.dataadj$Year),
                 YearZ = as.numeric(scale(ex.dataadj$Year)),
                 Dialect = as.factor(ex.dataadj$Dialect)
                 )
Det <- data.frame(
                 Type=paste0("Det_", ex.dataquant$quant),
                 POS="Det",
                 Agreement=as.numeric(ex.dataquant$agreement_new),
                 Text = as.factor(ex.dataquant$Text),
                 Year = as.integer(ex.dataquant$Year),
                 YearZ = as.numeric(scale(ex.dataquant$Year)),
                 Dialect = as.factor(ex.dataquant$Dialect)
)

Adj$Dialect <- factor(Adj$Dialect, levels = c("Southern", "WM", "EM", "Northern"))
Det$Dialect <- factor(Det$Dialect, levels = c("Southern", "WM", "EM", "Northern"))

Agreement <- rbind(Adj, Det)
Agreement <- transform(Agreement,
                       Type = as.factor(Type),
                       POS = as.factor(POS)
                       )
Adj <- transform(Det,
                 Type = as.factor(Type),
                 POS = as.factor(POS)
)
Det <- transform(Det,
                 Type = as.factor(Type),
                 POS = as.factor(POS)
                 )

# Absolute null model for POS (Adj/Det)
Agr00 <- glm(Agreement~POS, data = Agreement, family=binomial)
summary(Agr00)
vioplot(predict(Agr00, type="response")~Agreement$Agreement)

# Model corresponding to the faulty maths in the paper.
Agr0 <- glm(Agreement~POS*Dialect, data = Agreement, family=binomial)
summary(Agr0)
vioplot(predict(Agr0, type="response")~Agreement$Agreement)

# Add Text.
Agr1 <- glmer(Agreement~POS*Dialect+(1|Text), data = Agreement, family=binomial)
summary(Agr1)
vioplot(predict(Agr1, type="response")~Agreement$Agreement)
compare_performance(Agr1, Agr0)

# Add Time.
Agr2 <- glmer(Agreement~POS*Dialect+YearZ+(1|Text), data = Agreement, family=binomial)
summary(Agr2)
vioplot(predict(Agr2, type="response")~Agreement$Agreement)
compare_performance(Agr2, Agr0)


# Plot virtual paper modell against Full model
plot(predict(Agr2, type="response"), predict(Agr0, type="response"),
     pch=20, bty ="n",
     xlim = c(0,1), ylim = c(0,1),
     col = "black",
     xlab = "Estimates of the model equivalent to the calculations in the paper",
     ylab = "Estimates of model with more complex structure"
)

# Base rate and correctness of full model
Baserate <- length(which(Agreement$Agreement==1))/length(Agreement$Agreement)

Diffusion00 <- table(ifelse(predict(Agr00, type="response")<0.5, 0, 1), Agreement$Agreement)
Correct00 <- (Diffusion00[1,1]+Diffusion00[2,2])/sum(Diffusion00)

Diffusion0 <- table(ifelse(predict(Agr0, type="response")<0.5, 0, 1), Agreement$Agreement)
Correct0 <- (Diffusion0[1,1]+Diffusion0[2,2])/sum(Diffusion0)

# Base rate and correctness of full model
Diffusion2 <- table(ifelse(predict(Agr2, type="response")<0.5, 0, 1), Agreement$Agreement)
Correct2 <- (Diffusion2[1,1]+Diffusion2[2,2])/sum(Diffusion2)

# Add interaction of Time and Dialect.
Agr3a <- glmer(Agreement~POS*Dialect*YearZ+(1|Text), data = Agreement, family=binomial)
summary(Agr3a)

#library(rstanarm)
#Agr3 <- stan_glmer(Agreement~POS*Dialect*YearZ+(1|Text), data = Agreement, family=binomial)
#summary(Agr3)

