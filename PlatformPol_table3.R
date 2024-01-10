library(ggplot2)
library(survival)
library(dplyr)
library(tidyr)
library(stargazer)
library(broom)
library(survival)


df <- readRDS('./df2021.rds') 

# ---------------------------------------------------------------------------##
## TRANSFORMATION
## ---------------------------------------------------------------------------##

dfs <- df %>%
  select(id, episode, week, start, stop, change, gap, 
         cum_neg, cum_neg1, cum_neg_policy,cum_neg_policy1, cum_other,cum_other1, cum_total, cum_total1,cum_plat,cum_plat1,cum_reg, 
             cum_reg1, net_income,rival_change, rival_change1)

# ---------------------------------------------------------------------------##
## ANALYSIS
## ---------------------------------------------------------------------------##


# PWP-GAP TIME MODEL Model 1 - Main model, w/ net-income
pwp0.2<-  coxph(Surv(gap,change)~ cluster(id) + cum_neg1  + cum_other1 + cum_total1 +cum_reg1+ rival_change1+ net_income+ strata(episode)
                , data=dfs)

summary(pwp0.2)

# PWP-GAP TIME MODEL Model 2 - Main model, w/ net-income - Policy-specific

pwp0.2p<-  coxph(Surv(gap,change)~ cluster(id) + cum_neg_policy1  + cum_other1 + cum_total1 +cum_reg1+ rival_change1+ net_income+ strata(episode)
                 , data=dfs)

summary(pwp0.2p)

# PWP-GAP TIME MODEL Model 3 - Interaction Neg Coverage*Net-Income

pwp0.1<-  coxph(Surv(gap,change)~ cluster(id) + cum_neg1*net_income+ cum_other1 + cum_total1 +cum_reg1+ rival_change1+  strata(episode)
                , data=dfs)

summary(pwp0.1)


# PWP-GAP TIME MODEL Model 3 - Interaction Neg Coverage*Platform
dfs$id = as.factor(dfs$id)
dfs$id = relevel(dfs$id, ref = 3)

pwp1<-  coxph(Surv(gap,change)~  cluster(id) +cum_neg1*as.factor(id) +cum_other1 + cum_total1 + cum_reg1+ rival_change1 +strata(episode)
              , data=dfs)

pwp1.2<-  coxph(Surv(gap,change)~  cluster(id) +cum_neg1*as.factor(id) +cum_other1 + cum_total1 + cum_reg1+ rival_change1 +net_income +strata(episode)
                , data=dfs)

summary(pwp1)

##------------------------------------------------------------------------------##
## EXPORT TABLES 
## ---------------------------------------------------------------------------##

# Main models

OR2 <- exp(pwp0.2$coef)
CI2 <- exp(confint(pwp0.2))
CI2<- round(CI2, digits =3)
OR2 <- round (OR2, digits =3)

OR3 <- exp(pwp0.2p$coef)
CI3 <- exp(confint(pwp0.2p))
CI3<- round(CI3, digits =3)
OR3 <- round (OR3, digits =3)

OR4 <- exp(pwp0.1$coef)
CI4 <- exp(confint(pwp0.1))
CI4<- round(CI4, digits =3)
OR4 <- round (OR4, digits =3)


stargazer(pwp0.2, pwp0.2p, pwp0.1 ,type = "text", out="./newmodels4.html", title="Effect of cumulative negative coverage on future policy change", align = TRUE,
          coef = list(OR2, OR3, OR4),  ci.custom = list(CI2, CI3, CI4), digits = 2,
          dep.var.labels=c("Policy Change"), t.auto=F, p.auto=F, 
          covariate.labels=c( covariate.labels=c("Neg Coverage", "Neg Coverage - Policy","Non-Neg Coverage",
                                                 "Total Coverage","Regulatory Pressure","Competitor Change", "Neg Coverage*Net Income", "Net Income"),omit.stat=c("LL","ser","f"), calign=TRUE, no.space=TRUE))




