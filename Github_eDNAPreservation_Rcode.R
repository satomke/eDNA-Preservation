#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Preservation Experiment 1: Buffer and Temperature - 1 year ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


##### eDNA Concentrations ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Import Data: 'eDNA.Concentrations_Pres.Exp.I.csv'


## Managing Dataframe

#create new 'Treatment' column combining buffer and temperature
eDNA.Concentrations_Pres.Exp.I$Treatment <- paste(eDNA.Concentrations_Pres.Exp.I$Buffer, eDNA.Concentrations_Pres.Exp.I$Temperature)

#create new 'Technical_sampleID' column combining treatment and technical rep -- THIS PROVIDES A UNIQUE ID FOR EACH FILTER REPLICATE
eDNA.Concentrations_Pres.Exp.I$Technical_sampleID <- paste(eDNA.Concentrations_Pres.Exp.I$Treatment, eDNA.Concentrations_Pres.Exp.I$Time_Cont)
eDNA.Concentrations_Pres.Exp.I$Technical_sampleID <- paste(eDNA.Concentrations_Pres.Exp.I$Technical_sampleID, eDNA.Concentrations_Pres.Exp.I$Technical_rep)

#convert eDNA concentrations to numeric
eDNA.Concentrations_Pres.Exp.I$qs_conc_adjusted<-as.numeric(eDNA.Concentrations_Pres.Exp.I$qs_conc_adjusted)
eDNA.Concentrations_Pres.Exp.I$No_sample_pg.mL<-as.numeric(eDNA.Concentrations_Pres.Exp.I$No_sample_pg.mL)
str(eDNA.Concentrations_Pres.Exp.I)  


## Visualize Data

install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)

#Time as a factor

#Boxplots of eDNA concentrations per Treatment over Time
ggboxplot(eDNA.Concentrations_Pres.Exp.I, x="Time_Factor", y="qs_conc_adjusted", color = "Treatment",
          add = "mean",
          order = c("24 hours", "2 weeks", "1 month", "2 months", "4 months", "6 months", "8 months", "10 months", "1 year"),
          ylab = "eDNA Concentration (pg/mL)", xlab= "Time Preserved") +
  labs(color = "Treatment") +
  theme(legend.background = element_rect(linetype="solid", color = "darkgray"),
        legend.direction = "horizontal",
        plot.title = element_text(size=16),
        axis.title=element_text(size=16)) +
  ggtitle("eDNA Concentrations (Pres Exp I)")


#Boxplots of eDNA concentrations over Time, separated by Treatment
ggplot(eDNA.Concentrations_Pres.Exp.I, aes(x=factor(Time_Factor, level= c("24 hours", "2 weeks", "1 month", "2 months", "4 months", "6 months", "8 months", "10 months", "1 year")), y=qs_conc_adjusted))+
         geom_boxplot()+
  facet_wrap(~Treatment, ncol=1)+
  xlab("Time Preserved") + ylab("eDNA Concentration (pg/mL)")

#Boxplot of eDNA concentrations by treatment
ggplot(eDNA.Concentrations_Pres.Exp.I, aes(x=Treatment, y=qs_conc_adjusted))+
  geom_boxplot()+
  geom_jitter()
          
#Histograms of eDNA concentrations
hist(eDNA.Concentrations_Pres.Exp.I$qs_conc_adjusted) #all data

    #by group
ggplot(eDNA.Concentrations_Pres.Exp.I, aes(x=qs_conc_adjusted))+
  geom_histogram(fill = "white", color= "black")+
  facet_grid(Treatment ~ .)
          

#Time as a continuous variable, not a factor

#eDNA Concentrations over time for each Treatment group - separate plots per Treatment
ggplot(eDNA.Concentrations_Pres.Exp.I, aes(x=Time_Cont, y = qs_conc_adjusted, col = Treatment)) +
  geom_point(aes(color=Treatment))+
  geom_smooth(method = 'lm', se=T)+
  facet_wrap(vars(Treatment))

#eDNA Concentrations over time for each Treatment group - overlaid onto one plot
ggplot(eDNA.Concentrations_Pres.Exp.I, aes(x=Time_Cont, y = qs_conc_adjusted, col = Treatment)) +
  geom_point(na.rm = T, aes(color=Treatment))+
  geom_smooth(method = "lm", se=T)

#eDNA Concentrations over time - overlaid onto one plot
ggplot(eDNA.Concentrations_Pres.Exp.I, aes(x=Time_Cont, y = qs_conc_adjusted)) +
  geom_point(na.rm = T, aes(color=Treatment))+
  geom_smooth(method = 'lm', se=T)

hist(eDNA.Concentrations_Pres.Exp.I$qs_conc_adjusted)
  #by group
ggplot(eDNA.Concentrations_Pres.Exp.I, aes(x=qs_conc_adjusted))+
  geom_histogram(fill = "white", color= "black")+
  facet_grid(Treatment ~ .)


### Remove time Zero (i.e., samples extracted immediately)

#Filter out "Immediate"
install.packages("tidyverse")
library(tidyverse)

conc_data2 = eDNA.Concentrations_Pres.Exp.I %>%
  filter(Time_Cont>0)

str(conc_data2)
hist(conc_data2$qs_conc_adjusted) #skewed right with high-concentration outliers forming an independent cluster

## Visualize Data

#eDNA Concentrations over time for each Treatment group - separate plots per Treatment
ggplot(conc_data2, aes(x=Time_Cont, y = qs_conc_adjusted, col = Treatment)) +
  xlab("Time Preserved (weeks)") + ylab("eDNA Concentration (pg/ul)") +
  geom_point(aes(color=Treatment))+
  geom_smooth(method = 'lm', se=T)+
  facet_wrap(vars(Treatment))

#eDNA Concentrations over time for each Treatment group - overlaid onto one plot
ggplot(conc_data2, aes(x=Time_Cont, y = qs_conc_adjusted, col = Treatment)) +
  xlab("Time Preserved (weeks)") + ylab("eDNA Concentration (pg/ul)") +
  geom_point(na.rm = T, aes(color=Treatment))+
  geom_smooth(method = 'lm', se=T)

#eDNA Concentrations over time - overlaid onto one plot using loess smoothing method
ggplot(conc_data2, aes(x=Time_Cont, y = qs_conc_adjusted, col = Treatment)) +
  xlab("Time Preserved (weeks)") + ylab("eDNA Concentration (pg/ul)") +
  geom_point(na.rm = T, aes(color=Treatment))+
  geom_smooth(method = 'loess', se=T)

#eDNA Concentrations over time - overlaid onto one plot using lm smoothing method
ggplot(conc_data2, aes(x=Time_Cont, y = qs_conc_adjusted)) +
  geom_point(na.rm = T, aes(color=Treatment))+
  geom_smooth(method = 'lm', se=T)



###### GLMM - DNeasy Kit (Frozen & Ethanol) ####
install.packages("lme4")
library(lme4)

# Remove Longmire's preserved samples since these were extracted using a PCI method and cannot be compared directly to Frozen or Ethanol-preserved samples extracted with the Qiagen DNeasy Blood & Tissue kit

conc_data_kit = conc_data2 %>%
  filter(Buffer !="Longmires")
View(conc_data3)

ggplot(conc_data_kit, aes(x=Time_Cont, y = qs_conc_adjusted, col = Treatment)) +
  xlab("Time Preserved (weeks)") + ylab("eDNA Concentration (pg/ul)") +
  geom_point(na.rm = T, aes(color=Treatment))+
  geom_smooth(method = 'lm', se=T)

hist(conc_data_kit$qs_conc_adjusted)

glmm = glmer(qs_conc_adjusted ~ Time_Cont * Treatment + (1|Technical_sampleID),
             data = conc_data_kit,
             family = Gamma(link = "log"), #used log link function instead of null 
             na.action = na.omit, 
             control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=500000)))


summary(glmm)
anova(glmm)
library(car)
Anova(glmm)
#Interaction not significant -- could remove

#Compare with model lacking interaction
glmm2 = glmer(qs_conc_adjusted ~ Time_Cont + Treatment + (1|Technical_sampleID),
              data = conc_data_kit,
              family = Gamma(link = "log"), #used log link function instead of null 
              na.action = na.omit,
              control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=500000)))


summary(glmm2)
anova(glmm2)
anova(glmm, glmm2)
#No statistical difference between models - use simpler one lacking interaction term


# Plot residuals  
ypred = predict(glmm2)
res = residuals(glmm2, type = 'pearson')
plot(ypred,res) # some heteroskedasticity
hist(res) # looks ok
plot(res)


plot(glmm2) # heteroskedastic
qqnorm(residuals(glmm2))
qqline(residuals(glmm2))
hist(residuals(glmm2))


### Remove outliers to see if this helps the fit of the model

## Remove 2 outliers with very high eDNA concentrations: Ethanol 4 i (52 weeks) & Frozen -20 i (26 weeks)

#Remove 9 PCR replicates for Ethanol 4i 1 year (52 weeks)
library(tidyverse)

conc_data_kit2 = conc_data_kit %>%
  filter(Technical_sampleID !="Ethanol 4 52 1")

#Remove 9 PCR replicates for Frozen -20i 6 months (26 weeks)
conc_data_kit3 = conc_data_kit2 %>%
  filter(Technical_sampleID !="Frozen -20 26 1")

#eDNA Concentrations over time for each Treatment group - overlaid onto one plot
ggplot(conc_data_kit3, aes(x=Time_Cont, y = qs_conc_adjusted, col = Treatment)) +
  xlab("Time Preserved (weeks)") + ylab("eDNA Concentration (pg/ul)") +
  geom_point(na.rm = T, aes(color=Treatment))+
  geom_smooth(method = 'lm', se=T)


glmm3 = glmer(qs_conc_adjusted ~ Time_Cont * Treatment + (1|Technical_sampleID),
              data = conc_data_kit3,
              family = Gamma(link = "log"), #used log link function instead of null 
              na.action = na.omit, 
              control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=500000)))


summary(glmm3)
anova(glmm3)
library(car)
Anova(glmm3)
#Interaction not significant -- could remove

glmm4 = glmer(qs_conc_adjusted ~ Time_Cont + Treatment + (1|Technical_sampleID),
              data = conc_data_kit3,
              family = Gamma(link = "log"), #used log link function instead of null 
              na.action = na.omit,
              control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=500000)))


summary(glmm4)
anova(glmm4)
anova(glmm3, glmm4)
#No statistical difference between models - use simpler one lacking interaction term


# Plot residuals  
ypred = predict(glmm4)
res = residuals(glmm4, type = 'pearson')
plot(ypred,res) # some heteroskedasticity, but not horrible
hist(res) # looks ok
plot(res)


plot(glmm4) # heteroskedastic
qqnorm(residuals(glmm4))
qqline(residuals(glmm4))
hist(residuals(glmm4))
#not great residuals


#try scaling time
conc_data_kit3$time_scaled <- scale(conc_data_kit3$Time_Cont)

glmm5 = glmer(qs_conc_adjusted ~ time_scaled + Treatment + (1|Technical_sampleID),
              data = conc_data_kit3,
              family = Gamma(link = "log"), #used log link function instead of null 
              na.action = na.omit,
              control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=500000)))

# Plot residuals  
ypred = predict(glmm5)
res = residuals(glmm5, type = 'pearson')
plot(ypred,res) # some heteroskedasticity
hist(res) # looks ok
plot(res)


plot(glmm5) # heteroskedastic
qqnorm(residuals(glmm5))
qqline(residuals(glmm5))
hist(residuals(glmm5))


#try using gaussian dist
glmm6 = glmer(qs_conc_adjusted ~ time_scaled + Treatment + (1|Technical_sampleID),
              data = conc_data_kit3,
              family = gaussian(link = "log"), #used log link function instead of null 
              na.action = na.omit,
              control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=500000)))

# Plot residuals  
ypred = predict(glmm6)
res = residuals(glmm6, type = 'pearson')
plot(ypred,res)
hist(res)
plot(res)


plot(glmm6) # looks better
qqnorm(residuals(glmm6))
qqline(residuals(glmm6)) #looks better
hist(residuals(glmm6))

#removing outliers, scaling time, and using guassian dist helped meet assumptions

#confirm no interaction is better
glmm7 = glmer(qs_conc_adjusted ~ time_scaled * Treatment + (1|Technical_sampleID),
              data = conc_data_kit3,
              family = gaussian(link = "log"), #used log link function instead of null 
              na.action = na.omit,
              control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=500000)))

anova(glmm6, glmm7) 
#Models:
#glmm6: qs_conc_adjusted ~ time_scaled + Treatment + (1 | Technical_sampleID)
#glmm7: qs_conc_adjusted ~ time_scaled * Treatment + (1 | Technical_sampleID)
#npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)  
#glmm6    7 21564 21598 -10775    21550                       
#glmm7   10 21560 21609 -10770    21540 9.8211  3    0.02015 *

    #interaction better

summary(glmm7)
#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: gaussian  ( log )
#Formula: qs_conc_adjusted ~ time_scaled * Treatment + (1 | Technical_sampleID)
#Data: conc_data_kit3
#Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 5e+05))
#
#AIC      BIC   logLik deviance df.resid 
#21559.7  21608.6 -10769.8  21539.7      980 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-7.4937 -0.4578 -0.0185  0.4542  5.7386 
#
#Random effects:
#  Groups             Name        Variance Std.Dev.
#Technical_sampleID (Intercept)  2164854 1471    
#Residual                       13779893 3712    
#Number of obs: 990, groups:  Technical_sampleID, 110
#
#Fixed effects:
#  Estimate Std. Error t value Pr(>|z|)    
#(Intercept)                     10.37353    0.07492 138.470  < 2e-16 ***
#  time_scaled                      0.01444    0.07469   0.193  0.84674    
#TreatmentEthanol 4              -0.12943    0.10697  -1.210  0.22630    
#TreatmentFrozen -20              0.31811    0.10692   2.975  0.00293 ** 
#TreatmentFrozen -80              0.24064    0.10595   2.271  0.02313 *  
#time_scaled:TreatmentEthanol 4   0.08975    0.10848   0.827  0.40806    
#time_scaled:TreatmentFrozen -20 -0.14025    0.10567  -1.327  0.18444    
#time_scaled:TreatmentFrozen -80  0.18582    0.10563   1.759  0.07857 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr) tm_scl TrtmE4 TrF-20 TrF-80 t_:TE4 t_:TF-2
#time_scaled -0.017                                           
#TrtmntEthn4 -0.700  0.012                                    
#TrtmntFr-20 -0.701  0.012  0.491                             
#TrtmntFr-80 -0.707  0.012  0.495  0.495                      
#tm_scld:TE4  0.011 -0.689  0.015 -0.008 -0.008               
#tm_sc:TF-20  0.012 -0.707 -0.008 -0.013 -0.008  0.487        
#tm_sc:TF-80  0.012 -0.707 -0.008 -0.008 -0.017  0.487  0.500 

anova(glmm7)
#Analysis of Variance Table
#npar  Sum Sq Mean Sq F value
#time_scaled              1  1.7554  1.7554       0
#Treatment                3 22.7156  7.5719       0
#time_scaled:Treatment    3 10.2729  3.4243       0

Anova(glmm7)
#Analysis of Deviance Table (Type II Wald chisquare tests)
#
#Response: qs_conc_adjusted
#Chisq Df Pr(>Chisq)    
#time_scaled            1.5388  1    0.21480    
#Treatment             22.7157  3  4.629e-05 ***
#time_scaled:Treatment 10.2729  3    0.01638 *  


## Compare with model lacking random effect
glmm.fixed.only = glm(qs_conc_adjusted ~ time_scaled + Treatment,
                      data = conc_data_kit3,
                      family = gaussian(link = "log"), #used log link function instead of null 
                      na.action = na.omit)

anova(glmm7, glmm.fixed.only) 
#Data: conc_data_kit3
#Models:
#  glmm.fixed.only: qs_conc_adjusted ~ time_scaled + Treatment
#  glmm7: qs_conc_adjusted ~ time_scaled * Treatment + (1 | Technical_sampleID)
#npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
#glmm.fixed.only    6 22228 22257 -11108    22216                         
#glmm7             10 21560 21609 -10770    21540 675.89  4  < 2.2e-16 ***

    #including random effect greatly improves model



# Plot residuals  
ypred = predict(glmm7)
res = residuals(glmm7, type = 'pearson')
plot(ypred,res)
hist(res) 
plot(res)


plot(glmm7) # looks good
qqnorm(residuals(glmm7))
qqline(residuals(glmm7))
hist(residuals(glmm7))

# Diagnostics
install.packages("DHARMa")
library(DHARMa)

testDispersion(glmm7) #no dispersion
simulationOutput <- simulateResiduals(fittedModel = glmm7, plot = T) #residual plots
residuals(simulationOutput) 
plot(simulationOutput) #residual plots
plotQQunif(simulationOutput) # left plot in plot.DHARMa() separately; should follow diagonal line
plotResiduals(simulationOutput) # right plot in plot.DHARMa() separately; solid lines should pair with dashed lines

#remove NA's from dataset to run following code
install.packages("tidyverse")
library(tidyverse)
conc_data_kit3_na <- conc_data_kit3 %>% drop_na(qs_conc_adjusted)

plotResiduals(simulationOutput, form = conc_data_kit3_na$Treatment) #Ethanol treatments off

testUniformity(simulationOutput) # tests if the overall distribution conforms to expectations
testOutliers(simulationOutput) # tests if there are more simulation outliers than expected
testDispersion(simulationOutput) # tests if the simulated dispersion is equal to the observed dispersion
testDispersion(simulationOutput, alternative = "greater") # tests for overdispersion, specifically
#model is not overdispersed, so it must be underdispersed!
testQuantiles(simulationOutput) # fits a quantile regression or residuals against a predictor (default predicted value), and tests of this conforms to the expected quantile
testGeneric(simulationOutput, mean) # test if a generic summary statistics (user-defined) deviates from model expectations


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### BEST MODEL FOR EXPERIMENT 1 - DNeasy Kit ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
glmm7 = glmer(qs_conc_adjusted ~ time_scaled * Treatment + (1|Technical_sampleID),
              data = conc_data_kit3,
              family = gaussian(link = "log"),
              na.action = na.omit,
              control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=500000)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


###### Post hoc Test ####

#post hoc pairwise comparison of Treatments for experiment 1
install.packages("emmeans")
library(emmeans)
emtrends(glmm7, pairwise ~ Treatment, var = "time_scaled")
#$emtrends
#Treatment   time_scaled.trend     SE  df asymp.LCL asymp.UCL
#Ethanol -20            0.0144 0.0747 Inf   -0.1320    0.1608
#Ethanol 4              0.1042 0.0787 Inf   -0.0500    0.2584
#Frozen -20            -0.1258 0.0747 Inf   -0.2723    0.0207
#Frozen -80             0.2003 0.0747 Inf    0.0539    0.3467
#
#Confidence level used: 0.95 
#
#$contrasts
#contrast                     estimate    SE  df z.ratio p.value
#(Ethanol -20) - Ethanol 4     -0.0897 0.108 Inf  -0.827  0.8415
#(Ethanol -20) - (Frozen -20)   0.1402 0.106 Inf   1.327  0.5455
#(Ethanol -20) - (Frozen -80)  -0.1858 0.106 Inf  -1.759  0.2933
#Ethanol 4 - (Frozen -20)       0.2300 0.109 Inf   2.119  0.1469
#Ethanol 4 - (Frozen -80)      -0.0961 0.108 Inf  -0.886  0.8124
#(Frozen -20) - (Frozen -80)   -0.3261 0.106 Inf  -3.086  0.0109


###### GLMM - PCI (Longmires) ####

## Include only Longmire's preserved samples

conc_lm = conc_data2 %>%
  filter(Buffer =="Longmires")
View(conc_lm)

hist(conc_lm$qs_conc_adjusted)
ggplot(conc_lm, aes(x=qs_conc_adjusted))+
  geom_histogram(fill = "white", color= "black")+
  facet_grid(Treatment ~ .)

ggplot(conc_lm, aes(x=Time_Cont, y = qs_conc_adjusted, col = Treatment)) +
  xlab("Time Preserved (weeks)") + ylab("eDNA Concentration (pg/ul)") +
  geom_point(na.rm = T, aes(color=Treatment))+
  geom_smooth(method = 'lm', se=T)

hist(conc_lm$qs_conc_adjusted)


glmm.lm = glmer(qs_conc_adjusted ~ Time_Cont * Treatment + (1|Technical_sampleID),
                data = conc_lm,
                family = Gamma(link = "log"), #used log link function instead of null 
                na.action = na.omit, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=500000)))


summary(glmm.lm)
anova(glmm.lm)
library(car)
Anova(glmm.lm)
#Interaction not significant -- could remove

glmm.lm2 = glmer(qs_conc_adjusted ~ Time_Cont + Treatment + (1|Technical_sampleID),
                data = conc_lm,
                family = Gamma(link = "log"), #used log link function instead of null 
                na.action = na.omit, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=500000)))
#Convergence issues

#Try scaling time to deal with convergence issues
conc_lm$time_scaled<-scale(conc_lm$Time_Cont)

glmm.lm3 = glmer(qs_conc_adjusted ~ time_scaled + Treatment + (1|Technical_sampleID),
                 data = conc_lm,
                 family = Gamma(link = "log"), #used log link function instead of null 
                 na.action = na.omit, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=500000)))

# Plot residuals  
ypred = predict(glmm.lm3)
res = residuals(glmm.lm3, type = 'pearson')
plot(ypred,res) # looks good
hist(res) # looks good
plot(res)


plot(glmm.lm3) # good
qqnorm(residuals(glmm.lm3)) #good
qqline(residuals(glmm.lm3))
hist(residuals(glmm.lm3)) #good

## Compare with model including interaction term
glmm.lm4 = glmer(qs_conc_adjusted ~ time_scaled * Treatment + (1|Technical_sampleID),
                 data = conc_lm,
                 family = Gamma(link = "log"), #used log link function instead of null 
                 na.action = na.omit, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=500000)))

anova(glmm.lm3, glmm.lm4)
#Data: conc_lm
#Models:
#glmm.lm3: qs_conc_adjusted ~ time_scaled + Treatment + (1 | Technical_sampleID)
#glmm.lm4: qs_conc_adjusted ~ time_scaled * Treatment + (1 | Technical_sampleID)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
#glmm.lm3    5 9911.6 9932.8 -4950.8   9901.6                     
#glmm.lm4    6 9913.5 9939.0 -4950.8   9901.5 0.0294  1     0.8638

      # no interaction better


## Compare with model lacking random effect
glmm.fixed.only = glm(qs_conc_adjusted ~ time_scaled + Treatment,
                      data = conc_lm,
                      family = Gamma(link = "log"), #used log link function instead of null 
                      na.action = na.omit)


anova(glmm.lm3, glmm.fixed.only) #including random effect greatly improves model


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### BEST MODEL FOR EXPERIMENT 1 ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
glmm.lm3 = glmer(qs_conc_adjusted ~ time_scaled + Treatment + (1|Technical_sampleID),
                 data = conc_lm,
                 family = Gamma(link = "log"), #used log link function instead of null 
                 na.action = na.omit, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=500000)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summary(glmm.lm3)
#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: Gamma  ( log )
#Formula: qs_conc_adjusted ~ time_scaled + Treatment + (1 | Technical_sampleID)
#Data: conc_lm
#Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 5e+05))
#
#AIC      BIC   logLik deviance df.resid 
#9911.6   9932.8  -4950.8   9901.6      509 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-1.9414 -0.6119 -0.0364  0.5494  4.7891 
#
#Random effects:
#  Groups             Name        Variance Std.Dev.
#Technical_sampleID (Intercept) 0.1317   0.3629  
#Residual                       0.1717   0.4144  
#Number of obs: 514, groups:  Technical_sampleID, 60
#
#Fixed effects:
#  Estimate Std. Error t value Pr(>|z|)    
#(Intercept)            8.98084    0.12271  73.185   <2e-16 ***
#time_scaled            0.16188    0.09096   1.780   0.0751 .  
#TreatmentLongmires RT  0.14061    0.18079   0.778   0.4367    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr) tm_scl
#time_scaled -0.078       
#TrtmntLngRT -0.684  0.126

anova(glmm.lm3)
#Analysis of Variance Table
#npar  Sum Sq Mean Sq F value
#time_scaled    1 1.59357 1.59357  9.2785
#Treatment      1 0.32829 0.32829  1.9114

Anova(glmm.lm3)
#Analysis of Deviance Table (Type II Wald chisquare tests)
#
#Response: qs_conc_adjusted
#Chisq Df Pr(>Chisq)  
#time_scaled 3.1671  1    0.07514 .
#Treatment   0.6049  1    0.43673  



## Descriptive Statistics

tapply(conc_data4$qs_conc_adjusted, conc_data4$Treatment, mean, na.rm=TRUE)
#Ethanol -20    Ethanol 4   Frozen -20   Frozen -80  Longmires 4 Longmires RT 
#33207.51     29837.30     51174.46     43815.82     10175.36     11175.80 

tapply(conc_data4$qs_conc_adjusted, conc_data4$Treatment, sd, na.rm=TRUE)
#Ethanol -20    Ethanol 4   Frozen -20   Frozen -80  Longmires 4 Longmires RT 
#9774.338    11738.492    27734.307    18010.557     7023.639     7626.570 



## Plotting 

install.packages("RColorBrewer") #color schemes
library("RColorBrewer")
library(ggplot2)
library(ggpubr)

#Plot linear models for each treatment onto one graph - Immediate group and 2 outliers removed
ggplot(conc_data4, aes(x=Time_Cont, y = qs_conc_adjusted, col = Treatment)) +
  geom_point(na.rm = T, aes(color=Treatment))+
  geom_smooth(method = "glm", se=T)


ggplot(conc_data4, aes(x=Time_Cont, y = qs_conc_adjusted, col = Treatment)) +
       ylab("eDNA Concentration (pg/mL)") + xlab("Time Preserved (weeks)") +
  geom_point(na.rm = T, aes(color=Treatment))+
  geom_smooth(method = "glm", se=T) +
  theme(legend.position = c(0.6,0.9),
        legend.title.align=0.5,
        legend.background = element_rect(linetype="solid", color = "lightgray"),
        axis.title.y = element_text(size = 13, margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(size = 11, margin = margin(t = 7, r = 0, b = 0, l = 0)),
        axis.text=element_text(size=10, color = 'black'),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.8),
        panel.grid.major.y = element_line(color = "gray",
                                          size = 0.5,
                                          linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(0.25,0.5,0.25,0.75, unit="cm")) +
  guides(colour = guide_legend(nrow = 1)) #make legend 1 row



#Boxplots of eDNA concentrations over time w/out Immediate samples (no outliers)
ggboxplot(conc_data4, x="Time_Cont", y="qs_conc_adjusted", col = "Treatment",
          size = .4,
          outlier.size = .5,
          ylab = "eDNA Concentration (pg/mL)", xlab= "Time Preserved (weeks)") +
  labs(color = "Treatment") +
  theme(legend.background = element_rect(linetype="solid", color = "darkgray"),
        legend.position = c(0.70, 0.88),
        legend.title = element_blank(), 
        plot.title = element_text(size=14),
        axis.title=element_text(size=14),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm"))+
  stat_summary(fun=mean, geom = "line", size = 0.7, aes(group = Treatment, col = Treatment),
               position = position_dodge(0.8))+ 
  scale_color_brewer(palette = "Paired")+
  guides(colour = guide_legend(nrow = 2)) #make legend 2 row


#Boxplots of eDNA concentrations over time w/ Immediate samples (no outliers)

#Remove 9 PCR replicates for Ethanol 4i 1 year (52 weeks) and Remove 9 PCR replicates for Frozen -20i 6 months (26 weeks)
library(tidyverse)

conc_time2 = eDNA.Concentrations_Pres.Exp.I %>%
  filter(Technical_sampleID !="Ethanol 4 52 1") %>%
  filter(Technical_sampleID !="Frozen -20 26 1")

View(conc_time2)

#Manual colors
#vertical legend
boxplot_conc<-ggboxplot(conc_time2, x="Time_Cont", y="qs_conc_adjusted", col = "Treatment",
                        size = .4,
                        outlier.size = .5,
                        ylab = "eDNA Concentration (pg/mL)", xlab= "Time Preserved (weeks)") +
  labs(color = "Treatment") +
  theme(legend.background = element_blank(),
        legend.position = 'right',
        legend.title = element_blank(), 
        legend.text = element_text(size=11),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm"))+
  stat_summary(fun=mean, geom = "line", size = 0.7, aes(group = Treatment, col = Treatment),
               position = position_dodge(0.8))+
  scale_color_manual(breaks = c("Ethanol -20", "Ethanol 4", "Frozen -20", "Frozen -80", "Longmires RT", "Longmires 4", "Immediate na"),
                     values = c("Ethanol -20"="#F8766D", "Ethanol 4"="#BB9D00", "Frozen -20"="#00Bc59", "Frozen -80"="#00BFC4", "Longmires RT"="#fc61d5", "Longmires 4"="#CF78FF", "Immediate na"="#00A5FF"),
                     labels = c("Ethanol -20", "Ethanol 4", "Frozen -20", "Frozen -80", "Longmires RT", "Longmires 4", "Immediate"))

boxplot_conc

#horizontal legend
boxplot_conc2<-ggboxplot(conc_time2, x="Time_Cont", y="qs_conc_adjusted", col = "Treatment",
                         size = .4,
                         outlier.size = .5,
                         ylab = "eDNA Concentration (pg/mL)", xlab= "Time Preserved (weeks)") +
  labs(color = "Treatment") +
  theme(legend.background = element_blank(),
        legend.position = 'top',
        legend.title = element_blank(), 
        legend.text = element_text(size=8),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm"))+
  stat_summary(fun=mean, geom = "line", size = 0.7, aes(group = Treatment, col = Treatment),
               position = position_dodge(0.8))+
  scale_color_manual(breaks = c("Ethanol -20", "Ethanol 4", "Frozen -20", "Frozen -80", "Longmires RT", "Longmires 4", "Immediate na"),
                     values = c("Ethanol -20"="#F8766D", "Ethanol 4"="#BB9D00", "Frozen -20"="#00Bc59", "Frozen -80"="#00BFC4", "Longmires RT"="#fc61d5", "Longmires 4"="#CF78FF", "Immediate na"="#00A5FF"),
                     labels = c("Ethanol -20", "Ethanol 4", "Frozen -20", "Frozen -80", "Longmires RT", "Longmires 4", "Immediate"))+
  guides(colour = guide_legend(nrow = 1)) #make legend 1 row

boxplot_conc2



##### Detection Rates ####
#~~~~~~~~~~~~~~~~~~~~~~~#

## Import Data: 'Detections_PresExPI.csv'


## Managing Dataframe

#create new 'Treatment' column combining buffer and temperature
Detections_PresExpI$Treatment <- paste(Detections_PresExpI$Buffer, Detections_PresExpI$Temperature)
Detections_PresExpI$Treatment<- as.factor(Detections_PresExpI$Treatment)

#Create new 'Detection rate' column
Detections_PresExpI$detect_rate <- Detections_PresExpI$Detect_count/9


## Visualize Data

#boxplots with lines connecting means
library(ggplot2)
library(ggpubr)
ggboxplot(Detections_PresExpI, x="Time", y="detect_rate", col = "Treatment",
          size = .4,
          outlier.size = .5,
          ylab = "Detection Rate", xlab= "Time Preserved (weeks)") +
  labs(color = "Treatment") +
  theme(legend.background = element_rect(linetype="solid", color = "darkgray"),
        legend.position = "top",
        legend.title = element_blank(), 
        plot.title = element_text(size=14),
        axis.title=element_text(size=14),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm"))+
  stat_summary(fun=mean, geom = "line", size = 0.7, aes(group = Treatment, col = Treatment),
               position = position_dodge(0.8))+ 
  scale_color_brewer(palette = "Paired")+
  guides(colour = guide_legend(nrow = 2)) #make legend 2 row


#barplots
ggplot(data= Detections_PresExpI, mapping = aes(x=factor(Time), y=detect_rate, fill = Treatment))+
  ylab("Detection Rate") + xlab("Time Preserved (weeks)")+
  stat_summary(fun = "mean", geom = "bar", aes(width = 0.65), position = position_dodge(width = 0.75))+
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width = 0.4,
               ymax = 1,
               position = position_dodge(0.75)) +
  theme(legend.background = element_blank(),
        axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 7, b = 0, l = 0)),
        axis.title.x = element_text(size = 12, margin = margin(t = 7, r = 0, b = 0, l = 0)),
        axis.text=element_text(size=10),
        panel.background = element_rect(fill="grey97", colour = "black"),
        panel.grid.major.y = element_line(color = "gray",
                                        size = 0.5,
                                        linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm"))+
  coord_cartesian(ylim = c(0.25,1.0))


#boxplots among treatments
ggplot(data= Detections_PresExpI, mapping = aes(x=Treatment, y=detect_rate, fill = Treatment))+
  geom_boxplot()+
  geom_point(aes(col = Treatment), width = 0.3)
  

## Descriptive Stats

tapply(Detections_PresExpI$detect_rate, Detections_PresExpI$Treatment, mean)
#Ethanol -20    Ethanol 4   Frozen -20   Frozen -80  Longmires 4 Longmires RT 
#1.0000000    1.0000000    1.0000000    1.0000000    0.9444444    0.9603175 

tapply(Detections_PresExpI$detect_rate, Detections_PresExpI$Treatment, max)
#Ethanol -20    Ethanol 4   Frozen -20   Frozen -80  Longmires 4 Longmires RT 
#1            1            1            1            1            1 

tapply(Detections_PresExpI$detect_rate, Detections_PresExpI$Treatment, min)
#Ethanol -20    Ethanol 4   Frozen -20   Frozen -80  Longmires 4 Longmires RT 
#1.0000000    1.0000000    1.0000000    1.0000000    0.4444444    0.5555556 

tapply(Detections_PresExpI$detect_rate, Detections_PresExpI$Treatment, sd)
#Ethanol -20    Ethanol 4   Frozen -20   Frozen -80  Longmires 4 Longmires RT 
#0.0000000    0.0000000    0.0000000    0.0000000    0.1230179    0.1099290 



## Ggplots

# Use conc_time2 data for eDNA concentration boxplots - this is the full eDNA concentration dataset with the two most extreme outlier samples removed

#library(tidyverse)
#conc_time2 = eDNA.Concentrations_Pres.Exp.I %>%
#  filter(Technical_sampleID !="Ethanol 4 52 1") %>%
#  filter(Technical_sampleID !="Frozen -20 26 1")


#Boxplots - eDNA Conc over time, means connected by lines
ggboxplot(conc_time2, x="Time_Cont", y="qs_conc_adjusted", col = "Treatment",
                   size = .3,
                   outlier.size = .3,
                   ylab = "eDNA Concentration (pg/mL)", xlab= "Time Preserved (weeks)") +
  labs(color = "Treatment") +
  theme_bw() +
  theme(legend.background = element_blank(),
        legend.position = 'top',
        legend.title = element_blank(),
        legend.key = element_rect(fill=NA, color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(colour = "black", fill=NA, size=0.8),
        axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 3, b = 0, l = 0)),
        axis.title.x = element_text(size = 11, margin = margin(t = 7, r = 0, b = 0, l = 0)),
        axis.text=element_text(size=10, color = 'black'),
        plot.margin = margin(0.25,0.5,0,0.5, unit="cm"))+
  stat_summary(fun=mean, geom = "line", size = 0.5, aes(group = Treatment, col = Treatment),
               position = position_dodge(0.8))+
  scale_color_manual(breaks = c("Immediate na", "Ethanol -20", "Ethanol 4", "Frozen -20", "Frozen -80", "Longmires RT", "Longmires 4"),
                     values = c("Immediate na"="#00A5FF", "Ethanol -20"="#F8766D", "Ethanol 4"="#BB9D00", "Frozen -20"="#00Bc59", "Frozen -80"="#00BFC4", "Longmires RT"="#fc61d5", "Longmires 4"="#CF78FF"),
                     labels = c("Immediate", "Ethanol -20", "Ethanol 4", "Frozen -20", "Frozen -80", "Longmires RT", "Longmires 4"))+
  guides(colour = guide_legend(nrow = 1)) #make legend 1 row


#Combine boxplots & regression lines onto same plot
ggplot() +
  theme_bw() +
  geom_smooth(data = conc_time2,
              aes(x = Time_Cont,
                  y = qs_conc_adjusted,
                  color = Treatment,
                  fill = Treatment),
              alpha = 0.15,
              method = "glm",
              size=.8,
              se = T) +
  geom_boxplot(data = conc_time2,
               aes(x = Time_Cont,
                   y = qs_conc_adjusted,
                   group = interaction(Time_Factor, Treatment),
                   color = Treatment),
               width = 4,
               outlier.size = .8,
               position = position_dodge(7.5)) +
  scale_x_continuous(limits = c(-5, 56), breaks = c(0, 2, 4, 8, 17, 26, 35, 43, 52), expand = c(0,0)) +
  theme(legend.background = element_blank(),
        legend.position = 'top',
        legend.title = element_blank(),
        legend.key = element_rect(fill=NA, color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(colour = "black", fill=NA, size=0.8),
        axis.title.y = element_text(size = 11, margin = margin(t = 0, r = 3, b = 0, l = 0)),
        axis.title.x = element_text(size = 10, margin = margin(t = 7, r = 0, b = 0, l = 0)),
        axis.text=element_text(size=10, color = 'black'),
        plot.margin = margin(0.25,0.5,0,0.5, unit="cm")) +
  scale_color_manual(breaks = c("Ethanol -20", "Ethanol 4", "Frozen -20", "Frozen -80", "Longmires RT", "Longmires 4", "Immediate na"),
                     values = c("Ethanol -20"="#F8766D", "Ethanol 4"="#BB9D00", "Frozen -20"="#00Bc59", "Frozen -80"="#00BFC4", "Longmires RT"="#fc61d5", "Longmires 4"="#CF78FF", "Immediate na"="#00A5FF"),
                     labels = c("Ethanol -20", "Ethanol 4", "Frozen -20", "Frozen -80", "Longmires RT", "Longmires 4", "Immediate"))+
  xlab("Time Preserved (weeks)")+
  ylab("eDNA Concentration (pg/mL)") 



##### Manuscript Fig 4 ####
library(ggplot2)
library(ggpubr)


# Use for legend
legend.plot <- ggboxplot(conc_time2, x="Time_Cont", y="qs_conc_adjusted", col = "Treatment",
          size = .3,
          outlier.size = .3,
          ylab = "eDNA Concentration (pg/mL)", xlab= "Time Preserved (weeks)") +
  labs(color = "Treatment") +
  theme_bw() +
  theme(legend.background = element_blank(),
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'vertical',
        legend.box.margin=margin(0,0,-5,0),
        legend.spacing.y = unit(-10, "pt"),
        legend.title = element_blank(),
        legend.key = element_rect(fill=NA, color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(colour = "black", fill=NA, size=0.8),
        axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 3, b = 0, l = 0)),
        axis.title.x = element_text(size = 11, margin = margin(t = 7, r = 0, b = 0, l = 0)),
        axis.text=element_text(size=10, color = 'black'),
        plot.margin = margin(0.25,0.5,0,0.5, unit="cm"))+
  stat_summary(fun=mean, geom = "line", size = 0.5, aes(group = Treatment, col = Treatment, linetype = Extraction),
               position = position_dodge(0.8))+
  scale_color_manual(breaks = c("Immediate na", "Ethanol -20", "Ethanol 4", "Frozen -20", "Frozen -80", "Longmires RT", "Longmires 4"),
                     values = c("Immediate na"="#00A5FF", "Ethanol -20"="#F8766D", "Ethanol 4"="#BB9D00", "Frozen -20"="#00Bc59", "Frozen -80"="#00BFC4", "Longmires RT"="#fc61d5", "Longmires 4"="#CF78FF"),
                     labels = c("Immediate", "Ethanol -20", "Ethanol 4", "Frozen -20", "Frozen -80", "Longmires RT", "Longmires 4"))+
  guides(nrow = 2, colour = guide_legend(order = 1, nrow = 1), linetype = guide_legend(order = 2)) #make legend 1 row

legend.plot



#Boxplot for 'Immediate' group only
library(tidyverse)

conc_immed = conc_time2 %>%
  filter(Time_Cont == 0)

boxplot.imm <- ggplot() +
  theme_bw() +
  geom_boxplot(data = conc_immed,
               aes(x = Time_Cont,
                   y = qs_conc_adjusted,
                   color = Treatment),
               width = .4,
               outlier.size = .8)+
  scale_y_continuous(limits = c(0, 160000), breaks = c(0, 50000, 100000, 150000)) +
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = c(0))+
  theme(legend.background = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.key = element_rect(fill=NA, color = NA),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "gray",
                                          size = 0.5,
                                          linetype = 2),
        axis.line = element_blank(),
        panel.background = element_rect(colour = "black", fill=NA, size=0.8),
        axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 3, b = 0, l = 0)),
        axis.title.x = element_blank(),
        axis.text.y=element_text(size=10, color = 'black'),
        axis.text.x=element_text(size=10, color = 'black'),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.20,0,0.45,0.5, unit="cm"))+
  scale_color_manual(breaks = c("Immediate na"),
                     values = c( "Immediate na"="#00A5FF"),
                     labels = c("Immediate"))+
  ylab("eDNA Concentration (pg/mL)")

boxplot.imm

#Boxplots and regression lines for weeks 1-52 -- use 'conc_data4' which has 2 outliers and the Immediate group removed
boxplot.I <- ggplot() +
  theme_bw() +
  geom_smooth(data = conc_data4,
              aes(x = Time_Cont,
                  y = qs_conc_adjusted,
                  color = Treatment,
                  fill = Treatment,
                  linetype = Extraction),
              alpha = 0.15,
              method = "glm",
              size=.8,
              se = T,
              fullrange=TRUE) +
  geom_boxplot(data = conc_data4,
               aes(x = Time_Cont,
                   y = qs_conc_adjusted,
                   group = interaction(Time_Factor, Treatment),
                   color = Treatment),
               width = 2,
               outlier.size = .5,
               position = position_dodge(3)) +
  scale_y_continuous(limits = c(0, 160000), breaks = c(0, 50000, 100000, 150000)) +
  scale_x_continuous(limits = c(0, 54), breaks = c(2, 4, 8, 17, 26, 35, 43, 52), expand = c(0,0)) +
  theme(legend.background = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.key = element_rect(fill=NA, color = NA),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "gray",
                                          size = 0.5,
                                          linetype = 2),
        axis.line = element_blank(),
        panel.background = element_rect(colour = "black", fill=NA, size=0.8),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 10, hjust = 0.46),
        axis.text.y=element_blank(),
        axis.text.x=element_text(size=10, color = 'black'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0.20,0.5,0,0.1, unit="cm")) +
  scale_color_manual(breaks = c("Ethanol -20", "Ethanol 4", "Frozen -20", "Frozen -80", "Longmires RT", "Longmires 4"),
                    values = c("Ethanol -20"="#F8766D", "Ethanol 4"="#BB9D00", "Frozen -20"="#00Bc59", "Frozen -80"="#00BFC4", "Longmires RT"="#fc61d5", "Longmires 4"="#CF78FF"))+
    xlab("Time Preserved (weeks)")+
  ylab("eDNA Concentration (pg/mL)") +
  guides(colour = guide_legend(nrow = 1)) #make legend 1 row

boxplot.I


#Barplots - Detection rates over time
barplot<-ggplot(data= Detections_PresExpI, mapping = aes(x=factor(Time), y=detect_rate, fill = Treatment))+
  ylab("Detection Rate") + xlab("Time Preserved (weeks)")+
  stat_summary(fun = "mean", geom = "bar", aes(width = 0.65), position = position_dodge(width = 0.75))+
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width = 0.4,
               ymax = 1,
               position = position_dodge(0.75)) +
  theme(legend.position = 'none',
        axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(size = 10, margin = margin(t = 7, r = 0, b = 0, l = 0)),
        axis.text=element_text(size=10, color = 'black'),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.8),
        panel.grid.major.y = element_line(color = "gray",
                                          size = 0.5,
                                          linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.25,0.5,0.25,0.75, unit="cm"))+
  scale_fill_manual(breaks = c("Ethanol -20", "Ethanol 4", "Frozen -20", "Frozen -80", "Longmires RT", "Longmires 4"),
                    values = c("Ethanol -20"="#F8766D", "Ethanol 4"="#BB9D00", "Frozen -20"="#00Bc59", "Frozen -80"="#00BFC4", "Longmires RT"="#fc61d5", "Longmires 4"="#CF78FF"))+
  coord_cartesian(ylim = c(0.25,1.0))

barplot



#Arrange 2X1
library(ggpubr)
install.packages("cowplot")
library(cowplot)

legend<-ggpubrlegend<-get_legend(legend.plot)

ggarrange(ggarrange(boxplot.imm, boxplot.I, 
                    ncol = 2,
                    widths = c(.65,4)),
          barplot,
          nrow = 2,
          heights = c(1.5,1),
          labels = c("A", "B"),
          vjust = -0.5,
          
          legend.grob = legend,
          legend = "top",
          common.legend = TRUE)

ggsave(filename = "Fig4_ExpI_R2_700dpi.png", plot = last_plot(), dpi = 700, bg="white")




##### Fold Change Calculations - Compare eDNA concentrations between preserved samples and those extracted immediately ####

# Use eDNA.Concentrations_Pres.Exp.I.csv (with Technical_sampleID column created)

# Manage Data

#Create new dataframe in which Technical_sampleID is collapsed into one value that averages qs_conc_adjusted (i.e., eDNA concentrations) across PCR replicates per sample
library(dplyr)

Conc_Avg <- eDNA.Concentrations_Pres.Exp.I %>%
  group_by(Technical_sampleID) %>%   #Grouping dataframe by unique sample IDs
  mutate(qs_conc = mean(qs_conc_adjusted, na.rm = TRUE)) %>% #Finding mean of the unique sample IDs
  select(Buffer, Temperature, Time_Cont, Technical_rep, qs_conc) %>% #reducing cumbersome dataframe down to fewer columns
  distinct() #removing duplicate rows to get one Conc_Avg estimate for each technical replicate

Conc_Avg$Buffer <- as.factor(Conc_Avg$Buffer)

Conc_Avg


## All data - includes outliers ##

#Visualize data
library(ggpubr)
ggboxplot(Conc_Avg, x = "Buffer", y = "qs_conc")

#Median eDNA concentrations per group
tapply(Conc_Avg$qs_conc, Conc_Avg$Buffer, median)
#Ethanol    Frozen Immediate Longmires 
#27995.712 42187.580 29710.610  8421.753 

#Mean eDNA concentrations per group
tapply(Conc_Avg$qs_conc, Conc_Avg$Buffer, mean) #same as geometric mean
#Ethanol    Frozen Immediate Longmires 
#36127.73  50927.77  33498.97  10354.00 



#Immediate vs Ethanol
conc.ethl <- Conc_Avg %>%
  filter(Buffer == "Immediate"| Buffer == "Ethanol")

library(car)
leveneTest(conc.ethl$qs_conc ~ conc.ethl$Buffer) #p=0.634 -- good

wilcox.test(conc.ethl$qs_conc ~ conc.ethl$Buffer) #non-parameteric t-test
#Wilcoxon rank sum test with continuity correction
#
#data:  conc.ethl$qs_conc by conc.ethl$Buffer
#W = 178, p-value = 0.8211
#alternative hypothesis: true location shift is not equal to 0

#fold change:
log.fc.ethl<-log2(36127.73)-log2(33498.97)
log.fc.ethl
    #0.109

fc.ethl <- 2^log.fc.ethl
fc.ethl
    #1.08 -- Ethanol and Immediate eDNA concentrations are essentially the same - Ethanol is 11% higher 

#Boxplot 
library(ggplot2)
library(ggpubr)

level_order <- c('Immediate', 'Ethanol')

ggboxplot(conc.ethl, x="Buffer", y="qs_conc", col = "Buffer",
          size = 0.5,
          outlier.size = 3,
          ylab = "eDNA Concentration (pg/mL)", xlab = "Treatment") +
  labs(color = "Buffer") +
  theme(legend.position="none",
        plot.title = element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=14),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm"))+
  scale_color_manual(breaks = c("Immediate", "Ethanol"),
                     values = c("Immediate"="#00A5FF", "Ethanol"="#F8766D"), 
                     labels = c("Immediate", "Ethanol"))+
  scale_x_discrete(limits=level_order)


#Immediate vs Frozen
conc.froz <- Conc_Avg %>%
  filter(Buffer == "Immediate"| Buffer == "Frozen")

leveneTest(conc.froz$qs_conc ~ conc.froz$Buffer) #p=0.980 -- good

wilcox.test(conc.froz$qs_conc ~ conc.froz$Buffer) #non-parameteric t-test
#Wilcoxon rank sum test with continuity correction
#
#data:  conc.froz$qs_conc by conc.froz$Buffer
#W = 220, p-value = 0.2201
#alternative hypothesis: true location shift is not equal to 0

#fold change:
#fold change:
log.fc.froz<-log2(50927.77)-log2(33498.97)
log.fc.froz
#0.604

fc.froz <- 2^log.fc.froz
fc.froz
#1.52 -- Frozen and Immediate eDNA concentrations are essentially the same - Frozen is 52% higher 


#Boxplot 
level_order <- c('Immediate', 'Frozen')

ggboxplot(conc.froz, x="Buffer", y="qs_conc", col = "Buffer",
          size = 0.5,
          outlier.size = 3,
          ylab = "eDNA Concentration (pg/mL)", xlab = "Treatment") +
  labs(color = "Buffer") +
  theme(legend.position="none",
        plot.title = element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=14),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm"))+
  scale_color_manual(breaks = c("Immediate", "Frozen"),
                     values = c("Immediate"="#00A5FF", "Frozen"="#00Bc59"), 
                     labels = c("Immediate", "Frozen"))+
  scale_x_discrete(limits=level_order)


#Immediate vs Longmires
conc.long <- Conc_Avg %>%
  filter(Buffer == "Immediate"| Buffer == "Longmires")

leveneTest(conc.long$qs_conc ~ conc.long$Buffer) #p<0.000 --- not good

#Brunner Munzel Test (i.e., Generalized Wilcoxon Test) - for unequal variances
install.packages("brunnermunzel")
library(brunnermunzel)
brunnermunzel.test(conc.long$qs_conc ~conc.long$Buffer)
#Brunner-Munzel Test
#
#data:  conc.long$qs_conc by conc.long$Buffer
#Brunner-Munzel Test Statistic = -2.7393, df = 5.2628, p-value = 0.03868
#95 percent confidence interval:
#  -0.1361491  0.4750379
#sample estimates:
#  P(X<Y)+.5*P(X=Y) 
#0.1694444

#fold change:
log.fc.long<-log2(10354.00)-log2(33498.97)
log.fc.long
#-1.69

fc.long <- 2^log.fc.long
fc.long
#0.31 - Longmires eDNA concentrations are far lower than immediate -- they are about 31% of immediates, or about a third


#Boxplot 
ggboxplot(conc.long, x="Buffer", y="qs_conc", col = "Buffer",
          size = 0.5,
          outlier.size = 3,
          ylab = "eDNA Concentration (pg/mL)", xlab = "Treatment") +
  labs(color = "Buffer") +
  theme(legend.position="none",
        plot.title = element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=14),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm"))+
  scale_x_discrete(breaks = c("Longmires", "Immediate"))+
  scale_color_manual(breaks = c("Immediate", "Longmires"),
                     values = c("Immediate"="#00A5FF", "Longmires"="#CF78FF"), 
                     labels = c("Immediate", "Longmires"))




##### Inhibition - Assessing Cq differences ####

# Use conc_data2.csv --> Immediate group has been removed but no outliers have been removed

#library(tidyverse)
#conc_data2 = eDNA.Concentrations_Pres.Exp.I %>%
#    filter(Time_Cont>0)


# Manage data

#Need to average delta_Cq values across Technical_sampleID to remove pseudo-replication
install.packages("dplyr")
library(dplyr)

Delta_Cq <- conc_data2 %>%
  group_by(Technical_sampleID) %>%   #Grouping dataframe by unique sample IDs
  mutate(delta_Cq = mean(delta_Cq, na.rm = TRUE)) %>% #Finding mean of the unique sample IDs
  select(Buffer, Temperature, Time_Cont, Technical_rep, Treatment, delta_Cq) %>% #reducing cumbersome dataframe down to fewer columns
  distinct() #removing duplicate rows to get one Conc_Avg estimate for each technical replicate

Delta_Cq$Buffer <- as.factor(Delta_Cq$Buffer)
Delta_Cq$Treatment <- as.factor(Delta_Cq$Treatment)

# Visualize data
hist(Delta_Cq$delta_Cq) #skewed


#by group
ggplot(Delta_Cq, aes(x=delta_Cq))+
  geom_histogram(fill = "white", color= "black")+
  facet_grid(Treatment ~ .)
    
    #data not really normally distributed for all groups

#Boxplots
ggboxplot(Delta_Cq, x = "Treatment", y = "delta_Cq")

#Check for equal variances
library(car)
leveneTest(Delta_Cq$delta_Cq ~ Delta_Cq$Treatment) #p<0.000 - not equal among groups

## Must use non-parametric test: Kruskal Wallis Test

library(dplyr)
group_by(Delta_Cq, Treatment) %>%
  summarise(
    count = n(),
    mean = mean(delta_Cq, na.rm = TRUE),
    sd = sd(delta_Cq, na.rm = TRUE),
    median = median(delta_Cq, na.rm = TRUE),
    IQR = IQR(delta_Cq, na.rm = TRUE),
    min = min(delta_Cq),
    max = max(delta_Cq, na.rm = TRUE)
  )
#Treatment      count  mean    sd median   IQR    min   max
#1 Ethanol -20     28 0.357 0.329  0.338 0.266 -0.380 1.53 
#2 Ethanol 4       28 0.261 0.262  0.3   0.266 -0.469 0.622
#3 Frozen -20      28 0.418 0.394  0.405 0.301 -0.250 2.04 
#4 Frozen -80      28 0.300 0.313  0.313 0.361 -0.504 0.948
#5 Longmires 4     32 1.45  0.992  1.26  1.47   0.263 4.05 
#6 Longmires RT    28 1.44  0.992  1.15  1.40  -0.295 3.40 

### K-W Test
kruskal.test(delta_Cq ~ Treatment, data = Delta_Cq)
#Kruskal-Wallis rank sum test
#
#data:  delta_Cq by Treatment
#Kruskal-Wallis chi-squared = 72.499, df = 5, p-value = 3.092e-14

install.packages("dunn.test")
library(dunn.test)
dunn.test(Delta_Cq$delta_Cq, Delta_Cq$Treatment,
                     method = "BH") #BH = Benjamini & Hochberg p-value correction for multiple comparisons -- less conservative than bonferroni

#Kruskal-Wallis rank sum test
#
#data: x and group
#Kruskal-Wallis chi-squared = 72.4989, df = 5, p-value = 0
#
#
#Comparison of x by group                            
#(Benjamini-Hochberg)                              
#Col Mean-|
#  Row Mean |   Ethanol-20    Ethanol4   Frozen-20   Frozen-80   Longmire4
----------------------------------------------------------------
# Ethanol4  |   0.601115
#           |     0.3424
#           |
# Frozen-20 |  -0.653444  -1.254559
#           |     0.3501     0.1747
#           |
# Frozen-80 |   0.150278  -0.450836   0.803723
#           |     0.4717     0.3762     0.3162
#           |
# Longmire4 |  -5.174671  -5.795500  -4.499797  -5.329879
#           |    0.0000*    0.0000*    0.0000*    0.0000*
#           |
# LongmireRT |  -5.020920  -5.622035  -4.367476  -5.171199  -0.010913
#           |    0.0000*    0.0000*    0.0000*    0.0000*     0.4956
#
#alpha = 0.05
#Reject Ho if p <= alpha/2



##### Manuscript Fig 5 ####
library(ggplot2)
library(ggpubr)

ggboxplot(Delta_Cq, x="Treatment", y="delta_Cq", col = "Treatment",
          size = .5,
          outlier.shape = NA,
          ylab = "ΔCq", xlab= "Preservation Treatment") +
  labs(color = "Treatment") +
  geom_jitter(aes(color = Treatment), size = 1, width = .2) +
  theme_bw() +
  theme(legend.background = element_rect(linetype="solid", color = "darkgray"),
        legend.position = 'none',
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(colour = "black", fill=NA, size=0.8),
        axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 7, b = 0, l = 0)),
        axis.title.x = element_text(size = 11, margin = margin(t = 7, r = 0, b = 0, l = 0)),
        axis.text.y=element_text(size=10, color = 'black'),
        axis.text.x=element_text(size=9, color = 'black'),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm"))+
  stat_summary(fun=mean, geom = "line", size = 0.5, aes(group = Treatment, col = Treatment),
               position = position_dodge(0.8))+
  scale_color_manual(breaks = c("Ethanol -20", "Ethanol 4", "Frozen -20", "Frozen -80", "Longmires RT", "Longmires 4", "Immediate na"),
                     values = c("Ethanol -20"="#F8766D", "Ethanol 4"="#BB9D00", "Frozen -20"="#00Bc59", "Frozen -80"="#00BFC4", "Longmires RT"="#fc61d5", "Longmires 4"="#CF78FF", "Immediate na"="#00A5FF"),
                     labels = c("Ethanol -20", "Ethanol 4", "Frozen -20", "Frozen -80", "Longmires RT", "Longmires 4", "Immediate"))+
  guides(colour = guide_legend(nrow = 2)) #make legend 2 row

ggsave(filename = "Fig5_Cq_exp1_700dpi.png", plot = last_plot(), dpi = 700, bg="white")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Preservation Experiment 2: Buffer and Temperature - 1 year ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


##### eDNA Concentrations ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Import 'eDNA.Concentrations_PresExpII.csv'


## Managing Dataframe

#create new 'Treatment' column combining buffer and temperature
eDNA.Concentrations_PresExpII$Treatment <- paste(eDNA.Concentrations_PresExpII$Buffer, eDNA.Concentrations_PresExpII$Extraction)

#create new 'Tehcnical_sampleID' column combining treatment and filter rep
eDNA.Concentrations_PresExpII$Technical_sampleID <- paste(eDNA.Concentrations_PresExpII$Treatment, eDNA.Concentrations_PresExpII$Time_Cont)
eDNA.Concentrations_PresExpII$Technical_sampleID <- paste(eDNA.Concentrations_PresExpII$Technical_sampleID, eDNA.Concentrations_PresExpII$Technical_rep)

View(eDNA.Concentrations_PresExpII)

#convert concentrations to numeric
eDNA.Concentrations_PresExpII$qs_conc_adjusted<-as.numeric(iconv(eDNA.Concentrations_PresExpII$qs_conc_adjusted, 'utf-8', 'ascii', sub = ''))

str(eDNA.Concentrations_PresExpII)


## Visualize Data
library(ggplot2)
library(ggpubr)

ggboxplot(eDNA.Concentrations_PresExpII, x="Time_Factor", y="qs_conc_adjusted", color = "Treatment",
          add = "jitter",
          ylab = "eDNA Concentration (pg/mL)", xlab= "Time Preserved") +
  labs(color = "Treatment") +
  theme(legend.background = element_rect(linetype="solid", color = "darkgray"),
        legend.direction = "horizontal",
        plot.title = element_text(size=16),
        axis.title=element_text(size=16)) +
  ggtitle("eDNA Concentrations (Pres Exp II)")

#no jitter, add mean
ggboxplot(eDNA.Concentrations_PresExpII, x="Time_Factor", y="qs_conc_adjusted", color = "Treatment",
          add = "mean",
          ylab = "eDNA Concentration (pg/mL)", xlab= "Time Preserved") +
  labs(color = "Treatment") +
  theme(legend.background = element_rect(linetype="solid", color = "darkgray"),
        legend.direction = "horizontal",
        plot.title = element_text(size=16),
        axis.title=element_text(size=16)) +
  ggtitle("eDNA Concentrations (Pres Exp II)")


#plot each treatment group separately
ggplot(eDNA.Concentrations_PresExpII, aes(x=Time_Cont, y = qs_conc_adjusted, col = Treatment)) +
  geom_point(aes(color=Treatment))+
  geom_smooth(method = 'lm', se=T)+
  facet_wrap(vars(Treatment))

#plot all treatment groups on same plot - separate smoothing methods
ggplot(eDNA.Concentrations_PresExpII, aes(x=Time_Cont, y = qs_conc_adjusted, col = Treatment)) +
  geom_point(na.rm = T, aes(color=Treatment))+
  geom_smooth(method = "lm", se=T)

ggplot(eDNA.Concentrations_PresExpII, aes(x=Time_Cont, y = qs_conc_adjusted, col = Treatment)) +
  geom_point(na.rm = T, aes(color=Treatment))+
  geom_smooth(method = "loess", se=T)

#plot all treatment groups on same plot - global smoothing method
ggplot(eDNA.Concentrations_PresExpII, aes(x=Time_Cont, y = qs_conc_adjusted)) +
  geom_point(na.rm = T, aes(color=Treatment))+
  geom_smooth(method = 'lm', se=T)

#Boxplot of eDNA concentrations by treatment
ggplot(eDNA.Concentrations_PresExpII, aes(x=Treatment, y=qs_conc_adjusted))+
  geom_boxplot()+
  geom_jitter()


#Histograms of eDNA concentrations
hist(eDNA.Concentrations_PresExpII$qs_conc_adjusted) #all data
#not normally distributed

#by group
ggplot(eDNA.Concentrations_PresExpII, aes(x=qs_conc_adjusted))+
  geom_histogram(fill = "white", color= "black")+
  facet_grid(Treatment ~ .)
#Ethl-Kit and LM-Kit have somewhat normally distributed concentrations but Ethl-PCI and LM-PCI are skewed right


###### LMM (nlme) ####
install.packages("nlme")
library(nlme)

#lme() in nlme package cannot handle missing data in dependent variable
library(tidyr)
edna_conc_presII <- eDNA.Concentrations_PresExpII %>% drop_na(qs_conc_adjusted)
View(edna_conc_presII)
str(edna_conc_presII)


lmm_presII = lme(qs_conc_adjusted ~ Time_Cont * Treatment, random = ~1|Technical_sampleID,
                 data = edna_conc_presII,
                 method = 'ML', 
                 na.action = na.omit)
summary(lmm_presII)
library(car)
Anova(lmm_presII) #interaction significant


#compare without interaction
lmm_presII2 = lme(qs_conc_adjusted ~ Time_Cont + Treatment, random = ~1|Technical_sampleID,
                  data = edna_conc_presII,
                  method = 'ML', 
                  na.action = na.omit)
summary(lmm_presII2)
Anova(lmm_presII2)

anova(lmm_presII, lmm_presII2)
#Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#lmm_presII      1 10 11719.81 11764.78 -5849.907                        
#lmm_presII2     2  7 11724.11 11755.59 -5855.056 1 vs 2 10.29815  0.0162

    #Including interaction improves model and is significantly better based on likelihood ratio test


#compare without random effect
gls_presII = gls(qs_conc_adjusted ~ Time_Cont + Treatment,
                 method = "ML",
                 data = edna_conc_presII,
                 na.action = na.omit)

anova(lmm_presII, gls_presII)
#Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#lmm_presII     1 10 11719.81 11764.78 -5849.907                        
#gls_presII     2  6 12192.87 12219.85 -6090.437 1 vs 2 481.0604  <.0001

  #Including random effect is better


## Check Assumptions

#1: Existence of variance - do not need to check because it is always true
#2: Linearity - do not need to check because fixed effects are categorical
#3: Homogeneity - residuals vs predicted values (Tukey-Anscombe plot)
plot(lmm_presII) #heteroskedasticity pretty bad! Very fan shaped
#4: Normality of residuals - histogram, qqplot
hist(residuals(lmm_presII)) #residuals are fairly normally distributed
qqnorm(residuals(lmm_presII)) #qqplot looks bad - looks like some underdispersion
qqline(residuals(lmm_presII))


###Try adding variance structure -- variance allowed to vary among Treatment groups
lmm_var = lme(qs_conc_adjusted ~ Time_Cont * Treatment, random = ~1|Technical_sampleID,
              data = edna_conc_presII,
              weights = varIdent(form = ~1|Treatment),
              method = 'ML', 
              na.action = na.omit)
anova(lmm_var, lmm_presII) 
#Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#lmm_var        1 13 11480.64 11539.10 -5727.319                        
#lmm_presII     2 10 11719.81 11764.78 -5849.907 1 vs 2 245.1752  <.0001

#variance structure better


# variance allowed to vary among Treatment groups and with Time
lmm_var2 = lme(qs_conc_adjusted ~ Time_Cont * Treatment, random = ~1|Technical_sampleID,
               data = edna_conc_presII,
               weights = varComb (varIdent(form = ~1|Treatment),
                                  varFixed(~Time_Cont)),
               method = 'ML', 
               na.action = na.omit)
anova(lmm_var, lmm_var2) 
#Model df      AIC      BIC    logLik
#lmm_var      1 13 11480.64 11539.10 -5727.319
#lmm_var2     2 13 11636.97 11695.42 -5805.483

    # varIdent() for Treatment only is better

# variance allowed to vary for each Treatment group and models variance as a power of Time
lmm_var3 = lme(qs_conc_adjusted ~ Time_Cont * Treatment, random = ~1|Technical_sampleID,
               data = edna_conc_presII,
               weights = varComb (varIdent(form = ~1|Treatment),
                                  varPower(form = ~Time_Cont)),
               method = 'ML', 
               na.action = na.omit)
anova(lmm_var, lmm_var3) 
#Model df      AIC      BIC    logLik   Test   L.Ratio p-value
#lmm_var      1 13 11480.64 11539.10 -5727.319                         
#lmm_var3     2 14 11482.45 11545.41 -5727.227 1 vs 2 0.1835689  0.6683

    # No statistical difference -- keep simpilier model with varIdent() for Treatment only


# variance allowed to vary for each Treatment group and models variance as an exponential structure of Time
lmm_var4 = lme(qs_conc_adjusted ~ Time_Cont * Treatment, random = ~1|Technical_sampleID,
               data = edna_conc_presII,
               weights = varComb (varIdent(form = ~1|Treatment),
                                  varExp(form = ~Time_Cont)),
               method = 'ML', 
               na.action = na.omit)
anova(lmm_var, lmm_var4) 
#Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#lmm_var      1 13 11480.64 11539.10 -5727.319                        
#lmm_var4     2 14 11481.50 11544.46 -5726.751 1 vs 2 1.136331  0.2864

    # No statistical difference -- keep simpilier model with varIdent() for Treatment only



## Check Assumptions

#1: Existence of variance - do not need to check because it is always true
#2: Linearity - do not need to check because fixed effects are categorical
#3: Homogeneity - residuals vs predicted values (Tukey-Anscombe plot)
plot(lmm_var) #heteroskedasticity looks alright-- passable
#4: Normality of residuals - histogram, qqplot
hist(residuals(lmm_var)) #residuals are fairly normally distributed
qqnorm(residuals(lmm_var)) #qqplot looks ok but looks like some underdispersion
qqline(residuals(lmm_var))


plot(lmm_var, resid(., type="p")~fitted(.)|Treatment) #Heteroskedasticity really only bad in Ethanol-PCI
plot(lmm_var, resid(., type="p")~fitted(.)|Time_Cont) #Heteroskedasticity pretty good for all times
plot(lmm_var, resid(., type="p")~fitted(.)) #heterogeneity pretty good

qqnorm(lmm_var, ~resid(., type="normalized")|Treatment) #Errors still have overdispersed tails -- especially for PCI treatments
qqnorm(lmm_var, ~resid(.)|Time_Cont)#Errors still have overdispersed tails 
qqnorm(residuals(lmm_var)) #all samples
qqline(residuals(lmm_var))

plot(lmm_var, resid(., type="normalized")~fitted(.)) 
plot(lmm_var, resid(., type="normalized")~Time_Cont) #no clear variation across time
plot(lmm_var, resid(., type="normalized")~Time_Cont|Treatment)#Uses standardized residuals -- only some variation in Ethanol - PCI

hist(residuals(lmm_var)) #normally distributed, but long tails on both sides-- still a bit underdispersed but not quite as bad!


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### BEST MODEL FOR EXPERIMENT 2  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lmm_var = lme(qs_conc_adjusted ~ Time_Cont * Treatment, random = ~1|Technical_sampleID,
                    data = edna_conc_presII,
                    weights = varIdent(form = ~1|Treatment),
                    method = 'REML', 
                    na.action = na.omit)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summary(lmm_var)
#Linear mixed-effects model fit by REML
#Data: edna_conc_presII 
#AIC      BIC    logLik
#11391.88 11450.18 -5682.938
#
#Random effects:
#  Formula: ~1 | Technical_sampleID
#(Intercept) Residual
#StdDev:    1852.698 761.6775
#
#Variance function:
#  Structure: Different standard deviations per stratum
#Formula: ~1 | Treatment 
#Parameter estimates:
#  Ethanol Kit    Ethanol PCI Longmire's Kit Longmire's PCI 
#1.000000       1.232678       1.428608       3.069430 
#Fixed effects:  qs_conc_adjusted ~ Time_Cont * Treatment 
#                                       Value Std.Error  DF   t-value p-value
#(Intercept)                        4642.507  771.2934 587  6.019119  0.0000
#Time_Cont                            35.552   24.1837  68  1.470074  0.1462
#TreatmentEthanol PCI              -3297.175 1093.4610  68 -3.015356  0.0036
#TreatmentLongmire's Kit            4673.722 1096.2262  68  4.263465  0.0001
#TreatmentLongmire's PCI            1199.555 1142.5515  68  1.049891  0.2975
#Time_Cont:TreatmentEthanol PCI       -3.138   34.2883  68 -0.091509  0.9274
#Time_Cont:TreatmentLongmire's Kit   -59.298   35.5762  68 -1.666777  0.1002
#Time_Cont:TreatmentLongmire's PCI   -94.809   35.7366  68 -2.652984  0.0099
#Correlation: 
#                                  (Intr) Tm_Cnt TrEPCI TrtL'K TL'PCI T_C:TEP T_C:TK
#Time_Cont                         -0.840                                           
#TreatmentEthanol PCI              -0.705  0.593                                    
#TreatmentLongmire's Kit           -0.704  0.591  0.496                             
#TreatmentLongmire's PCI           -0.675  0.567  0.476  0.475                      
#Time_Cont:TreatmentEthanol PCI     0.593 -0.705 -0.840 -0.417 -0.400               
#Time_Cont:TreatmentLongmire's Kit  0.571 -0.680 -0.403 -0.816 -0.386  0.479        
#Time_Cont:TreatmentLongmire's PCI  0.569 -0.677 -0.401 -0.400 -0.842  0.477   0.460
#
#Standardized Within-Group Residuals:
#  Min         Q1        Med         Q3        Max 
#-3.6250205 -0.4948034 -0.0581462  0.4595668  5.5021538 
#
#Number of Observations: 663
#Number of Groups: 76 


library(car)
Anova(lmm_var)
#Analysis of Deviance Table (Type II tests)
#
#Response: qs_conc_adjusted
#Chisq Df Pr(>Chisq)    
#Time_Cont             0.0037  1    0.95155    
#Treatment           111.4066  3    < 2e-16 ***
#Time_Cont:Treatment   9.8363  3    0.02001 * 




#compare without interaction (use method = 'ML' to compare models)
lmm_var = lme(qs_conc_adjusted ~ Time_Cont * Treatment, random = ~1|Technical_sampleID,
               data = edna_conc_presII,
               weights = varIdent(form = ~1|Treatment),
               method = 'ML', 
               na.action = na.omit)

lmm_var.noint = lme(qs_conc_adjusted ~ Time_Cont + Treatment, random = ~1|Technical_sampleID,
                          data = edna_conc_presII,
                          weights = varIdent(form = ~1|Treatment),
                          method = 'ML', 
                          na.action = na.omit)

anova(lmm_var, lmm_var.noint)
#Model df      AIC      BIC   logLik   Test  L.Ratio p-value
#lmm_var           1 13 11480.64 11539.10 -5727.319                        
#lmm_var.noint     2 10 11484.92 11529.88 -5732.458 1 vs 2 10.27702  0.0164

## AIC is slightly better for interaction model; BIC is slightly better for no interaction model; log-likelihood slightly better for interaction model and there is a sig difference



###### Post hoc Test ####

#post hoc pairwise comparisons for slopes
library('emmeans')

emtrends(lmm_var, pairwise ~ Treatment, var = "Time_Cont") # use REML method
#$emtrends
#Treatment      Time_Cont.trend   SE df lower.CL upper.CL
#Ethanol Kit               35.6 24.2 68    -12.7    83.81
#Ethanol PCI               32.4 24.3 68    -16.1    80.92
#Longmire's Kit           -23.7 26.1 68    -75.8    28.32
#Longmire's PCI           -59.3 26.3 68   -111.8    -6.75
#
#Degrees-of-freedom method: containment 
#Confidence level used: 0.95 
#
#$contrasts
#contrast                        estimate   SE df t.ratio p.value
#Ethanol Kit - Ethanol PCI           3.14 34.3 68   0.092  0.9997
#Ethanol Kit - Longmire's Kit       59.30 35.6 68   1.667  0.3491
#Ethanol Kit - Longmire's PCI       94.81 35.7 68   2.653  0.0477
#Ethanol PCI - Longmire's Kit       56.16 35.7 68   1.575  0.3997
#Ethanol PCI - Longmire's PCI       91.67 35.8 68   2.559  0.0599
#Longmire's Kit - Longmire's PCI    35.51 37.1 68   0.958  0.7734
#
#Degrees-of-freedom method: containment 
#P value adjustment: tukey method for comparing a family of 4 estimates 


## Descriptive Statistics

tapply(edna_conc_presII$qs_conc_adjusted, edna_conc_presII$Treatment, mean)
#Ethanol Kit    Ethanol PCI Longmire's Kit Longmire's PCI 
#5600.390       2143.892       8778.795       4299.933 

tapply(edna_conc_presII$qs_conc_adjusted, edna_conc_presII$Treatment, sd)
#Ethanol Kit    Ethanol PCI Longmire's Kit Longmire's PCI 
#1278.297       1775.936       2903.177       3192.250 


tapply(edna_conc_presII$qs_conc_adjusted, edna_conc_presII$Extraction, mean)
#Kit      PCI 
#7003.205 3186.932 

tapply(edna_conc_presII$qs_conc_adjusted, edna_conc_presII$Extraction, sd)
#Kit      PCI 
#2667.307 2775.302 


tapply(edna_conc_presII$qs_conc_adjusted, edna_conc_presII$Buffer, mean)
#Ethanol Longmire's 
#  3901.269   6386.178 

tapply(edna_conc_presII$qs_conc_adjusted, edna_conc_presII$Buffer, sd)
#Ethanol Longmire's 
#  2317.070   3787.794 


##### Detection Rates ####

## Import Data: 'detections_presII.csv'


## Managing Dataframe

#create new 'Treatment' column combining buffer and temperature
detections_presII$Treatment <- paste(detections_presII$Buffer, detections_presII$Extraction)
detections_presII$Treatment<- as.factor(detections_presII$Treatment)

#Create new 'Detection rate' column
detections_presII$detect_rate <- detections_presII$Detect_count/9


## Visualize Data

#boxplots with lines connecting means
library(ggplot2)
library(ggpubr)
ggboxplot(detections_presII, x="Time", y="detect_rate", col = "Treatment",
          size = .4,
          outlier.size = .5,
          ylab = "Detection Rate", xlab= "Time Preserved (weeks)") +
  labs(color = "Treatment") +
  theme(legend.background = element_rect(linetype="solid", color = "darkgray"),
        legend.position = "top",
        legend.title = element_blank(), 
        plot.title = element_text(size=14),
        axis.title=element_text(size=14),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm"))+
  stat_summary(fun=mean, geom = "line", size = 0.7, aes(group = Treatment, col = Treatment),
               position = position_dodge(0.8))+ 
  scale_color_brewer(palette = "Paired")+
  guides(colour = guide_legend(nrow = 2)) #make legend 2 row


#barplots
ggplot(data= detections_presII, mapping = aes(x=factor(Time), y=detect_rate, fill = Treatment))+
  ylab("Detection Rate") + xlab("Time Preserved (weeks)")+
  stat_summary(fun = "mean", geom = "bar", aes(width = 0.65), position = position_dodge(width = 0.75))+
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width = 0.4,
               ymax = 1,
               position = position_dodge(0.75)) +
  theme(legend.background = element_blank(),
        axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 7, b = 0, l = 0)),
        axis.title.x = element_text(size = 12, margin = margin(t = 7, r = 0, b = 0, l = 0)),
        axis.text=element_text(size=10),
        panel.background = element_rect(fill="grey97", colour = "black"),
        panel.grid.major.y = element_line(color = "gray",
                                          size = 0.5,
                                          linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm"))+
  coord_cartesian(ylim = c(0.25,1.0))


#boxplots among treatments
ggplot(data= detections_presII, mapping = aes(x=Treatment, y=detect_rate, fill = Treatment))+
  geom_boxplot()+
  geom_point(aes(col = Treatment), width = 0.3)


### Descriptive Stats

tapply(detections_presII$detect_rate, detections_presII$Treatment, mean)
#Ethanol Kit    Ethanol PCI Longmire's Kit Longmire's PCI 
#1.0000000      0.9722222      1.0000000      0.9111111 

tapply(detections_presII$detect_rate, detections_presII$Treatment, max)
#Ethanol Kit    Ethanol PCI Longmire's Kit Longmire's PCI 
#1              1              1              1 

tapply(detections_presII$detect_rate, detections_presII$Treatment, min)
# Kit    Ethanol PCI Longmire's Kit Longmire's PCI 
#1.0000000      0.5555556      1.0000000      0.4444444

tapply(detections_presII$detect_rate, detections_presII$Treatment, sd)
#Ethanol Kit    Ethanol PCI Longmire's Kit Longmire's PCI 
#0.0000000      0.1011628      0.0000000      0.1377438 


##### Manuscript Fig 6 ####

library(ggplot2)
library(ggpubr)

#Boxplots & regression lines overlaid onto one plot - with legend
## use to extract legend for final figure
legend.plot.II <- ggplot() +
  theme_bw() +
  geom_smooth(data = edna_conc_presII,
              aes(x = Time_Cont,
                  y = qs_conc_adjusted,
                  color = Treatment,
                  fill = Treatment),
              alpha = 0.15,
              method = "glm",
              se=T) +
  geom_boxplot(data = edna_conc_presII,
               aes(x = Time_Cont,
                   y = qs_conc_adjusted,
                   group = interaction(Time_Factor, Treatment),
                   color = Treatment),
               width = 5,
               outlier.size = 1,
               position = position_dodge(6)) +
    scale_x_continuous(breaks = c(4, 13, 26, 39, 52)) +
    theme(legend.background = element_blank(),
          legend.position = 'top',
          legend.title = element_blank(), 
          legend.text = element_text(size=10),
          plot.title = element_text(size=14),
          axis.title=element_text(size=14),
          plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm")) +
  scale_colour_manual(values = c("Ethanol Kit"="#F8766D", "Ethanol PCI"="#BB9D00", "Longmire's Kit"="#00BFC4", "Longmire's PCI"="#CF78FF"))+
  xlab("Time Preserved (weeks)")+
  ylab("eDNA Concentration (pg/mL)")

legend.plot.II


#Boxplots & regression lines overlaid onto one plot - without legend
boxplot.II <- ggplot() +
  theme_bw() +
  geom_smooth(data = edna_conc_presII,
              aes(x = Time_Cont,
                  y = qs_conc_adjusted,
                  color = Treatment,
                  fill = Treatment),
              alpha = 0.15,
              method = "glm",
              fullrange=TRUE,
              size=.8,
              se=T) +
  geom_boxplot(data = edna_conc_presII,
               aes(x = Time_Cont,
                   y = qs_conc_adjusted,
                   group = interaction(Time_Factor, Treatment),
                   color = Treatment),
               width = 6,
               outlier.size = .8,
               position = position_dodge(7)) +
  scale_x_continuous(limits = c(0, 56), breaks = c(4, 13, 26, 39, 52), expand = c(0,0)) +
  theme(legend.background = element_rect(linetype="solid", color = "darkgray"),
        legend.position = 'none',
        legend.title = element_blank(),
        panel.grid.major.x = element_line(color = "gray",
                                          size = 0.5,
                                          linetype = 2),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(colour = "black", fill=NA, size=0.8),
        axis.title.y = element_text(size = 11, margin = margin(t = 0, r = 3, b = 0, l = 0)),
        axis.title.x = element_text(size = 10, margin = margin(t = 7, r = 0, b = 0, l = 0)),
        axis.text=element_text(size=10, color = 'black'),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.25,0.5,0,0.5, unit="cm")) +
  scale_colour_manual(values = c("Ethanol Kit"="#F8766D", "Ethanol PCI"="#BB9D00", "Longmire's Kit"="#00BFC4", "Longmire's PCI"="#CF78FF")) +
  xlab("Time Preserved (weeks)")+
  ylab("eDNA Concentration (pg/mL)") 

boxplot.II


#Barplots - Detection rates over time
barplot.II <- ggplot(data= detections_presII, mapping = aes(x=factor(Time), y=detect_rate, fill = Treatment))+
  ylab("Detection Rate") + xlab("Time Preserved (weeks)")+
  stat_summary(fun = "mean", geom = "bar", aes(width = 0.65), position = position_dodge(width = 0.75))+
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width = 0.4,
               ymax = 1,
               position = position_dodge(0.75)) +
  theme(legend.position = 'none',
        axis.title.y = element_text(size = 11, margin = margin(t = 0, r = 8, b = 0, l = 0)),
        axis.title.x = element_text(size = 10, margin = margin(t = 7, r = 0, b = 0, l = 0)),
        axis.text=element_text(size=10, color = 'black'),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.8),
        panel.grid.major.y = element_line(color = "gray",
                                          size = 0.5,
                                          linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.25,0.5,0.25,0.75, unit="cm"))+
  scale_fill_manual(breaks = c("Ethanol Kit", "Ethanol PCI", "Longmire's Kit", "Longmires PCI"),
                    values = c("Ethanol Kit"="#F8766D", "Ethanol PCI"="#BB9D00", "Longmire's Kit"="#00BFC4", "Longmire's PCI"="#CF78FF"))+
  coord_cartesian(ylim = c(0.25,1.0))

barplot.II

#Arrange 1x2
library(ggpubr)

legend2<-ggpubrlegend<-get_legend(legend.plot.II) #horizontal legend

ggarrange(ggarrange(boxplot.II, barplot.II, 
                    nrow=2,
                    heights = c(1.5,1),
                    vjust = 0.25,
                    font.label = list(size = 15),
                    labels = c("A", "B")),
          legend.grob = legend2,
          legend = "top",
          common.legend = TRUE)

ggsave(filename = "Fig6_ExpII_700dpi B.png", plot = last_plot(), dpi = 700, bg="white")



##### Inhibition - Assessing Cq differences ####

## Use eDNA.Concentrations_PresExpII.csv

## Managing Dataframe

#Create new 'Treatment' column combining buffer and temperature
#eDNA.Concentrations_PresExpII$Treatment <- paste(eDNA.Concentrations_PresExpII$Buffer, eDNA.Concentrations_PresExpII$Extraction)

#Create new 'Tehcnical_sampleID' column combining treatment and technical rep
#eDNA.Concentrations_PresExpII$Technical_sampleID <- paste(eDNA.Concentrations_PresExpII$Treatment, eDNA.Concentrations_PresExpII$Time)
#eDNA.Concentrations_PresExpII$Technical_sampleID <- paste(eDNA.Concentrations_PresExpII$Technical_sampleID, eDNA.Concentrations_PresExpII$Technical_rep)

#Need to average delta_Cq values across Technical_sampleID to remove pseudo-replication
install.packages("dplyr")
library(dplyr)

Delta_Cq <- eDNA.Concentrations_PresExpII %>%
  group_by(Technical_sampleID) %>%   #Grouping dataframe by unique sample IDs
  mutate(delta_Cq = mean(delta_Cq, na.rm = TRUE)) %>% #Finding mean of the unique sample IDs
  select(Buffer, Time_Cont, Extraction, Technical_rep, Treatment, delta_Cq) %>% #reducing cumbersome dataframe down to fewer columns
  distinct() #removing duplicate rows to get one Conc_Avg estimate for each technical replicate

Delta_Cq$Treatment <- as.factor(Delta_Cq$Treatment)


#ANOVA - differences in delta Cq values among Treatments
hist(Delta_Cq$delta_Cq)

library(ggplot2)
library(ggpubr)
#by group
ggplot(Delta_Cq, aes(x=delta_Cq))+
  geom_histogram(fill = "white", color= "black")+
  facet_grid(Treatment ~ .)

#data not really normally distributed in all groups

#Boxplots
ggboxplot(Delta_Cq, x = "Treatment", y = "delta_Cq")

#Check for equal variances
library(car)
leveneTest(Delta_Cq$delta_Cq ~ Delta_Cq$Treatment) #p<0.000 - not equal among groups

## Must use non-parametric test: Kruskal Wallis Test

library(dplyr)
group_by(Delta_Cq, Treatment) %>%
  summarise(
    count = n(),
    mean = mean(delta_Cq, na.rm = TRUE),
    sd = sd(delta_Cq, na.rm = TRUE),
    median = median(delta_Cq, na.rm = TRUE),
    IQR = IQR(delta_Cq, na.rm = TRUE),
    min = min(delta_Cq),
    max = max(delta_Cq, na.rm = TRUE)
  )
#Treatment        count  mean    sd median   IQR     min   max
#<fct>          <int> <dbl> <dbl>  <dbl> <dbl>   <dbl> <dbl>
#  1 Ethanol Kit    20 0.382 0.201  0.311 0.259  0.137  0.851
#2 Ethanol PCI       20 1.34  1.76   0.615 1.68  -0.0270 5.22 
#3 Longmire's Kit    16 0.379 0.244  0.354 0.168  0.0761 0.929
#4 Longmire's PCI    20 2.19  0.800  2.08  1.17   0.499  3.45 

### K-W Test
kruskal.test(delta_Cq ~ Treatment, data = Delta_Cq)
#Kruskal-Wallis rank sum test
#
#data:  delta_Cq by Treatment
#Kruskal-Wallis chi-squared = 30.537, df = 3, p-value = 1.064e-06

install.packages("dunn.test")
library(dunn.test)
dunn.test(Delta_Cq$delta_Cq, Delta_Cq$Treatment,
          method = "BH") #BH = Benjamini & Hochberg p-value correction for multiple comparisons -- less conservative than bonferroni
#Dunn's test compares means

#Kruskal-Wallis rank sum test
#
#data: x and group
#Kruskal-Wallis chi-squared = 30.5371, df = 3, p-value = 0
#
#
#Comparison of x by group                            
#(Benjamini-Hochberg)                              
#Col Mean-|
#  Row Mean |   Ethanol Kit    Ethanol PCI   Longmire Kit
------------------------------------------
#  Ethanol PCI |  -1.073988
#                0.2121
#
#  Longmire  Kit |  -0.005062   1.007503
#                   0.4980     0.1882
#
#  Longmire PCI |  -4.825789  -3.751800  -4.544735
#                   0.0000*    0.0002*    0.0000*
#  
#  alpha = 0.05
#Reject Ho if p <= alpha/2


##### Manuscript Fig 7 ####

library(ggplot2)
library(ggpubr)

ggboxplot(Delta_Cq, x="Treatment", y="delta_Cq", col = "Treatment",
          size = .5,
          outlier.shape = NA,
          ylab = "ΔCq", xlab= "Treatment") +
  labs(color = "Treatment") +
  geom_jitter(aes(color = Treatment), size = 1, width = .2) +
  theme(legend.background = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(), 
        legend.text = element_text(size=10),
        plot.title = element_text(size=14),
        panel.background = element_rect(colour = "black", fill=NA, size=0.8),
        axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 7, b = 0, l = 0)),
        axis.title.x = element_text(size = 12, margin = margin(t = 7, r = 0, b = 0, l = 0)),
        axis.text.y=element_text(size=10, color = 'black'),
        axis.text.x=element_text(size=10, color = 'black'),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm"))+
  stat_summary(fun=mean, geom = "line", size = 0.7, aes(group = Treatment, col = Treatment),
               position = position_dodge(0.8))+
  scale_color_manual(breaks = c("Ethanol Kit", "Ethanol PCI", "Longmire's Kit", "Longmire's PCI"),
                     values = c("Ethanol Kit"="#F8766D", "Ethanol PCI"="#BB9D00", "Longmire's Kit"="#00BFC4", "Longmire's PCI"="#CF78FF"), 
                     labels = c("Ethanol Kit", "Ethanol PCI", "Longmire's Kit", "Longmire's PCI"))+
  guides(colour = guide_legend(nrow = 1)) #make legend 1 row


ggsave(filename = "Fig7_Cq_exp2_700dpi.png", plot = last_plot(), dpi = 700, bg="white")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Preservation Experiment 3: Buffer and Temperature - 4 years ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


##### eDNA Concentrations ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Import 'eDNA.Concentrations_PresExpIII.csv'


## Managing Dataframe

#create new 'Treatment' column combining buffer and temperature
eDNA.Concentrations_PresExpIII$Treatment <- paste(eDNA.Concentrations_PresExpIII$Buffer, eDNA.Concentrations_PresExpIII$Temperature)

#create new 'Tehcnical_sampleID' column combining treatment and technical rep
eDNA.Concentrations_PresExpIII$Technical_sampleID <- paste(eDNA.Concentrations_PresExpIII$Treatment, eDNA.Concentrations_PresExpIII$Time)
eDNA.Concentrations_PresExpIII$Technical_sampleID <- paste(eDNA.Concentrations_PresExpIII$Technical_sampleID, eDNA.Concentrations_PresExpIII$Technical_rep)

View(eDNA.Concentrations_PresExpIII)

#convert concentrations to numeric
eDNA.Concentrations_PresExpIII$qs_conc<-as.numeric(eDNA.Concentrations_PresExpIII$QS_conc..pg.mL.)

eDNA.Concentrations_PresExpIII$Treatment<-as.factor(eDNA.Concentrations_PresExpIII$Treatment)

eDNA.Concentrations_PresExpIII$Technical_sampleID<-as.factor(eDNA.Concentrations_PresExpIII$Technical_sampleID)

str(eDNA.Concentrations_PresExpIII)


## Visualize Data
library(ggplot2)
library(ggpubr)

ggboxplot(eDNA.Concentrations_PresExpIII, x="Time", y="qs_conc", color = "Treatment",
          add = "jitter",
          ylab = "eDNA Concentration (pg/mL)", xlab= "Time Preserved") +
  labs(color = "Treatment") +
  theme(legend.background = element_rect(linetype="solid", color = "darkgray"),
        legend.direction = "horizontal",
        plot.title = element_text(size=16),
        axis.title=element_text(size=16)) +
  ggtitle("eDNA Concentrations (Pres Exp III)")

#no jitter, add mean
ggboxplot(eDNA.Concentrations_PresExpIII, x="Time", y="qs_conc", color = "Treatment",
          add = "mean",
          ylab = "eDNA Concentration (pg/mL)", xlab= "Time Preserved") +
  labs(color = "Treatment") +
  theme(legend.background = element_rect(linetype="solid", color = "darkgray"),
        legend.direction = "horizontal",
        plot.title = element_text(size=16),
        axis.title=element_text(size=16)) +
  ggtitle("eDNA Concentrations (Pres Exp III)")


#Linear regression lines
ggplot(eDNA.Concentrations_PresExpIII, aes(x=Time, y = qs_conc, col = Treatment)) +
  geom_point(aes(color=Treatment))+
  geom_smooth(method = 'lm', se=T)+
  facet_wrap(vars(Treatment))

ggplot(eDNA.Concentrations_PresExpIII, aes(x=Time, y = qs_conc, col = Treatment)) +
  geom_point(na.rm = T, aes(color=Treatment))+
  geom_smooth(method = "lm", se=T)


#Boxplots of eDNA concentrations by treatment
ggplot(eDNA.Concentrations_PresExpIII, aes(x=Treatment, y=qs_conc))+
  geom_boxplot()+
  geom_jitter()


#Histograms of eDNA concentrations
hist(eDNA.Concentrations_PresExpIII$qs_conc) #all data
#not normally distributed

#by group
ggplot(eDNA.Concentrations_PresExpIII, aes(x=qs_conc))+
  geom_histogram(fill = "white", color= "black")+
  facet_grid(Treatment ~ .)



###### LMM (nlme) ####
library(nlme)

#lme() in nlme package cannot handle missing data in dependent variable
library(tidyr)
edna_conc_presIII <- eDNA.Concentrations_PresExpIII %>% drop_na(qs_conc)
View(edna_conc_presIII)
str(edna_conc_presIII)


#include interaction
lmm_presIII = lme(qs_conc ~ Time * Treatment, random = ~1|Technical_sampleID,
                  data = edna_conc_presIII,
                  method = 'ML', 
                  na.action = na.omit)
summary(lmm_presIII)
library(car)
Anova(lmm_presIII) #interaction significant

#compare without interaction
lmm_presIII2 = lme(qs_conc ~ Time + Treatment, random = ~1|Technical_sampleID,
                   data = edna_conc_presIII,
                   method = 'ML', 
                   na.action = na.omit)
summary(lmm_presIII2)
anova(lmm_presIII, lmm_presIII2)
#Including interaction improves model and is significantly better based on likelihood ratio test

#compare without random effect
gls_presIII = gls(qs_conc ~ Time + Treatment,
                  method = "ML",
                  data = edna_conc_presIII,
                  na.action = na.omit)
anova(lmm_presIII, gls_presIII)
#Including random effect improves model


## Check Assumptions

#1: Existence of variance - do not need to check because it is always true
#2: Linearity - do not need to check because fixed effects are categorical
#3: Homogeneity - residuals vs predicted values (Tukey-Anscombe plot)
plot(lmm_presIII) #heteroskedasticity looks pretty bad - fan shaped but only at extremes
#4: Normality of residuals - histogram, qqplot
hist(residuals(lmm_presIII)) #residuals are fairly normally distributed, but with outliers on both sides
qqnorm(residuals(lmm_presIII)) #qqplot looks ok except with some outliers
qqline(residuals(lmm_presIII))


###Add variance structure
lmm_var = lme(qs_conc ~ Time * Treatment, random = ~1|Technical_sampleID,
              data = edna_conc_presIII,
              weights = varIdent(form = ~1|Treatment),
              method = 'ML', 
              na.action = na.omit)
anova(lmm_var, lmm_presIII) #variance structure better

lmm_var2 = lme(qs_conc ~ Time * Treatment, random = ~1|Technical_sampleID,
               data = edna_conc_presIII,
               weights = varComb (varIdent(form = ~1|Treatment),
                                  varIdent(form = ~1|Time)),
               method = 'ML', 
               na.action = na.omit)
anova(lmm_var, lmm_var2) # 2 variance structures better


## Check Assumptions

#1: Existence of variance - do not need to check because it is always true
#2: Linearity - do not need to check because fixed effects are categorical
#3: Homogeneity - residuals vs predicted values (Tukey-Anscombe plot)
plot(lmm_var2) #heteroskedasticity is better but still looks fan shaped
#4: Normality of residuals - histogram, qqplot
hist(residuals(lmm_var2)) #still outliers
qqnorm(residuals(lmm_var2)) #qqplot still looks bad - looks like some underdispersion
qqline(residuals(lmm_var2))


plot(lmm_var2, resid(., type="p")~fitted(.)|Treatment) #Heteroskedasticity really only bad in Ethanol RT, ok elsewhere
plot(lmm_var2, resid(., type="p")~fitted(.)|Time) #Heteroskedasticity bad for 1 week and 4yrs
plot(lmm_var2, resid(., type="p")~fitted(.)) #heterogeneity pretty good
plot(lmm_var2, which = c(1), col = edna_conc_presIII$Treatment) #colored by treatment -- definitely a pattern with treatments

qqnorm(lmm_var2, ~resid(., type="normalized")|Treatment) #looks ok
qqnorm(lmm_var2, ~resid(.)|Time)# bad overdispersion in 1 week
qqnorm(residuals(lmm_var2)) #all samples
qqline(residuals(lmm_var2))

plot(lmm_var2, resid(., type="normalized")~fitted(.)) 
plot(lmm_var2, resid(., type="normalized")~Time) #no clear variation across time
plot(lmm_var2, resid(., type="normalized")~Time|Treatment)#Uses standardized residuals -- only some variation in Ethanol - PCI

hist(residuals(lmm_var2)) #normally distributed, but long tails on both sides-- still underdispersed but not quite as bad?


## Removing 3 PCR outliers
#Remove 3 PCR reps in Ethanol RT at 1 week that were run on plate 'EMM Makeups & PresExpIII' (wells H7, H8, H9)
conc_no.out<- edna_conc_presIII[-c(67:69),]


lmm_no.out = lme(qs_conc ~ Time * Treatment, random = ~1|Technical_sampleID,
                 data = conc_no.out,
                 method = 'ML', 
                 na.action = na.omit)
summary(lmm_no.out)
Anova(lmm_no.out) #interaction significant

#compare without interaction
lmm_no.out2 = lme(qs_conc ~ Time + Treatment, random = ~1|Technical_sampleID,
                  data = conc_no.out,
                  method = 'ML', 
                  na.action = na.omit)
summary(lmm_no.out2)
anova(lmm_no.out, lmm_no.out2)
#Including interaction improves model and is significantly better based on likelihood ratio test

#compare without random effect
gls_no.out = gls(qs_conc ~ Time + Treatment,
                 method = "ML",
                 data = conc_no.out,
                 na.action = na.omit)
anova(lmm_no.out, gls_no.out)
#Including random effect improves model

## Check Assumptions

#1: Existence of variance - do not need to check because it is always true
#2: Linearity - do not need to check because fixed effects are categorical
#3: Homogeneity - residuals vs predicted values (Tukey-Anscombe plot)
plot(lmm_no.out) #heteroskedasticity still looks fan shaped, but is much better
#4: Normality of residuals - histogram, qqplot
hist(residuals(lmm_no.out)) #looks very good
qqnorm(residuals(lmm_no.out)) #qqplot looks good
qqline(residuals(lmm_no.out))


plot(lmm_no.out, resid(., type="p")~fitted(.)|Treatment)
plot(lmm_no.out, resid(., type="p")~fitted(.)|Time)
plot(lmm_no.out, resid(., type="p")~fitted(.)) #heterogeneity pretty good
plot(lmm_no.out, which = c(1), col = conc_no.out$Treatment) #colored by treatment -- definitely a pattern with treatments

qqnorm(lmm_no.out, ~resid(., type="normalized")|Treatment) #looks ok
qqnorm(lmm_no.out, ~resid(.)|Time)# looks ok
qqnorm(residuals(lmm_no.out)) #all samples
qqline(residuals(lmm_no.out))

plot(lmm_no.out, resid(., type="normalized")~fitted(.)) 
plot(lmm_no.out, resid(., type="normalized")~Time) #no clear variation across time
plot(lmm_no.out, resid(., type="normalized")~Time|Treatment)

hist(residuals(lmm_no.out)) #looks good


## Adding variance structure to model without 3 PCR reps - Treatment only
lmm_var_no.out = lme(qs_conc ~ Time * Treatment, random = ~1|Technical_sampleID,
                     data = conc_no.out,
                     weights = varIdent(form = ~1|Treatment),
                     method = 'ML', 
                     na.action = na.omit)
anova(lmm_var_no.out, lmm_no.out) #variance structure better

## Adding variance structure to model without 3 PCR reps - Treatment and Time
lmm_var2_no.out = lme(qs_conc ~ Time * Treatment, random = ~1|Technical_sampleID,
                      data = conc_no.out,
                      weights = varComb (varIdent(form = ~1|Treatment),
                                         varIdent(form = ~1|Time)),
                      method = 'ML', 
                      na.action = na.omit)
anova(lmm_var_no.out, lmm_var2_no.out) # 1 variance structure for Treatment better


## Check Assumptions - variance structure

#1: Existence of variance - do not need to check because it is always true
#2: Linearity - do not need to check because fixed effects are categorical
#3: Homogeneity - residuals vs predicted values (Tukey-Anscombe plot)
plot(lmm_var_no.out) #heteroskedasticity pretty good
#4: Normality of residuals - histogram, qqplot
hist(residuals(lmm_var_no.out)) #looks very good
qqnorm(residuals(lmm_var_no.out)) #qqplot looks good
qqline(residuals(lmm_var_no.out))


plot(lmm_var_no.out, resid(., type="p")~fitted(.)|Treatment)
plot(lmm_var_no.out, resid(., type="p")~fitted(.)|Time)
plot(lmm_var_no.out, resid(., type="p")~fitted(.)) #heterogeneity pretty good
plot(lmm_var_no.out, which = c(1), col = conc_no.out$Treatment) #colored by treatment

qqnorm(lmm_var_no.out, ~resid(., type="normalized")|Treatment) #looks ok
qqnorm(lmm_var_no.out, ~resid(.)|Time)# looks ok
qqnorm(residuals(lmm_var_no.out)) #all samples
qqline(residuals(lmm_var_no.out))

plot(lmm_var_no.out, resid(., type="normalized")~fitted(.)) 
plot(lmm_var_no.out, resid(., type="normalized")~Time) #no clear variation across time
plot(lmm_var_no.out, resid(., type="normalized")~Time|Treatment)#Uses standardized residuals 

hist(residuals(lmm_var_no.out)) #pretty good


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### BEST MODEL FOR EXPERIMENT 3 -  variance structure for treatment only ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lmm_best = lme(qs_conc ~ Time * Treatment, random = ~1|Technical_sampleID,
               data = conc_no.out,
               weights = varIdent(form = ~1|Treatment),
               method = 'REML', 
               na.action = na.omit)

summary(lmm_best)
#Linear mixed-effects model fit by REML
#Data: conc_no.out 
#AIC      BIC    logLik
#5158.531 5208.421 -2566.265
#
#Random effects:
#  Formula: ~1 | Technical_sampleID
#(Intercept) Residual
#StdDev:    632.4802 330.4213
#
#Variance function:
#  Structure: Different standard deviations per stratum
#Formula: ~1 | Treatment 
#Parameter estimates:
#  Ethanol -20   Ethanol 4  Ethanol RT  Frozen -20 
#1.0000000   1.0092752   0.6794552   0.9936853 
#Fixed effects:  qs_conc ~ Time * Treatment 
#Value Std.Error  DF   t-value p-value
#(Intercept)              1659.5836 206.00173 292  8.056163  0.0000
#Time                        0.6900   1.71531  51  0.402288  0.6892
#TreatmentEthanol 4       -351.5348 291.38949  51 -1.206409  0.2332
#TreatmentEthanol RT      -272.6777 297.67902  51 -0.916012  0.3640
#TreatmentFrozen -20        -8.5882 291.29054  51 -0.029483  0.9766
#Time:TreatmentEthanol 4     4.2042   2.42631  51  1.732776  0.0892
#Time:TreatmentEthanol RT   -5.4992   2.43436  51 -2.258996  0.0282
#Time:TreatmentFrozen -20    2.2271   2.42548  51  0.918208  0.3628
#Correlation: 
#  (Intr) Time   TrtmE4 TrtERT TrF-20 Tm:TE4 T:TERT
#Time                     -0.586                                          
#TreatmentEthanol 4       -0.707  0.414                                   
#TreatmentEthanol RT      -0.692  0.405  0.489                            
#TreatmentFrozen -20      -0.707  0.414  0.500  0.489                     
#Time:TreatmentEthanol 4   0.414 -0.707 -0.586 -0.287 -0.293              
#Time:TreatmentEthanol RT  0.413 -0.705 -0.292 -0.596 -0.292  0.498       
#Time:TreatmentFrozen -20  0.414 -0.707 -0.293 -0.287 -0.586  0.500  0.498
#
#Standardized Within-Group Residuals:
#  Min         Q1        Med         Q3        Max 
#-2.6479808 -0.6022157 -0.1029220  0.5101807  3.9397559 
#
#Number of Observations: 351
#Number of Groups: 59 

Anova(lmm_best)
#Analysis of Deviance Table (Type II tests)
#
#Response: qs_conc
#Chisq Df Pr(>Chisq)    
#Time            1.2035  1  0.2726166    
#Treatment      14.2264  3  0.0026127 ** 
#Time:Treatment 17.7380  3  0.0004981 ***


###### Post hoc Test ####

#post hoc pairwise comparisons for slopes
library('emmeans')

emtrends(lmm_best, pairwise ~ Treatment, var = "Time")
$emtrends
#Treatment   Time.trend   SE df lower.CL upper.CL
#Ethanol -20       0.69 1.72 51   -2.754     4.13
#Ethanol 4         4.89 1.72 51    1.449     8.34
#Ethanol RT       -4.81 1.73 51   -8.277    -1.34
#Frozen -20        2.92 1.71 51   -0.526     6.36
#
#Degrees-of-freedom method: containment 
#Confidence level used: 0.95 

#$contrasts
#contrast                     estimate   SE df t.ratio p.value
#(Ethanol -20) - Ethanol 4       -4.20 2.43 51  -1.733  0.3177
#(Ethanol -20) - Ethanol RT       5.50 2.43 51   2.259  0.1214
#(Ethanol -20) - (Frozen -20)    -2.23 2.43 51  -0.918  0.7952
#Ethanol 4 - Ethanol RT           9.70 2.43 51   3.985  0.0012
#Ethanol 4 - (Frozen -20)         1.98 2.43 51   0.815  0.8471
#Ethanol RT - (Frozen -20)       -7.73 2.43 51  -3.174  0.0132



## Descriptive Statistics (using data with 3 PCR outliers removed)

tapply(conc_no.out$qs_conc, conc_no.out$Treatment, mean)
#Ethanol -20   Ethanol 4  Ethanol RT  Frozen -20 
#1708.1170   1652.2811    966.7186   1856.1681

tapply(conc_no.out$qs_conc, conc_no.out$Treatment, sd)
#Ethanol -20   Ethanol 4  Ethanol RT  Frozen -20 
#801.4332    868.3811    662.1519    633.8206 


#Mean eDNA concentrations after 2 weeks of storage
library(tidyverse)
conc_1week = conc_no.out %>%
  filter(Time == 1)

tapply(conc_1week$qs_conc, conc_1week$Treatment, mean)
#Ethanol -20   Ethanol 4  Ethanol RT  Frozen -20 
#1430.717    1335.712    1730.864    1948.247 

tapply(conc_1week$qs_conc, conc_1week$Treatment, sd)
#Ethanol -20   Ethanol 4  Ethanol RT  Frozen -20 
#442.5129    551.0483    690.7520    552.2018 



#Mean eDNA concentrations after 208 weeks (4 years) of storage
library(tidyverse)
conc_4years = conc_no.out %>%
  filter(Time == 208)

tapply(conc_4years$qs_conc, conc_4years$Treatment, mean)
#Ethanol -20   Ethanol 4  Ethanol RT  Frozen -20 
#1801.999    2326.173     388.372    2259.191 

tapply(conc_4years$qs_conc, conc_4years$Treatment, sd)
#Ethanol -20   Ethanol 4  Ethanol RT  Frozen -20 
#1193.8495   1076.3564    125.9189    593.2460 



## ggplot
ggboxplot(conc_no.out, x="Time", y="qs_conc", col = "Treatment",
          size = .4,
          outlier.size = .5,
          ylab = "eDNA Concentration (pg/mL)", xlab= "Time Preserved (weeks)") +
  labs(color = "Treatment") +
  theme(legend.background = element_rect(linetype="solid", color = "darkgray"),
        legend.position = c(0.70, 0.88),
        legend.title = element_blank(), 
        plot.title = element_text(size=14),
        axis.title=element_text(size=14),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm"))+
  stat_summary(fun=mean, geom = "line", size = 0.7, aes(group = Treatment, col = Treatment),
               position = position_dodge(0.8))+ 
  scale_color_brewer(palette = "Paired")+
  guides(colour = guide_legend(nrow = 2)) #make legend 2 row



##### Detection Rates ####

## Important 'detections_exp3.csv'


## Manage Dataframe

#create new 'Treatment' column combining buffer and temperature
detections_exp3$Treatment <- paste(detections_exp3$Buffer, detections_exp3$Temperature)
detections_exp3$Treatment<- as.factor(detections_exp3$Treatment)

#Create new 'Detection rate' column
detections_exp3$detect_rate <- detections_exp3$Detect_count/6


## Visualize Data
library(ggplot2)

#barplots
ggplot(data= detections_exp3, mapping = aes(x=factor(Time), y=detect_rate, fill = Treatment))+
  ylab("Detection Rate") + xlab("Time Preserved (weeks)")+
  stat_summary(fun = "mean", geom = "bar", aes(width = 0.65), position = position_dodge(width = 0.75))+
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width = 0.4,
               ymax = 1,
               position = position_dodge(0.75)) +
  theme(legend.background = element_blank(),
        axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 7, b = 0, l = 0)),
        axis.title.x = element_text(size = 12, margin = margin(t = 7, r = 0, b = 0, l = 0)),
        axis.text=element_text(size=10),
        panel.background = element_rect(fill="grey97", colour = "black"),
        panel.grid.major.y = element_line(color = "gray",
                                          size = 0.5,
                                          linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm"))+
  coord_cartesian(ylim = c(0.25,1.0))


## Ggplots
library(ggplot2)
library(ggpubr)

#Boxplots of treatment groups for each time period, means connected by lines
ggboxplot(conc_no.out, x="Time", y="qs_conc", col = "Treatment",
          size = .5,
          outlier.size = 1,
          ylab = "eDNA Concentration (pg/mL)", xlab= "Time Preserved") +
  labs(color = "Treatment") +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank(), 
        legend.key=element_blank(),
        axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 7, b = 0, l = 0)),
        axis.title.x = element_text(size = 12, margin = margin(t = 7, r = 0, b = 0, l = 0)),
        plot.title = element_text(size=14),
        axis.text=element_text(size=10, color = 'black'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(colour = "black", fill=NA, size=0.8),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm"))+
  stat_summary(fun=mean, geom = "line", size = 0.8, aes(group = Treatment, col = Treatment),
               position = position_dodge(0.8))+ 
  scale_x_discrete(labels=c("1" = "1 week", "2" = "2 weeks",
                            "208" = "4 years"))+
  guides(colour = guide_legend(nrow = 1)) #make legend 2 row



#Linear regression lines + boxplots
ggplot() +
  theme_bw() +
  geom_smooth(data = conc_no.out,
              aes(x = Time,
                  y = qs_conc,
                  color = Treatment,
                  fill = Treatment),
              alpha = 0.15,
              method = "glm",
              fullrange=TRUE,
              size=.8,
              se=T) +
  geom_boxplot(data = conc_no.out,
               aes(x = Time,
                   y = qs_conc,
                   group = interaction(Time, Treatment),
                   color = Treatment),
               width = 10,
               outlier.size = 1,
               position = position_dodge(13)) +
  scale_x_continuous(limits = c(-3, 216), breaks = c(1, 2, 208), expand = c(0,0)) +
  theme(legend.background = element_blank(),
        legend.position = 'top',
        legend.title = element_blank(), 
        legend.text = element_text(size=10),
        legend.key = element_rect(fill=NA, color = NA),
        panel.grid.major.x = element_line(color = "gray",
                                          size = 0.5,
                                          linetype = 2),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(colour = "black", fill=NA, size=0.8),
        axis.title.y = element_text(size = 11, margin = margin(t = 0, r = 3, b = 0, l = 0)),
        axis.title.x = element_text(size = 10, margin = margin(t = 7, r = 0, b = 0, l = 0)),
        axis.text=element_text(size=10, color = 'black'),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.25,0.5,0,0.5, unit="cm")) +
  
  xlab("Time Preserved (weeks)")+
  ylab("eDNA Concentration (pg/mL)")



##### Manuscript Fig 8 ####

#use for horizontal legend 
boxplot.III.legend<- ggplot() +
  theme_bw() +
  geom_smooth(data = conc_no.out,
              aes(x = Time,
                  y = qs_conc,
                  color = Treatment,
                  fill = Treatment),
              alpha = 0.15,
              method = "glm",
              fullrange=TRUE,
              size=.8,
              se=T) +
  geom_boxplot(data = conc_no.out,
               aes(x = Time,
                   y = qs_conc,
                   group = interaction(Time, Treatment),
                   color = Treatment),
               width = .5,
               outlier.size = .8) +
  scale_x_continuous(limits = c(-3, 216), breaks = c(1, 2, 208), expand = c(0,0)) +
  scale_colour_manual(values = c("Ethanol -20"="#F8766D", "Ethanol 4"="#BB9D00", "Ethanol RT"="#00B8E7", "Frozen -20"="#00Bc59")) +
  scale_fill_manual(values = c("Ethanol -20"="#F8766D", "Ethanol 4"="#BB9D00", "Ethanol RT"="#00B8E7", "Frozen -20"="#00Bc59")) +
  theme(legend.background = element_blank(),
        legend.position = 'top',
        legend.title = element_blank(), 
        legend.text = element_text(size=10),
        legend.key = element_rect(fill=NA, color = NA),
        panel.grid.major.x = element_line(color = "gray",
                                          size = 0.5,
                                          linetype = 2),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(colour = "black", fill=NA, size=0.8),
        axis.title.y = element_text(size = 11, margin = margin(t = 0, r = 3, b = 0, l = 0)),
        axis.title.x = element_text(size = 10, margin = margin(t = 7, r = 0, b = 0, l = 0)),
        axis.text=element_text(size=10, color = 'black'),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.25,0.5,0,0.5, unit="cm")) +
  
  xlab("Time Preserved (weeks)")+
  ylab("eDNA Concentration (pg/mL)")

boxplot.III.legend


#Boxplot, no legend
##geom_rect: add grayed out area to indicate that DNA extractions did not take place between 2 weeks and 4 years -- data is missing from this time period
boxplot.III <- ggplot() +
  theme_bw() +
  geom_rect(aes(xmin = 5, xmax = 204, ymin = -Inf, ymax = Inf), 
            color= "lightgray",
            fill = "lightgray",
            alpha=.4) +
  geom_smooth(data = conc_no.out,
              aes(x = Time,
                  y = qs_conc,
                  color = Treatment,
                  fill = Treatment),
              alpha = 0.15,
              method = "glm",
              fullrange=TRUE,
              size=.5,
              se=T) +
  geom_boxplot(data = conc_no.out,
               aes(x = Time,
                   y = qs_conc,
                   group = interaction(Time, Treatment),
                   color = Treatment),
               width = 5,
               outlier.size = .4) +
  scale_x_continuous(limits = c(-3, 212), breaks = c(1, 2, 208), 
                     expand = c(0,0),
                     guide = guide_axis(n.dodge = 1.5)) +
  scale_colour_manual(values = c("Ethanol -20"="#F8766D", "Ethanol 4"="#BB9D00", "Ethanol RT"="#00B8E7", "Frozen -20"="#00Bc59")) +
  scale_fill_manual(values = c("Ethanol -20"="#F8766D", "Ethanol 4"="#BB9D00", "Ethanol RT"="#00B8E7", "Frozen -20"="#00Bc59")) +
  theme(legend.background = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(), 
        legend.text = element_text(size=10),
        legend.key = element_rect(fill=NA, color = NA),
        panel.grid.major.x = element_line(color = "gray",
                                          size = 0.5,
                                          linetype = 2),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(colour = "black", fill=NA, size=0.8),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=10, color = 'black'),
        axis.text.x=element_text(size=8, color = 'black'),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.25,0.5,0.5,0.5, unit="cm")) +
  
  xlab("Time Preserved (weeks)")+
  ylab("eDNA Concentration (pg/mL)") 

boxplot.III



boxplot_zoom1 <- ggplot() +
  theme_bw() +
  geom_smooth(data = conc_no.out,
              aes(x = Time,
                  y = qs_conc,
                  color = Treatment,
                  fill = Treatment),
              alpha = 0.15,
              method = "glm",
              fullrange=TRUE,
              size=.5,
              se=T) +
  geom_boxplot(data = conc_no.out,
               aes(x = Time,
                   y = qs_conc,
                   group = interaction(Time, Treatment),
                   color = Treatment),
               width = .7,
               outlier.size = 1,
               position = position_dodge(.8)) +
  scale_x_continuous(limits = c(-3, 216), breaks = c(1, 2, 208), expand = c(0,0)) +
  theme(legend.background = element_rect(linetype="solid", color = "darkgray"),
        legend.position = 'none',
        legend.title = element_blank(),
        panel.grid.major.x = element_line(color = "gray",
                                          size = 0.5,
                                          linetype = 2),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(colour = "black", fill=NA, size=0.8),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text=element_text(size=10, color = 'black'),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.25,0.5,0,0.5, unit="cm")) +
  scale_colour_manual(values = c("Ethanol -20"="#F8766D", "Ethanol 4"="#BB9D00", "Ethanol RT"="#00B8E7", "Frozen -20"="#00Bc59")) +
  scale_fill_manual(values = c("Ethanol -20"="#F8766D", "Ethanol 4"="#BB9D00", "Ethanol RT"="#00B8E7", "Frozen -20"="#00Bc59")) +
  xlab("Time Preserved (weeks)")+
  ylab("eDNA Concentration (pg/mL)")+
  coord_cartesian(xlim = c(0.5, 2.5))

boxplot_zoom1

boxplot_zoom2 <- ggplot() +
  theme_bw() +
  geom_smooth(data = conc_no.out,
              aes(x = Time,
                  y = qs_conc,
                  color = Treatment,
                  fill = Treatment),
              alpha = 0.15,
              method = "glm",
              fullrange=TRUE,
              size=.5,
              se=T) +
  geom_boxplot(data = conc_no.out,
               aes(x = Time,
                   y = qs_conc,
                   group = interaction(Time, Treatment),
                   color = Treatment),
               width = .7,
               outlier.size = 1,
               position = position_dodge(.8)) +
  scale_x_continuous(limits = c(-3, 216), breaks = c(1, 2, 208), expand = c(0,0)) +
  theme(legend.background = element_rect(linetype="solid", color = "darkgray"),
        legend.position = 'none',
        legend.title = element_blank(),
        panel.grid.major.x = element_line(color = "gray",
                                          size = 0.5,
                                          linetype = 2),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(colour = "black", fill=NA, size=0.8),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text=element_text(size=10, color = 'black'),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.25,0.5,0,0.5, unit="cm")) +
  scale_colour_manual(values = c("Ethanol -20"="#F8766D", "Ethanol 4"="#BB9D00", "Ethanol RT"="#00B8E7", "Frozen -20"="#00Bc59")) +
  scale_fill_manual(values = c("Ethanol -20"="#F8766D", "Ethanol 4"="#BB9D00", "Ethanol RT"="#00B8E7", "Frozen -20"="#00Bc59")) +
  xlab("Time Preserved (weeks)")+
  coord_cartesian(xlim = c(207.5, 208.5))

boxplot_zoom2


#Arrange 1x2
library(ggpubr)
install.packages("grid")
library(grid) #need for textGrob function to use one axis label for all plots

legend3<-ggpubrlegend<-get_legend(boxplot.III.legend) #horizontal legend

fig8<- ggarrange(boxplot.III,
          ggarrange(boxplot_zoom1, boxplot_zoom2, 
                    ncol= 2,
                    widths = c(1.6, 1)),
          nrow = 2,
          heights = c(1,1.5),
          legend.grob = legend3,
          legend = "top",
          common.legend = TRUE)

annotate_figure(fig8, 
                left = textGrob("eDNA Concentration (pg/mL)", rot = 90, vjust = 1, gp = gpar(cex = 1)),
                bottom = textGrob("         Time Preserved (weeks)", gp = gpar(cex = .9)))

ggsave(filename = "Fig8_ExpIII_700dpi.png", plot = last_plot(), dpi = 700, bg="white")

