rm(list = ls())

R.version # 4.5.0

#####Cargamos paquetes requeridos
library(bkmr)
library(dplyr)
library(fields)
library(corrplot)
library(ggplot2)
library(ggpubr)
library(Hmisc)
library(fastDummies)
library(stringr)

root <- "" #Introduce here your path to the repository
setwd(paste0(root, "\Functional-Decline-ENRICA-BKMR"))

enrica <- read.csv("data/enrica_metales_sarcopenia.csv")


outcomes <- list(
  sppb = "ewgsop2_perform_sppbw0",
  vel = "ewgsop2_perform_velw0",
  grip = "ewgsop2_strength_gripw0"
)

##In outcome_name, write one of the values in outcomes. If any other outcome were wanted to be analysed, add it to the list
outcome_name <- "sppb"
outcome <- outcomes[[outcome_name]]

if (class(enrica[[outcome]]) == "integer" && length(unique(na.omit(enrica[[outcome]]))) == 2){
  outcome_type="binomial"
} else{
  outcome_type="gaussian"
}

metal_groups <- list(
  serum = list(
    ALL = c("Al", "Co", "Cr", "Cu", "Fe", "Mg", "Mn", "Mo", "Ni", "Pb", "Se", "V", "Zn"),
    ESSENTIAL = c("Ni", "Co", "Fe", "Mg", "Mn", "Mo", "Se", "V", "Zn"),
    TOXIC = c("Al", "Pb", "Cu", "Cr")
    ),
  blood = list(
    ALL = c("Cd", "Hg_whb", "Mn_whb", "Pb_whb", "Se_whb"),
    ESSENTIAL = c("Mn_whb","Se_whb"),
    TOXIC = c("Cd", "Hg_whb", "Pb_whb")
  )
)

##Write a matrix (serum or blood) and a group of metals to be used as mix (ALL, ESSENTIAL or TOXIC)
## --> Note that bivariate analyses will give error for blood essential group, as it only has 2 metals while 3 are required
matrix <- "serum"
group <- "TOXIC"

metals <- metal_groups[[matrix]][[group]]


model_covars <- list(
  model1 = c('w17sexo','edadw0','w17fuma', 'w17educa_3cat', 'w17imc3'),
  model2 = c('w17sexo','edadw0', 'w17fuma', 'w17educa_3cat', 'w17imc3', 'w17cv', 'w17dai_hypertension', 'w17dai_diabetes')
)

##Select the model to be runned, model1 includes main covariates while model2 includes comorbilities.
model = "model1"

covariates <- model_covars[[model]]

check_dels <- function(x, n){
  new_n <- nrow(x)
  dels <- n-new_n
  print(paste("Se han eliminado", dels, "NAs", sep=" "))
  return(new_n)
}
n <- nrow(enrica)
enrica <- subset(enrica, !is.na(w17IR_DEGREES) & w17IR_DEGREES<5); n <- check_dels(enrica, n)
enrica <- enrica[complete.cases(enrica[metal_groups[[matrix]][["ALL"]]]), ]; n <- check_dels(enrica, n)
enrica <- enrica[complete.cases(enrica[model_covars[["model1"]]]), ]; n <- check_dels(enrica, n)
enrica <- enrica[complete.cases(enrica[outcome]), ]; n <- check_dels(enrica, n)


enrica <- enrica %>%
  select(all_of(metals),
         all_of(covariates),
         !!sym(outcome))



#Splines are calculated for age
knots_age <- quantile(enrica$edadw0, probs = c(0.05, 0.50, 0.95), na.rm = TRUE)

age_rcs <- rcspline.eval(enrica$edadw0,
                       knots = knots_age,
                       inclx = FALSE)

covariates[covariates == "edadw0"] <- "age_rcs"

enrica$age_rcs <- as.vector(age_rcs)

covs_matrix <- matrix(nrow = nrow(enrica), ncol = 0)

##BKMR needs continious variables scaled and multicotomic ones as dummies, this loop prepares the covariates
for (var in covariates) {
  x <- enrica[[var]]
  n_values <- length(unique(na.omit(x)))
  
  if (is.numeric(x) && n_values > 5) {
    #Scale continuous
    z <- scale(x)
    covs_matrix <- cbind(covs_matrix, z)
    colnames(covs_matrix)[ncol(covs_matrix)] <- paste0(var, "_z")
    
  } else if (n_values > 2) { #Dummies are generated for multicotomic variables
    dummies <- dummy_cols(
      as.data.frame(x),
      select_columns = "x",
      remove_first_dummy = TRUE,
      remove_selected_columns = TRUE
    )
    colnames(dummies) <- paste0(var, "_", sub("^x_", "", colnames(dummies)))
    
    covs_matrix <- cbind(covs_matrix, as.matrix(dummies))
  }
}

#Matrix Z is generated (exposures). BKMR requires it to be log and scaled
mixture <- enrica[ , metals]

lnmixture   <- mixture %>%
  mutate_at(1:length(mixture), ~ log(. + 0.001))

lnmixture_z <- lnmixture %>%
  mutate_at(1:length(mixture), ~ scale(.)[, 1])


#Clean the environment
rm(list = setdiff(ls(), c("matrix","group","model","outcome","outcome_name", "outcome_type", "enrica", "covs_matrix", "lnmixture_z")))

####Generate the matrix of knots representative of the n dimensional space where n = number of exposures
set.seed(10)
knots=50
knots50 <- fields::cover.design(lnmixture_z, nd = knots)$design

save(knots50, file=paste0("knots/",group,"_ENRICA_",outcome_name,"_knots50.RData"))


################################################
###         Fit Models                       ###
################################################


#####Fit BKMR models WITH Gaussian predictive process using

set.seed(1000)

iter = 10000
fit_vs_knots50 <-  kmbayes(y=enrica[[outcome]], Z=lnmixture_z, X=covs_matrix, iter=iter, verbose=TRUE, varsel=TRUE, est.h = TRUE,
                           knots=knots50,
                           family=outcome_type)

save(fit_vs_knots50, file=paste(paste0("fit_models/",matrix,"/",group),"BKMR_ENRICA",outcome_name, model,paste0("vs_knots",knots,".RData"), sep = "_"))
summary(fit_vs_knots50)



load(paste(paste0("fit_models/",matrix,"/",group),"BKMR_ENRICA",outcome_name, model,paste0("vs_knots",knots,".RData"), sep = "_"))



###Names to standarize procedure
modeltoplot      <- fit_vs_knots50   ##Name of model object
modeltoplot.name <- paste(outcome_name, model, group, paste0("fit_vs_knots",knots), sep = "_") ##Name of model for saving purposes
plot.name        <- paste(outcome_name, model, group, paste0("vs_knots",knots), sep = "_")     ##Part that changed in plot name 

Z <- lnmixture_z        ## Z matrix to match what was used in model
sel <- seq(iter/4+1,iter,by=1) # For plotting, we delete first 25% iterations as these are burn-in and the model has not converged



##################################################################
### MODELS FOR PLOTTING. IF ALREADY CREATED, SKIP TO NEXT LOAD ###
##################################################################

#Evaluates each exposure effect in a flexible way when the rest of the mixture is fixed at its median
pred.resp.univar <- PredictorResponseUnivar(fit = modeltoplot, sel=sel, method="approx")

#Makes pairs of exposures and evaluates the effect of one of them while the other is fixed at a particular quantile while the rest of the mixture is fixed at its median
pred.resp.bivar  <- PredictorResponseBivar(fit = modeltoplot,  min.plot.dist = 1, sel=sel, method="approx")

#The same as the previous one, but fixing the second exposure at specific quartiles, we set it at p25, p50 and p75
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = Z,
                                                       both_pairs = TRUE, qs = c(0.25, 0.5, 0.75))

#Estimates the mixture effect fixed at p50 and compares that effect to when it is fixed at different quantiles (from 0.25 to 0.75 by 0.05 in this case)
risks.overall <- OverallRiskSummaries(fit = modeltoplot, qs = seq(0.25, 0.75, by = 0.05), q.fixed = 0.5, 
                                      method = "approx",sel=sel)

#Estimates the effect of 1 IQR increase in each metal when the rest of the mixture is fixed at specific quantiles
risks.singvar <- SingVarRiskSummaries(fit = modeltoplot, qs.diff = c(0.25, 0.75),
                                      q.fixed = c(0.25, 0.50, 0.75), method = "approx")

#Compares the effect in the previous model when the mixture is fixed at a particular quantile to when it is fixed at another quantile (i.e p75-p25)
risks.int <- SingVarIntSummaries(fit = modeltoplot, qs.diff = c(0.25, 0.75), qs.fixed = c(0.25, 0.75))


save(pred.resp.univar, pred.resp.bivar, pred.resp.bivar.levels, risks.overall, risks.singvar, risks.int, 
     file=paste0("saved_model/",matrix,"/", modeltoplot.name,"_plots.RData"))

##################################################################################################


load(paste0("saved_model/",matrix,"/", modeltoplot.name,"_plots.RData"))


##############################################
###        PLOTS                           ###
##############################################

### Correlation matrix
cor.Z <- cor(lnmixture_z, use="complete.obs")
corrplot.mixed(cor.Z, upper = "ellipse", lower.col="black")


#Save PDF with the relevant plots and PIPs
pdf(paste(paste0("plots/",matrix,"/", group), "BKMR_ENRICA", outcome_name, model, ".pdf", sep = "_"))

#For appropiate titles
markers <- list(
  sppb = "SPPB",
  grip = "Grip Strength",
  vel = "Gait Speed"
)
marker <- markers[[strsplit(outcome_name, "w")[[1]][1]]]
wave <- paste0("wave ",strsplit(outcome_name, "w")[[1]][2]) #If longitudinal studies were carried out

pips <- ExtractPIPs(modeltoplot); colnames(pips)[1] <- "Metal"

#Table with PIPs
ggtexttable(pips, rows = NULL,
            theme = ttheme(base_size = 25)) %>%
  annotate_figure(
    top = text_grob(
      paste("Posterior Inclusion Probabilities (PIPs) for", str_to_lower(group), matrix, "metals on low",
            marker), size = 14))


TracePlot(fit = modeltoplot, par = "beta", sel = sel, ylab = expression(beta))

########## UNIVARIATE ##########
pred.resp.univar %>% 
  ggplot(aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + labs(y = "Effect Estimate", x = "Z-score log-transformed serum metal concentrations") + 
  facet_wrap(~ variable) + theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  ggtitle(paste("Cross-sectional association between", str_to_lower(group), matrix, "metals and low", marker, sep = " "))

########## BIVARIATE ##########
pred.resp.bivar.levels %>% 
  ggplot(aes(z1, est)) + 
  geom_smooth(aes(color = quantile), stat = "identity", linewidth = 0.5) +
  
  scale_color_manual(
    values = c(
      "0.25" = "#619cff",
      "0.5"  = "#00ba38",
      "0.75" = "#f8766d"
    ),
    name = "Quantile of Metal2"
  ) +
  
  facet_grid(
    variable2 ~ variable1,
    scales = "free",
    space  = "free",
    labeller = labeller(
      variable1 = label_wrap_gen(4),
      variable2 = label_wrap_gen(4)
    )
  ) +
  
  # Standarize axes
  scale_x_continuous(
    limits = c(-7, 7),
    breaks = c(-5, 0, 5)
  ) +

  scale_y_continuous(
    limits = c(-0.8, 0.8), #Increase these limits is effects do not fit
    breaks = c(-0.5, 0, 0.5)
  ) +
  
  ggtitle(paste0("Low ",marker,": h(Metal1 | quantiles of Metal2)")) +
  
  xlab("Z-score log-transformed serum Metal1 concentrations") + 
  ylab("Effect Estimate") +
  theme_bw(base_size = 10) +
  
  theme(
    plot.title = element_text(size = 12, hjust = 0),
    axis.text.x = element_text(size = 6, angle = 0, vjust = 1),
    axis.text.y = element_text(size = 6),
    strip.text.x = element_text(size = 7),
    strip.text.y = element_text(size = 7),
    strip.background = element_rect(fill = "white", color = "grey70"),
    panel.spacing = unit(0, "lines"),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5)
  )


########## RISK OVERALL ##########
ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_hline(yintercept = 00, linetype = "dashed", color = "gray") + 
  geom_pointrange() + theme_bw() +
  labs(x = "Mixture quantile", y = "Overall effect estimate") +
  ggtitle(paste(str_to_title(group), matrix, "Metals mixture overall effect on low",marker, sep = " "))


########## RISK SINGLE VARIABLE ##########
risks.singvar %>% 
  ggplot(aes(variable, est, ymin = est - 1.96*sd,  ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "gray") +  
  geom_pointrange(position = position_dodge(width = 0.75)) +  coord_flip() + 
  theme_bw() +
  labs(x = "", y = "Effect estimate per IQR increase", col = "Fixed Quantile")+
  ggtitle(paste("Conditional estimates across quantiles of", str_to_lower(group), matrix, "metals on low",marker, sep = " ")) +
  scale_color_manual(
    values = c(
      "0.25" = "#619cff",
      "0.5"  = "#00ba38",
      "0.75" = "#f8766d"    
    ),
    labels = c("P25", "P50", "P75")
  ) +
  theme(
    plot.title = element_text(size = 11))



########## RISK INTERACTION ##########
risks.int %>% 
  ggplot(aes(variable, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  geom_hline(yintercept = 0, lty = 2, col = "gray") + coord_flip() + theme_bw() +
  labs(x = "", y = "Difference in effect estimate per IQR increase (mixture fixed at P75 - P25)") +
  ggtitle(paste("Difference in", str_to_lower(group), matrix, "metals effects estimate on low",marker, "between mixture fixed at P25 and P75", sep = " ")) +
  theme(
    plot.title = element_text(size = 9))

dev.off()
  
