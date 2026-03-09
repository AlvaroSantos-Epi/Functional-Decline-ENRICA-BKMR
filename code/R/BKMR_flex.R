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

path <- rstudioapi::getSourceEditorContext()$path
setwd(gsub("/code/BKMR_flex.R", "", path))

enrica <- read.csv("data/enrica_metales_sarcopenia.csv")

##Here we load lists with the outcomes, metal groups and covariate models, below you will find variables to specify your desired analysis
outcomes <- list(
  sppb = "ewgsop2_perform_sppbw0",
  vel = "ewgsop2_perform_velw0",
  grip = "ewgsop2_strength_gripw0",
  chair = "ewgsop2_strength_chairw0",
  calf = "calfcircumf_dicotw0",
  sarcopenia = "sarcopenia4"
)

metal_groups <- list(
  serum = list(
    ALL = c("Al", "Co", "Cr", "Cu", "Fe", "Mg", "Mn", "Mo", "Ni", "Pb", "Se", "V", "Zn"),
    ESSENTIAL = c("Ni", "Co", "Fe", "Mg", "Mn", "Mo", "Se", "V", "Zn"),
    ESSENTIAL2 = c("Ni", "Co", "Mg", "Mn", "Mo", "Se", "V", "Zn"),
    TOXIC = c("Al", "Pb", "Cu", "Cr"),
    TOXIC2 = c("Al", "Fe", "Pb", "Cu", "Cr")
  ),
  blood = list(
    ALL = c("Cd", "Hg_whb", "Mn_whb", "Pb_whb", "Se_whb"),
    ESSENTIAL = c("Mn_whb","Se_whb"),
    TOXIC = c("Cd", "Hg_whb", "Pb_whb")
  )
)

model_covars <- list(
  model1 = c('w17sexo','edadw0','w17fuma', 'w17educa_3cat', 'w17imc3'),
  model2 = c('w17sexo','edadw0','w17fuma', 'w17educa_3cat', 'w17imc3', 'w17dai_hypertension', 'w17dai_diabetes', 'w17cv'),
  model3 = c('w17sexo','edadw0', 'w17fuma', 'w17educa_3cat', 'w17imc3', 'medas4', 'pa4', 'alcohol4')
)

##In outcome_name, write one of the values in outcomes. If any other outcome were wanted to be analysed, add it to the list
outcome_name <- "chair"
outcome <- outcomes[[outcome_name]]

##Write a matrix (serum or blood) and a group of metals to be used as mix (ALL, ESSENTIAL or TOXIC)
## --> Note that bivariate analyses will give error for blood essential group, as it only has 2 metals while 3 are required
matrix <- "serum"
group <- "ESSENTIAL2"
metals <- metal_groups[[matrix]][[group]]

##Select the model to be runned, model1 includes main covariates while model2 includes comorbilities.
model = "model3"
covariates <- model_covars[[model]]


##This automatically stablishes whether your outcome is binomial or continous
if (class(enrica[[outcome]]) == "integer" && length(unique(na.omit(enrica[[outcome]]))) == 2){
  outcome_type="binomial"
} else{
  outcome_type="gaussian"
}

##Selection criteria and shows how many participants are dropped
check_dels <- function(x, n){
  new_n <- nrow(x)
  dels <- n-new_n
  print(paste(dels, "NAs have been dropped", sep=" "))
  return(new_n)
}
n <- nrow(enrica)
#Sin exploración física
enrica <- enrica[complete.cases(enrica[, unlist(outcomes)]), ]; n <- check_dels(enrica, n)
#Sin visita de enfermera (sin extracción de sangre)
enrica <- enrica[complete.cases(enrica[, "w17vis_enf"]), ]; n <- check_dels(enrica, n)
#Sin datos de algún metal
enrica <- enrica[complete.cases(enrica[metal_groups[[matrix]][["ALL"]]]), ]; n <- check_dels(enrica, n)
#Sin datos de alguna coviariables
enrica <- enrica[complete.cases(enrica[model_covars[[model]]]), ]; n <- check_dels(enrica, n)
#Enfermedad renal crónica severa o missing
enrica <- subset(enrica, !is.na(w17IR_DEGREES) & w17IR_DEGREES<5); n <- check_dels(enrica, n)


enrica <- enrica %>%
  select(all_of(metals),
         all_of(covariates),
         !!sym(outcome))




##We generate an empty matrix to which we will be adding the covariates
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
set.seed(1000)
knots=75
knots_name = paste0("knots", knots)
assign(knots_name, fields::cover.design(lnmixture_z, nd = knots)$design)

#save(list = knots_name, file=paste0("knots/",group,"_ENRICA_",outcome_name,"_knots",knots,".RData"))

################################################
###         Fit Models                       ###
################################################


#####Fit BKMR models WITH Gaussian predictive process using

set.seed(1000)

iter = 50000
fit_name <- paste0("fit_vs_",knots,"knots","_",iter,"iter")
assign(fit_name,kmbayes(y=enrica[[outcome]], Z=lnmixture_z, X=covs_matrix, iter=iter, verbose=TRUE, varsel=TRUE, est.h = TRUE,
                           knots=get(knots_name),
                           family=outcome_type))

file_name <- paste(paste0("fit_models/",matrix,"/",group),"BKMR_ENRICA",outcome_name, model,paste0("vs_knots",knots,"_iter", iter,".RData"), sep = "_")
save(list = fit_name, file=file_name)


load(file_name)
summary(get(fit_name))



###Names to standarize procedure
modeltoplot      <- get(fit_name)   ##Name of model object
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

###Function to later obtain y-axis labels
getrange <- function(x, effect_col, error_col) {
  
  model.name <- deparse(substitute(x))
  
  if (model.name!= "pred.resp.bivar.levels"){
    upper_bound <- x[[effect_col]] + (1.96 * x[[error_col]])
    lower_bound <- x[[effect_col]] - (1.96 * x[[error_col]])
    
    max_val <- max(abs(c(upper_bound, lower_bound)), na.rm = TRUE)
    limit <- ceiling(max_val * 10) / 10
  }
  
  else {  #We apply a different logic for bivariate as no CI are shown and there is a need to save space
    upper_bound <- x[[effect_col]] 
    lower_bound <- x[[effect_col]]
    
    limit <- max(abs(c(upper_bound, lower_bound)), na.rm = TRUE)
  }
  
  return(c(-limit, limit))
}

### Correlation matrix
cor.Z <- cor(lnmixture_z, use="complete.obs")
corrplot.mixed(cor.Z, upper = "ellipse", lower.col="black")

#For appropiate titles
markers <- list(
  grip = "low grip strength",
  chair = "low chair stand performance",
  calf = "low calf circumference",
  sppb = "low SPPB score",
  vel = "low gait speed",
  sarcopenia = "sarcopenia"
  
)

marker <- markers[[outcome_name]]


pips <- ExtractPIPs(modeltoplot); colnames(pips)[1] <- "Metal"

write.csv(pips, file= paste0("tables/pips/", paste(matrix, group, outcome_name, model, sep="_"), ".csv"), row.names = FALSE)

#Table with PIPs
pips <- ggtexttable(pips, rows = NULL,
            theme = ttheme(base_size = 25)) %>%
  annotate_figure(
    top = text_grob(
      paste("Posterior Inclusion Probabilities (PIPs) for", str_to_lower(group), matrix, "metals on",
            marker), size = 14))

ggsave(paste("figures/pips",outcome_name, matrix, group, paste0(model,".png"),sep="_"), plot = pips,  width = 8, height = 6, dpi = 300)


########## UNIVARIATE ##########
range <- getrange(pred.resp.univar, effect_col = "est", error_col = "se")

univariate <- pred.resp.univar %>% 
  ggplot(aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray", linewidth = 0.5) +
  geom_smooth(stat = "identity") + labs(y = "Effect Estimate", x = "Z-score log-transformed serum metal concentrations") + 
  facet_wrap(~ variable) + theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  scale_y_continuous(
    limits = c(range[1], range[2]),
    breaks = seq(range[1], range[2], length.out = 5),
    labels = function(x) sprintf("%.2f", x)
  ) +
  ggtitle(paste("Cross-sectional association between", str_to_lower(group), matrix, "metals and", marker, sep = " "))

#univariate

ggsave(paste("figures/univariate",outcome_name, matrix, group, paste0(model,".png"),sep="_"), plot = univariate,  width = 8, height = 6, dpi = 300)

########## BIVARIATE ##########
range <- getrange(pred.resp.bivar.levels, effect_col = "est", error_col = "se")

bivariate <- pred.resp.bivar.levels %>% 
  ggplot(aes(z1, est)) + 
  geom_smooth(aes(color = quantile), stat = "identity", linewidth = 0.5) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray", linewidth = 0.5) +
  
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
    limits = c(range[1], range[2]),
    breaks = seq(floor((range[1]*0.6)*10)/10, ceiling((range[2]*0.6)*10)/10, length.out = 3),
    labels = function(x) sprintf("%.2f", x)
  ) +
  
  ggtitle(paste0(str_to_sentence(marker),": h(Metal1 | quantiles of Metal2)")) +
  
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

#bivariate

ggsave(paste("figures/bivariate",outcome_name, matrix, group, paste0(model,".png"),sep="_"), plot = bivariate,  width = 8, height = 6, dpi = 300)

########## RISK OVERALL ##########
range <- getrange(risks.overall, effect_col = "est", error_col = "sd")

overall <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray", linewidth = 0.5) +
  geom_pointrange() + theme_bw() +
  scale_y_continuous(
    limits = c(range[1], range[2]),
    breaks = seq(range[1], range[2], length.out = 5),
    labels = function(x) sprintf("%.2f", x)
  ) +
  labs(x = "Mixture quantile", y = "Overall effect estimate") +
  ggtitle(paste(str_to_title(group), matrix, "Metals mixture overall effect on",marker, sep = " "))

#overall

ggsave(paste("figures/overall",outcome_name, matrix, group, paste0(model,".png"),sep="_"), plot = overall,  width = 8, height = 6, dpi = 300)

########## RISK SINGLE VARIABLE ##########
range <- getrange(risks.singvar, effect_col = "est", error_col = "sd")

singvar <- risks.singvar %>% 
  ggplot(aes(variable, est, ymin = est - 1.96*sd,  ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray", linewidth = 0.5) +
  geom_pointrange(position = position_dodge(width = 0.75)) +  coord_flip() + 
  theme_bw() +
  scale_y_continuous(
    limits = c(range[1], range[2]),
    breaks = seq(range[1], range[2], length.out = 5),
    labels = function(x) sprintf("%.2f", x)
  ) +
  labs(x = "", y = "Effect estimate per IQR increase", col = "Fixed Quantile")+
  ggtitle(paste("Conditional estimates across quantiles of", str_to_lower(group), matrix, "metals on",marker, sep = " ")) +
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

#singvar

ggsave(paste("figures/singvar",outcome_name, matrix, group, paste0(model,".png"),sep="_"), plot = singvar,  width = 8, height = 6, dpi = 300)

########## RISK INTERACTION ##########
range <- getrange(risks.int, effect_col = "est", error_col = "sd")

int <- risks.int %>% 
  ggplot(aes(variable, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray", linewidth = 0.5) + coord_flip() + theme_bw() +
  scale_y_continuous(
    limits = c(range[1], range[2]),
    breaks = seq(range[1], range[2], length.out = 5),
    labels = function(x) sprintf("%.2f", x)
  ) +
  labs(x = "", y = "Difference in effect estimate per IQR increase (mixture fixed at P75 - P25)") +
  ggtitle(paste("Difference in", str_to_lower(group), matrix, "metals effects estimate on",marker, "between mixture fixed at P25 and P75", sep = " ")) +
  theme(
    plot.title = element_text(size = 9))

#int

ggsave(paste("figures/interaction",outcome_name, matrix, group, paste0(model,".png"),sep="_"), plot = int,  width = 8, height = 6, dpi = 300)


#Save PDF with the relevant plots and PIPs
pdf(paste(paste0("plots/",matrix,"/", group), "BKMR_ENRICA", outcome_name, model,paste0(iter, ".pdf"), sep = "_"))

pips
TracePlot(fit = modeltoplot, par = "beta", sel = sel, ylab = expression(beta))
univariate
bivariate
overall
singvar
int

dev.off()
