# STEP-by-STEP Multiple Mean Comparison analysis
# 05/06/2021
# Author: David Bandiera, contact at bandiera.david@gmail.com

# AUTHOR'S COMMENTS: 
# In this code you will find a step by step method to compute a Multiple Mean Comparison analysis in R. In this case we have 2 factors, the time and the group. 
# The time is a within subject factor (4 conditions: PreMatch, 24h, 48h and 72h) and the group is a between-subject factor (2 conditions: HWI or CWI).
# First we will load the packages required and open the data file "Example-MultipleMeanComparison.csv". 
# Then, we will perform descriptive statistics, plot data graph to observe data, check that basal data are not different between group (shouldn't be different 
# because no treatment has been applied yet, if different, data has to be expressed in % of the basal value), check for normality and homogeneity, perform 
# non-parametric or parametric-test and finally save the results. 
# Thank you and have fun !
# Please feel free to reach me if you have any question. 

# STUDY-CASE: 
# 22 subjects played a football simulated game. Before the match ("PreMatch"), 24h, 48h and 72h after, they performed isometric maximal voluntary force measurment (IMVC). 
# After the match, subjects were divided in 2 groups experiencing 25-min sessions of either cold water immersion (CWI, n = 11) or 
# hot water immersion (HWI, n = 11). We want to know if the immersion temperature have an impact on the force recovery. To do so, we will 
# perform an Multiple Mean Comparison analysis. 

# SOURCES :
#https://www.datanovia.com/en/lessons/mixed-anova-in-r/

# PACKAGE COMPILATION
library("mise")
mise() #clear/close/delete variables
library("ggpubr")
library("rstatix")
library("tidyverse")
library("readxl")
library("data.table")
library("PMCMRplus")

# OPEN THE DATA FILE
ds <- read.csv2("~/Example-MultipleMeanComparison.CSV") #for CSV. Write the path where your dataset is stored.
#ds <- read_excel("~/yourdatafile.XLSX") #For XLSX. 

# ADD 2 COLUMNS TO DETERMINE THE TIME AND THE TYPE OF BATH
ds$Bath<-ifelse(grepl("_A_", ds$Code), "HWI", "CWI")
ds$Time<-ifelse(grepl("_1", ds$Code), 'PreMatch', ifelse(grepl("_2", ds$Code), '24h', ifelse(grepl("_3", ds$Code), '48h', '72h')))
ds$Time <- factor(ds$Time, levels = c("PreMatch", "24h", "48h", "72h"))

# CHOOSE THE VARIBALE (here example with IMVC)
ds$param <- ds$IMVC
ylab1 <- "IMVC"
ds$id <- sub("_.*", "", ds$Code) # Extract text before "_" in ds$Code. Extract the subject ID (ex : 'S1'). Needed for Mixed model ANOVA (line 161). 

# SUPRESS EXTREME OUTLIERS
#If extreme outliers (as determined in line 79), suppress subject(s) and restart from line 53.
#ds <- ds[!(ds$id == "S1"),] #code to suppress a subject, here an example with Subject 1 = S1. 

# DETERMINE ds$id AS FACTOR
ds$id <- as.factor(ds$id)

# SUMMARY STATISTICS
resume <- ds %>%
  group_by(Bath, Time) %>%
  get_summary_stats(param, type = "mean_sd")

# VISUALIZE THE DATA
plot1 <- ggline(
  ds, 
  x = "Time", 
  y = "param",
  #ylim = c(0,1100),
  ylab = ylab1,
  xlab = "",
  color = "Bath", 
  add = c("mean_sd", "jitter"), #remove "jitter" if you don'y want to see individual measures
  add.params = list(size = 2, alpha = 0.2),
  order = c("PreMatch", "24h", "48h", "72h"),
  palette = c("#DA483B", "#4486F4"),
  error.plot = "errorbar",
) + theme(legend.position = "none") 
plot1

# RESEARCH FOR OUTLIERS
#If extreme outliers, come back to line 32. 
Outliers <- ds %>%
  group_by(Time, Bath) %>%
  identify_outliers(param)
view(Outliers)

# DISTRIBUTION OF THE DATA AT PREMATCH (BASAL VALUES)
#Is there a significant difference between CWI and HWI at PreMatch? Do a mean independant comparison between CWI and HWI. 
#If yes : put values in % of PreMatch Values -> See after, line 114. 
#If no, we can continue with raw data.
ParamPre <- c(ds$param[ds$Time == "PreMatch"])
IDPre <- c(ds$id[ds$Time == "PreMatch"])
BathPre <- c(ds$Bath[ds$Time == "PreMatch"])
dsPre <- data.frame(IDPre, BathPre, ParamPre) #New dataframe created with only PreMatch data. 

Normalite_Pre <- dsPre %>% #normality check
  group_by(BathPre) %>%
  shapiro_test(ParamPre)
Normalite_Pre

dsPre$BathPre <- as.factor(dsPre$BathPre) #homogeneity check because independent groups
dsPre %>% levene_test(ParamPre ~ BathPre)

stat.test.Pre <- dsPre %>% #Wilcoxon non-parametric test if normality is not verified (Significant Shapiro's test). 
  rstatix::wilcox_test(ParamPre ~ BathPre, 
                       detailed = TRUE) %>%
  add_significance()
stat.test.Pre

stat.test.Pre <- dsPre %>% #Student t-test parametric test if normality is verified (Non-significant Shapiro's test). 
  t_test(ParamPre ~ BathPre, 
         detailed = TRUE, 
         var.equal = TRUE,) %>% #var.equal = TRUE if homogeneity is verified (Levene's test non significant). var.equal = FALSE if not. 
  add_significance()
stat.test.Pre

# If CWI and HWI PreMatch values are significantly different = express data as % of PreMatch values. 
#  for (n in 1:n_levels) { 
#   subject <- levels(ds$id)[n]
#   ref <- ds$param[ds$Time == "PreMatch" & ds$id == subject] # basal value = PreMatch.
#   ds$param2[ds$Time == "PreMatch" & ds$id == subject] <- (ds$param[ds$Time == "PreMatch" & ds$id == subject]/ref-1)*100
#   ds$param2[ds$Time == "24h" & ds$id == subject] <- (ds$param[ds$Time == "24h" & ds$id == subject]/ref-1)*100
#   ds$param2[ds$Time == "48h" & ds$id == subject] <- (ds$param[ds$Time == "48h" & ds$id == subject]/ref-1)*100
#   ds$param2[ds$Time == "72h" & ds$id == subject] <- (ds$param[ds$Time == "72h" & ds$id == subject]/ref-1)*100
#   }
# ds$param <- NA; ds$param <- ds$param2

# ds3 <- ds #Summary Stats in % of PreMatch.
# ds <- ds[!(ds$Time == "PreMatch"),] 
# resume <- ds %>%
#   group_by(Bath, Time) %>%
#   get_summary_stats(param, type = "mean_sd")

# OBSERVE INDIVIDUAL DATA
#Spaghetti plots.
interaction.plot(ds$Time[ds$Bath == "HWI"], ds$id[ds$Bath == "HWI"], ds$param[ds$Bath == "HWI"], legend = T, col = c(1:11))
interaction.plot(ds$Time[ds$Bath == "CWI"], ds$id[ds$Bath == "CWI"], ds$param[ds$Bath == "CWI"], legend = T, col = c(1:11))

# NORMALITY ASSUMPTION
shapiroIndividuel <- ds %>%
  group_by(Time, Bath) %>%
  shapiro_test(param)
shapiroIndividuel

ggqqplot(ds, "param", ggtheme = theme_bw()) +
 facet_grid(Time ~ Bath)

# HOMOGENEITY ASSUMPTION
#for between subject group = Bath. 
ds$Bath <- as.factor(ds$Bath)
homogeneity <- ds %>% 
  group_by(Time) %>% 
  levene_test(param ~ Bath)
homogeneity

# NON-PARAMETRIC TEST 
#If data are not normally distributed. 
ds$Time <- factor(ds$Time)
ds$id <- factor(ds$id)
Oneway.Time <- friedman_test(param~Time | id, data = ds) #Within group factor: Friedman test. 
Oneway.Time
ConoverTime <- frdAllPairsConoverTest(ds$param, ds$Time, ds$id, p.adjust.method = "bonferroni") #Post-Hoc Conover Test.
ConoverTime

Oneway.Bath <- kruskal.test(param~Bath, data = ds) #Between group factor: Kruskal-Wallis test. 
Oneway.Bath
pwcBath <- ds %>% #Post-hoc Wilcoxon test. 
  rstatix::wilcox_test(param ~ Bath, 
                       detailed = TRUE) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
pwcBath
#No interaction time*group for non-parametric test. 

# PARAMETRIC TEST : TWO WAY MIXED ANOVA TEST
#If data are distributed normally. 
#Mauchly's test for sphericity assumption is automatically performed and Greenhouse correction is suggested if needed.
ds2 <- data.frame(id = ds$id, param = ds$param, Bath = ds$Bath, Time = ds$Time) # New Dataset to avoid the R error : "Error in `contrasts..." 
res.aov.brut <- anova_test(
  data = ds2, #dataset.
  dv = param, #[numeric] the dependent (or outcome) variable name.
  wid = id,   #[factor] column name containing individuals/subjects identifier. Should be unique per individual.
  between = Bath, #between subject factor. 
  within = Time,  #within subject factor. 
  effect.size = "pes", 
)
MixedResAov <- get_anova_table(res.aov.brut)
view(MixedResAov)

# POST-HOC TEST FOR ANOVA

#IF SIGNIFICANT INTERACTION:
#Group simple main effect: Effect of Group at each Time
SimpleMainEffect.Bath <- ds2 %>% # do not work when data are expressed in % of PreMatch because CWI and HWI first values are 0%. 
  group_by(Time) %>%
  anova_test(dv = param, wid = id, between = Bath, effect.size = "pes") %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
SimpleMainEffect.Bath
#If significant : mean comparison between Group at each Time.
pwcInterBath <- ds2 %>%
  group_by(Time) %>%
  pairwise_t_test(param ~ Bath, p.adjust.method = "bonferroni")
#Visualize the data. Group effect. 
pwcInterBath <- pwcInterBath %>% add_xy_position(x = "Time")
plotbarTime <- ggbarplot(
  ds, 
  x = "Time", 
  y = "param",
  ylab = ylab1,
  add = c("mean_sd", "point"),
  color = "Bath", 
  palette = c("#DA483B", "#4486F4"),
  position = position_dodge(0.8),
)
plotbarTime + # display significant differences on the graph
  stat_pvalue_manual(pwcInterBath, tip.length = 0, hide.ns = FALSE) +
  labs(
    subtitle = get_test_label(MixedResAov, detailed = TRUE),
    caption = get_pwc_label(pwcInterBath)
  )

#Time Effect: Effet of Time for each Group
SimpleMainEffect.Time <- ds2 %>%
  group_by(Bath) %>% 
  anova_test(dv = param, wid = id, within = Time, effect.size = "pes") %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
SimpleMainEffect.Time
#If significant : mean comparison between Time at for each Group.
pwcInterTime <- ds2 %>%
  group_by(Bath) %>%
  pairwise_t_test(
    param ~ Time, paired = TRUE, #within subject factor 
    p.adjust.method = "bonferroni"
  ) %>%
  select(-df, -statistic, -p) # Remove details
#Visualize the data. Time effect. 
pwcInterTime <- pwcInterTime %>% add_xy_position(x = "Time")
plot1 + # display significant differences on the graph
  stat_pvalue_manual(pwcInterTime, tip.length = 0, hide.ns = TRUE, color = "Bath") #+
  #labs(
  #  subtitle = get_test_label(MixedResAov, detailed = TRUE),
  #  caption = get_pwc_label(pwcInterTime)
  #)

# IF NON-SIGNIFICANT INTERACTION:
# IF SIGNIFICANT GROUP EFFECT 
pwcBath <- ds2 %>% #mean comparison for group
  pairwise_t_test(
    param ~ Bath, 
    p.adjust.method = "bonferroni"
  )
pwcBath
#Plot group main effect 
plotbarBath <- ggbarplot(
  ds, 
  x = "Bath", 
  y = "param",
  #ylim = c(100,1200),
  ylab = "Recovery of IMVC (N)",
  xlab = "",
  add = c("mean_sd", "jitter"),
  color = "Bath", 
  palette = c("#DA483B", "#4486F4"),
  error.plot = "upper_errorbar",
  width = 0.6,
  #position = position_dodge(0.1),
) + theme(legend.position = "none") 
plotbarBath
pwcBath <- pwcBath %>% add_xy_position(x = "Time")
#pwcBath$y.position <- 1300 # height of the significant bars
plotbarBath + # add significant bars
  stat_pvalue_manual(pwcBath, tip.length = 0, hide.ns = FALSE)# +
  #labs(
  #  subtitle = get_test_label(MixedResAov, detailed = TRUE),
  #  caption = get_pwc_label(pwcBath)
  #)

# IF SIGNIFICANT TIME EFFECT 
pwcTime <- ds2 %>%
  pairwise_t_test(
    param ~ Time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
view(pwcTime)
#Plot time main effect 
pwcTime <- pwcTime %>% add_xy_position(x = "Time")
#pwcTime$y.position[1] <- 1190 # height of the significant bars
#pwcTime$y.position[5] <- 1220 # height of the significant bars
plot1 + 
 stat_pvalue_manual(pwcTime, tip.length = 0, hide.ns = TRUE) # +
  # labs(
  #   subtitle = get_test_label(MixedResAov, detailed = TRUE),
  #   caption = get_pwc_label(pwcTime)
  # )

# SAVE RESULTS AND GRAPH
#Descriptive data
setwd("~/Descriptives/") #write the path where you want to save data
resume_name <- paste("Descriptives-", ylab1, ".csv", sep = "")
write_csv(resume, resume_name, col_names = TRUE)
#Statistic Test
test_name1 <- paste("Test-", ylab1, ".csv", sep = "")
write_csv(MixedResAov, test_name1, col_names = TRUE)
#fwrite(pwcTime, test_name) # for post-hoc test
#fwrite(Oneway.Time, test_name1) #for time non-parametric test
#fwrite(Oneway.Bath, test_name1) #for group non-parametric test
#Graph
plotname <- paste("Graph-", ylab1, ".eps", sep = "")
ggsave(file = plotname, width = 9, height = 7.17, units = "cm") #save the graph currently displayed

# END 
