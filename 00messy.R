# 1 setup R --------------------------------------------------------------------

options(scipen=999) #disables scientific notation
options(max.print=1000000) #enable long outputs

if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('psych')) install.packages('psych'); library('psych')
if (!require('lavaan')) install.packages('lavaan'); library('lavaan')
if (!require('semTools')) install.packages('semTools'); library('semTools')
if (!require('GPArotation')) install.packages('GPArotation'); library('GPArotation')
if (!require('semPlot')) install.packages('semPlot'); library('semPlot')

# 2 load & setup data ----------------------------------------------------------
#load df and filter unused vars
df <- read.csv("bshit.csv")
df <- df[, c(1:35,48:54,83:102)]
cat(colnames(df), sep="\n") #list of vars

#specify missing values (9s for items not used in pilot)
df[36:58][df[36:58] == 9] <- NA
df[59:62][df[59:62] == "9 NA"] <- NA

#if needed exclude small groups for invariance analyses
#df[6][df[6] == "3 NB"] <- NA
#df[6][df[6] == "4 OT"] <- NA

#define vars as ordered, just if needed
#df$stbbu1 <- as.ordered(df$stbbu1)
#df$stbbu2 <- as.ordered(df$stbbu2)
#df$stbbu3 <- as.ordered(df$stbbu3)
#df$stbbu4 <- as.ordered(df$stbbu4)
#df$stbbu5 <- as.ordered(df$stbbu5)
#df$stbbu6 <- as.ordered(df$stbbu6)
#df$stbbu7 <- as.ordered(df$stbbu7)
#df$stbbu8 <- as.ordered(df$stbbu8)
#df$stbse1 <- as.ordered(df$stbse1)
#df$stbse2 <- as.ordered(df$stbse2)
#df$stbse3 <- as.ordered(df$stbse3)
#df$stbse4 <- as.ordered(df$stbse4)
#df$stbse5 <- as.ordered(df$stbse5)
#df$stbse6 <- as.ordered(df$stbse6)
#df$nssgv1 <- as.ordered(df$nssgv1)
#df$nssgv2 <- as.ordered(df$nssgv2)
#df$nssgv3 <- as.ordered(df$nssgv3)
#df$nssgv4 <- as.ordered(df$nssgv4)
#df$nssgv5 <- as.ordered(df$nssgv5)
#df$nssgv6 <- as.ordered(df$nssgv6)
#df$nssgv7 <- as.ordered(df$nssgv7)
#df$nsssc1 <- as.ordered(df$nsssc1)
#df$nsssc2 <- as.ordered(df$nsssc2)
#df$nsssc3 <- as.ordered(df$nsssc3)
#df$nsssc4 <- as.ordered(df$nsssc4)
#df$nsssc5 <- as.ordered(df$nsssc5)
#df$nsssc6 <- as.ordered(df$nsssc6)
#df$sapps1 <- as.ordered(df$sapps1)
#df$sapps2 <- as.ordered(df$sapps2)
#df$sapps3 <- as.ordered(df$sapps3)
#df$sapps4 <- as.ordered(df$sapps4)
#df$sapco1 <- as.ordered(df$sapco1)
#df$sapco2 <- as.ordered(df$sapco2)
#df$sapco3 <- as.ordered(df$sapco3)
#df$sapco4 <- as.ordered(df$sapco4)
#df$sappa1 <- as.ordered(df$sappa1)
#df$sappa2 <- as.ordered(df$sappa2)
#df$sappa3 <- as.ordered(df$sappa3)
#df$sappa4 <- as.ordered(df$sappa4)
#df$sapsc1 <- as.ordered(df$sapsc1)
#df$sapsc2 <- as.ordered(df$sapsc2)
#df$sapsc3 <- as.ordered(df$sapsc3)
#df$sapsc4 <- as.ordered(df$sapsc4)

# 3 univar analyses ------------------------------------------------------------
#freq tables for everything
#type of participant
table(df$unique);table(df$expsit)
#background info
table(df$agepar);table(df$gender)
table(df$region);table(df$poccup)
table(df$religr);table(df$politr)
table(df$pedlev);table(df$fedlev)
#responses sstb
table(df$stbbu1);table(df$stbbu2);table(df$stbbu3);table(df$stbbu4)
table(df$stbbu5);table(df$stbbu6);table(df$stbbu7);table(df$stbbu8)
table(df$stbse1);table(df$stbse2);table(df$stbse3);table(df$stbse4)
table(df$stbse5);table(df$stbse6)
#responses nss
table(df$nssgv1);table(df$nssgv2);table(df$nssgv3);table(df$nssgv4)
table(df$nssgv5);table(df$nssgv6);table(df$nssgv7);
table(df$nsssc1);table(df$nsssc2);table(df$nsssc3);table(df$nsssc4);
table(df$nsssc5);table(df$nsssc6)
#responses crt
table(df$crtco1);table(df$crtco2);table(df$crtco3);table(df$crtco4)
table(df$crtco5);table(df$crtco6);table(df$crtco7)
#responses isap
table(df$sapps1);table(df$sapps2);table(df$sapps3);table(df$sapps4)
table(df$sapco1);table(df$sapco2);table(df$sapco3);table(df$sapco4)
table(df$sappa1);table(df$sappa2);table(df$sappa3);table(df$sappa4)
table(df$sapsc1);table(df$sapsc2);table(df$sapsc3);table(df$sapsc4)

# 4 reliability analyses--------------------------------------------------------

#alpha + omega x instrument
#sstb bullshit
round(psych::alpha(df[,09:16])$total$std.alpha,3) 
round(suppressWarnings(psych::omega(df[,09:16])$omega.tot),3)
#sstb sense
round(psych::alpha(df[,17:22])$total$std.alpha,3) 
round(suppressWarnings(psych::omega(df[,17:22])$omega.tot),3)
#nssgv
round(psych::alpha(df[,23:29])$total$std.alpha,3) 
round(suppressWarnings(psych::omega(df[,23:29])$omega.tot),3)
#nsssc
round(psych::alpha(df[,30:35])$total$std.alpha,3) 
round(suppressWarnings(psych::omega(df[,30:35])$omega.tot),3)
#crt correct responses
round(psych::alpha(df[,36:42])$total$std.alpha,3) 
round(suppressWarnings(psych::omega(df[,36:42])$omega.tot),3)
#isap pseudoscience
round(psych::alpha(df[,43:46])$total$std.alpha,3) 
round(suppressWarnings(psych::omega(df[,43:46])$omega.tot),3)
#isap conspiracy
round(psych::alpha(df[,47:50])$total$std.alpha,3) 
round(suppressWarnings(psych::omega(df[,47:50])$omega.tot),3)
#isap paranormal
round(psych::alpha(df[,51:54])$total$std.alpha,3) 
round(suppressWarnings(psych::omega(df[,51:54])$omega.tot),3)
#isap science
round(psych::alpha(df[,55:58])$total$std.alpha,3) 
round(suppressWarnings(psych::omega(df[,55:58])$omega.tot),3)


# 5 cfas for each instrument ---------------------------------------------------
##cfa sstb
mod <-'
bul =~ stbbu1 + stbbu2 + stbbu3 + stbbu4 + stbbu5 + stbbu6 + stbbu7 + stbbu8
sen =~ stbse1 + stbse2 + stbse3 + stbse4 + stbse5 + stbse6
'
fit <- lavaan::cfa(mod, data=df,estimator="ULSMV")
lavaan::fitMeasures(fit, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr"))
lavaan::standardizedSolution(fit)

##cfa nss
mod <-'
ngv =~ nssgv1 + nssgv2 + nssgv3 + nssgv4 + nssgv5 + nssgv6 + nssgv7
nsc =~ nsssc1 + nsssc2 + nsssc3 + nsssc4 + nsssc5 + nsssc6
'
fit <- lavaan::cfa(mod, data=df,estimator="ULSMV")
lavaan::fitMeasures(fit, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr"))
lavaan::standardizedSolution(fit)

##cfa crt
mod <-'
crt =~ crtco1 + crtco2 + crtco3 + crtco4 + crtco5 + crtco6 + crtco7
'
fit <- lavaan::cfa(mod, data=df,estimator="MLR")
lavaan::fitMeasures(fit, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr"))
lavaan::standardizedSolution(fit)

##cfa isap
mod <-'
pse =~ sapps1 + sapps2 + sapps3 + sapps4
con =~ sapco1 + sapco2 + sapco3 + sapco4
par =~ sappa1 + sappa2 + sappa3 + sappa4
sci =~ sapsc1 + sapsc2 + sapsc3 + sapsc4

'
fit <- lavaan::cfa(mod, data=df,estimator="ULSMV")
lavaan::fitMeasures(fit, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr"))
lavaan::standardizedSolution(fit)

# 6 cfa for full measurement model ---------------------------------------------
#mod1
#mod <-'
#crt =~ crtco1 + crtco2 + crtco3 + crtco4 + crtco5 + crtco6 + crtco7
#bul =~ stbbu1 + stbbu2 + stbbu3 + stbbu4 + stbbu5 + stbbu6 + stbbu7 + stbbu8
#sen =~ stbse1 + stbse2 + stbse3 + stbse4 + stbse5 + stbse6
#ngv =~ nssgv1 + nssgv2 + nssgv3 + nssgv4 +  nssgv5 + nssgv6 + nssgv7
#nsc =~ nsssc1 + nsssc2 + nsssc3 + nsssc4 + nsssc5 + nsssc6
#pse =~ sapps1 + sapps2 + sapps3 + sapps4
#con =~ sapco1 + sapco2 + sapco3 + sapco4
#par =~ sappa1 + sappa2 + sappa3 + sappa4
#sci =~ sapsc1 + sapsc2 + sapsc3 + sapsc4
#'

#mod2
mod <-'
crt =~ crtco1 + crtco2 + crtco3 + crtco4 + crtco6 + crtco7 #crtco5
bul =~ stbbu2 + stbbu3 + stbbu4 + stbbu6 + stbbu7 + stbbu8 #stbbu1 stbbu5
sen =~ stbse1 + stbse2 + stbse4 + stbse5 + stbse6 # stbse3
ngv =~ nssgv1 + nssgv2 + nssgv3 + nssgv4 + nssgv7 # nssgv5 nssgv6
nsc =~ nsssc1 + nsssc3 + nsssc4 + nsssc5 #nsssc2 nsssc6
pse =~ sapps1 + sapps2 + sapps3 #sapps4
con =~ sapco1 + sapco2 + sapco4 #sapco3
par =~ sappa1 + sappa2 + sappa4 #sappa3
sci =~ sapsc1 + sapsc2 + sapsc4 #sapsc3
'
fit <- lavaan::cfa(mod, data=df,estimator="MLR")
#fit <- lavaan::cfa(mod, data=df,estimator="WLSMV")
lavaan::fitMeasures(fit, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr"))
lavaan::standardizedSolution(fit)

# 7 cfa diagrams ---------------------------------------------------------------

semPlot::semPaths(fit, whatLabels = "std", layout = "circle", edge.label.cex = 0.8,structural=TRUE)
semPlot::semPaths(fit, whatLabels = "std", layout = "tree", edge.label.cex = 0.8,structural=TRUE)
semPlot::semPaths(fit, whatLabels = "std", layout = "spring", edge.label.cex = 0.8,structural=TRUE)

semPlot::semPaths(fit, whatLabels = "std", layout = "circle", edge.label.cex = 0.8,structural=FALSE)
semPlot::semPaths(fit, whatLabels = "std", layout = "tree", edge.label.cex = 0.8,structural=FALSE)
semPlot::semPaths(fit, whatLabels = "std", layout = "spring", edge.label.cex = 0.8,structural=FALSE)

semPlot::semPaths(fit, whatLabels = "std", layout = "circle", 
                  edge.label.cex = 0.8, structural=FALSE,
                  sizeMan = 3,      # Adjust the size of manifest variables
                  sizeLat = 5)      # Adjust the size of latent variables
semPlot::semPaths(fit, whatLabels = "std", layout = "tree", 
                  edge.label.cex = 0.8, structural=FALSE,
                  sizeMan = 3,      # Adjust the size of manifest variables
                  sizeLat = 5)      # Adjust the size of latent variables
semPlot::semPaths(fit, whatLabels = "std", layout = "spring", 
                  edge.label.cex = 0.8, structural=FALSE,
                  sizeMan = 3,      # Adjust the size of manifest variables
                  sizeLat = 5)      # Adjust the size of latent variables




# 8 playing with factor scores  ------------------------------------------------
df2 <- data.frame(lavaan::lavPredict(fit,append.data=FALSE))
round(cor(df2),3)
cat(colnames(df2), sep="\n") #list of vars
hist(scale(df2$crt))
hist(scale(df2$bul))
hist(scale(df2$sen))
hist(scale(df2$ngv))
hist(scale(df2$nsc))
hist(scale(df2$pse))
hist(scale(df2$con))
hist(scale(df2$par))
hist(scale(df2$sci))

plot(df2$crt,df2$pse)
plot(df2$nsc,df2$sci)

# 9 Multigroup CFAs -------------------------------------------------------
#invariance x gender
fitconfig <- lavaan::cfa(mod, data=df, estimator="MLR", group="gender")
fitmetric <- lavaan::cfa(mod, data=df, estimator="MLR", group="gender", group.equal = c("loadings"))
fitscalar <- lavaan::cfa(mod, data=df, estimator="MLR", group="gender", group.equal = c("loadings","intercepts"))
fitresidu <- lavaan::cfa(mod, data=df, estimator="MLR", group="gender", group.equal = c("loadings","intercepts","residuals"))
paste(round(lavaan::fitMeasures(fitconfig, c("chisq","df","cfi.scaled","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")),3))
paste(round(lavaan::fitMeasures(fitmetric, c("chisq","df","cfi.scaled","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")),3))
paste(round(lavaan::fitMeasures(fitscalar, c("chisq","df","cfi.scaled","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")),3))
paste(round(lavaan::fitMeasures(fitresidu, c("chisq","df","cfi.scaled","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")),3))
lavaan::anova(fitconfig,fitmetric,fitscalar,fitresidu)
