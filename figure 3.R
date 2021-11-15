# Figure 3 

source("src/set the data.R")

a1 <- mrmc(empirical_auc(isCancer, rating),
           test = test,
           reader = reader,
           case = index,
           data = DT, 
           method = "DeLong",  
           design = 1)

# Summary output from model 
su <- summary(a1)
tms <- su$test_means
td <- su$test_diffs

# AUC 95% CI (Without CAD)
e1 <- tms[1,'Estimate']
lci1 <- tms[1,'CI'][1]
uci1 <- tms[1,'CI'][2]

# AUC 95% CI (With CAD)
e2 <- tms[2,'Estimate']
lci2 <- tms[2,'CI'][1]
uci2 <- tms[2,'CI'][2]

# Difference (With - Without CAD)
e3 <- td$Estimate*-1
lci3 <- td$CI[,'Upper']*-1
uci3 <- td$CI[,'Lower']*-1

# merge reader_responses data set with patient data file 
dtlm1 <- merge(rrlm1,pt, by ="index")
dtlm2 <- merge(rrlm2,pt, by ="index")

# Drawing curves based on empirical ROC analysis
roc1 <- roc(dtlm1$isCancer,dtlm1$rating)
roc2 <- roc(dtlm2$isCancer,dtlm2$rating)

# true positive and specificity 
tpr1 <- roc1$sensitivities
tpr2 <- roc2$sensitivities 
fpr1 <- 1 - roc1$specificities 
fpr2 <- 1 - roc2$specificities

aucstr1 <- printfn(e1,lci1,uci1,digits = 2, nsmall= 2)
aucstr2 <- printfn(e2, lci2,uci2,digits = 2,nsmall = 2)

# Plot ------------------------------------------------------

#png(file = "figures/figure 3.png", res = 300, width = 1800*1.3, height=1800)

plot.new()

plot.window(xlim = c(0,1),ylim =c(0,1))

rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray98")

box(col = "black")

grid(col = "azure2")

lines(x = fpr1, y= tpr1,lwd = 1.8, type = "l",col = "steelblue3")

lines(x = fpr2, y = tpr2,lwd = 1.8, type = "l", col = "gold2")

axis(1,cex.axis = 0.8)

axis(2,cex.axis = 0.8)

title(xlab = "False positive rate (1 - specificity)", 
      ylab = "True positive rate (sensitivity)", 
      cex.lab = 0.8)

legend(x = 0.55, y = 0.3, 
       legend=c(paste0("Without CAD: ",aucstr1), 
                paste0("With CAD: ", aucstr2)
                ),
       col=c("steelblue3","gold2"), lty = c(1,1), cex=0.8,
       title="AUC (95% CI)", text.font=4, bg='azure')

#dev.off()

