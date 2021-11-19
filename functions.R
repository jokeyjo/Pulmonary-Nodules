# functions

printfn <- function(est,lci,uci,digits = 1,nsmall=1,scale = 1){
  paste0(format(est*scale,digits = digits, nsmall = nsmall), " (", 
         format(lci*scale,digits = digits, nsmall = nsmall)," to ",
         format(uci*scale,digits = digits, nsmall = nsmall),")")
}


binary_FN <- function(truth, rating) {
  MRMCaov:::binary_metric(truth, rating, function(truth, rating) {
    events <- truth == levels(truth)[2]
    pos <- rating == levels(truth)[2]
    sens <- mean(pos[events])
    fns <- 1 - sens
    fns
  })
}


binary_FP <- function(truth, rating) {
  MRMCaov:::binary_metric(truth, rating, function(truth, rating) {
    events <- truth == levels(truth)[2]
    pos <- rating == levels(truth)[2]
    spec <- mean(!pos[!events])
    fps <- 1 - spec
    fps
  })
}



binary_LRpos <- function(truth, rating) {
  MRMCaov:::binary_metric(truth, rating, function(truth, rating) {
    events <- truth == levels(truth)[2]
    pos <- rating == levels(truth)[2]
    sens <- mean(pos[events])
    spec <- mean(!pos[!events])
    if (spec == 1) {
      stop("binary_LRpos undefined for specifity equal 1", call. = FALSE)
    }
    sens / (1 - spec)
  })
}

binary_LRneg <- function(truth, rating) {
  MRMCaov:::binary_metric(truth, rating, function(truth, rating) {
    events <- truth == levels(truth)[2]
    pos <- rating == levels(truth)[2]
    sens <- mean(pos[events])
    spec <- mean(!pos[!events])
    if (sens == 1) {
      stop("binary_LRneg undefined for sensitivity equal 1", call. = FALSE)
    }
    (1 - sens)/spec
  })
}

# bootstrap function for Reclassification analysis

bf = function(d,ind){
  bd = wdt[][ind]
  abs = xtabs(formula = ~ cut(bd[isCancer==0,LOM2r/100],cutoff,right=F) + 
                cut(bd[isCancer==0,LOM1r/100],cutoff, right=F))
  pres = xtabs(formula = ~ cut(bd[isCancer==1, LOM2r/100],cutoff,right=F) + 
                 cut(bd[isCancer==1,LOM1r/100],cutoff, right=F))
  # Cancer up 
  (p1 = sum(pres[lower.tri(pres)])/1800)
  # Cancer down
  (p2 = sum(pres[upper.tri(pres)])/1800)
  # Net cancer 
  nc = p1 - p2 
  # Benign Down 
  (p3 = sum(abs[upper.tri(abs)])/1800)
  ## Benign Up 
  (p4 = sum(abs[lower.tri(abs)])/1800)
  ## Net Benign 
  nb = p3 - p4
  (NRI = p1 - p2 + p3 - p4)
  return(c(p1,p2,nc,p3,p4,nb,NRI))
}

