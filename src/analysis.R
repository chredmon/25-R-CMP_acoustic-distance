## Charles Redmon
## 2024-11-19

## packages
library(plyr)
library(reshape2)
library(tuneR)
library(dtw)
library(soundgen)

## functions
to_dB <- function(x) { 20 * log10(x / 0.00002) }

## data
ddir <- "../data/CaST"

## lexicon
items <- read.csv("items.csv") 
el <- read.csv("min-pairs.csv")

## items <- items[, c("Item", "Phon", "Nsyll")]
## names(items) <- c("WORD", "PHON", "NSYLL")

## ## generate edge list
## el <- data.frame(W1="", W2="", P1="", P2="", X1="", X2="")

## for (i in 1:(nrow(items)-1)) {
    
##     cat(paste0("\r", round(100*(i/14766), 1), "%"))
    
##     w1 <- items[i, "WORD"]
##     p1 <- items[i, "PHON"]
##     ns1 <- items[i, "NSYLL"]
##     p1v <- strsplit(p1, "")[[1]]
    
##     for (j in (i+1):nrow(items)) {

##         w2 <- items[j, "WORD"]
##         p2 <- items[j, "PHON"]

##         ns2 <- items[j, "NSYLL"]

##         if (nchar(p1) == nchar(p2) & ns1 == ns2) {

##             p2v <- strsplit(p2, "")[[1]]            
##             diff1 <- p1v[p1v != p2v]
            
##             if (!is.null(diff) & length(diff1) == 1) {

##                 diff2 <- p2v[p2v != p1v]
##                 el1 <- data.frame(W1=w1, W2=w2, P1=p1, P2=p2,
##                                   X1=diff1, X2=diff2)
##                 el <- rbind(el, el1)
                
##             }
            
##         }
                
##     }

## }


## el <- el[-1, ]

## el$Contrast <- ifelse(el$X1 < el$X2, paste0(el$X1, "--", el$X2),
##                       paste0(el$X2, "--", el$X1))

## saveRDS(el, "../data/min-pairs.rds")
## write.csv(el, "../data/min-pairs.csv", row.names=FALSE)

## el <- readRDS("../data/min-pairs.rds")

## ## remove words not in LexF
## words <- unique(c(el$W1, el$W2))
## words <- paste0(words, ".wav")
## missing_words <- words[!(words %in% list.files("../data/LexF"))]
## missing_words <- append(missing_words,
##                         words[!(words %in% list.files("../data/LexM"))])
## missing_words <- gsub(".wav", "", missing_words, fixed=TRUE)
## el <- el[!(el$W1 %in% missing_words | el$W2 %in% missing_words), ]

## saveRDS(el, "../data/model-pairs.rds")
## write.csv(el, "../data/model-pairs.csv", row.names=FALSE)

## el <- readRDS("../data/model-pairs.rds")


## parameter set
     ## melfcc(samples, sr = samples@samp.rate, wintime = 0.025, 
     ##     hoptime = 0.01, numcep = 12, lifterexp = 0.6, htklifter = FALSE,
     ##     sumpower = TRUE, preemph = 0.97, dither = FALSE,
     ##     minfreq = 0, maxfreq = sr/2, nbands = 40, bwidth = 1, 
     ##     dcttype = c("t2", "t1", "t3", "t4"), 
     ##     fbtype = c("mel", "htkmel", "fcmel", "bark"), usecmp = FALSE, 
     ##     modelorder = NULL, spec_out = FALSE, frames_in_rows = TRUE)


## set up data for simulation
el$F1_1 <- 0
el$F1_2 <- 0
el$M1_1 <- 0
el$M1_2 <- 0
el$F2_1 <- 0
el$F2_2 <- 0
el$M2_1 <- 0
el$M2_2 <- 0



## fScale <- c("mel", "htkmel", "fcmel", "bark")
## nCep <- c(6, 8, 10, 12, 14, 16)
## nBands <- c(16, 18, 20, 22, 24, 26)
## freqLim <- c(5000, 6000, 7000, 8000, 9000, 10000, 11000)
## winSize <- c(0.015, 0.02, 0.025, 0.03, 0.035, 0.04) 
## stepSize <- c(0.005, 0.01, 0.015, 0.02, 0.025, 0.03)
## tGrid <- tGrid[tGrid$win >= tGrid$step + 0.01, ] 
## powSpec <- c(0, 1)
## dct <- c("t1", "t2", "t3", "t4")
## preEmph <- c(0, 0.91, 0.93, 0.95, 0.97, 0.99)
## plp <- c(0, 1)
## htklift <- c(0, 1)

library(doParallel)
registerDoParallel(cores=2)

foreach (i=1:2) %dopar% {

    par_i <- ifelse(htklift[i] == 0, FALSE, TRUE)
        
    el1 <- el

    for (sp in c("F1", "M1", "F2", "M2")) {

        for (rp in c(1, 2)) {

            items1 <- items[items$Speaker == sp & items$Rep == rp, ]
            
            for (j in 1:nrow(el1)) {
                
                s1 <- el1[j, "S1"]
                s2 <- el1[j, "S2"]
                
                s1_fname <- as.character(items1[items1$Phon == s1, "Filepath"])
                s2_fname <- as.character(items1[items1$Phon == s2, "Filepath"])
                
                s1 <- readWave(file.path(ddir, s1_fname))
                s2 <- readWave(file.path(ddir, s2_fname))

                ## word 1
                c1 <- melfcc(s1, maxfreq=8000, nbands=20, htklifter=TRUE,
                             lifterexp=22, sumpower=FALSE, fbtype="htkmel",
                             dcttype="t3", frames_in_rows=FALSE)
                c1a <- getRMS(s1, windowLength=25, step=10)
                c1a <- as.numeric(c1a$detailed)
                c1a <- to_dB(c1a)
                c1a <- c1a[1:dim(c1)[2]]

                c1[1, ] <- c1a

                ## word 2
                c2 <- melfcc(s2, maxfreq=8000, nbands=20, htklifter=TRUE,
                             lifterexp=22, sumpower=FALSE, fbtype="htkmel",
                             dcttype="t3", frames_in_rows=FALSE)
                
                c2a <- getRMS(s2, windowLength=25, step=10)
                c2a <- as.numeric(c2a$detailed)
                c2a <- to_dB(c2a)
                c2a <- c2a[1:dim(c2)[2]]
                
                c2[1, ] <- c2a

                edist <- try(dXm <- dtw(t(c1), t(c2))$normalizedDistance,
                             silent=TRUE)

                el1[j, paste0(sp, "_", rp)] <- mean(edist)
        
            }
            
        }

    }

    outfname <- paste0("htk_baseline.csv")
    write.csv(el1, file.path("output", outfname), row.names=FALSE)
    
}
    

stopImplicitCluster()


## plot
library(reshape2)
library(ggplot2)
d <- read.csv("output/htk_baseline.csv")
d1 <- melt(d, id.vars=c("DiffPos", "S1", "S2", "Contrast", "PhonX", "FeatX"),
           measure.vars=c("F1_1", "F1_2", "M1_1", "M1_2", "F2_1", "F2_2", "M2_1", "M2_2"),
           variable.name="Set", value.name="ADist")

pd1 <- ddply(d1, .(DiffPos, FeatX), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))

featdist <- c(rep(1, times=10), rep(2, times=5), 3)
names(featdist) <- c("voi_aff", "voi_fric", "voi_plos", "man_obs", "man_son",
                     "plc_vwl", "plc_fric", "plc_nas", "plc_plos", "nasXlat",
                     "voiXman", "voiXplc", "manXplc_obs", "manXplc_son",
                     "nasXson", "multi")                 

pd1$FDist <- mapvalues(pd1$FeatX, from=names(featdist), to=featdist)

pd1 <- pd1[pd1$FDist == 1, ]

pd1$FeatX <- mapvalues(pd1$FeatX,
                       from=c("voi_aff", "voi_fric", "voi_plos", "man_obs", "man_son",
                              "plc_vwl", "plc_fric", "plc_nas", "plc_plos", "nasXlat"),
                       to=c("V (af)", "V (fr)", "V (pl)", "M (obs)", "M (son)",
                            "Vowel", "P (fr)", "P (nas)", "P (pl)", "n-l"))

pd1$FeatX <- factor(pd1$FeatX, levels=c("Vowel", "V (fr)", "V (pl)", "V (af)",
                                        "M (obs)", "M (son)",
                                        "P (fr)", "P (nas)", "P (pl)", "n-l"))

pd1$DiffPos <- factor(pd1$DiffPos, levels=c("V", "CV", "VC"))
pd1$FDist <- as.integer(pd1$FDist)


names(pd1)[1] <- "Position"

p <- ggplot(pd1, aes(x=FeatX, y=mu, colour=Position)) +
    geom_pointrange(aes(ymin=lwr, ymax=upr), position=position_dodge(0.5),
                    linewidth=1) +
    scale_colour_manual(values=c("black", "#389826", "#9558B2")) +
    scale_x_discrete(guide=guide_axis(angle=45)) +
    labs(x="Feature", y="Normalised Distance") +
    theme_classic() +
    theme(legend.position="top",
          axis.text.x=element_text(colour="black", size=12),
          axis.title.x=element_text(size=14),
          axis.text.y=element_text(colour="black", size=12),
          axis.title.y=element_text(size=14))

p

ggsave("htk-baseline.png", p, width=10, height=4, units="in", dpi=600)


## Frequency Range

dfns <- list.files("output", "^flim*", full.names=TRUE)
dL <- lapply(dfns, read.csv)
flim <- c(10, 11, 5, 6, 7, 8, 9)

for (i in 1:7) {

    dL[[i]][, "FLim"] <- flim[i]

}

d1 <- do.call("rbind", dL)

d1 <- melt(d1, id.vars=c("DiffPos", "S1", "S2", "Contrast", "PhonX", "FeatX", "FLim"),
           measure.vars=c("F1_1", "F1_2", "M1_1", "M1_2", "F2_1", "F2_2", "M2_1", "M2_2"),
           variable.name="Set", value.name="ADist")

feats <- c("man_obs", "man_son", "nasXlat", "plc_fric", "plc_nas",
           "plc_plos", "plc_vwl", "voi_aff", "voi_fric", "voi_plos")

feat_red <- c("M (obs)", "M/P (son)", "M/P (son)", "P (obs)", "M/P (son)",
              "P (obs)", "P (vwl)", "VOI", "VOI", "VOI")

d1$FeatX <- mapvalues(d1$FeatX, from=feats, to=feat_red)

pd0 <- ddply(d1, .(FLim), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))

pd0$FeatX <- "All"

d1 <- d1[d1$FeatX %in% feat_red, ]


pd1 <- ddply(d1, .(FeatX, FLim), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))


d1$FeatX <- factor(d1$FeatX, levels=c("P (vwl)", "M/P (son)", "P (obs)", "M (obs)", "VOI"))

pd <- rbind(pd0, pd1)

pd$FeatX <- factor(pd$FeatX,
                   levels=c("All", "P (vwl)", "M/P (son)", "P (obs)", "M (obs)", "VOI"))

cols <- c(black="#000000", red="#CB3C33", blue="#4063D8",
          green="#389826", purple="#9558B2", pink="#CB3376")

names(cols) <- NULL

p <- ggplot(pd, aes(x=FLim, y=mu, colour=FeatX)) +
    geom_pointrange(aes(ymin=lwr, ymax=upr), position=position_dodge(0.5),
                    linewidth=1) +
    geom_line(position=position_dodge(0.5)) +
    scale_colour_manual(values=cols) +
    scale_x_continuous(breaks=seq(5, 11, by=1)) +
    labs(colour="Feature", x="Frequency Limit (kHz)", y="Normalised Distance") +
    theme_classic() +
    theme(legend.position="top",
          axis.text.x=element_text(colour="black", size=12),
          axis.title.x=element_text(size=14),
          axis.text.y=element_text(colour="black", size=12),
          axis.title.y=element_text(size=14))

p

ggsave("freq-lim.png", p, width=8, height=7, units="in", dpi=600)


## Frequency Bands

dfns <- list.files("output", "^nband*", full.names=TRUE)
dL <- lapply(dfns, read.csv)
nband <- c(16, 18, 20, 22, 24, 26)

for (i in 1:6) {

    dL[[i]][, "NBand"] <- nband[i]

}

d1 <- do.call("rbind", dL)

d1 <- melt(d1, id.vars=c("DiffPos", "S1", "S2", "Contrast", "PhonX", "FeatX", "NBand"),
           measure.vars=c("F1_1", "F1_2", "M1_1", "M1_2", "F2_1", "F2_2", "M2_1", "M2_2"),
           variable.name="Set", value.name="ADist")

feats <- c("man_obs", "man_son", "nasXlat", "plc_fric", "plc_nas",
           "plc_plos", "plc_vwl", "voi_aff", "voi_fric", "voi_plos")

feat_red <- c("M (obs)", "M/P (son)", "M/P (son)", "P (obs)", "M/P (son)",
              "P (obs)", "P (vwl)", "VOI", "VOI", "VOI")

d1$FeatX <- mapvalues(d1$FeatX, from=feats, to=feat_red)

pd0 <- ddply(d1, .(NBand), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))

pd0$FeatX <- "All"

d1 <- d1[d1$FeatX %in% feat_red, ]


pd1 <- ddply(d1, .(FeatX, NBand), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))


d1$FeatX <- factor(d1$FeatX, levels=c("P (vwl)", "M/P (son)", "P (obs)", "M (obs)", "VOI"))

pd <- rbind(pd0, pd1)

pd$FeatX <- factor(pd$FeatX,
                   levels=c("All", "P (vwl)", "M/P (son)", "P (obs)", "M (obs)", "VOI"))

cols <- c(black="#000000", red="#CB3C33", blue="#4063D8",
          green="#389826", purple="#9558B2", pink="#CB3376")

names(cols) <- NULL

p <- ggplot(pd, aes(x=NBand, y=mu, colour=FeatX)) +
    geom_pointrange(aes(ymin=lwr, ymax=upr), position=position_dodge(0.5),
                    linewidth=1) +
    geom_line(position=position_dodge(0.5)) +
    scale_colour_manual(values=cols) +
    scale_x_continuous(breaks=nband) +
    labs(colour="Feature", x="Number of Frequency Bands", y="Normalised Distance") +
    theme_classic() +
    theme(legend.position="top",
          axis.text.x=element_text(colour="black", size=12),
          axis.title.x=element_text(size=14),
          axis.text.y=element_text(colour="black", size=12),
          axis.title.y=element_text(size=14))

ggsave("n-bands.png", p, width=8, height=7, units="in", dpi=600)



## Cepstral Coefficients

dfns <- list.files("output", "^ncep*", full.names=TRUE)
dL <- lapply(dfns, read.csv)
ncep <- c(10, 12, 14, 16, 6, 8)

for (i in 1:6) {

    dL[[i]][, "NCep"] <- ncep[i]

}

d1 <- do.call("rbind", dL)

d1 <- melt(d1, id.vars=c("DiffPos", "S1", "S2", "Contrast", "PhonX", "FeatX", "NCep"),
           measure.vars=c("F1_1", "F1_2", "M1_1", "M1_2", "F2_1", "F2_2", "M2_1", "M2_2"),
           variable.name="Set", value.name="ADist")

feats <- c("man_obs", "man_son", "nasXlat", "plc_fric", "plc_nas",
           "plc_plos", "plc_vwl", "voi_aff", "voi_fric", "voi_plos")

feat_red <- c("M (obs)", "M/P (son)", "M/P (son)", "P (obs)", "M/P (son)",
              "P (obs)", "P (vwl)", "VOI", "VOI", "VOI")

d1$FeatX <- mapvalues(d1$FeatX, from=feats, to=feat_red)

pd0 <- ddply(d1, .(NCep), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))

pd0$FeatX <- "All"

d1 <- d1[d1$FeatX %in% feat_red, ]


pd1 <- ddply(d1, .(FeatX, NCep), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))


d1$FeatX <- factor(d1$FeatX, levels=c("P (vwl)", "M/P (son)", "P (obs)", "M (obs)", "VOI"))

pd <- rbind(pd0, pd1)

pd$FeatX <- factor(pd$FeatX,
                   levels=c("All", "P (vwl)", "M/P (son)", "P (obs)", "M (obs)", "VOI"))

cols <- c(black="#000000", red="#CB3C33", blue="#4063D8",
          green="#389826", purple="#9558B2", pink="#CB3376")

names(cols) <- NULL

p <- ggplot(pd, aes(x=NCep, y=mu, colour=FeatX)) +
    geom_pointrange(aes(ymin=lwr, ymax=upr), position=position_dodge(0.5),
                    linewidth=1) +
    geom_line(position=position_dodge(0.5)) +
    scale_colour_manual(values=cols) +
    scale_x_continuous(breaks=ncep) +
    labs(colour="Feature", x="Number of Cepstral Coefs", y="Normalised Distance") +
    theme_classic() +
    theme(legend.position="top",
          axis.text.x=element_text(colour="black", size=12),
          axis.title.x=element_text(size=14),
          axis.text.y=element_text(colour="black", size=12),
          axis.title.y=element_text(size=14))

p

ggsave("n-cep.png", p, width=8, height=7, units="in", dpi=600)



## Window Size

dfns <- list.files("output", "^win*", full.names=TRUE)
ws <- gsub("output/win_", "", dfns)
ws <- gsub("_.*", "", ws)
ws <- as.numeric(ws)
dL <- lapply(dfns, read.csv)

for (i in 1:21) {

    dL[[i]][, "WSize"] <- ws[i]

}

d1 <- do.call("rbind", dL)

d1 <- melt(d1, id.vars=c("DiffPos", "S1", "S2", "Contrast", "PhonX", "FeatX", "WSize"),
           measure.vars=c("F1_1", "F1_2", "M1_1", "M1_2", "F2_1", "F2_2", "M2_1", "M2_2"),
           variable.name="Set", value.name="ADist")

feats <- c("man_obs", "man_son", "nasXlat", "plc_fric", "plc_nas",
           "plc_plos", "plc_vwl", "voi_aff", "voi_fric", "voi_plos")

feat_red <- c("M (obs)", "M/P (son)", "M/P (son)", "P (obs)", "M/P (son)",
              "P (obs)", "P (vwl)", "VOI", "VOI", "VOI")

d1$FeatX <- mapvalues(d1$FeatX, from=feats, to=feat_red)

pd0 <- ddply(d1, .(WSize), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))

pd0$FeatX <- "All"

d1 <- d1[d1$FeatX %in% feat_red, ]


pd1 <- ddply(d1, .(FeatX, WSize), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))


d1$FeatX <- factor(d1$FeatX, levels=c("P (vwl)", "M/P (son)", "P (obs)", "M (obs)", "VOI"))

pd <- rbind(pd0, pd1)

pd$FeatX <- factor(pd$FeatX,
                   levels=c("All", "P (vwl)", "M/P (son)", "P (obs)", "M (obs)", "VOI"))

cols <- c(black="#000000", red="#CB3C33", blue="#4063D8",
          green="#389826", purple="#9558B2", pink="#CB3376")

names(cols) <- NULL

p <- ggplot(pd, aes(x=WSize, y=mu, colour=FeatX)) +
    geom_pointrange(aes(ymin=lwr, ymax=upr), position=position_dodge(0.5),
                    linewidth=1) +
    geom_line(position=position_dodge(0.5)) +
    scale_colour_manual(values=cols) +
    scale_x_continuous(breaks=unique(ws)) +
    labs(colour="Feature", x="Window Size (ms)", y="Normalised Distance") +
    theme_classic() +
    theme(legend.position="top",
          axis.text.x=element_text(colour="black", size=12),
          axis.title.x=element_text(size=14),
          axis.text.y=element_text(colour="black", size=12),
          axis.title.y=element_text(size=14))

p

ggsave("w-size.png", p, width=8, height=7, units="in", dpi=600)



## Step Size

dfns <- list.files("output", "^win*", full.names=TRUE)
ss <- gsub(".*_", "", dfns)
ss <- gsub(".csv", "", ss)
ss <- as.numeric(ss)
dL <- lapply(dfns, read.csv)

for (i in 1:21) {

    dL[[i]][, "SSize"] <- ss[i]

}

d1 <- do.call("rbind", dL)

d1 <- melt(d1, id.vars=c("DiffPos", "S1", "S2", "Contrast", "PhonX", "FeatX", "SSize"),
           measure.vars=c("F1_1", "F1_2", "M1_1", "M1_2", "F2_1", "F2_2", "M2_1", "M2_2"),
           variable.name="Set", value.name="ADist")

feats <- c("man_obs", "man_son", "nasXlat", "plc_fric", "plc_nas",
           "plc_plos", "plc_vwl", "voi_aff", "voi_fric", "voi_plos")

feat_red <- c("M (obs)", "M/P (son)", "M/P (son)", "P (obs)", "M/P (son)",
              "P (obs)", "P (vwl)", "VOI", "VOI", "VOI")

d1$FeatX <- mapvalues(d1$FeatX, from=feats, to=feat_red)

pd0 <- ddply(d1, .(SSize), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))

pd0$FeatX <- "All"

d1 <- d1[d1$FeatX %in% feat_red, ]


pd1 <- ddply(d1, .(FeatX, SSize), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))


d1$FeatX <- factor(d1$FeatX, levels=c("P (vwl)", "M/P (son)", "P (obs)", "M (obs)", "VOI"))

pd <- rbind(pd0, pd1)

pd$FeatX <- factor(pd$FeatX,
                   levels=c("All", "P (vwl)", "M/P (son)", "P (obs)", "M (obs)", "VOI"))

cols <- c(black="#000000", red="#CB3C33", blue="#4063D8",
          green="#389826", purple="#9558B2", pink="#CB3376")

names(cols) <- NULL

p <- ggplot(pd, aes(x=SSize, y=mu, colour=FeatX)) +
    geom_pointrange(aes(ymin=lwr, ymax=upr), position=position_dodge(0.5),
                    linewidth=1) +
    geom_line(position=position_dodge(0.5)) +
    scale_colour_manual(values=cols) +
    scale_x_continuous(breaks=unique(ss)) +
    labs(colour="Feature", x="Step Size (ms)", y="Normalised Distance") +
    theme_classic() +
    theme(legend.position="top",
          axis.text.x=element_text(colour="black", size=12),
          axis.title.x=element_text(size=14),
          axis.text.y=element_text(colour="black", size=12),
          axis.title.y=element_text(size=14))

p

ggsave("s-size.png", p, width=8, height=7, units="in", dpi=600)


## Pre-emphasis

dfns <- list.files("output", "^pemph*", full.names=TRUE)
pemph <- c(0.99, 0.97, 0.95, 0.93, 0.91, 0.8)

dL <- lapply(dfns, read.csv)

for (i in 1:6) {

    dL[[i]][, "PEmph"] <- pemph[i]

}

d1 <- do.call("rbind", dL)

d1 <- melt(d1, id.vars=c("DiffPos", "S1", "S2", "Contrast", "PhonX", "FeatX", "PEmph"),
           measure.vars=c("F1_1", "F1_2", "M1_1", "M1_2", "F2_1", "F2_2", "M2_1", "M2_2"),
           variable.name="Set", value.name="ADist")

feats <- c("man_obs", "man_son", "nasXlat", "plc_fric", "plc_nas",
           "plc_plos", "plc_vwl", "voi_aff", "voi_fric", "voi_plos")

feat_red <- c("M (obs)", "M/P (son)", "M/P (son)", "P (obs)", "M/P (son)",
              "P (obs)", "P (vwl)", "VOI", "VOI", "VOI")

d1$FeatX <- mapvalues(d1$FeatX, from=feats, to=feat_red)

pd0 <- ddply(d1, .(PEmph), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))

pd0$FeatX <- "All"

d1 <- d1[d1$FeatX %in% feat_red, ]


pd1 <- ddply(d1, .(FeatX, PEmph), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))


d1$FeatX <- factor(d1$FeatX, levels=c("P (vwl)", "M/P (son)", "P (obs)", "M (obs)", "VOI"))

pd <- rbind(pd0, pd1)

pd$FeatX <- factor(pd$FeatX,
                   levels=c("All", "P (vwl)", "M/P (son)", "P (obs)", "M (obs)", "VOI"))

cols <- c(black="#000000", red="#CB3C33", blue="#4063D8",
          green="#389826", purple="#9558B2", pink="#CB3376")

names(cols) <- NULL

p <- ggplot(pd, aes(x=PEmph, y=mu, colour=FeatX)) +
    geom_pointrange(aes(ymin=lwr, ymax=upr), position=position_dodge(0.1),
                    linewidth=1) +
    ## geom_line(position=position_dodge(0.5)) +
    scale_colour_manual(values=cols) +
    ## scale_x_continuous(breaks=unique(pemph)[order(unique(pemph))],
    ##                    labels=c("None", seq(0.91, 0.99, by=0.02))) +
    labs(colour="Feature", x="Pre-emphasis", y="Normalised Distance") +
    theme_classic() +
    theme(legend.position="top",
          axis.text.x=element_text(colour="black", size=12),
          axis.title.x=element_text(size=14),
          axis.text.y=element_text(colour="black", size=12),
          axis.title.y=element_text(size=14))

p

ggsave("pre-emph.png", p, width=8, height=7, units="in", dpi=600)




## Auditory Scaling

dfns <- list.files("output", "^fs_*", full.names=TRUE)
fs <- c("Bark", "Mel (ICSI)", "Mel (HTK)", "Mel")
dL <- lapply(dfns, read.csv)

for (i in 1:4) {

    dL[[i]][, "FS"] <- fs[i]

}

d1 <- do.call("rbind", dL)

d1 <- melt(d1, id.vars=c("DiffPos", "S1", "S2", "Contrast", "PhonX", "FeatX", "FS"),
           measure.vars=c("F1_1", "F1_2", "M1_1", "M1_2", "F2_1", "F2_2", "M2_1", "M2_2"),
           variable.name="Set", value.name="ADist")

feats <- c("man_obs", "man_son", "nasXlat", "plc_fric", "plc_nas",
           "plc_plos", "plc_vwl", "voi_aff", "voi_fric", "voi_plos")

feat_red <- c("M (obs)", "M/P (son)", "M/P (son)", "P (obs)", "M/P (son)",
              "P (obs)", "P (vwl)", "VOI", "VOI", "VOI")

d1$FeatX <- mapvalues(d1$FeatX, from=feats, to=feat_red)

pd0 <- ddply(d1, .(FS), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))

pd0$FeatX <- "All"

d1 <- d1[d1$FeatX %in% feat_red, ]


pd1 <- ddply(d1, .(FeatX, FS), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))


d1$FeatX <- factor(d1$FeatX, levels=c("P (vwl)", "M/P (son)", "P (obs)", "M (obs)", "VOI"))

pd <- rbind(pd0, pd1)

pd$FeatX <- factor(pd$FeatX,
                   levels=c("All", "P (vwl)", "M/P (son)", "P (obs)", "M (obs)", "VOI"))

pd$FS <- factor(pd$FS, c("Bark", "Mel", "Mel (HTK)", "Mel (ICSI)"))

cols <- c(black="darkgrey", red="#CB3C33", blue="#4063D8",
          green="#389826", purple="#9558B2", pink="#CB3376")

names(cols) <- NULL

p <- ggplot(pd, aes(x=FS, y=mu, fill=FeatX)) +
    geom_bar(stat="identity", position=position_dodge(0.95), colour="black") +
    geom_errorbar(aes(ymin=lwr, ymax=upr), colour="black", width=0.2,
                  position=position_dodge(0.95)) +
    scale_fill_manual(values=cols) +
    labs(colour="Feature", x="Auditory Scale", y="Normalised Distance") +
    theme_classic() +
    theme(legend.position="top",
          axis.text.x=element_text(colour="black", size=12),
          axis.title.x=element_text(size=14),
          axis.text.y=element_text(colour="black", size=12),
          axis.title.y=element_text(size=14))

ggsave("aud-scale.png", p, width=8, height=7, units="in", dpi=600)



## PLP and Liftering

dfn1 <- list.files("output", "^plp_*", full.names=TRUE)
dfn2 <- list.files("output", "^htklift_*", full.names=TRUE)
dL <- lapply(c(dfn1, dfn2), read.csv)

dL[[1]][, "SVar"] <- "PLP"
dL[[2]][, "SVar"] <- "PLP"
dL[[3]][, "SVar"] <- "Liftering"
dL[[4]][, "SVar"] <- "Liftering"

dL[[1]][, "YN"] <- "No"
dL[[2]][, "YN"] <- "Yes"
dL[[3]][, "YN"] <- "No"
dL[[4]][, "YN"] <- "Yes"


d1 <- do.call("rbind", dL)

d1 <- melt(d1, id.vars=c("DiffPos", "S1", "S2", "Contrast", "PhonX", "FeatX", "SVar", "YN"),
           measure.vars=c("F1_1", "F1_2", "M1_1", "M1_2", "F2_1", "F2_2", "M2_1", "M2_2"),
           variable.name="Set", value.name="ADist")

feats <- c("man_obs", "man_son", "nasXlat", "plc_fric", "plc_nas",
           "plc_plos", "plc_vwl", "voi_aff", "voi_fric", "voi_plos")

feat_red <- c("M (obs)", "M/P (son)", "M/P (son)", "P (obs)", "M/P (son)",
              "P (obs)", "P (vwl)", "VOI", "VOI", "VOI")

d1$FeatX <- mapvalues(d1$FeatX, from=feats, to=feat_red)

pd0 <- ddply(d1, .(SVar, YN), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))

pd0$FeatX <- "All"

d1 <- d1[d1$FeatX %in% feat_red, ]


pd1 <- ddply(d1, .(SVar, YN, FeatX), summarise,
             mu = mean(ADist, na.rm=TRUE),
             upr = quantile(ADist, 0.75, na.rm=TRUE),
             lwr = quantile(ADist, 0.25, na.rm=TRUE))


d1$FeatX <- factor(d1$FeatX, levels=c("P (vwl)", "M/P (son)", "P (obs)", "M (obs)", "VOI"))

pd <- rbind(pd0, pd1)

pd$FeatX <- factor(pd$FeatX,
                   levels=c("All", "P (vwl)", "M/P (son)", "P (obs)", "M (obs)", "VOI"))

pd$SVar <- factor(pd$SVar, c("PLP", "Liftering"))
pd$YN <- factor(pd$YN)

cols <- c(black="darkgrey", red="#CB3C33", blue="#4063D8",
          green="#389826", purple="#9558B2", pink="#CB3376")

names(cols) <- NULL

p <- ggplot(pd, aes(x=YN, y=mu, fill=FeatX)) +
    facet_wrap(~SVar, ncol=2) +
    geom_bar(stat="identity", position=position_dodge(0.95), colour="black") +
    geom_errorbar(aes(ymin=lwr, ymax=upr), colour="black", width=0.2,
                  position=position_dodge(0.95)) +
    scale_fill_manual(values=cols) +
    labs(y="Normalised Distance") +
    theme_classic() +
    theme(legend.position="none",
          axis.title.x=element_blank(),
          axis.text.x=element_text(colour="black", size=12),
          axis.text.y=element_text(colour="black", size=12),
          axis.title.y=element_text(size=14))

ggsave("aud-scale.png", p, width=8, height=7, units="in", dpi=600)





el1a <- el1[, as.character(1:27)]
el1a <- t(el1a)
el1a <- rowMeans(el1a)
el1b <- cbind(pGrid, data.frame(Distance=el1a))

ddply(el1b, .(FreqLim), summarise, mu = mean(Distance))
ddply(el1b, .(winSize), summarise, mu = mean(Distance))
ddply(el1b, .(stepSize), summarise, mu = mean(Distance))


## melfcc(d, , lifterexp=22, htklifter=TRUE, nbands=20, maxfreq=8000, 
##          sumpower=FALSE, fbtype="htkmel", dcttype="t3")










el1a <- el1[, as.character(1:27)]
el1a <- t(el1a)
el1a <- rowMeans(el1a)
el1b <- cbind(pGrid, data.frame(Distance=el1a))

ddply(el1b, .(FreqLim), summarise, mu = mean(Distance))
ddply(el1b, .(winSize), summarise, mu = mean(Distance))
ddply(el1b, .(stepSize), summarise, mu = mean(Distance))


## melfcc(d, , lifterexp=22, htklifter=TRUE, nbands=20, maxfreq=8000, 
##          sumpower=FALSE, fbtype="htkmel", dcttype="t3")




## features
## Vowel, Son, Nasal, Liquid, ObsVoi, ObsMan, ObsPOA,
## PlosVoi, FricVoi, AffVoi, PlosPOA, FricPOA
##  [1] "^" "t" "u" "&" "y" "#" "v" "n" "I" "D" "z" "f" ">" "r" "a" "w" "i" "T" "h"
## [20] "b" "d" "m" "l" "k" "o" "s" "J" "$" "E" "g" "S" "R" "U" "e" "C" "p" "N" "%"
## [39] "Z"

vowels <- c("^", "u", "&", "y", "#", "I", ">", "a", "w", "i", "o", "$", "E",
            "R", "U", "e", "%")
sons <- c("n", "r", "m", "l", "N")
obs <- c("p", "t", "k", "f", "T", "s", "S", "C", "h",
         "b", "d", "g", "v", "D", "z", "Z", "J")
nasals <- c("m", "n", "N")
liquids <- c("r", "l")

vlobs <- c("p", "t", "k", "f", "T", "s", "S", "C", "h")
vdobs <- c("b", "d", "g", "v", "D", "z", "Z", "J")

plos <- c("p", "t", "k", "b", "d", "g")
fric <- c("f", "T", "s", "S", "h", "v", "D", "z", "Z")
aff <- c("C", "J")

lab <- c("p", "b")
ld <- c("f", "v")
dent <- c("T", "D")
alv <- c("t", "d", "s", "z")
pa <- c("S", "Z", "C", "J")
vel <- c("k", "g")

vlplos <- c("p", "t", "k")
vdplos <- c("b", "d", "g")

vlfric <- c("f", "T", "s", "S", "h")
vdfric <- c("v", "D", "z", "Z")

vlaff <- "C"
vdaff <- "J"

labFrics <- c("f", "v")
dentFrics <- c("T", "D")
alvFrics <- c("s", "z")
paFrics <- c("S", "Z")

el$VowelX <- ifelse(el$X1 %in% vowels & el$X2 %in% vowels, 1, 0)
el$SonX <- ifelse(el$X1 %in% sons & el$X2 %in% sons, 1, 0)
el$ObsX <- ifelse(el$X1 %in% obs & el$X2 %in% obs, 1, 0)
el$NasalX <- ifelse(el$X1 %in% nasals & el$X2 %in% nasals, 1, 0)
el$LiquidX <- ifelse(el$X1 %in% liquids & el$X2 %in% liquids, 1, 0)

el$ObsVoiX <- ifelse((el$X1 %in% vlobs & el$X2 %in% vdobs) |
                  (el$X1 %in% vdobs & el$X2 %in% vlobs), 1, 0)

el$ObsManX <- ifelse((el$X1 %in% plos & el$X2 %in% c(fric, aff)) |
                  (el$X1 %in% fric & el$X2 %in% c(plos, aff)) |
                  (el$X1 %in% aff & el$X2 %in% c(plos, fric)), 1, 0)

el$ObsPOAX <- ifelse((el$X1 %in% lab & el$X2 %in% c(ld, dent, alv, pa, vel)) |
                  (el$X1 %in% ld & el$X2 %in% c(lab, dent, alv, pa, vel)) |
                  (el$X1 %in% dent & el$X2 %in% c(lab, ld, alv, pa, vel)) |
                  (el$X1 %in% alv & el$X2 %in% c(lab, ld, dent, pa, vel)) |
                  (el$X1 %in% pa & el$X2 %in% c(lab, ld, dent, alv, vel)) |
                  (el$X1 %in% vel & el$X2 %in% c(lab, ld, dent, alv, pa)), 1, 0)

el$PlosVoiX <- ifelse((el$X1 %in% vlplos & el$X2 %in% vdplos) |
                   (el$X1 %in% vdplos & el$X2 %in% vlplos), 1, 0)

el$FricVoiX <- ifelse((el$X1 %in% vlfric & el$X2 %in% vdfric) |
                   (el$X1 %in% vdfric & el$X2 %in% vlfric), 1, 0)

el$AffVoiX <- ifelse((el$X1 %in% vlaff & el$X2 %in% vdaff) |
                  (el$X1 %in% vdaff & el$X2 %in% vlaff), 1, 0)

el$PlosPOAX <- ifelse((el$X1 %in% c("p", "b") & el$X2 %in% c("t", "d", "k", "g")) |
                   (el$X1 %in% c("t", "d") & el$X2 %in% c("p", "b", "k", "g")) |
                   (el$X1 %in% c("k", "g") & el$X2 %in% c("p", "b", "t", "d")), 1, 0)

el$FricPOAX <- ifelse((el$X1 %in% labFrics & el$X2 %in% c(dentFrics, alvFrics, paFrics, "h")) |
                   (el$X1 %in% dentFrics & el$X2 %in% c(labFrics, alvFrics, paFrics, "h")) |
                   (el$X1 %in% alvFrics & el$X2 %in% c(labFrics, dentFrics, paFrics, "h")) |
                   (el$X1 %in% paFrics & el$X2 %in% c(labFrics, dentFrics, alvFrics, "h")) |
                   (el$X1 == "h" & el$X2 %in% c(labFrics, dentFrics, alvFrics, paFrics)), 1, 0)



el2 <- cbind(el, el1[, -c(1:7)])

saveRDS(el2, "../data/output_HTK2.rds")
write.csv(el2, "../data/output_HTK2.csv", row.names=FALSE)

library(plyr)

vowelX <- aggregate(el2[, 21:26],
                    by=list(VowelX=el2$VowelX),
                    FUN=function(x) c(mu=mean(x, na.rm=TRUE),
                                      dev=sd(x, na.rm=TRUE)))

sonX <- aggregate(el2[, 21:26],
                  by=list(SonX=el2$SonX),
                  FUN=function(x) c(mu=mean(x, na.rm=TRUE),
                                    dev=sd(x, na.rm=TRUE)))

obsX <- aggregate(el2[, 21:26],
                  by=list(ObsX=el2$ObsX),
                  FUN=function(x) c(mu=mean(x, na.rm=TRUE),
                                    dev=sd(x, na.rm=TRUE)))

nasalX <- aggregate(el2[, 21:26],
                    by=list(NasalX=el2$NasalX),
                    FUN=function(x) c(mu=mean(x, na.rm=TRUE),
                                      dev=sd(x, na.rm=TRUE)))

liquidX <- aggregate(el2[, 21:26],
                     by=list(LiquidX=el2$LiquidX),
                     FUN=function(x) c(mu=mean(x, na.rm=TRUE),
                                       dev=sd(x, na.rm=TRUE)))

obsVoiX <- aggregate(el2[, 21:26],
                     by=list(ObsVoiX=el2$ObsVoiX),
                     FUN=function(x) c(mu=mean(x, na.rm=TRUE),
                                       dev=sd(x, na.rm=TRUE)))

obsManX <- aggregate(el2[, 21:26],
                     by=list(ObsManX=el2$ObsManX),
                     FUN=function(x) c(mu=mean(x, na.rm=TRUE),
                                       dev=sd(x, na.rm=TRUE)))

obsPOAX <- aggregate(el2[, 21:26],
                     by=list(ObsPOAX=el2$ObsPOAX),
                     FUN=function(x) c(mu=mean(x, na.rm=TRUE),
                                       dev=sd(x, na.rm=TRUE)))

plosVoiX <- aggregate(el2[, 21:26],
                      by=list(PlosVoiX=el2$PlosVoiX),
                      FUN=function(x) c(mu=mean(x, na.rm=TRUE),
                                        dev=sd(x, na.rm=TRUE)))

fricVoiX <- aggregate(el2[, 21:26],
                      by=list(FricVoiX=el2$FricVoiX),
                      FUN=function(x) c(mu=mean(x, na.rm=TRUE),
                                        dev=sd(x, na.rm=TRUE)))

affVoiX <- aggregate(el2[, 21:26],
                     by=list(AffVoiX=el2$AffVoiX),
                     FUN=function(x) c(mu=mean(x, na.rm=TRUE),
                                       dev=sd(x, na.rm=TRUE)))

plosPOAX <- aggregate(el2[, 21:26],
                      by=list(PlosPOAX=el2$PlosPOAX),
                      FUN=function(x) c(mu=mean(x, na.rm=TRUE),
                                        dev=sd(x, na.rm=TRUE)))

fricPOAX <- aggregate(el2[, 21:26],
                      by=list(FricPOAX=el2$FricPOAX),
                      FUN=function(x) c(mu=mean(x, na.rm=TRUE),
                                        dev=sd(x, na.rm=TRUE)))


vowelX <- vowelX[2, -1]
sonX <- sonX[2, -1]
obsX <- obsX[2, -1]
nasalX <- nasalX[2, -1]
liquidX <- liquidX[2, -1]
obsVoiX <- obsVoiX[2, -1]
obsManX <- obsManX[2, -1]
obsPOAX <- obsPOAX[2, -1]
plosVoiX <- plosVoiX[2, -1]
fricVoiX <- fricVoiX[2, -1]
affVoiX <- affVoiX[2, -1]
plosPOAX <- plosPOAX[2, -1]
fricPOAX <- fricPOAX[2, -1]

summ <- rbind(vowelX, sonX, obsX, nasalX, liquidX,
              obsVoiX, obsManX, obsPOAX, plosVoiX,
              fricVoiX, affVoiX, plosPOAX, fricPOAX)

summ <- cbind(data.frame(Feature = names(el2)[8:20]), as.matrix(summ))

library(reshape2)

pDF <- melt(summ, id.vars="Feature",
            variable.name="Model",
            value.name="Distance")

pDF <- cbind(pDF[grep(".mu", pDF$Model), ],
             pDF[-grep(".mu", pDF$Model), ])

pDF <- pDF[, -c(4:5)]
names(pDF)[4] <- "Dev"

pDF$Speaker <- ifelse(grepl("_F", pDF$Model), "F01", "M01")
pDF$Model <- ifelse(grepl("dd.", pDF$Model, fixed=TRUE), "Baseline + Delta + Delta-Delta",
                    ifelse(grepl("d.", pDF$Model, fixed=TRUE), "Baseline + Delta",
                           "Baseline"))

pDF$Feature <- factor(pDF$Feature,
                      levels=c("VowelX", "SonX", "ObsX", "NasalX",
                               "LiquidX",
                               "ObsVoiX", "PlosVoiX", "FricVoiX", "AffVoiX",
                               "ObsPOAX", "PlosPOAX", "FricPOAX", "ObsManX"))

pDF$Feature <- mapvalues(pDF$Feature, from = levels(pDF$Feature),
               to = c("VWL", "SON", "OBS", "NAS", "LIQ",
                      "VOI(obs)", "VOI(plos)", "VOI(fric)", "VOI(aff)",
                      "POA(obs)", "POA(plos)", "POA(fric)",
                      "MAN(obs)"))

library(ggplot2)

p <- ggplot(pDF, aes(x=Feature, y=Distance, colour=Speaker)) +
     facet_wrap(~Model, ncol=1, scales="free") +
     geom_point(position=position_dodge(0.9)) +
     geom_pointrange(aes(ymin=Distance-Dev, ymax=Distance+Dev),
                     position=position_dodge(0.9)) +
     scale_colour_manual(values=c("blue", "red")) +
     labs(x="Featural Contrast") + 
     theme_bw() +
    theme(axis.text.x=element_text(color="black", angle=45,
                                   vjust=0.5))

ggsave("fig1.png", p, height=9, width=8, units="in", dpi=600)


