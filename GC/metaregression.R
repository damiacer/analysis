require("meta")
require("metafor")
require("here")
require("readxl")

setwd("P:/CONSULTATION/Gakuba_C")

gk <- read_excel("meta_data.xlsx")

gk <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", 
                      "Gakuba_C", "meta_data.xlsx"))

gk
dim(gk)
names(gk)

gk2 = subset(gk, select = c("author", "year", "NACtotal", "CONTRtotal",
                            "NACevents", "CONTRevents", "NAC_sexmale", "NAC_agemead"))

gk2$NAC_agemead = as.numeric(as.character(gk2$NAC_agemead))
gk2$NAC_sexmale = as.numeric(as.character(gk2$NAC_sexmale))

ai = gk2$NACevents 
bi = (gk2$NACtotal - gk2$NACevents)
ci = gk2$CONTRevents 
di = (gk2$CONTRtotal - gk2$CONTRtotal)
n1i = gk2$NACtotal
n2i = gk2$CONTRtotal

dat <- escalc(measure="PETO", ai=ai, n1i=n1i, drop00 = TRUE,
              slab=paste(author, ", ", year, sep=""), 
              ci=ci, n2i=n2i, 
              data=gk2)
dat

# random-effects model
res <- rma(yi, vi, data=dat, mods = ~ gk2$NAC_agemead + gk2$NAC_sexmale)
predict(res)
res

weights <- paste0(fmtx(weights(res), digits=1), "%")
weights[weights == "NA%"] <- ""
options(na.action = "na.pass")
forest(res)
forest(res, addpred=TRUE, header=TRUE, atransf = exp, 
       ilab = cbind(ai, n1i, ci, n2i, weights), ilab.xpos = c(-11.5,-10.5,-8.5,-7.5,-5.5),
       cex = 0.75, xlim = c(-16,6))
text(c(-11.5,-10.5,-8.5,-7.5,-5.5), res$k+6, cex = 0.75, c("Events", "Total", "Events", "Total", ""))
text(c(-11.0, -8.1, -5.5), res$k+7, cex = 0.75, c("N-acetylcysteine", "Control", "Weights"))
text(-16, -1, pos=4, cex=0.75, bquote(paste(
  "RE Model (Q = ", .(fmtx(res$QE, digits=2)),
  ", df = ", .(res$k - res$p), ", ",
  .(fmtp(res$QEp, digits=3, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)), "; ",
  I^2, " = ", .(fmtx(res$I2, digits=1)), "%)")))

#-excluding studies for 0r missing

require(tidyverse)

gk$ORmiss = gk$Ormort
gk$ORmiss[is.na(gk$ORmiss)] <- "miss"
table(gk$ORmiss, useNA = "always")

gkcomplete = gk[!(gk$ORmiss == "miss"),]
dim(gkcomplete)

#-analysis on complete base

gkcomplete2 = subset(gkcomplete, select = c("author", "year", "NACtotal", "CONTRtotal",
                                            "NACevents", "CONTRevents", "NAC_sexmale", "NAC_agemead"))

gkcomplete2$NAC_agemead = as.numeric(as.character(gkcomplete2$NAC_agemead))
gkcomplete2$NAC_sexmale = as.numeric(as.character(gkcomplete2$NAC_sexmale))

ai = gkcomplete2$NACevents 
bi = (gkcomplete2$NACtotal - gkcomplete2$NACevents)
ci = gkcomplete2$CONTRevents 
di = (gkcomplete2$CONTRtotal - gkcomplete2$CONTRtotal)
n1i = gkcomplete2$NACtotal
n2i = gkcomplete2$CONTRtotal

dat <- escalc(measure="PETO", ai=ai, n1i=n1i, drop00 = TRUE,
              slab=paste(author, ", ", year, sep=""), 
              ci=ci, n2i=n2i, 
              data=gkcomplete2)
dat

# random-effects model
res <- rma(yi, vi, data=dat, mods = ~ gkcomplete2$NAC_agemead + gkcomplete2$NAC_sexmale)
predict(res)
res

weights <- paste0(fmtx(weights(res), digits=1), "%")
weights[weights == "NA%"] <- ""
options(na.action = "na.pass")
forest(res)
forest(res, addpred=TRUE, header=TRUE, atransf = exp, 
       ilab = cbind(ai, n1i, ci, n2i, weights), ilab.xpos = c(-11.5,-10.5,-8.5,-7.5,-5.5),
       cex = 0.75, xlim = c(-16,6))
text(c(-11.5,-10.5,-8.5,-7.5,-5.5), res$k+6, cex = 0.75, c("Events", "Total", "Events", "Total", ""))
text(c(-11.0, -8.1, -5.5), res$k+7, cex = 0.75, c("N-acetylcysteine", "Control", "Weights"))
text(-16, -1, pos=4, cex=0.75, bquote(paste(
  "RE Model (Q = ", .(fmtx(res$QE, digits=2)),
  ", df = ", .(res$k - res$p), ", ",
  .(fmtp(res$QEp, digits=3, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)), "; ",
  I^2, " = ", .(fmtx(res$I2, digits=1)), "%)")))

# glmm model 

modelglmm1 <- rma.glmm(ai = ai, 
                       bi = bi, 
                       ci = ci, 
                       di = di, 
                       #n1i = n1i, 
                       #n2i = n2i,
                       method = "ML",
                       model="CM.AL",
                       measure = "OR",
                       verbose = FALSE,
                       #slab=paste(author, ", ", year, sep=""), 
                       #data = gkcomplete2,
)

modelglmm1
predict(m3glmm)
