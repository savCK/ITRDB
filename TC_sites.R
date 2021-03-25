
library(dplR)
library(graphics)
library(utils)


########### USA SITES ############

##### ALT #####
ALT.rwl <- read.rwl(fname = "ALT.rwl", format = "auto")
sens1(ALT.rwl)
[1] 0.6140343
corr.rwl.seg(rwl = ALT.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(ALT.rwl)
ALT.rwi <- detrend(rwl = ALT.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
ALT.crn <- chron(x = ALT.rwi, prefix = "ALT", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = ALT.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Altamha River | ALT", xlab="Time", ylab="RWI")

rwi.stats(ALT.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      60      60 18.938   968    0  968    0.334      NA   0.334     1    0.334 0.905 9.494

int_ALT <- interseries.cor(ALT.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
mean(int_ALT$res.cor)
0.543059228


ALT.sss <- sss(ALT.rwi)
ALT.ids <- autoread.ids(ALT.rwl)
ALT.sss2 <- sss(ALT.rwi,ALT.ids)
yr <- time(ALT.rwl)
plot(yr,ALT.sss,type="l",ylim=c(0.4,1),
     col="darkblue",lwd=2,xlab="Year",ylab="SSS")
lines(yr,ALT.sss2,lty="dashed",
      col="darkgreen",lwd=2)


##### BEA #####
BEA.rwl <- read.rwl(fname = "BEA.rwl", format = "auto")
sens1(BEA.rwl)
[1] 0.2469431
corr.rwl.seg(rwl = BEA.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(BEA.rwl)
BEA.rwi <- detrend(rwl = BEA.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
BEA.crn <- chron(x = BEA.rwi, prefix = "BEA", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = BEA.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Bear Island | BEA", xlab="Time", ylab="RWI")

rwi.stats(BEA.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      38      38 23.339   703    0  703    0.238      NA   0.238     1    0.238 0.879 7.289
interseries.cor(BEA.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.56224401





##### BLA #####
BLA.rwl <- read.rwl(fname = "BLA.rwl", format = "auto")
sens1(BLA.rwl)
[1] 0.5391995
corr.rwl.seg(rwl = BLA.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(BLA.rwl)
BLA.rwi <- detrend(rwl = BLA.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
BLA.crn <- chron(x = BLA.rwi, prefix = "BLA", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = BLA.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Black River, SC | BLA", xlab="Time", ylab="RWI")

rwi.stats(BLA.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      94      94 25.603  2411    0 2411    0.328      NA   0.328     1    0.328 0.926 12.489
interseries.cor(BLA.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.590286036





##### BLK #####
BLK.rwl <- read.rwl(fname = "BLK.rwl", format = "auto")
sens1(BLK.rwl)
[1] 0.6411152
corr.rwl.seg(rwl = BLK.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(NC_BLK.rwl)
BLK.rwi <- detrend(rwl = BLK.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
BLK.crn <- chron(x = BLK.rwi, prefix = "BLK", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = BLK.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Black River, NC | BLK", xlab="Time", ylab="RWI")

rwi.stats(BLK.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1     184     184 34.775  9654    0 9654    0.396      NA   0.396     1    0.396 0.958 22.815
interseries.cor(BLK.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.626508502




##### BUR #####
BUR.rwl <- read.rwl(fname = "BUR.rwl", format = "auto")
sens1(BUR.rwl)
[1] 0.2248026
corr.rwl.seg(rwl = BUR.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(BUR.rwl)
BUR.rwi <- detrend(rwl = BUR.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
BUR.crn <- chron(x = BUR.rwi, prefix = "BUR", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = BUR.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Burling Tract | BUR", xlab="Time", ylab="RWI")

rwi.stats(BUR.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      26      26 17.331   325    0  325    0.427      NA   0.427     1    0.427 0.928 12.906
interseries.cor(BUR.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.611303812


##### BWR #####
BWR.rwl <- read.rwl(fname = "BWR.rwl", format = "auto")
sens1(BWR.rwl)
[1] 0.5005037
corr.rwl.seg(rwl = BWR.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(BWR.rwl)
BWR.rwi <- detrend(rwl = BWR.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
BWR.crn <- chron(x = BWR.rwi, prefix = "BWR", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = BWR.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Blackwater River | BWR", xlab="Time", ylab="RWI")

rwi.stats(BWR.rwi)
n.cores n.trees     n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      65      65 21.56  1277    0 1277    0.335      NA   0.335     1    0.335 0.916 10.848
interseries.cor(BWR.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.594513403



##### CHI #####
CHI.rwl <- read.rwl(fname = "CHI.rwl", format = "auto")
sens1(CHI.rwl)
[1] 0.5078837
corr.rwl.seg(rwl = CHI.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(CHI.rwl)
CHI.rwi <- detrend(rwl = CHI.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
CHI.crn <- chron(x = CHI.rwi, prefix = "CHI", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = CHI.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Chickahominy River | CHI", xlab="Time", ylab="RWI")

rwi.stats(CHI.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      22      22 11.409   231    0  231    0.289      NA   0.289     1    0.289 0.823 4.645
interseries.cor(CHI.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.514808023





##### CHK #####
CHK.rwl <- read.rwl(fname = "CHK.rwl", format = "auto")
sens1(CHK.rwl)
[1] 0.5230295
corr.rwl.seg(rwl = CHK.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(CHK.rwl)
CHK.rwi <- detrend(rwl = CHK.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
CHK.crn <- chron(x = CHK.rwi, prefix = "CHK", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = CHK.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Choctawhatchee River | CHK", xlab="Time", ylab="RWI")

rwi.stats(CHK.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1     111     111 31.928  3342    0 3342    0.352      NA   0.352     1    0.352 0.945 17.331
interseries.cor(CHK.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.567072389



##### COL #####
COL.rwl <- read.rwl(fname = "COL.rwl", format = "auto")
sens1(COL.rwl)
[1] 0.3321891
corr.rwl.seg(rwl = COL.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(COL.rwl)
COL.rwi <- detrend(rwl = COL.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
COL.crn <- chron(x = COL.rwi, prefix = "COL", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = COL.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Coleto Creek | COL", xlab="Time", ylab="RWI")

rwi.stats(COL.rwi)
n.cores n.trees     n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      62      62 34.92  1811    0 1811     0.43      NA    0.43     1     0.43 0.963 26.353
interseries.cor(COL.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.663107031



##### DAL #####
DAL.rwl <- read.rwl(fname = "DAL.rwl", format = "auto")
sens1(DAL.rwl)
[1] 0.2423805
corr.rwl.seg(rwl = DAL.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(DAL.rwl)
DAL.rwi <- detrend(rwl = DAL.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
DAL.crn <- chron(x = DAL.rwi, prefix = "DAL", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = DAL.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Dale City | DAL", xlab="Time", ylab="RWI")

rwi.stats(DAL.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      24      24 16.513   276    0  276    0.514      NA   0.514     1    0.514 0.946 17.457
interseries.cor(DAL.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.635023425



##### DIS #####
DIS.rwl <- read.rwl(fname = "DIS.rwl", format = "auto")
sens1(DIS.rwl)
[1] 0.3914873
corr.rwl.seg(rwl = DIS.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(DIS.rwl)
DIS.rwi <- detrend(rwl = DIS.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
DIS.crn <- chron(x = DIS.rwi, prefix = "DIS", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = DIS.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Great Dismal Swamp | DIS", xlab="Time", ylab="RWI")

rwi.stats(DIS.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      22      22 16.286   231    0  231    0.467      NA   0.467     1    0.467 0.935 14.279
interseries.cor(DIS.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.616839984




##### DRA #####
DRA.rwl <- read.rwl(fname = "DRA.rwl", format = "auto")
sens1(DRA.rwl)
[1] 0.4371132
corr.rwl.seg(rwl = DRA.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(DRA.rwl)
DRA.rwi <- detrend(rwl = DRA.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
DRA.crn <- chron(x = DRA.rwi, prefix = "DRA", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = DRA.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Dragon Run | DRA", xlab="Time", ylab="RWI")

rwi.stats(DRA.rwi)
n.cores n.trees     n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      24      24 7.493   153    0  153     0.29      NA    0.29     1     0.29 0.754 3.064
interseries.cor(DRA.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.543194567



##### EBE #####
EBE.rwl <- read.rwl(fname = "EBE.rwl", format = "auto")
sens1(EBE.rwl)
[1] 0.6278351
corr.rwl.seg(rwl = EBE.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(EBE.rwl)
EBE.rwi <- detrend(rwl = EBE.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
EBE.crn <- chron(x = EBE.rwi, prefix = "EBE", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = EBE.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Ebenezer Creek | EBE", xlab="Time", ylab="RWI")

rwi.stats(EBE.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      59      59 27.192  1233    0 1233     0.44      NA    0.44     1     0.44 0.955 21.323
interseries.cor(EBE.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.664346285




##### FBS #####
FBS.rwl <- read.rwl(fname = "FBS.rwl", format = "auto")
sens1(FBS.rwl)
[1] 0.3391281
corr.rwl.seg(rwl = FBS.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(FBS.rwl)
FBS.rwi <- detrend(rwl = FBS.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
FBS.crn <- chron(x = FBS.rwi, prefix = "FBS", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = FBS.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Francis Beidler Swamp | FBS", xlab="Time", ylab="RWI")

rwi.stats(FBS.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff  eps    snr
1      49      49 27.847  1123    0 1123    0.359      NA   0.359     1    0.359 0.94 15.614
interseries.cor(FBS.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.599012637





##### FHS #####
FHS.rwl <- read.rwl(fname = "FHS.rwl", format = "auto")
sens1(FHS.rwl)
[1] 0.5508099
corr.rwl.seg(rwl = FHS.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(FHS.rwl)
FHS.rwi <- detrend(rwl = FHS.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
FHS.crn <- chron(x = FHS.rwi, prefix = "FHS", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = FHS.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Four Holes Swamp | FHS", xlab="Time", ylab="RWI")

rwi.stats(FHS.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      42      42 15.127   536    0  536    0.274      NA   0.274     1    0.274 0.851 5.719
interseries.cor(FHS.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.545279712



##### FLO #####
FLO.rwl <- read.rwl(fname = "FLO.rwl", format = "auto")
sens1(FLO.rwl)
[1] 0.2860229
corr.rwl.seg(rwl = FLO.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(FLO.rwl)
FLO.rwi <- detrend(rwl = FLO.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
FLO.crn <- chron(x = FLO.rwi, prefix = "FLO", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = FLO.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Flomation National Area | FLO", xlab="Time", ylab="RWI")

rwi.stats(FLO.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      43      43 22.742   823    0  823     0.39      NA    0.39     1     0.39 0.936 14.519
interseries.cor(FLO.rwi,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.629903798




##### GBY #####
GBY.rwl <- read.rwl(fname = "GBY.rwl", format = "auto")
sens1(GBY.rwl)
[1] 0.4483488
corr.rwl.seg(rwl = GBY.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(GBY.rwl)
GBY.rwi <- detrend(rwl = GBY.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
GBY.crn <- chron(x = GBY.rwi, prefix = "GBY", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = GBY.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Grand Bay National Estuary | GBY", xlab="Time", ylab="RWI")

rwi.stats(GBY.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      30      30 18.604   435    0  435    0.486      NA   0.486     1    0.486 0.946 17.583
interseries.cor(GBY.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.627656003




##### GRE #####
GRE.rwl <- read.rwl(fname = "GRE.rwl", format = "auto")
sens1(GRE.rwl)
[1] 0.5487316
corr.rwl.seg(rwl = GRE.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(GRE.rwl)
GRE.rwi <- detrend(rwl = GRE.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
GRE.crn <- chron(x = GRE.rwi, prefix = "GRE", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = GRE.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Greenwood Plantation | GRE", xlab="Time", ylab="RWI")

rwi.stats(GRE.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      47      47 27.761  1002    0 1002    0.329      NA   0.329     1    0.329 0.932 13.611
interseries.cor(GRE.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.58160513





##### GRP #####
GRP.rwl <- read.rwl(fname = "GRP.rwl", format = "auto")
sens1(GRP.rwl)
[1] 0.4061981
corr.rwl.seg(rwl = GRP.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(GRP.rwl)
GRP.rwi <- detrend(rwl = GRP.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
GRP.crn <- chron(x = GRP.rwi, prefix = "GRP", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = GRP.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Guadalupe River State Park | GRP", xlab="Time", ylab="RWI")

rwi.stats(GRP.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      37      37 18.698   545    0  545    0.257      NA   0.257     1    0.257 0.866 6.461
interseries.cor(GRP.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.478660605



##### GUT #####
GUT.rwl <- read.rwl(fname = "GUT.rwl", format = "auto")
sens1(GUT.rwl)
[1] 0.4222328
corr.rwl.seg(rwl = GUT.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(GUT.rwl)
GUT.rwi <- detrend(rwl = GUT.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
GUT.crn <- chron(x = GUT.rwi, prefix = "GUT", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = GUT.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Devil's Gut | GUT", xlab="Time", ylab="RWI")

rwi.stats(GUT.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      58      58 16.077   895    0  895    0.319      NA   0.319     1    0.319 0.883 7.514
interseries.cor(GUT.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.578920624






##### HAL #####
HAL.rwl <- read.rwl(fname = "HAL.rwl", format = "auto")
sens1(HAL.rwl)
[1] 0.3314869
corr.rwl.seg(rwl = HAL.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(HAL.rwl)
HAL.rwi <- detrend(rwl = HAL.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
HAL.crn <- chron(x = HAL.rwi, prefix = "HAL", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = HAL.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Lavaca River | HAL", xlab="Time", ylab="RWI")

rwi.stats(HAL.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      76      76 35.463  2417    0 2417    0.527      NA   0.527     1    0.527 0.975 39.462
interseries.cor(HAL.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.729739718



##### HHO #####
HHO.rwl <- read.rwl(fname = "HHO.rwl", format = "auto")
sens1(HHO.rwl)
[1] 0.2310483
corr.rwl.seg(rwl = HHO.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(HHO.rwl)
HHO.rwi <- detrend(rwl = HHO.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
HHO.crn <- chron(x = HHO.rwi, prefix = "HHO", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = HHO.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Hampton Hills | HHO", xlab="Time", ylab="RWI")

rwi.stats(HHO.rwi)
n.cores n.trees     n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      32      32 18.78   496    0  496    0.265      NA   0.265     1    0.265 0.872 6.785
interseries.cor(HHO.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.507792281





##### JWJ #####
JWJ.rwl <- read.rwl(fname = "JWJ.rwl", format = "auto")
sens1(JWJ.rwl)
[1] 0.3630133
corr.rwl.seg(rwl = JWJ.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(JWJ.rwl)
JWJ.rwi <- detrend(rwl = JWJ.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
JWJ.crn <- chron(x = JWJ.rwi, prefix = "JWJ", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = JWJ.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="JW Jones Ecological Research Center | JWJ", xlab="Time", ylab="RWI")

rwi.stats(JWJ.rwi)
n.cores n.trees      n n.tot n.wt  n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1     242     242 76.526 24276    0 24276     0.37      NA    0.37     1     0.37 0.978 44.93
interseries.cor(JWJ.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.59626181





##### KIS #####
KIS.rwl <- read.rwl(fname = "KIS.rwl", format = "auto")
sens1(KIS.rwl)
[1] 0.4626289
corr.rwl.seg(rwl = KIS.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(KIS.rwl)
KIS.rwi <- detrend(rwl = KIS.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
KIS.crn <- chron(x = KIS.rwi, prefix = "KIS", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = KIS.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Kisatchie Hills | KIS", xlab="Time", ylab="RWI")

rwi.stats(KIS.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      48      48 16.413   597    0  597    0.327      NA   0.327     1    0.327 0.889 7.989
interseries.cor(KIS.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.562492229





##### LAS #####
LAS.rwl <- read.rwl(fname = "LAS.rwl", format = "auto")
sens1(LAS.rwl)
[1] 0.4917812
corr.rwl.seg(rwl = LAS.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(LAS.rwl)
LAS.rwi <- detrend(rwl = LAS.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
LAS.crn <- chron(x = LAS.rwi, prefix = "LAS", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = LAS.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Lassiter Swamp | LAS", xlab="Time", ylab="RWI")

rwi.stats(LAS.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      25      25 14.072   293    0  293    0.313      NA   0.313     1    0.313 0.865 6.415
interseries.cor(LAS.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.545054076




##### LEN #####
LEN.rwl <- read.rwl(fname = "LEN.rwl", format = "auto")
sens1(LEN.rwl)
[1] 0.269539
corr.rwl.seg(rwl = LEN.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(LEN.rwl)
LEN.rwi <- detrend(rwl = LEN.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
LEN.crn <- chron(x = LEN.rwi, prefix = "LEN", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = LEN.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="O'Leno State Park | LEN", xlab="Time", ylab="RWI")

rwi.stats(LEN.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      24      24 17.887   276    0  276    0.213      NA   0.213     1    0.213 0.829 4.854
interseries.cor(LEN.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.423333034



##### LIV #####
LIV.rwl <- read.rwl(fname = "LIV.rwl", format = "auto")
sens1(LIV.rwl)
[1] 0.2458225
corr.rwl.seg(rwl = LIV.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(LIV.rwl)
LIV.rwi <- detrend(rwl = LIV.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
LIV.crn <- chron(x = LIV.rwi, prefix = "LIV", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = LIV.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Live Oak | LIV", xlab="Time", ylab="RWI")

rwi.stats(LIV.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      25      25 15.428   300    0  300     0.34      NA    0.34     1     0.34 0.888 7.937
interseries.cor(LIV.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.48455886





##### LWR #####
LWR.rwl <- read.rwl(fname = "LWR.rwl", format = "auto")
sens1(LWR.rwl)
[1] 0.4321357
corr.rwl.seg(rwl = LWR.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(LWR.rwl)
LWR.rwi <- detrend(rwl = LWR.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
LWR.crn <- chron(x = LWR.rwi, prefix = "LWR", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = LWR.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Lower Withlacoochee River | LWR", xlab="Time", ylab="RWI")

rwi.stats(LWR.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps  snr
1      28      28 10.348   376    0  376    0.176      NA   0.176     1    0.176 0.688 2.21
interseries.cor(LWR.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.378445975





##### MCO #####
MCO.rwl <- read.rwl(fname = "MCO.rwl", format = "auto")
sens1(MCO.rwl)
[1] 0.2853269
corr.rwl.seg(rwl = MCO.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(MCO.rwl)
MCO.rwi <- detrend(rwl = MCO.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
MCO.crn <- chron(x = MCO.rwi, prefix = "MCO", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = MCO.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Moore's Creek | MCO", xlab="Time", ylab="RWI")

rwi.stats(MCO.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      24      24 12.128   276    0  276     0.29      NA    0.29     1     0.29 0.832 4.946
interseries.cor(MCO.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.521241633



##### MOO #####
MOO.rwl <- read.rwl(fname = "MOO.rwl", format = "auto")
sens1(MOO.rwl)
[1] 0.2891744
corr.rwl.seg(rwl = MOO.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(MOO.rwl)
MOO.rwi <- detrend(rwl = MOO.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
MOO.crn <- chron(x = MOO.rwi, prefix = "MOO", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = MOO.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Moody Tract | MOO", xlab="Time", ylab="RWI")

rwi.stats(MOO.rwi)
n.cores n.trees     n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      10      10 8.247    45    0   45    0.353      NA   0.353     1    0.353 0.818 4.498
interseries.cor(MOO.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.51629483





##### NHS #####
NHS.rwl <- read.rwl(fname = "NHS.rwl", format = "auto")
sens1(NHS.rwl)
[1] 0.2600352
corr.rwl.seg(rwl = NHS.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(NHS.rwl)
NHS.rwi <- detrend(rwl = NHS.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
NHS.crn <- chron(x = NHS.rwi, prefix = "NHS", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = NHS.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="New Hill Beaver Tree Farm | NHS", xlab="Time", ylab="RWI")

rwi.stats(NHS.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      33      33 23.456   528    0  528    0.198      NA   0.198     1    0.198 0.853 5.794
interseries.cor(NHS.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.365798052




##### NOT #####
NOT.rwl <- read.rwl(fname = "NOT.rwl", format = "auto")
sens1(NOT.rwl)
[1] 0.5862978
corr.rwl.seg(rwl = NOT.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(NOT.rwl)
NOT.rwi <- detrend(rwl = NOT.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
NOT.crn <- chron(x = NOT.rwi, prefix = "NOT", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = NOT.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Nottoway River | NOT", xlab="Time", ylab="RWI")

rwi.stats(NOT.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      53      53 21.905  1047    0 1047    0.364      NA   0.364     1    0.364 0.926 12.511
interseries.cor(NOT.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.601899677




##### OCM #####
OCM.rwl <- read.rwl(fname = "OCM.rwl", format = "auto")
sens1(OCM.rwl)
[1] 0.5289681
corr.rwl.seg(rwl = OCM.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(OCM.rwl)
OCM.rwi <- detrend(rwl = OCM.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
OCM.crn <- chron(x = OCM.rwi, prefix = "OCM", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = OCM.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Ocmulgee River | OCM", xlab="Time", ylab="RWI")

rwi.stats(OCM.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      41      41 13.625   458    0  458    0.348      NA   0.348     1    0.348 0.879 7.283
interseries.cor(OCM.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.615089878




##### ONF #####
ONF.rwl <- read.rwl(fname = "ONF.rwl", format = "auto")
sens1(ONF.rwl)
[1] 0.3236371
corr.rwl.seg(rwl = ONF.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(ONF.rwl)
ONF.rwi <- detrend(rwl = ONF.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
ONF.crn <- chron(x = ONF.rwi, prefix = "ONF", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = ONF.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Ocala National Forest | ONF", xlab="Time", ylab="RWI")

rwi.stats(ONF.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      35      35 24.534   566    0  566    0.309      NA   0.309     1    0.309 0.916 10.955
interseries.cor(ONF.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.471246509




##### PAS #####
PAS.rwl <- read.rwl(fname = "PAS.rwl", format = "auto")
sens1(PAS.rwl)
[1] 0.6080528
corr.rwl.seg(rwl = PAS.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(PAS.rwl)
PAS.rwi <- detrend(rwl = PAS.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
PAS.crn <- chron(x = PAS.rwi, prefix = "PAS", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = PAS.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Pascagoula River | PAS", xlab="Time", ylab="RWI")

rwi.stats(PAS.rwi)
n.cores n.trees     n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      32      32 14.08   447    0  447    0.375      NA   0.375     1    0.375 0.894 8.442
interseries.cor(PAS.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.615971756




##### PER #####
PER.rwl <- read.rwl(fname = "PER.rwl", format = "auto")
sens1(PER.rwl)
[1] 0.4734148
corr.rwl.seg(rwl = PER.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(PER.rwl)
PER.rwi <- detrend(rwl = PER.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
PER.crn <- chron(x = PER.rwi, prefix = "PER", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = PER.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Pearl River | PER", xlab="Time", ylab="RWI")

rwi.stats(PER.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff  eps    snr
1      30      30 18.231   413    0  413    0.357      NA   0.357     1    0.357 0.91 10.138
interseries.cor(PER.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.593452527




##### PTB #####
PTB.rwl <- read.rwl(fname = "PTB.rwl", format = "auto")
sens1(PTB.rwl)
[1] 0.5967774
corr.rwl.seg(rwl = PTB.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(PTB.rwl)
PTB.rwi <- detrend(rwl = PTB.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
PTB.crn <- chron(x = PTB.rwi, prefix = "PTB", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = PTB.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Peachtree Bottoms | PTB", xlab="Time", ylab="RWI")

rwi.stats(PTB.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      39      39 20.157   679    0  679    0.406      NA   0.406     1    0.406 0.932 13.768
interseries.cor(PTB.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.651429569




##### SBP #####
SBP.rwl <- read.rwl(fname = "SBP.rwl", format = "auto")
sens1(SBP.rwl)
[1] 0.5448442
corr.rwl.seg(rwl = SBP.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(SBP.rwl)
SBP.rwi <- detrend(rwl = SBP.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
SBP.crn <- chron(x = SBP.rwi, prefix = "SBP", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = SBP.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="San Bernard River | SBP", xlab="Time", ylab="RWI")

rwi.stats(SBP.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      27      27 12.142   277    0  277    0.459      NA   0.459     1    0.459 0.911 10.299
interseries.cor(SBP.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.64616283



##### SHO #####
SHO.rwl <- read.rwl(fname = "SHO.rwl", format = "auto")
sens1(SHO.rwl)
[1] 0.3087999
corr.rwl.seg(rwl = SHO.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(SHO.rwl)
SHO.rwi <- detrend(rwl = SHO.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
SHO.crn <- chron(x = SHO.rwi, prefix = "SHO", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = SHO.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Jeffries Smokehouse | SHO", xlab="Time", ylab="RWI")

rwi.stats(SHO.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      31      31 18.347   399    0  399    0.239      NA   0.239     1    0.239 0.852 5.775
interseries.cor(SHO.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.433746058


##### SPA #####
SPA.rwl <- read.rwl(fname = "SPA.rwl", format = "auto")
sens1(SPA.rwl)
[1] 0.2357965
corr.rwl.seg(rwl = SPA.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(SPA.rwl)
SPA.rwi <- detrend(rwl = SPA.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
SPA.crn <- chron(x = SPA.rwi, prefix = "SPA", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = SPA.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Spalding | SPA", xlab="Time", ylab="RWI")

rwi.stats(SPA.rwi)
n.cores n.trees     n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      56      56 38.86  1431    0 1431    0.369      NA   0.369     1    0.369 0.958 22.724
interseries.cor(SPA.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.583



##### SUW #####
SUW.rwl <- read.rwl(fname = "SUW.rwl", format = "auto")
sens1(SUW.rwl)
[1] 0.3781456
corr.rwl.seg(rwl = SUW.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(SUW.rwl)
SUW.rwi <- detrend(rwl = SUW.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
SUW.crn <- chron(x = SUW.rwi, prefix = "SUW", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = SUW.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Suwannee | SUW", xlab="Time", ylab="RWI")

rwi.stats(SUW.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      42      42 20.639   811    0  811     0.32      NA    0.32     1     0.32 0.907 9.726
interseries.cor(SUW.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.555443014





##### UWR #####
UWR.rwl <- read.rwl(fname = "UWR.rwl", format = "auto")
sens1(UWR.rwl)
[1] 0.6058353
corr.rwl.seg(rwl = UWR.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(UWR.rwl)
UWR.rwi <- detrend(rwl = UWR.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
UWR.crn <- chron(x = UWR.rwi, prefix = "UWR", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = UWR.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Upper Withlacoochee River | UWR", xlab="Time", ylab="RWI")

rwi.stats(UWR.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      47      47 20.224   828    0  828    0.279      NA   0.279     1    0.279 0.887 7.835
interseries.cor(UWR.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.547261057





##### WAN #####
WAN.rwl <- read.rwl(fname = "WAN.rwl", format = "auto")
sens1(WAN.rwl)
[1] 0.2511192
corr.rwl.seg(rwl = WAN.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(WAN.rwl)
WAN.rwi <- detrend(rwl = WAN.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
WAN.crn <- chron(x = WAN.rwi, prefix = "WAN", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = WAN.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Wannee | WAN", xlab="Time", ylab="RWI")

rwi.stats(WAN.rwi)
n.cores n.trees     n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      15      15 8.944   105    0  105    0.223      NA   0.223     1    0.223 0.719 2.561
interseries.cor(WAN.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.476357887





##### WIR #####
WIR.rwl <- read.rwl(fname = "WIR.rwl", format = "auto")
sens1(WIR.rwl)
[1] 0.4285208
corr.rwl.seg(rwl = WIR.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(WIR.rwl)
WIR.rwi <- detrend(rwl = WIR.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
WIR.crn <- chron(x = WIR.rwi, prefix = "WIR", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = WIR.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Middle Withlacoochee River | WIR", xlab="Time", ylab="RWI")

rwi.stats(WIR.rwi)
n.cores n.trees     n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      27      27 11.32   312    0  312    0.262      NA   0.262     1    0.262 0.801 4.024
interseries.cor(WIR.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.5243598





##### WOO #####
WOO.rwl <- read.rwl(fname = "WOO.rwl", format = "auto")
sens1(WOO.rwl)
[1] 0.2971017
corr.rwl.seg(rwl = WOO.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(WOO.rwl)
WOO.rwi <- detrend(rwl = WOO.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
WOO.crn <- chron(x = WOO.rwi, prefix = "WOO", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = WOO.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Weymouth Woods | WOO", xlab="Time", ylab="RWI")

rwi.stats(WOO.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      34      34 26.016   561    0  561      0.3      NA     0.3     1      0.3 0.918 11.171
interseries.cor(WOO.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.517816712



##### YEG #####
YEG.rwl <- read.rwl(fname = "YEG.rwl", format = "auto")
sens1(YEG.rwl)
[1] 0.2973489
corr.rwl.seg(rwl = YEG.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(YEG.rwl)
YEG.rwi <- detrend(rwl = YEG.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
YEG.crn <- chron(x = YEG.rwi, prefix = "YEG", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = YEG.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Yegua Creek | YEG", xlab="Time", ylab="RWI")

rwi.stats(YEG.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      37      37 26.199   618    0  618    0.521      NA   0.521     1    0.521 0.966 28.451
interseries.cor(YEG.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.712974065




##### ZLB #####
ZLB.rwl <- read.rwl(fname = "ZLB.rwl", format = "auto")
sens1(ZLB.rwl)
[1] 0.2987101
corr.rwl.seg(rwl = ZLB.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05,
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL)
markers <- pointer(ZLB.rwl)
ZLB.rwi <- detrend(rwl = ZLB.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
ZLB.crn <- chron(x = ZLB.rwi, prefix = "ZLB", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = ZLB.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1,
         main="Zoar State Forest | ZLB", xlab="Time", ylab="RWI")

rwi.stats(ZLB.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      27      27 16.252   351    0  351    0.248      NA   0.248     1    0.248 0.843 5.368
interseries.cor(ZLB.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.399783781


















########### MEXICO SITES ###########

##### JAR #####
JAR.rwl <- read.rwl(fname = "JAR.rwl", format = "auto")
sens1(JAR.rwl)
[1] 0.3452043
corr.rwl.seg(rwl = JAR.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(JAR.rwl)
JAR.rwi <- detrend(rwl = JAR.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
JAR.crn <- chron(x = JAR.rwi, prefix = "JAR", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = JAR.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Jaramillo | JAR", xlab="Time", ylab="RWI")

rwi.stats(JAR.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      25      25 15.423   300    0  300    0.363      NA   0.363     1    0.363 0.898 8.798
interseries.cor(JAR.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.60073556




##### LIB #####
LIB.rwl <- read.rwl(fname = "LIB.rwl", format = "auto")
sens1(LIB.rwl)
[1] 0.4040196
corr.rwl.seg(rwl = LIB.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(LIB.rwl)
LIB.rwi <- detrend(rwl = LIB.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
LIB.crn <- chron(x = LIB.rwi, prefix = "LIB", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = LIB.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Libres | LIB", xlab="Time", ylab="RWI")

rwi.stats(LIB.rwi)
n.cores n.trees     n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      41      41 33.22   820    0  820    0.477      NA   0.477     1    0.477 0.968 30.284
interseries.cor(LIB.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.664220451


##### MAL #####
MAL.rwl <- read.rwl(fname = "MAL.rwl", format = "auto")
sens1(MAL.rwl)
[1] 0.4824841
corr.rwl.seg(rwl = MAL.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(MAL.rwl)
MAL.rwi <- detrend(rwl = MAL.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
MAL.crn <- chron(x = MAL.rwi, prefix = "MAL", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = MAL.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="El Malpaso | MAL", xlab="Time", ylab="RWI")

rwi.stats(MAL.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      51      51 25.949   966    0  966    0.295      NA   0.295     1    0.295 0.916 10.835
interseries.cor(MAL.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.537435192



##### PAN #####
PAN.rwl <- read.rwl(fname = "PAN.rwl", format = "auto")
sens1(PAN.rwl)
[1] 0.289502
corr.rwl.seg(rwl = PAN.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(PAN.rwl)
PAN.rwi <- detrend(rwl = PAN.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
PAN.crn <- chron(x = PAN.rwi, prefix = "PAN", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = PAN.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Paso Nacional | PAN", xlab="Time", ylab="RWI")

rwi.stats(PAN.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      89      89 32.031  3627    0 3627    0.313      NA   0.313     1    0.313 0.936 14.623
interseries.cor(PAN.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.594349928



##### PEN #####
PEN.rwl <- read.rwl(fname = "PEN.rwl", format = "auto")
sens1(PEN.rwl)
[1] 0.3137458
corr.rwl.seg(rwl = PEN.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(PEN.rwl)
PEN.rwi <- detrend(rwl = PEN.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
PEN.crn <- chron(x = PEN.rwi, prefix = "PEN", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = PEN.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Pena Nevada | PEN", xlab="Time", ylab="RWI")

rwi.stats(PEN.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      82      82 20.716  2329    0 2329    0.411      NA   0.411     1    0.411 0.935 14.447
interseries.cor(PEN.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.661734668



##### PPO #####
PPO.rwl <- read.rwl(fname = "PPO.rwl", format = "auto")
sens1(PPO.rwl)
[1] 0.2623086
corr.rwl.seg(rwl = PPO.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(PPO.rwl)
PPO.rwi <- detrend(rwl = PPO.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
PPO.crn <- chron(x = PPO.rwi, prefix = "PPO", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = PPO.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Parque Pico de Orizaba | PPO", xlab="Time", ylab="RWI")

rwi.stats(PPO.rwi)
n.cores n.trees     n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      18      18 7.444   127    0  127    0.235      NA   0.235     1    0.235 0.696 2.287
interseries.cor(PPO.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.456550806



##### RAN #####
RAN.rwl <- read.rwl(fname = "RAN.rwl", format = "auto")
sens1(RAN.rwl)
[1] 0.296293
corr.rwl.seg(rwl = RAN.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(RAN.rwl)
RAN.rwi <- detrend(rwl = RAN.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
RAN.crn <- chron(x = RAN.rwi, prefix = "RAN", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = RAN.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Rancho Nuevo | RAN", xlab="Time", ylab="RWI")

rwi.stats(RAN.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff eps   snr
1      20      20 11.451   190    0  190    0.259      NA   0.259     1    0.259 0.8 4.006
interseries.cor(RAN.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.476149525


##### RDC #####
RDC.rwl <- read.rwl(fname = "RDC.rwl", format = "auto")
sens1(RDC.rwl)
[1] 0.4317849
corr.rwl.seg(rwl = RDC.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(RDC.rwl)
RDC.rwi <- detrend(rwl = RDC.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
RDC.crn <- chron(x = RDC.rwi, prefix = "RDC", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = RDC.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Rancho del Cielo | RDC", xlab="Time", ylab="RWI")

rwi.stats(RDC.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1      27      27 14.884   335    0  335    0.359      NA   0.359     1    0.359 0.893 8.335
interseries.cor(RDC.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.599981756


##### SAB #####
SAB.rwl <- read.rwl(fname = "SAB.rwl", format = "auto")
sens1(SAB.rwl)
[1] 0.4396837
corr.rwl.seg(rwl = SAB.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(SAB.rwl)
SAB.rwi <- detrend(rwl = SAB.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
SAB.crn <- chron(x = SAB.rwi, prefix = "SAB", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = SAB.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Rio Sabinas | SAB", xlab="Time", ylab="RWI")

rwi.stats(SAB.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps  snr
1      66      66 30.293  1906    0 1906    0.198      NA   0.198     1    0.198 0.882 7.48
interseries.cor(SAB.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.438734144


##### SCU #####
SCU.rwl <- read.rwl(fname = "SCU.rwl", format = "auto")
sens1(SCU.rwl)
[1] 0.3857435
corr.rwl.seg(rwl = SCU.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(SCU.rwl)
SCU.rwi <- detrend(rwl = SCU.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
SCU.crn <- chron(x = SCU.rwi, prefix = "SCU", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = SCU.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Santa Maria de Las Cuevas | SCU", xlab="Time", ylab="RWI")

rwi.stats(SCU.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps    snr
1      51      51 32.809  1275    0 1275    0.482      NA   0.482     1    0.482 0.968 30.548
interseries.cor(SCU.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.707016508


##### SJT #####
SJT.rwl <- read.rwl(fname = "SJT.rwl", format = "auto")
sens1(SJT.rwl)
[1] 0.3216114
corr.rwl.seg(rwl = SJT.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(SJT.rwl)
SJT.rwi <- detrend(rwl = SJT.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
SJT.crn <- chron(x = SJT.rwi, prefix = "SJT", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = SJT.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="San Juan de Teotihuacan | SJT", xlab="Time", ylab="RWI")

rwi.stats(SJT.rwi)
n.cores n.trees     n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff  eps   snr
1       4       4 3.469     6    0    6    0.339      NA   0.339     1    0.339 0.64 1.778
interseries.cor(SJT.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.274051675


##### VER #####
VER.rwl <- read.rwl(fname = "VER.rwl", format = "auto")
sens1(VER.rwl)
[1] 0.3649314
corr.rwl.seg(rwl = VER.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(VER.rwl)
VER.rwi <- detrend(rwl = VER.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
VER.crn <- chron(x = VER.rwi, prefix = "VER", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = VER.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Rio Verde | VER", xlab="Time", ylab="RWI")

rwi.stats(VER.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps  snr
1      28      28 14.463   336    0  336    0.205      NA   0.205     1    0.205 0.788 3.72
interseries.cor(VER.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.385274904


##### VIL #####
VIL.rwl <- read.rwl(fname = "VIL.rwl", format = "auto")
sens1(VIL.rwl)
[1] 0.3345764
corr.rwl.seg(rwl = VIL.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(VIL.rwl)
VIL.rwi <- detrend(rwl = VIL.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
VIL.crn <- chron(x = VIL.rwi, prefix = "VIL", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = VIL.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Villareal | VIL", xlab="Time", ylab="RWI")

rwi.stats(VIL.rwi)
n.cores n.trees      n n.tot n.wt n.bt rbar.tot rbar.wt rbar.bt c.eff rbar.eff   eps   snr
1     119     119 59.083  6598    0 6598    0.388      NA   0.388     1    0.388 0.974 37.39
interseries.cor(VIL.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
0.667035845


















####### DOESN'T WORK ##########

##### UCO #####
UCO.rwl <- read.rwl(fname = "UCO.rwl", format = "auto")
sens1(UCO.rwl)

corr.rwl.seg(rwl = UCO.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) 
markers <- pointer(UCO.rwl)
UCO.rwi <- detrend(rwl = UCO.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
UCO.crn <- chron(x = UCO.rwi, prefix = "UCO", biweight = TRUE, prewhiten = FALSE)

crn.plot(crn = UCO.crn, add.spline = TRUE, nyrs = NULL, f = 0.9, crn.line.col='grey50',
         spline.line.col='firebrick3', samp.depth.col='aliceblue', samp.depth.border.col='lightskyblue2',
         crn.lwd=1, spline.lwd=3.0, abline.pos=1, abline.col='lightsteelblue4', abline.lty=1,abline.lwd=1, 
         main="Union Camp Big Woods | UCO", xlab="Time", ylab="RWI")

rwi.stats(UCO.rwi)

interseries.cor(UCO.rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))




