require(inline)
require(RcppArmadillo)


src <- '
#include <Rcpp.h>
#include <math.h>
#include <RcppArmadillo.h>
#include <algorithm>
#include <iostream>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;


// Input parameters 

NumericVector pars(ParFit);

// Read Matrix

IntegerMatrix Lmask(Land_mask);
int nrow = Lmask.nrow();
int ncol = Lmask.ncol();

IntegerMatrix Ddir(dat_dir);
int nrow2 = Ddir.nrow();
int ncol2 = Ddir.ncol();

NumericMatrix Ddis(dat_dis);
int nrow3 = Ddis.nrow();
int ncol3 = Ddis.ncol();

NumericMatrix Sriv_init(Sriv);
NumericMatrix SrivN_init(SrivN);

int nrow4 = Sriv_init.nrow();
int ncol4 = Sriv_init.ncol();

NumericMatrix RArea(RivArea);

NumericMatrix HumanN(dat_humanN);
IntegerMatrix TestMat(nrow3, ncol3);


// Define Scalar and Vector

int m = 0;
int l = 0;
int lminus1 = 0;
int lplus1 = 0;

IntegerVector simlength(dim3);


double v = 43.2/2;
double N2Oloss = 0;
double Nloss = 0;
double Nloss2 = 0;
double Nconc = 0;
double Nconc2 = 0;

// Read Array

//NumericVector vecArray(Sriv);
//NumericVector vecArray3(Qout);
NumericVector vecArray4(VISITro);

//NumericVector vecArrayN(SrivN);
//NumericVector vecArrayN3(QoutN);
NumericVector vecArrayN4(VISITnl);

NumericVector vecArrayTas(dat_tas);

//IntegerVector arrayDims = vecArray.attr("dim");
//IntegerVector arrayDims3 = vecArray3.attr("dim");
IntegerVector arrayDims4 = vecArray4.attr("dim");

//IntegerVector arrayDimsN = vecArrayN.attr("dim");
//IntegerVector arrayDimsN3 = vecArrayN3.attr("dim");
IntegerVector arrayDimsN4 = vecArrayN4.attr("dim");

IntegerVector arrayDimsTas = vecArrayTas.attr("dim");

//arma::cube cubeArray(vecArray.begin(), arrayDims[0], arrayDims[1], arrayDims[2], false);
//arma::cube cubeArray3(vecArray3.begin(), arrayDims3[0], arrayDims3[1], arrayDims3[2], false);
arma::cube cubeArray4(vecArray4.begin(), arrayDims4[0], arrayDims4[1], arrayDims4[2], false);

//arma::cube cubeArrayN(vecArrayN.begin(), arrayDimsN[0], arrayDimsN[1], arrayDimsN[2], false);
//arma::cube cubeArrayN3(vecArrayN3.begin(), arrayDimsN3[0], arrayDimsN3[1], arrayDimsN3[2], false);
arma::cube cubeArrayN4(vecArrayN4.begin(), arrayDimsN4[0], arrayDimsN4[1], arrayDimsN4[2], false);

arma::cube cubeArrayTas(vecArrayTas.begin(), arrayDimsTas[0], arrayDimsTas[1], arrayDimsTas[2], false);

// Make 3d array

//arma::cube cubeArray = zeros<cube>(arrayDims[0], arrayDims[1], arrayDims[2]);
//arma::cube cubeArrayN = zeros<cube>(arrayDims[0], arrayDims[1], arrayDims[2]);
//arma::cube cubeArray3 = zeros<cube>(arrayDims[0], arrayDims[1], arrayDims[2]);
//arma::cube cubeArrayN3 = zeros<cube>(arrayDims[0], arrayDims[1], arrayDims[2]);
//arma::cube TestArray = zeros<cube>(arrayDims[0], arrayDims[1], arrayDims[2]);

arma::cube cubeArray = zeros<cube>(nrow4, ncol4, simlength[2]);
arma::cube cubeArrayN = zeros<cube>(nrow4, ncol4, simlength[2]);
arma::cube cubeArray3 = zeros<cube>(nrow4, ncol4, simlength[2]);
arma::cube cubeArrayN3 = zeros<cube>(nrow4, ncol4, simlength[2]);
arma::cube TestArray = zeros<cube>(nrow4, ncol4, simlength[2]);


// Make Matrix for output

NumericMatrix IntNout(720, 360);

// Initialize Sriv and SrivN

for(int l1 = 0; l1 < 720; l1++){
for(int m1 = 0; m1 < 360; m1++){
cubeArray(l1, m1, 0) = Sriv_init(l1, m1);
cubeArrayN(l1, m1, 0) = SrivN_init(l1, m1);
}
}

// River routing scheme

for(int t = 0; t < simlength[2]-1; t++){

for(int j = 0; j < nrow-1; j++){

l = Lmask(j,0) - 1; 
m = Lmask(j,1) - 1;

cubeArray(l,m,t) += cubeArray4(l,m,t/31);
cubeArrayN(l,m,t) += cubeArrayN4(l,m,t/31) + HumanN(l,m);
}

for(int j = 0; j < (nrow-1); j++){

l = Lmask(j,0) - 1; 
m = Lmask(j,1) - 1;

if(l == 0){
lminus1 = 719;
} else {
lminus1 = l - 1;
}

if(l == 719){
lplus1 = 0;
} else {
lplus1 = l + 1;
}



// Calcurate in-stream process for Nitrogen

Nconc = cubeArrayN(l,m,t)/cubeArray(l,m,t)*1000*1000; // mg-N/L

if((isnan(Nconc)) || Nconc < 0){
Nconc = 0;
}

if(Nconc > 5000){
Nconc = 5000;
}

Nloss = (Nconc*pow(10, pars[0]))/(pars[1] + Nconc) * exp((cubeArrayTas(l,m,t/30) - 20)/10*log(pars[2])) * RArea(l,m); // mg-N/L

//Nloss = 27*exp(0.51* log(Nconc) - 2) * exp(log(2)/10*cubeArrayTas(l,m,t/30)) * RArea(l,m); // mg-N/L

if(Nloss > cubeArrayN(l,m,t)){
Nloss = cubeArrayN(l,m,t) * 0.9;
}


double tempQin = 0;
double tempNin = 0;

if(Ddir(l,m-1) == 5){
tempQin += cubeArray(l,m-1,t) * std::min(v/Ddis(l,m-1), 1.0);
tempNin += cubeArrayN(l,m-1,t) * std::min(v/Ddis(l,m-1), 1.0);
TestMat(l,m) += 1;
} if(Ddir(lminus1,m-1) == 4){
tempQin += cubeArray(lminus1,m-1,t) * std::min(v/Ddis(lminus1,m-1), 1.0);
tempNin += cubeArrayN(lminus1,m-1,t) * std::min(v/Ddis(lminus1,m-1), 1.0);
TestMat(l,m) += 1;
} if(Ddir(lminus1,m) == 3){
tempQin += cubeArray(lminus1,m,t) * std::min(v/Ddis(lminus1,m), 1.0);
tempNin += cubeArrayN(lminus1,m,t) * std::min(v/Ddis(lminus1,m), 1.0);
TestMat(l,m) += 1;
} if(Ddir(lminus1,m+1) == 2){
tempQin += cubeArray(lminus1,m+1,t) * std::min(v/Ddis(lminus1,m+1), 1.0);
tempNin += cubeArrayN(lminus1,m+1,t) * std::min(v/Ddis(lminus1,m+1), 1.0);
TestMat(l,m) += 1;
} if(Ddir(l,m+1) == 1){
tempQin += cubeArray(l,m+1,t) * std::min(v/Ddis(l,m+1), 1.0);
tempNin += cubeArrayN(l,m+1,t) * std::min(v/Ddis(l,m+1), 1.0);
TestMat(l,m) += 1;
} if(Ddir(lplus1,m+1) == 8){
tempQin += cubeArray(lplus1,m+1,t) * std::min(v/Ddis(lplus1,m+1), 1.0);
tempNin += cubeArrayN(lplus1,m+1,t) * std::min(v/Ddis(lplus1,m+1), 1.0);
TestMat(l,m) += 1;
} if(Ddir(lplus1,m) == 7){
tempQin += cubeArray(lplus1,m,t) * std::min(v/Ddis(lplus1,m), 1.0);
tempNin += cubeArrayN(lplus1,m,t) * std::min(v/Ddis(lplus1,m), 1.0);
TestMat(l,m) += 1;
} if(Ddir(lplus1,m-1) == 6){
tempQin += cubeArray(lplus1,m-1,t) * std::min(v/Ddis(lplus1,m-1), 1.0);
tempNin += cubeArrayN(lplus1,m-1,t) * std::min(v/Ddis(lplus1,m-1), 1.0);
TestMat(l,m) += 1;
}

cubeArray3(l,m,t) = cubeArray(l,m,t) * std::min(v/Ddis(l,m), 1.0);        
cubeArrayN3(l,m,t) = cubeArrayN(l,m,t) * std::min(v/Ddis(l,m), 1.0);

// To avoid oscilation
if(cubeArrayN3(l,m,t) < 0){
cubeArrayN3(l,m,t) = 1;
}


// Output variable for Test

// TestArray(l,m,t) = cubeArrayN4(l,m,t/31);
// TestArray(l,m,t) = Nconc;
// TestArray(l,m,t) = Nloss * 0.1; //When MCMC, please replace here for Speed
// TestArray(l,m,t) = Nloss * (0.013/(1 + pow(0.007753, log(6.631481/cubeArrayTas(l,m,t/30)))) + 0.001846);
TestArray(l,m,t) = Nloss * (0.012/(1 + exp(0.5 * (cubeArrayTas(l,m,t/30)-8)))+ 0.001846);

IntNout(l, m) += cubeArrayN3(l,m,t);

// Calcurate mass balance
cubeArray(l,m,t+1) = cubeArray(l,m,t) + tempQin - cubeArray3(l,m,t);
cubeArrayN(l,m,t+1) = cubeArrayN(l,m,t) + tempNin - cubeArrayN3(l,m,t) - Nloss;
}
}

return(List::create(Named("Sriv")= cubeArray, 
Named("Qout") = cubeArray3,
Named("SrivN")= cubeArrayN,                     
Named("QoutN") = cubeArrayN3,
Named("TestOut") = TestArray,
Named("QoutNint") = IntNout));
'


FitRiverFlow = cxxfunction(signature(
    dim3 = "integer",
    dat_dir = "integer",
    Land_mask = "integer",
    dat_dis = "numeric",
    Sriv="numeric",
    #Qout="numeric",
    #    Qin="numeric",
    SrivN="numeric",
    #    QinN="numeric",
    #QoutN="numeric",
    VISITro="numeric",
    VISITnl="numeric",
    RivArea="numeric",
    dat_humanN = "numeric",
    dat_tas = "numeric",
    ParFit = "numeric"
),
body=src,
plugin="RcppArmadillo")


temp_ro <- nc_open("Data_pd/Nsim_INI2020/visit_200901c_NEX-101_MMA11_ROF.nc")
VISITro_all <- ncvar_get(temp_ro, "ROF")
nc_close(temp_ro)
VISITro_all[is.na(VISITro_all)] <- 0
for(i in 1:1428) VISITro_all[,,i] <- VISITro_all[,,i]/30*1000*1000*area_mat # kg/m2/month to kg/grid-cell/day

#temp_N <- nc_open("Data_pd/Nsim_INI2020/visit_200901c_NEX-101_MMA11_NLEACH.nc")
#temp_N <- nc_open("Data_pd/Nsim_INI2020/visit_200831b_NEX-102_MMA11_NLEACH.nc")
#VISITnl_all <- ncvar_get(temp_N, "NLEACH")
#nc_close(temp_N)

temp_Na <- nc_open("Data_pd/Nsim_INI2020/VisitOutputs/visit_200831b_NEX-102_MMA11_NLEACH_crop.nc")
VISITnl_a <- ncvar_get(temp_Na, "NLEACH_crop")
nc_close(temp_Na)
temp_Nb <- nc_open("Data_pd/Nsim_INI2020/VisitOutputs/visit_200831b_NEX-102_MMA11_NLEACH_nat.nc")
VISITnl_b <- ncvar_get(temp_Nb, "NLEACH_nat")
nc_close(temp_Nb)
temp <- nc_open("Data_pd/Nsim_INI2020/VisitOutputs/visit_200831b_NEX-102_MMA11_fCrop.nc")
fcrop <- ncvar_get(temp, "fCrop")
nc_close(temp)

VISITnl_all <- array(as.double(0.0), c(720, 360, 1428))
#for(i in 1:1428) VISITnl_all[,,i] <- (VISITnl_a[,,i] * fcrop[,,i] + VISITnl_b[,,i] * (1- fcrop[,,i])) 
for(i in 1:1428) VISITnl_all[,,i] <- VISITnl_a[,,i]  * (fcrop[,,i])  + VISITnl_b[,,i]* (1- fcrop[,,i]-urban_frac_data)

rm(VISITnl_a); rm(VISITnl_b)


VISITnl_all[is.na(VISITnl_all)] <- 0
for(i in 1:1428) VISITnl_all[,,i] <- VISITnl_all[,,i]/1000/30.5*100*area_mat # g-N/ha/month to kg-N/grid-cell/day

rm(fcrop)
gc();gc();

Sriv <- array(as.double(0.0), c(720, 360, 360))
Sriv[,,1] <- 0#VISITro2[,,1]
Sriv_init <- Sriv[,,1]
SrivN  <- array(0, c(720, 360, 360))
SrivN[,,1] <- 0#VISITro2[,,1]
SrivN_init <- SrivN[,,1]
DistMat[is.na(DistMat)] <- 0
VISITro_all[is.na(VISITro_all)] <- 0
VISITnl_all[is.na(VISITnl_all)] <- 0
HumanN[is.na(HumanN)] <- 0


system.time(
    testout2 <- FitRiverFlow(dim3 = c(720, 360, 360),
                             dat_dir = dat_dir,
                             Land_mask = LandMask_Loop,
                             dat_dis = DistMat,
                             Sriv = Sriv[,,1],
                             SrivN = SrivN[,,1],
                             VISITro = VISITro_all[,,(1201:1212 -12)],
                             VISITnl = VISITnl_all[,,(1201:1212 -12)],
                             RivArea = RivArea,
                             dat_humanN = HumanN[,,30],
                             dat_tas = dat_tas[,360:1,],
                             ParFit = c(log10(96.3), 422, 1))
)
image.plot(testout2$QoutN[,,2])

abc <- apply(testout2$QoutN[,,1:360], MARGIN = c(1,2), sum, na.rm=TRUE)
sum(abc*dat_dir2, na.rm=TRUE)/1000/1000/1000
#(lm(abc[,][cbind(dat_he$Xex, dat_he$Yex)]/1000/1000 ~ dat_he$He2011))

Sriv[,,1] <- testout2$Sriv[,,359]
SrivN[,,1] <- testout2$SrivN[,,359]


### MCMC #### 

dat_he <- read.csv(file="Data_pd/RiverListGlobal.csv", header=TRUE)
dat_he <- dat_he[!is.na(dat_he$Xex), ]

#混合回数と引数の用意
iter <- 20000
Vmax <- Kno3 <- Q10 <-  sigma <- loglik_stor <-  numeric(iter+1)

Y <- dat_he$He2011

# Initial values

Vmax[1] <- Vmax_t <- log10(96.3) #Vmax_o
Kno3[1] <- Kno3_t <- 0.422 #Kno3_o
Q10[1] <- Q10_t <- 1.4 #Q10_o
sigma[1] <- sigma_t <- 2.5

testout <- FitRiverFlow(dim3 = c(720, 360, 360),
                        dat_dir = dat_dir,
                        Land_mask = LandMask_Loop,
                        dat_dis = DistMat,
                        Sriv = Sriv[,,1],
                        SrivN = SrivN[,,1],
                        VISITro = VISITro_all[,,1201:1212],
                        VISITnl = VISITnl_all[,,1201:1212],
                        RivArea = RivArea,
                        dat_humanN = HumanN[,,30],
                        dat_tas = dat_tas[,360:1,],
                        ParFit = c(Vmax[1], Kno3[1], Q10[1]))

X <- testout$QoutNint[cbind(dat_he$Xex, dat_he$Yex)]/1000/1000

oldll <- sum(dnorm(log(Y)-log(X), mean=0, sd=sigma[1], log=T), na.rm = TRUE) +
    dunif(Vmax[1], min = 1, max = 3, log = TRUE) +
    dunif(Kno3[1], min = 0.01, max = 1000, log = TRUE) +
    dunif(Q10[1], min = 1, max = 4, log = TRUE) + 
    dunif(sigma[1], 0, 4, log=T)

# Iteration

loglik_stor[1] <- oldll

a <- Sys.time()

for (i in 2:iter) {
    #can=x+runif(1,-eps,eps)
    #マルコフチェーンを生成してるところ
    
    Vmax_can <- Vmax_t + runif(1, -0.2, 0.2)
    Kno3_can <- Kno3_t*exp(runif(1, -0.5, 0.5))
    Q10_can <- Q10_t + runif(1, -0.2, 0.2)
    if(Q10_can < 1) Q10_can <- Q10_t + runif(1, 0, 0.1)
    if(Q10_can > 4) Q10_can <- Q10_t + runif(1, -0.1)
    sigma_can <- sigma_t * exp(runif(1, -0.1, 0.1))
    
    testout <- FitRiverFlow(dim3 = c(720, 360, 360),
                            dat_dir = dat_dir,
                            Land_mask = LandMask_Loop,
                            dat_dis = DistMat,
                            Sriv = Sriv[,,1],
                            SrivN = SrivN[,,1],
                            VISITro = VISITro_all[,,1201:1212],
                            VISITnl = VISITnl_all[,,1201:1212],
                            RivArea = RivArea,
                            dat_humanN = HumanN[,,30],
                            dat_tas = dat_tas[,360:1,],
                            ParFit = c(Vmax_can, Kno3_can, Q10_can))
    
    X_new <- testout$QoutNint[cbind(dat_he$Xex, dat_he$Yex)]/1000/1000
    
    loglik <- sum(dnorm(log(Y)-log(X_new), mean=0, sd=sigma_can, log=T), na.rm=TRUE) + 
        dunif(Vmax_can, min = 1, max = 3, log = TRUE) +
        dunif(Kno3_can, min = 0.01, max = 1000, log = TRUE) +
        dunif(Q10_can, min = 1, max = 4, log = TRUE) + 
        dunif(sigma_can, 0, 4, log=T)
    
    
    if (loglik-oldll > log(runif(1))){ 
        Vmax_t <- Vmax_can
        Kno3_t <- Kno3_can
        Q10_t <- Q10_can
        sigma_t <- sigma_can
        oldll=loglik
    }
    
    Vmax[i] <- Vmax_t
    Kno3[i] <- Kno3_t
    Q10[i] <- Q10_t
    sigma[i] <- sigma_t
    loglik_stor[i] <- oldll
    
    print(c(i, Sys.time()-a, loglik_stor[i]))
    #print(c(i, Sys.time()-a, log_post_old, log_post_new, Vmax_new, Kno3_new, Q10_new, sigma_new))
}

# MCMC loop end

### Store parameters ###

Vmax_o <- c(Vmax_o, Vmax)
Kno3_o <- c(Kno3_o, Kno3)
Q10_o <- c(Q10_o, Q10)
sigma_o <- c(sigma_o, sigma)
loglik_stor_o <- c(loglik_stor_o, loglik_stor)

PostParam <- data.frame(Vmax = Vmax, Kno3 = Kno3, Q10 = Q10, sigma = sigma, loglik = loglik_stor)
#save(PostParam, file="Data_pd/PostParam20191206.rdata")
#PostParam3 <- PostParam

filename <- paste0("~/OneDrive/OneDrive - 国立環境研究所/Research/GlobalNleach/Data_pd/PostParam", format(Sys.time(),"%m%d"),".rdata")
#save(PostParam, file=filename)

#load(file="Data_pd/PostParam0529.rdata")

### Checc results ###

sum(testout$QoutNint*dat_dir2, na.rm=TRUE)/1000/1000/1000

postMean <- c(mean(PostParam[5001:9999,1]), 
                mean(PostParam[5001:9999,2]), mean(PostParam[5001:9999,3]))

postout <- FitRiverFlow(dim3 = c(720, 360, 360),
                        dat_dir = dat_dir,
                        Land_mask = LandMask_Loop,
                        dat_dis = DistMat,
                        Sriv = Sriv[,,1],
                        SrivN = SrivN[,,1],
                        VISITro = VISITro_all[,,1201:1212],
                        VISITnl = VISITnl_all[,,1201:1212],
                        RivArea = RivArea,
                        dat_humanN = HumanN[,,30],
                        dat_tas = dat_tas[,360:1,],
                        ParFit = postMean)

sum(postout$QoutNint*dat_dir2, na.rm=TRUE)/1000/1000/1000

ggplot(data = data.frame(x = dat_he$He2011, y = postout$QoutNint[cbind(dat_he$Xex, dat_he$Yex)], rivername = dat_he$NameBasin)) +
    geom_abline(slope = 1, intercept = 0, col="gold", lwd=2) + 
    geom_point(aes(x, y/1000/1000), pch=21, fill= "slateblue2", size=3) + 
    geom_text_repel(aes(x, y/1000/1000, label = rivername)) +
    xlab("Observed DIN load [Gg-N/Year]") + ylab("Simulated DIN load [Gg-N/Year]") + 
    theme(text = element_text(size = 20), axis.text= element_text(size = 20)) +
    geom_smooth(method='lm', aes(x, y/1000/1000)) + 
    scale_x_log10(limits=c(0.1, 10000), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
    scale_y_log10(limits=c(0.1, 10000), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))

abc <- apply(postout$QoutN[,,1:360], MARGIN = c(1,2), sum, na.rm=TRUE)
summary(lm(abc[,][cbind(dat_he$Xex, dat_he$Yex)]/1000/1000 ~ dat_he$He2011))

sqrt(sum((abc[,][cbind(dat_he$Xex, dat_he$Yex)]/1000/1000 - dat_he$He2011)^2, na.rm =TRUE)/sum(!is.na(dat_he$He2011)))

