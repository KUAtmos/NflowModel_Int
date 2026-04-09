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

Nloss = (pars[0]*Nconc)/(pars[1] + Nconc) * exp((cubeArrayTas(l,m,t/30) - 20)/10*log(pars[2])) * RArea(l,m); // mg-N/L

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


VISIToRN = cxxfunction(signature(
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
