# VISIToRN (ビジターエヌ)

VISIToRN (VISIT Off-line River Nitrogen scheme) is a function that calculates nitrogen transport in rivers and denitrification processes in river channels (Nishina et al., 2021).
This function calculate N and water loadings using river routing models (e.g., TRIP model) and evaluate N2O and N2 emissions in river surface. 

Nishina, K., Ito, A., Zhou, F., Yan, X., Hayashi, S., & Winiwarter, W. (2021). 
Historical trends of riverine nitrogen loading from land to the East China Sea: a model-based evaluation. 
Environmental Research Communications, 3(8), 085005.

## Requirements

下記を参考にC++のコンパイラーをインストールする。
https://teuder.github.io/rcpp4everyone_ja/020_install.html

その後、下記のパッケージを実行環境に追加
install.packages(c("RcppArmadillo", "inline"))


## VISIToRN_func.R


VISIToRN (VISIT Off-line River Nitrogen scheme) is a function that calculates the transport of nitrogen in rivers and the denitrification process within river channels.
This file contains the definitions of the VISIToRN functions. The contents of src are written in C++, so the Rcpp package (which actually requires RcppArmadillo for using 3D arrays in C++).
Before loading with source(), please install the RcppArmadillo and inline packages in your R environment.

The return values of VISIToRN is output as a list containing multiple arrays.　It uses a significant amount of memory.


## Data_pd

1年分の計算できるデータセットを用意
VISITnl_nat_ag.rdatがVISITの計算結果による窒素負荷ですが
VISITnl_nat.rdatは、自然起源だけを抜き出したものですので
AIMで計算されたものと合算して入力するのも可です。


## Demo_VISIToRN

Data_pdのリストを読み込んで、とりあえず一年走るコードです。


## 注意点

VISIToRNはかなりメモリーを必要とするので、とりあえず一年だけの計算するものをDemoとして作成しました。
またN2OやN2などの排出は、内部では計算されていますが、出力しないようになっています。




