library("deSolve")
system("R CMD SHLIB model8.cpp")
dyn.load("model8.dll")

InvalidDataOffset <- 10000
Initial <- 1

G <- 4.7
Tinc1 <- 3
Tinc2 <- 1

a1 <- 1/Tinc1
a2 <- 1/Tinc2

gamma <- 1/((G - (Tinc1+Tinc2)) * 2)

calcGammaProfile <- function(mean, sd)
{
    shape = mean^2 / sd^2
    scale = sd^2 / mean
    
    kbegin = max(0, ceiling(mean - sd * 3))
    kend = max(kbegin + 1, floor(mean + sd * 3))

    result = NULL
    result$kbegin = -kend
    result$kend = -kbegin
    result$values = numeric(result$kend - result$kbegin)
    i = 1
    for (k in kbegin:kend) {
        result$values[i] = pgamma(k-0.5, shape=shape, scale=scale) -
            pgamma(k+0.5, shape=shape, scale=scale)
        i = i + 1
    }

    result$values = result$values / sum(result$values)
    result$values = rev(result$values)

    result
}

convolute <- function(values, i1, i2, profile)
{
 filter(values, profile$values, method="convolution", sides=1)[(i1 + profile$kend):(i2 + profile$kend)]
}


diedProfile <- calcGammaProfile


calculateModel <- function(params, period)
{

    beta0_LL_yy_ww <- params[1]
    beta0_LL_yy_mm <- params[2]
    beta0_LL_yy_wm <- params[3]
    beta0_LL_yy_mw <- params[4]
    beta0_LL_oo_ww <- params[5]
    beta0_LL_oo_mm <- params[6]
    beta0_LL_oo_wm <- params[7]
    beta0_LL_oo_mw <- params[8]
    beta0_LL_yo_ww <- params[9]
    beta0_LL_yo_mm <- params[10]
    beta0_LL_yo_wm <- params[11]
    beta0_LL_yo_mw <- params[12]
    beta0_HH_yy_ww <- params[13]
    beta0_HH_yy_mm <- params[14]
    beta0_HH_yy_wm <- params[15]
    beta0_HH_yy_mw <- params[16]
    beta0_HH_oo_ww <- params[17]
    beta0_HH_oo_mm <- params[18]
    beta0_HH_oo_wm <- params[19]
    beta0_HH_oo_mw <- params[20]
    beta0_HH_yo_ww <- params[21]
    beta0_HH_yo_mm <- params[22]
    beta0_HH_yo_wm <- params[23]
    beta0_HH_yo_mw <- params[24]
    beta0_LH_yy_ww <- params[25]
    beta0_LH_yy_mm <- params[26]
    beta0_LH_yy_wm <- params[27]
    beta0_LH_yy_mw <- params[28]
    beta0_HL_yy_ww <- params[29]
    beta0_HL_yy_mm <- params[30]
    beta0_HL_yy_wm <- params[31]
    beta0_HL_yy_mw <- params[32]
    beta1_LL_yy_ww <- params[33]
    beta1_LL_yy_mm <- params[34]
    beta1_LL_yy_wm <- params[35]
    beta1_LL_yy_mw <- params[36]
    beta1_LL_oo_ww <- params[37]
    beta1_LL_oo_mm <- params[38]
    beta1_LL_oo_wm <- params[39]
    beta1_LL_oo_mw <- params[40]
    beta1_LL_yo_ww <- params[41]
    beta1_LL_yo_mm <- params[42]
    beta1_LL_yo_wm <- params[43]
    beta1_LL_yo_mw <- params[44]
    beta1_HH_yy_ww <- params[45]
    beta1_HH_yy_mm <- params[46]
    beta1_HH_yy_wm <- params[47]
    beta1_HH_yy_mw <- params[48]
    beta1_HH_oo_ww <- params[49]
    beta1_HH_oo_mm <- params[50]
    beta1_HH_oo_wm <- params[51]
    beta1_HH_oo_mw <- params[52]
    beta1_HH_yo_ww <- params[53]
    beta1_HH_yo_mm <- params[54]
    beta1_HH_yo_wm <- params[55]
    beta1_HH_yo_mw <- params[56]
    beta1_LH_yy_ww <- params[57]
    beta1_LH_yy_mm <- params[58]
    beta1_LH_yy_wm <- params[59]
    beta1_LH_yy_mw <- params[60]
    beta1_HL_yy_ww <- params[61]
    beta1_HL_yy_mm <- params[62]
    beta1_HL_yy_wm <- params[63]
    beta1_HL_yy_mw <- params[64]
    beta2_LL_yy_ww <- params[65]
    beta2_LL_yy_mm <- params[66]
    beta2_LL_yy_wm <- params[67]
    beta2_LL_yy_mw <- params[68]
    beta2_LL_oo_ww <- params[69]
    beta2_LL_oo_mm <- params[70]
    beta2_LL_oo_wm <- params[71]
    beta2_LL_oo_mw <- params[72]
    beta2_LL_yo_ww <- params[73]
    beta2_LL_yo_mm <- params[74]
    beta2_LL_yo_wm <- params[75]
    beta2_LL_yo_mw <- params[76]
    beta2_HH_yy_ww <- params[77]
    beta2_HH_yy_mm <- params[78]
    beta2_HH_yy_wm <- params[79]
    beta2_HH_yy_mw <- params[80]
    beta2_HH_oo_ww <- params[81]
    beta2_HH_oo_mm <- params[82]
    beta2_HH_oo_wm <- params[83]
    beta2_HH_oo_mw <- params[84]
    beta2_HH_yo_ww <- params[85]
    beta2_HH_yo_mm <- params[86]
    beta2_HH_yo_wm <- params[87]
    beta2_HH_yo_mw <- params[88]
    beta2_LH_yy_ww <- params[89]
    beta2_LH_yy_mm <- params[90]
    beta2_LH_yy_wm <- params[91]
    beta2_LH_yy_mw <- params[92]
    beta2_HL_yy_ww <- params[93]
    beta2_HL_yy_mm <- params[94]
    beta2_HL_yy_wm <- params[95]
    beta2_HL_yy_mw <- params[96]
    beta3_LL_yy_ww <- params[97]
    beta3_LL_yy_mm <- params[98]
    beta3_LL_yy_wm <- params[99]
    beta3_LL_yy_mw <- params[100]
    beta3_LL_oo_ww <- params[101]
    beta3_LL_oo_mm <- params[102]
    beta3_LL_oo_wm <- params[103]
    beta3_LL_oo_mw <- params[104]
    beta3_LL_yo_ww <- params[105]
    beta3_LL_yo_mm <- params[106]
    beta3_LL_yo_wm <- params[107]
    beta3_LL_yo_mw <- params[108]
    beta3_HH_yy_ww <- params[109]
    beta3_HH_yy_mm <- params[110]
    beta3_HH_yy_wm <- params[111]
    beta3_HH_yy_mw <- params[112]
    beta3_HH_oo_ww <- params[113]
    beta3_HH_oo_mm <- params[114]
    beta3_HH_oo_wm <- params[115]
    beta3_HH_oo_mw <- params[116]
    beta3_HH_yo_ww <- params[117]
    beta3_HH_yo_mm <- params[118]
    beta3_HH_yo_wm <- params[119]
    beta3_HH_yo_mw <- params[120]
    beta3_LH_yy_ww <- params[121]
    beta3_LH_yy_mm <- params[122]
    beta3_LH_yy_wm <- params[123]
    beta3_LH_yy_mw <- params[124]
    beta3_HL_yy_ww <- params[125]
    beta3_HL_yy_mm <- params[126]
    beta3_HL_yy_wm <- params[127]
    beta3_HL_yy_mw <- params[128]
    beta4_LL_yy_ww <- params[129]
    beta4_LL_yy_mm <- params[130]
    beta4_LL_yy_wm <- params[131]
    beta4_LL_yy_mw <- params[132]
    beta4_LL_oo_ww <- params[133]
    beta4_LL_oo_mm <- params[134]
    beta4_LL_oo_wm <- params[135]
    beta4_LL_oo_mw <- params[136]
    beta4_LL_yo_ww <- params[137]
    beta4_LL_yo_mm <- params[138]
    beta4_LL_yo_wm <- params[139]
    beta4_LL_yo_mw <- params[140]
    beta4_HH_yy_ww <- params[141]
    beta4_HH_yy_mm <- params[142]
    beta4_HH_yy_wm <- params[143]
    beta4_HH_yy_mw <- params[144]
    beta4_HH_oo_ww <- params[145]
    beta4_HH_oo_mm <- params[146]
    beta4_HH_oo_wm <- params[147]
    beta4_HH_oo_mw <- params[148]
    beta4_HH_yo_ww <- params[149]
    beta4_HH_yo_mm <- params[150]
    beta4_HH_yo_wm <- params[151]
    beta4_HH_yo_mw <- params[152]
    beta4_LH_yy_ww <- params[153]
    beta4_LH_yy_mm <- params[154]
    beta4_LH_yy_wm <- params[155]
    beta4_LH_yy_mw <- params[156]
    beta4_HL_yy_ww <- params[157]
    beta4_HL_yy_mm <- params[158]
    beta4_HL_yy_wm <- params[159]
    beta4_HL_yy_mw <- params[160]
    beta5_LL_yy_ww <- params[161]
    beta5_LL_yy_mm <- params[162]
    beta5_LL_yy_wm <- params[163]
    beta5_LL_yy_mw <- params[164]
    beta5_LL_oo_ww <- params[165]
    beta5_LL_oo_mm <- params[166]
    beta5_LL_oo_wm <- params[167]
    beta5_LL_oo_mw <- params[168]
    beta5_LL_yo_ww <- params[169]
    beta5_LL_yo_mm <- params[170]
    beta5_LL_yo_wm <- params[171]
    beta5_LL_yo_mw <- params[172]
    beta5_HH_yy_ww <- params[173]
    beta5_HH_yy_mm <- params[174]
    beta5_HH_yy_wm <- params[175]
    beta5_HH_yy_mw <- params[176]
    beta5_HH_oo_ww <- params[177]
    beta5_HH_oo_mm <- params[178]
    beta5_HH_oo_wm <- params[179]
    beta5_HH_oo_mw <- params[180]
    beta5_HH_yo_ww <- params[181]
    beta5_HH_yo_mm <- params[182]
    beta5_HH_yo_wm <- params[183]
    beta5_HH_yo_mw <- params[184]
    beta5_LH_yy_ww <- params[185]
    beta5_LH_yy_mm <- params[186]
    beta5_LH_yy_wm <- params[187]
    beta5_LH_yy_mw <- params[188]
    beta5_HL_yy_ww <- params[189]
    beta5_HL_yy_mm <- params[190]
    beta5_HL_yy_wm <- params[191]
    beta5_HL_yy_mw <- params[192]
    beta6_LL_yy_ww <- params[193]
    beta6_LL_yy_mm <- params[194]
    beta6_LL_yy_wm <- params[195]
    beta6_LL_yy_mw <- params[196]
    beta6_LL_oo_ww <- params[197]
    beta6_LL_oo_mm <- params[198]
    beta6_LL_oo_wm <- params[199]
    beta6_LL_oo_mw <- params[200]
    beta6_LL_yo_ww <- params[201]
    beta6_LL_yo_mm <- params[202]
    beta6_LL_yo_wm <- params[203]
    beta6_LL_yo_mw <- params[204]
    beta6_HH_yy_ww <- params[205]
    beta6_HH_yy_mm <- params[206]
    beta6_HH_yy_wm <- params[207]
    beta6_HH_yy_mw <- params[208]
    beta6_HH_oo_ww <- params[209]
    beta6_HH_oo_mm <- params[210]
    beta6_HH_oo_wm <- params[211]
    beta6_HH_oo_mw <- params[212]
    beta6_HH_yo_ww <- params[213]
    beta6_HH_yo_mm <- params[214]
    beta6_HH_yo_wm <- params[215]
    beta6_HH_yo_mw <- params[216]
    beta6_LH_yy_ww <- params[217]
    beta6_LH_yy_mm <- params[218]
    beta6_LH_yy_wm <- params[219]
    beta6_LH_yy_mw <- params[220]
    beta6_HL_yy_ww <- params[221]
    beta6_HL_yy_mm <- params[222]
    beta6_HL_yy_wm <- params[223]
    beta6_HL_yy_mw <- params[224]
    beta7_LL_yy_ww <- params[225]
    beta7_LL_yy_mm <- params[226]
    beta7_LL_yy_wm <- params[227]
    beta7_LL_yy_mw <- params[228]
    beta7_LL_oo_ww <- params[229]
    beta7_LL_oo_mm <- params[230]
    beta7_LL_oo_wm <- params[231]
    beta7_LL_oo_mw <- params[232]
    beta7_LL_yo_ww <- params[233]
    beta7_LL_yo_mm <- params[234]
    beta7_LL_yo_wm <- params[235]
    beta7_LL_yo_mw <- params[236]
    beta7_HH_yy_ww <- params[237]
    beta7_HH_yy_mm <- params[238]
    beta7_HH_yy_wm <- params[239]
    beta7_HH_yy_mw <- params[240]
    beta7_HH_oo_ww <- params[241]
    beta7_HH_oo_mm <- params[242]
    beta7_HH_oo_wm <- params[243]
    beta7_HH_oo_mw <- params[244]
    beta7_HH_yo_ww <- params[245]
    beta7_HH_yo_mm <- params[246]
    beta7_HH_yo_wm <- params[247]
    beta7_HH_yo_mw <- params[248]
    beta7_LH_yy_ww <- params[249]
    beta7_LH_yy_mm <- params[250]
    beta7_LH_yy_wm <- params[251]
    beta7_LH_yy_mw <- params[252]
    beta7_HL_yy_ww <- params[253]
    beta7_HL_yy_mm <- params[254]
    beta7_HL_yy_wm <- params[255]
    beta7_HL_yy_mw <- params[256]
    
    beta8_LL_yy_ww <- params[257]
    beta8_LL_yy_mm <- params[258]
    beta8_LL_yy_wm <- params[259]
    beta8_LL_yy_mw <- params[260]
    beta8_LL_oo_ww <- params[261]
    beta8_LL_oo_mm <- params[262]
    beta8_LL_oo_wm <- params[263]
    beta8_LL_oo_mw <- params[264]
    beta8_LL_yo_ww <- params[265]
    beta8_LL_yo_mm <- params[266]
    beta8_LL_yo_wm <- params[267]
    beta8_LL_yo_mw <- params[268]
    beta8_HH_yy_ww <- params[269]
    beta8_HH_yy_mm <- params[270]
    beta8_HH_yy_wm <- params[271]
    beta8_HH_yy_mw <- params[272]
    beta8_HH_oo_ww <- params[273]
    beta8_HH_oo_mm <- params[274]
    beta8_HH_oo_wm <- params[275]
    beta8_HH_oo_mw <- params[276]
    beta8_HH_yo_ww <- params[277]
    beta8_HH_yo_mm <- params[278]
    beta8_HH_yo_wm <- params[279]
    beta8_HH_yo_mw <- params[280]
    beta8_LH_yy_ww <- params[281]
    beta8_LH_yy_mm <- params[282]
    beta8_LH_yy_wm <- params[283]
    beta8_LH_yy_mw <- params[284]
    beta8_HL_yy_ww <- params[285]
    beta8_HL_yy_mm <- params[286]
    beta8_HL_yy_wm <- params[287]
    beta8_HL_yy_mw <- params[288]
    
    died_latency.y <- params[289]
    died_latency.o <- params[290]
    
    died_rate_L.y_w <- params[291]
    died_rate_L.y_m <- params[292]
    died_rate_L.o_w <- params[293]
    died_rate_L.o_m <- params[294]
    
    died_rate_H.y_w <- params[295]
    died_rate_H.y_m <- params[296]
    died_rate_H.o_w <- params[297]
    died_rate_H.o_m <- params[298]
     
    t0_morts <- params[299]
    DLsd <- params[300]
     
    
    ## convolution profile to infer hospitalisation count

    died_cv_profile.y = diedProfile(died_latency.y, DLsd)
    died_cv_profile.o = diedProfile(died_latency.o, DLsd)

    
    padding = max(-died_cv_profile.y$kbegin, -died_cv_profile.o$kbegin) + 1

    state <- NULL

    state$L.S_y_w <- rep(L.N_y_w, padding + period)
    state$L.E1_y_w <- rep(0, padding + period)
    state$L.E2_y_w <- rep(0, padding + period)
    state$L.I_y_w <- rep(0, padding + period)
    state$L.R_y_w <- rep(0, padding + period)
    state$L.died_y_w <- rep(0, padding + period)
    
    state$L.S_y_m <- rep(L.N_y_m - Initial, padding + period)
    state$L.E1_y_m <- rep(Initial, padding + period)
    state$L.E2_y_m <- rep(0, padding + period)
    state$L.I_y_m <- rep(0, padding + period)
    state$L.R_y_m <- rep(0, padding + period)
    state$L.died_y_m <- rep(0, padding + period)
    
    state$L.S_o_w <- rep(L.N_o_w, padding + period)
    state$L.E1_o_w <- rep(0, padding + period)
    state$L.E2_o_w <- rep(0, padding + period)
    state$L.I_o_w <- rep(0, padding + period)
    state$L.R_o_w <- rep(0, padding + period)
    state$L.died_o_w <- rep(0, padding + period)
    
    state$L.S_o_m <- rep(L.N_o_m, padding + period)
    state$L.E1_o_m <- rep(0, padding + period)
    state$L.E2_o_m <- rep(0, padding + period)
    state$L.I_o_m <- rep(0, padding + period)
    state$L.R_o_m <- rep(0, padding + period)
    state$L.died_o_m <- rep(0, padding + period)
    
    state$H.S_y_w <- rep(H.N_y_w, padding + period)
    state$H.E1_y_w <- rep(0, padding + period)
    state$H.E2_y_w <- rep(0, padding + period)
    state$H.I_y_w <- rep(0, padding + period)
    state$H.R_y_w <- rep(0, padding + period)
    state$H.died_y_w <- rep(0, padding + period)
    
    state$H.S_y_m <- rep(H.N_y_m - Initial, padding + period)
    state$H.E1_y_m <- rep(Initial, padding + period)
    state$H.E2_y_m <- rep(0, padding + period)
    state$H.I_y_m <- rep(0, padding + period)
    state$H.R_y_m <- rep(0, padding + period)
    state$H.died_y_m <- rep(0, padding + period)
    
    state$H.S_o_w <- rep(H.N_o_w, padding + period)
    state$H.E1_o_w <- rep(0, padding + period)
    state$H.E2_o_w <- rep(0, padding + period)
    state$H.I_o_w <- rep(0, padding + period)
    state$H.R_o_w <- rep(0, padding + period)
    state$H.died_o_w <- rep(0, padding + period)
    
    state$H.S_o_m <- rep(H.N_o_m, padding + period)
    state$H.E1_o_m <- rep(0, padding + period)
    state$H.E2_o_m <- rep(0, padding + period)
    state$H.I_o_m <- rep(0, padding + period)
    state$H.R_o_m <- rep(0, padding + period)
    state$H.died_o_m <- rep(0, padding + period)
    
    state$R.LL_yy_ww<- rep(0, padding + period)
    state$R.LL_yy_mm<- rep(0, padding + period)
    state$R.LL_yy_wm<- rep(0, padding + period)
    state$R.LL_yy_mw<- rep(0, padding + period)
    state$R.LL_oo_ww<- rep(0, padding + period)
    state$R.LL_oo_mm<- rep(0, padding + period)
    state$R.LL_oo_wm<- rep(0, padding + period)
    state$R.LL_oo_mw<- rep(0, padding + period)
    state$R.LL_yo_ww<- rep(0, padding + period)
    state$R.LL_yo_mm<- rep(0, padding + period)
    state$R.LL_yo_wm<- rep(0, padding + period)
    state$R.LL_yo_mw<- rep(0, padding + period)
    state$R.HH_yy_ww<- rep(0, padding + period)
    state$R.HH_yy_mm<- rep(0, padding + period)
    state$R.HH_yy_wm<- rep(0, padding + period)
    state$R.HH_yy_mw<- rep(0, padding + period)
    state$R.HH_oo_ww<- rep(0, padding + period)
    state$R.HH_oo_mm<- rep(0, padding + period)
    state$R.HH_oo_wm<- rep(0, padding + period)
    state$R.HH_oo_mw<- rep(0, padding + period)
    state$R.HH_yo_ww<- rep(0, padding + period)
    state$R.HH_yo_mm<- rep(0, padding + period)
    state$R.HH_yo_wm<- rep(0, padding + period)
    state$R.HH_yo_mw<- rep(0, padding + period)
    state$R.LH_yy_ww<- rep(0, padding + period)
    state$R.LH_yy_mm<- rep(0, padding + period)
    state$R.LH_yy_wm<- rep(0, padding + period)
    state$R.LH_yy_mw<- rep(0, padding + period)
    state$R.HL_yy_ww<- rep(0, padding + period)
    state$R.HL_yy_mm<- rep(0, padding + period)
    state$R.HL_yy_wm<- rep(0, padding + period)
    state$R.HL_yy_mw<- rep(0, padding + period)
    
  
    state$i <- padding + 1

    parms <- c(beta0_LL_yy_ww = beta0_LL_yy_ww,
               beta0_LL_yy_mm = beta0_LL_yy_mm,
               beta0_LL_yy_wm = beta0_LL_yy_wm,
               beta0_LL_yy_mw = beta0_LL_yy_mw,
               beta0_LL_oo_ww = beta0_LL_oo_ww,
               beta0_LL_oo_mm = beta0_LL_oo_mm,
               beta0_LL_oo_wm = beta0_LL_oo_wm,
               beta0_LL_oo_mw = beta0_LL_oo_mw,
               beta0_LL_yo_ww = beta0_LL_yo_ww,
               beta0_LL_yo_mm = beta0_LL_yo_mm,
               beta0_LL_yo_wm = beta0_LL_yo_wm,
               beta0_LL_yo_mw = beta0_LL_yo_mw,
               beta0_HH_yy_ww = beta0_HH_yy_ww,
               beta0_HH_yy_mm = beta0_HH_yy_mm,
               beta0_HH_yy_wm = beta0_HH_yy_wm,
               beta0_HH_yy_mw = beta0_HH_yy_mw,
               beta0_HH_oo_ww = beta0_HH_oo_ww,
               beta0_HH_oo_mm = beta0_HH_oo_mm,
               beta0_HH_oo_wm = beta0_HH_oo_wm,
               beta0_HH_oo_mw = beta0_HH_oo_mw,
               beta0_HH_yo_ww = beta0_HH_yo_ww,
               beta0_HH_yo_mm = beta0_HH_yo_mm,
               beta0_HH_yo_wm = beta0_HH_yo_wm,
               beta0_HH_yo_mw = beta0_HH_yo_mw,
               beta0_LH_yy_ww = beta0_LH_yy_ww,
               beta0_LH_yy_mm = beta0_LH_yy_mm,
               beta0_LH_yy_wm = beta0_LH_yy_wm,
               beta0_LH_yy_mw = beta0_LH_yy_mw,
               beta0_HL_yy_ww = beta0_HL_yy_ww,
               beta0_HL_yy_mm = beta0_HL_yy_mm,
               beta0_HL_yy_wm = beta0_HL_yy_wm,
               beta0_HL_yy_mw = beta0_HL_yy_mw,
               beta1_LL_yy_ww = beta1_LL_yy_ww,
               beta1_LL_yy_mm = beta1_LL_yy_mm,
               beta1_LL_yy_wm = beta1_LL_yy_wm,
               beta1_LL_yy_mw = beta1_LL_yy_mw,
               beta1_LL_oo_ww = beta1_LL_oo_ww,
               beta1_LL_oo_mm = beta1_LL_oo_mm,
               beta1_LL_oo_wm = beta1_LL_oo_wm,
               beta1_LL_oo_mw = beta1_LL_oo_mw,
               beta1_LL_yo_ww = beta1_LL_yo_ww,
               beta1_LL_yo_mm = beta1_LL_yo_mm,
               beta1_LL_yo_wm = beta1_LL_yo_wm,
               beta1_LL_yo_mw = beta1_LL_yo_mw,
               beta1_HH_yy_ww = beta1_HH_yy_ww,
               beta1_HH_yy_mm = beta1_HH_yy_mm,
               beta1_HH_yy_wm = beta1_HH_yy_wm,
               beta1_HH_yy_mw = beta1_HH_yy_mw,
               beta1_HH_oo_ww = beta1_HH_oo_ww,
               beta1_HH_oo_mm = beta1_HH_oo_mm,
               beta1_HH_oo_wm = beta1_HH_oo_wm,
               beta1_HH_oo_mw = beta1_HH_oo_mw,
               beta1_HH_yo_ww = beta1_HH_yo_ww,
               beta1_HH_yo_mm = beta1_HH_yo_mm,
               beta1_HH_yo_wm = beta1_HH_yo_wm,
               beta1_HH_yo_mw = beta1_HH_yo_mw,
               beta1_LH_yy_ww = beta1_LH_yy_ww,
               beta1_LH_yy_mm = beta1_LH_yy_mm,
               beta1_LH_yy_wm = beta1_LH_yy_wm,
               beta1_LH_yy_mw = beta1_LH_yy_mw,
               beta1_HL_yy_ww = beta1_HL_yy_ww,
               beta1_HL_yy_mm = beta1_HL_yy_mm,
               beta1_HL_yy_wm = beta1_HL_yy_wm,
               beta1_HL_yy_mw = beta1_HL_yy_mw,
               beta2_LL_yy_ww = beta2_LL_yy_ww,
               beta2_LL_yy_mm = beta2_LL_yy_mm,
               beta2_LL_yy_wm = beta2_LL_yy_wm,
               beta2_LL_yy_mw = beta2_LL_yy_mw,
               beta2_LL_oo_ww = beta2_LL_oo_ww,
               beta2_LL_oo_mm = beta2_LL_oo_mm,
               beta2_LL_oo_wm = beta2_LL_oo_wm,
               beta2_LL_oo_mw = beta2_LL_oo_mw,
               beta2_LL_yo_ww = beta2_LL_yo_ww,
               beta2_LL_yo_mm = beta2_LL_yo_mm,
               beta2_LL_yo_wm = beta2_LL_yo_wm,
               beta2_LL_yo_mw = beta2_LL_yo_mw,
               beta2_HH_yy_ww = beta2_HH_yy_ww,
               beta2_HH_yy_mm = beta2_HH_yy_mm,
               beta2_HH_yy_wm = beta2_HH_yy_wm,
               beta2_HH_yy_mw = beta2_HH_yy_mw,
               beta2_HH_oo_ww = beta2_HH_oo_ww,
               beta2_HH_oo_mm = beta2_HH_oo_mm,
               beta2_HH_oo_wm = beta2_HH_oo_wm,
               beta2_HH_oo_mw = beta2_HH_oo_mw,
               beta2_HH_yo_ww = beta2_HH_yo_ww,
               beta2_HH_yo_mm = beta2_HH_yo_mm,
               beta2_HH_yo_wm = beta2_HH_yo_wm,
               beta2_HH_yo_mw = beta2_HH_yo_mw,
               beta2_LH_yy_ww = beta2_LH_yy_ww,
               beta2_LH_yy_mm = beta2_LH_yy_mm,
               beta2_LH_yy_wm = beta2_LH_yy_wm,
               beta2_LH_yy_mw = beta2_LH_yy_mw,
               beta2_HL_yy_ww = beta2_HL_yy_ww,
               beta2_HL_yy_mm = beta2_HL_yy_mm,
               beta2_HL_yy_wm = beta2_HL_yy_wm,
               beta2_HL_yy_mw = beta2_HL_yy_mw,
               beta3_LL_yy_ww = beta3_LL_yy_ww,
               beta3_LL_yy_mm = beta3_LL_yy_mm,
               beta3_LL_yy_wm = beta3_LL_yy_wm,
               beta3_LL_yy_mw = beta3_LL_yy_mw,
               beta3_LL_oo_ww = beta3_LL_oo_ww,
               beta3_LL_oo_mm = beta3_LL_oo_mm,
               beta3_LL_oo_wm = beta3_LL_oo_wm,
               beta3_LL_oo_mw = beta3_LL_oo_mw,
               beta3_LL_yo_ww = beta3_LL_yo_ww,
               beta3_LL_yo_mm = beta3_LL_yo_mm,
               beta3_LL_yo_wm = beta3_LL_yo_wm,
               beta3_LL_yo_mw = beta3_LL_yo_mw,
               beta3_HH_yy_ww = beta3_HH_yy_ww,
               beta3_HH_yy_mm = beta3_HH_yy_mm,
               beta3_HH_yy_wm = beta3_HH_yy_wm,
               beta3_HH_yy_mw = beta3_HH_yy_mw,
               beta3_HH_oo_ww = beta3_HH_oo_ww,
               beta3_HH_oo_mm = beta3_HH_oo_mm,
               beta3_HH_oo_wm = beta3_HH_oo_wm,
               beta3_HH_oo_mw = beta3_HH_oo_mw,
               beta3_HH_yo_ww = beta3_HH_yo_ww,
               beta3_HH_yo_mm = beta3_HH_yo_mm,
               beta3_HH_yo_wm = beta3_HH_yo_wm,
               beta3_HH_yo_mw = beta3_HH_yo_mw,
               beta3_LH_yy_ww = beta3_LH_yy_ww,
               beta3_LH_yy_mm = beta3_LH_yy_mm,
               beta3_LH_yy_wm = beta3_LH_yy_wm,
               beta3_LH_yy_mw = beta3_LH_yy_mw,
               beta3_HL_yy_ww = beta3_HL_yy_ww,
               beta3_HL_yy_mm = beta3_HL_yy_mm,
               beta3_HL_yy_wm = beta3_HL_yy_wm,
               beta3_HL_yy_mw = beta3_HL_yy_mw,
               beta4_LL_yy_ww = beta4_LL_yy_ww,
               beta4_LL_yy_mm = beta4_LL_yy_mm,
               beta4_LL_yy_wm = beta4_LL_yy_wm,
               beta4_LL_yy_mw = beta4_LL_yy_mw,
               beta4_LL_oo_ww = beta4_LL_oo_ww,
               beta4_LL_oo_mm = beta4_LL_oo_mm,
               beta4_LL_oo_wm = beta4_LL_oo_wm,
               beta4_LL_oo_mw = beta4_LL_oo_mw,
               beta4_LL_yo_ww = beta4_LL_yo_ww,
               beta4_LL_yo_mm = beta4_LL_yo_mm,
               beta4_LL_yo_wm = beta4_LL_yo_wm,
               beta4_LL_yo_mw = beta4_LL_yo_mw,
               beta4_HH_yy_ww = beta4_HH_yy_ww,
               beta4_HH_yy_mm = beta4_HH_yy_mm,
               beta4_HH_yy_wm = beta4_HH_yy_wm,
               beta4_HH_yy_mw = beta4_HH_yy_mw,
               beta4_HH_oo_ww = beta4_HH_oo_ww,
               beta4_HH_oo_mm = beta4_HH_oo_mm,
               beta4_HH_oo_wm = beta4_HH_oo_wm,
               beta4_HH_oo_mw = beta4_HH_oo_mw,
               beta4_HH_yo_ww = beta4_HH_yo_ww,
               beta4_HH_yo_mm = beta4_HH_yo_mm,
               beta4_HH_yo_wm = beta4_HH_yo_wm,
               beta4_HH_yo_mw = beta4_HH_yo_mw,
               beta4_LH_yy_ww = beta4_LH_yy_ww,
               beta4_LH_yy_mm = beta4_LH_yy_mm,
               beta4_LH_yy_wm = beta4_LH_yy_wm,
               beta4_LH_yy_mw = beta4_LH_yy_mw,
               beta4_HL_yy_ww = beta4_HL_yy_ww,
               beta4_HL_yy_mm = beta4_HL_yy_mm,
               beta4_HL_yy_wm = beta4_HL_yy_wm,
               beta4_HL_yy_mw = beta4_HL_yy_mw,
               beta5_LL_yy_ww = beta5_LL_yy_ww,
               beta5_LL_yy_mm = beta5_LL_yy_mm,
               beta5_LL_yy_wm = beta5_LL_yy_wm,
               beta5_LL_yy_mw = beta5_LL_yy_mw,
               beta5_LL_oo_ww = beta5_LL_oo_ww,
               beta5_LL_oo_mm = beta5_LL_oo_mm,
               beta5_LL_oo_wm = beta5_LL_oo_wm,
               beta5_LL_oo_mw = beta5_LL_oo_mw,
               beta5_LL_yo_ww = beta5_LL_yo_ww,
               beta5_LL_yo_mm = beta5_LL_yo_mm,
               beta5_LL_yo_wm = beta5_LL_yo_wm,
               beta5_LL_yo_mw = beta5_LL_yo_mw,
               beta5_HH_yy_ww = beta5_HH_yy_ww,
               beta5_HH_yy_mm = beta5_HH_yy_mm,
               beta5_HH_yy_wm = beta5_HH_yy_wm,
               beta5_HH_yy_mw = beta5_HH_yy_mw,
               beta5_HH_oo_ww = beta5_HH_oo_ww,
               beta5_HH_oo_mm = beta5_HH_oo_mm,
               beta5_HH_oo_wm = beta5_HH_oo_wm,
               beta5_HH_oo_mw = beta5_HH_oo_mw,
               beta5_HH_yo_ww = beta5_HH_yo_ww,
               beta5_HH_yo_mm = beta5_HH_yo_mm,
               beta5_HH_yo_wm = beta5_HH_yo_wm,
               beta5_HH_yo_mw = beta5_HH_yo_mw,
               beta5_LH_yy_ww = beta5_LH_yy_ww,
               beta5_LH_yy_mm = beta5_LH_yy_mm,
               beta5_LH_yy_wm = beta5_LH_yy_wm,
               beta5_LH_yy_mw = beta5_LH_yy_mw,
               beta5_HL_yy_ww = beta5_HL_yy_ww,
               beta5_HL_yy_mm = beta5_HL_yy_mm,
               beta5_HL_yy_wm = beta5_HL_yy_wm,
               beta5_HL_yy_mw = beta5_HL_yy_mw,
               beta6_LL_yy_ww = beta6_LL_yy_ww,
               beta6_LL_yy_mm = beta6_LL_yy_mm,
               beta6_LL_yy_wm = beta6_LL_yy_wm,
               beta6_LL_yy_mw = beta6_LL_yy_mw,
               beta6_LL_oo_ww = beta6_LL_oo_ww,
               beta6_LL_oo_mm = beta6_LL_oo_mm,
               beta6_LL_oo_wm = beta6_LL_oo_wm,
               beta6_LL_oo_mw = beta6_LL_oo_mw,
               beta6_LL_yo_ww = beta6_LL_yo_ww,
               beta6_LL_yo_mm = beta6_LL_yo_mm,
               beta6_LL_yo_wm = beta6_LL_yo_wm,
               beta6_LL_yo_mw = beta6_LL_yo_mw,
               beta6_HH_yy_ww = beta6_HH_yy_ww,
               beta6_HH_yy_mm = beta6_HH_yy_mm,
               beta6_HH_yy_wm = beta6_HH_yy_wm,
               beta6_HH_yy_mw = beta6_HH_yy_mw,
               beta6_HH_oo_ww = beta6_HH_oo_ww,
               beta6_HH_oo_mm = beta6_HH_oo_mm,
               beta6_HH_oo_wm = beta6_HH_oo_wm,
               beta6_HH_oo_mw = beta6_HH_oo_mw,
               beta6_HH_yo_ww = beta6_HH_yo_ww,
               beta6_HH_yo_mm = beta6_HH_yo_mm,
               beta6_HH_yo_wm = beta6_HH_yo_wm,
               beta6_HH_yo_mw = beta6_HH_yo_mw,
               beta6_LH_yy_ww = beta6_LH_yy_ww,
               beta6_LH_yy_mm = beta6_LH_yy_mm,
               beta6_LH_yy_wm = beta6_LH_yy_wm,
               beta6_LH_yy_mw = beta6_LH_yy_mw,
               beta6_HL_yy_ww = beta6_HL_yy_ww,
               beta6_HL_yy_mm = beta6_HL_yy_mm,
               beta6_HL_yy_wm = beta6_HL_yy_wm,
               beta6_HL_yy_mw = beta6_HL_yy_mw,
               beta7_LL_yy_ww = beta7_LL_yy_ww,
               beta7_LL_yy_mm = beta7_LL_yy_mm,
               beta7_LL_yy_wm = beta7_LL_yy_wm,
               beta7_LL_yy_mw = beta7_LL_yy_mw,
               beta7_LL_oo_ww = beta7_LL_oo_ww,
               beta7_LL_oo_mm = beta7_LL_oo_mm,
               beta7_LL_oo_wm = beta7_LL_oo_wm,
               beta7_LL_oo_mw = beta7_LL_oo_mw,
               beta7_LL_yo_ww = beta7_LL_yo_ww,
               beta7_LL_yo_mm = beta7_LL_yo_mm,
               beta7_LL_yo_wm = beta7_LL_yo_wm,
               beta7_LL_yo_mw = beta7_LL_yo_mw,
               beta7_HH_yy_ww = beta7_HH_yy_ww,
               beta7_HH_yy_mm = beta7_HH_yy_mm,
               beta7_HH_yy_wm = beta7_HH_yy_wm,
               beta7_HH_yy_mw = beta7_HH_yy_mw,
               beta7_HH_oo_ww = beta7_HH_oo_ww,
               beta7_HH_oo_mm = beta7_HH_oo_mm,
               beta7_HH_oo_wm = beta7_HH_oo_wm,
               beta7_HH_oo_mw = beta7_HH_oo_mw,
               beta7_HH_yo_ww = beta7_HH_yo_ww,
               beta7_HH_yo_mm = beta7_HH_yo_mm,
               beta7_HH_yo_wm = beta7_HH_yo_wm,
               beta7_HH_yo_mw = beta7_HH_yo_mw,
               beta7_LH_yy_ww = beta7_LH_yy_ww,
               beta7_LH_yy_mm = beta7_LH_yy_mm,
               beta7_LH_yy_wm = beta7_LH_yy_wm,
               beta7_LH_yy_mw = beta7_LH_yy_mw,
               beta7_HL_yy_ww = beta7_HL_yy_ww,
               beta7_HL_yy_mm = beta7_HL_yy_mm,
               beta7_HL_yy_wm = beta7_HL_yy_wm,
               beta7_HL_yy_mw = beta7_HL_yy_mw,
               beta8_LL_yy_ww = beta8_LL_yy_ww,
               beta8_LL_yy_mm = beta8_LL_yy_mm,
               beta8_LL_yy_wm = beta8_LL_yy_wm,
               beta8_LL_yy_mw = beta8_LL_yy_mw,
               beta8_LL_oo_ww = beta8_LL_oo_ww,
               beta8_LL_oo_mm = beta8_LL_oo_mm,
               beta8_LL_oo_wm = beta8_LL_oo_wm,
               beta8_LL_oo_mw = beta8_LL_oo_mw,
               beta8_LL_yo_ww = beta8_LL_yo_ww,
               beta8_LL_yo_mm = beta8_LL_yo_mm,
               beta8_LL_yo_wm = beta8_LL_yo_wm,
               beta8_LL_yo_mw = beta8_LL_yo_mw,
               beta8_HH_yy_ww = beta8_HH_yy_ww,
               beta8_HH_yy_mm = beta8_HH_yy_mm,
               beta8_HH_yy_wm = beta8_HH_yy_wm,
               beta8_HH_yy_mw = beta8_HH_yy_mw,
               beta8_HH_oo_ww = beta8_HH_oo_ww,
               beta8_HH_oo_mm = beta8_HH_oo_mm,
               beta8_HH_oo_wm = beta8_HH_oo_wm,
               beta8_HH_oo_mw = beta8_HH_oo_mw,
               beta8_HH_yo_ww = beta8_HH_yo_ww,
               beta8_HH_yo_mm = beta8_HH_yo_mm,
               beta8_HH_yo_wm = beta8_HH_yo_wm,
               beta8_HH_yo_mw = beta8_HH_yo_mw,
               beta8_LH_yy_ww = beta8_LH_yy_ww,
               beta8_LH_yy_mm = beta8_LH_yy_mm,
               beta8_LH_yy_wm = beta8_LH_yy_wm,
               beta8_LH_yy_mw = beta8_LH_yy_mw,
               beta8_HL_yy_ww = beta8_HL_yy_ww,
               beta8_HL_yy_mm = beta8_HL_yy_mm,
               beta8_HL_yy_wm = beta8_HL_yy_wm,
               beta8_HL_yy_mw = beta8_HL_yy_mw,
               
               N_L_y_w = L.N_y_w,
               N_L_y_m = L.N_y_m,
               N_L_o_w = L.N_o_w,
               N_L_o_m = L.N_o_m,
               N_H_y_w = H.N_y_w,
               N_H_y_m = H.N_y_m,
               N_H_o_w = H.N_o_w,
               N_H_o_m = H.N_o_m,
              
               t0 = 1E10,
               t1 = 1E10,
               t2 = 1E10,
               t3 = 1E10,
               t4 = 1E10,
               t5 = 1E10,
               t6 = 1E10,
               t7 = 1E10,
               t8 = 1E10,
               a1 = a1,
               a2 = a2,
               gamma = gamma
    )

    Y <- c(S_L_y_w = L.N_y_w, E1_L_y_w = 0, E2_L_y_w = 0, I_L_y_w = 0, R_L_y_w = 0,
           S_L_y_m = L.N_y_m-Initial, E1_L_y_m = Initial, E2_L_y_m = 0, I_L_y_m = 0, R_L_y_m = 0,
           S_L_o_w = L.N_o_w, E1_L_o_w = 0, E2_L_o_w = 0, I_L_o_w = 0, R_L_o_w = 0,
           S_L_o_m = L.N_o_m, E1_L_o_m = 0, E2_L_o_m = 0, I_L_o_m = 0, R_L_o_m = 0,
           S_H_y_w = H.N_y_w, E1_H_y_w = 0, E2_H_y_w = 0, I_H_y_w = 0, R_H_y_w = 0,
           S_H_y_m = H.N_y_m-Initial, E1_H_y_m = Initial, E2_H_y_m = 0, I_H_y_m = 0, R_H_y_m = 0,
           S_H_o_w = H.N_o_w, E1_H_o_w = 0, E2_H_o_w = 0, I_H_o_w = 0, R_H_o_w = 0,
           S_H_o_m = H.N_o_m, E1_H_o_m = 0, E2_H_o_m = 0, I_H_o_m = 0, R_H_o_m = 0)

    times <- (padding + 1):(padding + period)

    out <- ode(Y, times, func = "derivs", parms = parms,
               dllname = "model8",
               initfunc = "initmod", nout = 34)

    state$L.S_y_w[(padding + 1):(padding + period)] = out[,2]
    state$L.S_y_m[(padding + 1):(padding + period)] = out[,7]
    state$L.S_o_w[(padding + 1):(padding + period)] = out[,12]
    state$L.S_o_m[(padding + 1):(padding + period)] = out[,17]
    state$H.S_y_w[(padding + 1):(padding + period)] = out[,22]
    state$H.S_y_m[(padding + 1):(padding + period)] = out[,27]
    state$H.S_o_w[(padding + 1):(padding + period)] = out[,32]
    state$H.S_o_m[(padding + 1):(padding + period)] = out[,37]
    
    s2 <- convolute(state$L.S_y_w, padding + 1, padding + period, died_cv_profile.y)
    state$L.died_y_w[(padding + 1):(padding + period)] = (L.N_y_w - s2) * died_rate_L.y_w
    
    s2 <- convolute(state$L.S_y_m, padding + 1, padding + period, died_cv_profile.y)
    state$L.died_y_m[(padding + 1):(padding + period)] = (L.N_y_m - s2) * died_rate_L.y_m
    
    s2 <- convolute(state$L.S_o_w, padding + 1, padding + period, died_cv_profile.o)
    state$L.died_o_w[(padding + 1):(padding + period)] = (L.N_o_w - s2) * died_rate_L.o_w
    
    s2 <- convolute(state$L.S_o_m, padding + 1, padding + period, died_cv_profile.o)
    state$L.died_o_m[(padding + 1):(padding + period)] = (L.N_o_m - s2) * died_rate_L.o_m
    
    s2 <- convolute(state$H.S_y_w, padding + 1, padding + period, died_cv_profile.y)
    state$H.died_y_w[(padding + 1):(padding + period)] = (H.N_y_w - s2) * died_rate_L.y_w
    
    s2 <- convolute(state$H.S_y_m, padding + 1, padding + period, died_cv_profile.y)
    state$H.died_y_m[(padding + 1):(padding + period)] = (H.N_y_m - s2) * died_rate_L.y_m
    
    s2 <- convolute(state$H.S_o_w, padding + 1, padding + period, died_cv_profile.o)
    state$H.died_o_w[(padding + 1):(padding + period)] = (H.N_o_w - s2) * died_rate_L.o_w
    
    s2 <- convolute(state$H.S_o_m, padding + 1, padding + period, died_cv_profile.o)
    state$H.died_o_m[(padding + 1):(padding + period)] = (H.N_o_m - s2) * died_rate_L.o_m
    
    state$died = state$L.died_y_w + state$L.died_y_m + state$L.died_o_w + state$L.died_o_m + 
                  state$H.died_y_w + state$H.died_y_m + state$H.died_o_w + state$H.died_o_m
      
    data_offset = InvalidDataOffset

    lds <- which(state$died > t0_morts)
    if (length(lds) > 0) {
        data_offset <- lds[1] - lockdown_offset

        t0 <- data_offset + lockdown_offset
        t1 <- data_offset + lockdown_offset + lockdown_transition_period
        t2 <- data_offset + point2_offset
        t3 <- data_offset + point3_offset
        t4 <- data_offset + point4_offset
        t5 <- data_offset + point5_offset
        t6 <- data_offset + point6_offset
        t7 <- data_offset + point7_offset
        t8 <- 4E2
        
        parms <- c(beta0_LL_yy_ww = beta0_LL_yy_ww,
                   beta0_LL_yy_mm = beta0_LL_yy_mm,
                   beta0_LL_yy_wm = beta0_LL_yy_wm,
                   beta0_LL_yy_mw = beta0_LL_yy_mw,
                   beta0_LL_oo_ww = beta0_LL_oo_ww,
                   beta0_LL_oo_mm = beta0_LL_oo_mm,
                   beta0_LL_oo_wm = beta0_LL_oo_wm,
                   beta0_LL_oo_mw = beta0_LL_oo_mw,
                   beta0_LL_yo_ww = beta0_LL_yo_ww,
                   beta0_LL_yo_mm = beta0_LL_yo_mm,
                   beta0_LL_yo_wm = beta0_LL_yo_wm,
                   beta0_LL_yo_mw = beta0_LL_yo_mw,
                   beta0_HH_yy_ww = beta0_HH_yy_ww,
                   beta0_HH_yy_mm = beta0_HH_yy_mm,
                   beta0_HH_yy_wm = beta0_HH_yy_wm,
                   beta0_HH_yy_mw = beta0_HH_yy_mw,
                   beta0_HH_oo_ww = beta0_HH_oo_ww,
                   beta0_HH_oo_mm = beta0_HH_oo_mm,
                   beta0_HH_oo_wm = beta0_HH_oo_wm,
                   beta0_HH_oo_mw = beta0_HH_oo_mw,
                   beta0_HH_yo_ww = beta0_HH_yo_ww,
                   beta0_HH_yo_mm = beta0_HH_yo_mm,
                   beta0_HH_yo_wm = beta0_HH_yo_wm,
                   beta0_HH_yo_mw = beta0_HH_yo_mw,
                   beta0_LH_yy_ww = beta0_LH_yy_ww,
                   beta0_LH_yy_mm = beta0_LH_yy_mm,
                   beta0_LH_yy_wm = beta0_LH_yy_wm,
                   beta0_LH_yy_mw = beta0_LH_yy_mw,
                   beta0_HL_yy_ww = beta0_HL_yy_ww,
                   beta0_HL_yy_mm = beta0_HL_yy_mm,
                   beta0_HL_yy_wm = beta0_HL_yy_wm,
                   beta0_HL_yy_mw = beta0_HL_yy_mw,
                   beta1_LL_yy_ww = beta1_LL_yy_ww,
                   beta1_LL_yy_mm = beta1_LL_yy_mm,
                   beta1_LL_yy_wm = beta1_LL_yy_wm,
                   beta1_LL_yy_mw = beta1_LL_yy_mw,
                   beta1_LL_oo_ww = beta1_LL_oo_ww,
                   beta1_LL_oo_mm = beta1_LL_oo_mm,
                   beta1_LL_oo_wm = beta1_LL_oo_wm,
                   beta1_LL_oo_mw = beta1_LL_oo_mw,
                   beta1_LL_yo_ww = beta1_LL_yo_ww,
                   beta1_LL_yo_mm = beta1_LL_yo_mm,
                   beta1_LL_yo_wm = beta1_LL_yo_wm,
                   beta1_LL_yo_mw = beta1_LL_yo_mw,
                   beta1_HH_yy_ww = beta1_HH_yy_ww,
                   beta1_HH_yy_mm = beta1_HH_yy_mm,
                   beta1_HH_yy_wm = beta1_HH_yy_wm,
                   beta1_HH_yy_mw = beta1_HH_yy_mw,
                   beta1_HH_oo_ww = beta1_HH_oo_ww,
                   beta1_HH_oo_mm = beta1_HH_oo_mm,
                   beta1_HH_oo_wm = beta1_HH_oo_wm,
                   beta1_HH_oo_mw = beta1_HH_oo_mw,
                   beta1_HH_yo_ww = beta1_HH_yo_ww,
                   beta1_HH_yo_mm = beta1_HH_yo_mm,
                   beta1_HH_yo_wm = beta1_HH_yo_wm,
                   beta1_HH_yo_mw = beta1_HH_yo_mw,
                   beta1_LH_yy_ww = beta1_LH_yy_ww,
                   beta1_LH_yy_mm = beta1_LH_yy_mm,
                   beta1_LH_yy_wm = beta1_LH_yy_wm,
                   beta1_LH_yy_mw = beta1_LH_yy_mw,
                   beta1_HL_yy_ww = beta1_HL_yy_ww,
                   beta1_HL_yy_mm = beta1_HL_yy_mm,
                   beta1_HL_yy_wm = beta1_HL_yy_wm,
                   beta1_HL_yy_mw = beta1_HL_yy_mw,
                   beta2_LL_yy_ww = beta2_LL_yy_ww,
                   beta2_LL_yy_mm = beta2_LL_yy_mm,
                   beta2_LL_yy_wm = beta2_LL_yy_wm,
                   beta2_LL_yy_mw = beta2_LL_yy_mw,
                   beta2_LL_oo_ww = beta2_LL_oo_ww,
                   beta2_LL_oo_mm = beta2_LL_oo_mm,
                   beta2_LL_oo_wm = beta2_LL_oo_wm,
                   beta2_LL_oo_mw = beta2_LL_oo_mw,
                   beta2_LL_yo_ww = beta2_LL_yo_ww,
                   beta2_LL_yo_mm = beta2_LL_yo_mm,
                   beta2_LL_yo_wm = beta2_LL_yo_wm,
                   beta2_LL_yo_mw = beta2_LL_yo_mw,
                   beta2_HH_yy_ww = beta2_HH_yy_ww,
                   beta2_HH_yy_mm = beta2_HH_yy_mm,
                   beta2_HH_yy_wm = beta2_HH_yy_wm,
                   beta2_HH_yy_mw = beta2_HH_yy_mw,
                   beta2_HH_oo_ww = beta2_HH_oo_ww,
                   beta2_HH_oo_mm = beta2_HH_oo_mm,
                   beta2_HH_oo_wm = beta2_HH_oo_wm,
                   beta2_HH_oo_mw = beta2_HH_oo_mw,
                   beta2_HH_yo_ww = beta2_HH_yo_ww,
                   beta2_HH_yo_mm = beta2_HH_yo_mm,
                   beta2_HH_yo_wm = beta2_HH_yo_wm,
                   beta2_HH_yo_mw = beta2_HH_yo_mw,
                   beta2_LH_yy_ww = beta2_LH_yy_ww,
                   beta2_LH_yy_mm = beta2_LH_yy_mm,
                   beta2_LH_yy_wm = beta2_LH_yy_wm,
                   beta2_LH_yy_mw = beta2_LH_yy_mw,
                   beta2_HL_yy_ww = beta2_HL_yy_ww,
                   beta2_HL_yy_mm = beta2_HL_yy_mm,
                   beta2_HL_yy_wm = beta2_HL_yy_wm,
                   beta2_HL_yy_mw = beta2_HL_yy_mw,
                   beta3_LL_yy_ww = beta3_LL_yy_ww,
                   beta3_LL_yy_mm = beta3_LL_yy_mm,
                   beta3_LL_yy_wm = beta3_LL_yy_wm,
                   beta3_LL_yy_mw = beta3_LL_yy_mw,
                   beta3_LL_oo_ww = beta3_LL_oo_ww,
                   beta3_LL_oo_mm = beta3_LL_oo_mm,
                   beta3_LL_oo_wm = beta3_LL_oo_wm,
                   beta3_LL_oo_mw = beta3_LL_oo_mw,
                   beta3_LL_yo_ww = beta3_LL_yo_ww,
                   beta3_LL_yo_mm = beta3_LL_yo_mm,
                   beta3_LL_yo_wm = beta3_LL_yo_wm,
                   beta3_LL_yo_mw = beta3_LL_yo_mw,
                   beta3_HH_yy_ww = beta3_HH_yy_ww,
                   beta3_HH_yy_mm = beta3_HH_yy_mm,
                   beta3_HH_yy_wm = beta3_HH_yy_wm,
                   beta3_HH_yy_mw = beta3_HH_yy_mw,
                   beta3_HH_oo_ww = beta3_HH_oo_ww,
                   beta3_HH_oo_mm = beta3_HH_oo_mm,
                   beta3_HH_oo_wm = beta3_HH_oo_wm,
                   beta3_HH_oo_mw = beta3_HH_oo_mw,
                   beta3_HH_yo_ww = beta3_HH_yo_ww,
                   beta3_HH_yo_mm = beta3_HH_yo_mm,
                   beta3_HH_yo_wm = beta3_HH_yo_wm,
                   beta3_HH_yo_mw = beta3_HH_yo_mw,
                   beta3_LH_yy_ww = beta3_LH_yy_ww,
                   beta3_LH_yy_mm = beta3_LH_yy_mm,
                   beta3_LH_yy_wm = beta3_LH_yy_wm,
                   beta3_LH_yy_mw = beta3_LH_yy_mw,
                   beta3_HL_yy_ww = beta3_HL_yy_ww,
                   beta3_HL_yy_mm = beta3_HL_yy_mm,
                   beta3_HL_yy_wm = beta3_HL_yy_wm,
                   beta3_HL_yy_mw = beta3_HL_yy_mw,
                   beta4_LL_yy_ww = beta4_LL_yy_ww,
                   beta4_LL_yy_mm = beta4_LL_yy_mm,
                   beta4_LL_yy_wm = beta4_LL_yy_wm,
                   beta4_LL_yy_mw = beta4_LL_yy_mw,
                   beta4_LL_oo_ww = beta4_LL_oo_ww,
                   beta4_LL_oo_mm = beta4_LL_oo_mm,
                   beta4_LL_oo_wm = beta4_LL_oo_wm,
                   beta4_LL_oo_mw = beta4_LL_oo_mw,
                   beta4_LL_yo_ww = beta4_LL_yo_ww,
                   beta4_LL_yo_mm = beta4_LL_yo_mm,
                   beta4_LL_yo_wm = beta4_LL_yo_wm,
                   beta4_LL_yo_mw = beta4_LL_yo_mw,
                   beta4_HH_yy_ww = beta4_HH_yy_ww,
                   beta4_HH_yy_mm = beta4_HH_yy_mm,
                   beta4_HH_yy_wm = beta4_HH_yy_wm,
                   beta4_HH_yy_mw = beta4_HH_yy_mw,
                   beta4_HH_oo_ww = beta4_HH_oo_ww,
                   beta4_HH_oo_mm = beta4_HH_oo_mm,
                   beta4_HH_oo_wm = beta4_HH_oo_wm,
                   beta4_HH_oo_mw = beta4_HH_oo_mw,
                   beta4_HH_yo_ww = beta4_HH_yo_ww,
                   beta4_HH_yo_mm = beta4_HH_yo_mm,
                   beta4_HH_yo_wm = beta4_HH_yo_wm,
                   beta4_HH_yo_mw = beta4_HH_yo_mw,
                   beta4_LH_yy_ww = beta4_LH_yy_ww,
                   beta4_LH_yy_mm = beta4_LH_yy_mm,
                   beta4_LH_yy_wm = beta4_LH_yy_wm,
                   beta4_LH_yy_mw = beta4_LH_yy_mw,
                   beta4_HL_yy_ww = beta4_HL_yy_ww,
                   beta4_HL_yy_mm = beta4_HL_yy_mm,
                   beta4_HL_yy_wm = beta4_HL_yy_wm,
                   beta4_HL_yy_mw = beta4_HL_yy_mw,
                   beta5_LL_yy_ww = beta5_LL_yy_ww,
                   beta5_LL_yy_mm = beta5_LL_yy_mm,
                   beta5_LL_yy_wm = beta5_LL_yy_wm,
                   beta5_LL_yy_mw = beta5_LL_yy_mw,
                   beta5_LL_oo_ww = beta5_LL_oo_ww,
                   beta5_LL_oo_mm = beta5_LL_oo_mm,
                   beta5_LL_oo_wm = beta5_LL_oo_wm,
                   beta5_LL_oo_mw = beta5_LL_oo_mw,
                   beta5_LL_yo_ww = beta5_LL_yo_ww,
                   beta5_LL_yo_mm = beta5_LL_yo_mm,
                   beta5_LL_yo_wm = beta5_LL_yo_wm,
                   beta5_LL_yo_mw = beta5_LL_yo_mw,
                   beta5_HH_yy_ww = beta5_HH_yy_ww,
                   beta5_HH_yy_mm = beta5_HH_yy_mm,
                   beta5_HH_yy_wm = beta5_HH_yy_wm,
                   beta5_HH_yy_mw = beta5_HH_yy_mw,
                   beta5_HH_oo_ww = beta5_HH_oo_ww,
                   beta5_HH_oo_mm = beta5_HH_oo_mm,
                   beta5_HH_oo_wm = beta5_HH_oo_wm,
                   beta5_HH_oo_mw = beta5_HH_oo_mw,
                   beta5_HH_yo_ww = beta5_HH_yo_ww,
                   beta5_HH_yo_mm = beta5_HH_yo_mm,
                   beta5_HH_yo_wm = beta5_HH_yo_wm,
                   beta5_HH_yo_mw = beta5_HH_yo_mw,
                   beta5_LH_yy_ww = beta5_LH_yy_ww,
                   beta5_LH_yy_mm = beta5_LH_yy_mm,
                   beta5_LH_yy_wm = beta5_LH_yy_wm,
                   beta5_LH_yy_mw = beta5_LH_yy_mw,
                   beta5_HL_yy_ww = beta5_HL_yy_ww,
                   beta5_HL_yy_mm = beta5_HL_yy_mm,
                   beta5_HL_yy_wm = beta5_HL_yy_wm,
                   beta5_HL_yy_mw = beta5_HL_yy_mw,
                   beta6_LL_yy_ww = beta6_LL_yy_ww,
                   beta6_LL_yy_mm = beta6_LL_yy_mm,
                   beta6_LL_yy_wm = beta6_LL_yy_wm,
                   beta6_LL_yy_mw = beta6_LL_yy_mw,
                   beta6_LL_oo_ww = beta6_LL_oo_ww,
                   beta6_LL_oo_mm = beta6_LL_oo_mm,
                   beta6_LL_oo_wm = beta6_LL_oo_wm,
                   beta6_LL_oo_mw = beta6_LL_oo_mw,
                   beta6_LL_yo_ww = beta6_LL_yo_ww,
                   beta6_LL_yo_mm = beta6_LL_yo_mm,
                   beta6_LL_yo_wm = beta6_LL_yo_wm,
                   beta6_LL_yo_mw = beta6_LL_yo_mw,
                   beta6_HH_yy_ww = beta6_HH_yy_ww,
                   beta6_HH_yy_mm = beta6_HH_yy_mm,
                   beta6_HH_yy_wm = beta6_HH_yy_wm,
                   beta6_HH_yy_mw = beta6_HH_yy_mw,
                   beta6_HH_oo_ww = beta6_HH_oo_ww,
                   beta6_HH_oo_mm = beta6_HH_oo_mm,
                   beta6_HH_oo_wm = beta6_HH_oo_wm,
                   beta6_HH_oo_mw = beta6_HH_oo_mw,
                   beta6_HH_yo_ww = beta6_HH_yo_ww,
                   beta6_HH_yo_mm = beta6_HH_yo_mm,
                   beta6_HH_yo_wm = beta6_HH_yo_wm,
                   beta6_HH_yo_mw = beta6_HH_yo_mw,
                   beta6_LH_yy_ww = beta6_LH_yy_ww,
                   beta6_LH_yy_mm = beta6_LH_yy_mm,
                   beta6_LH_yy_wm = beta6_LH_yy_wm,
                   beta6_LH_yy_mw = beta6_LH_yy_mw,
                   beta6_HL_yy_ww = beta6_HL_yy_ww,
                   beta6_HL_yy_mm = beta6_HL_yy_mm,
                   beta6_HL_yy_wm = beta6_HL_yy_wm,
                   beta6_HL_yy_mw = beta6_HL_yy_mw,
                   beta7_LL_yy_ww = beta7_LL_yy_ww,
                   beta7_LL_yy_mm = beta7_LL_yy_mm,
                   beta7_LL_yy_wm = beta7_LL_yy_wm,
                   beta7_LL_yy_mw = beta7_LL_yy_mw,
                   beta7_LL_oo_ww = beta7_LL_oo_ww,
                   beta7_LL_oo_mm = beta7_LL_oo_mm,
                   beta7_LL_oo_wm = beta7_LL_oo_wm,
                   beta7_LL_oo_mw = beta7_LL_oo_mw,
                   beta7_LL_yo_ww = beta7_LL_yo_ww,
                   beta7_LL_yo_mm = beta7_LL_yo_mm,
                   beta7_LL_yo_wm = beta7_LL_yo_wm,
                   beta7_LL_yo_mw = beta7_LL_yo_mw,
                   beta7_HH_yy_ww = beta7_HH_yy_ww,
                   beta7_HH_yy_mm = beta7_HH_yy_mm,
                   beta7_HH_yy_wm = beta7_HH_yy_wm,
                   beta7_HH_yy_mw = beta7_HH_yy_mw,
                   beta7_HH_oo_ww = beta7_HH_oo_ww,
                   beta7_HH_oo_mm = beta7_HH_oo_mm,
                   beta7_HH_oo_wm = beta7_HH_oo_wm,
                   beta7_HH_oo_mw = beta7_HH_oo_mw,
                   beta7_HH_yo_ww = beta7_HH_yo_ww,
                   beta7_HH_yo_mm = beta7_HH_yo_mm,
                   beta7_HH_yo_wm = beta7_HH_yo_wm,
                   beta7_HH_yo_mw = beta7_HH_yo_mw,
                   beta7_LH_yy_ww = beta7_LH_yy_ww,
                   beta7_LH_yy_mm = beta7_LH_yy_mm,
                   beta7_LH_yy_wm = beta7_LH_yy_wm,
                   beta7_LH_yy_mw = beta7_LH_yy_mw,
                   beta7_HL_yy_ww = beta7_HL_yy_ww,
                   beta7_HL_yy_mm = beta7_HL_yy_mm,
                   beta7_HL_yy_wm = beta7_HL_yy_wm,
                   beta7_HL_yy_mw = beta7_HL_yy_mw,
                   beta8_LL_yy_ww = beta8_LL_yy_ww,
                   beta8_LL_yy_mm = beta8_LL_yy_mm,
                   beta8_LL_yy_wm = beta8_LL_yy_wm,
                   beta8_LL_yy_mw = beta8_LL_yy_mw,
                   beta8_LL_oo_ww = beta8_LL_oo_ww,
                   beta8_LL_oo_mm = beta8_LL_oo_mm,
                   beta8_LL_oo_wm = beta8_LL_oo_wm,
                   beta8_LL_oo_mw = beta8_LL_oo_mw,
                   beta8_LL_yo_ww = beta8_LL_yo_ww,
                   beta8_LL_yo_mm = beta8_LL_yo_mm,
                   beta8_LL_yo_wm = beta8_LL_yo_wm,
                   beta8_LL_yo_mw = beta8_LL_yo_mw,
                   beta8_HH_yy_ww = beta8_HH_yy_ww,
                   beta8_HH_yy_mm = beta8_HH_yy_mm,
                   beta8_HH_yy_wm = beta8_HH_yy_wm,
                   beta8_HH_yy_mw = beta8_HH_yy_mw,
                   beta8_HH_oo_ww = beta8_HH_oo_ww,
                   beta8_HH_oo_mm = beta8_HH_oo_mm,
                   beta8_HH_oo_wm = beta8_HH_oo_wm,
                   beta8_HH_oo_mw = beta8_HH_oo_mw,
                   beta8_HH_yo_ww = beta8_HH_yo_ww,
                   beta8_HH_yo_mm = beta8_HH_yo_mm,
                   beta8_HH_yo_wm = beta8_HH_yo_wm,
                   beta8_HH_yo_mw = beta8_HH_yo_mw,
                   beta8_LH_yy_ww = beta8_LH_yy_ww,
                   beta8_LH_yy_mm = beta8_LH_yy_mm,
                   beta8_LH_yy_wm = beta8_LH_yy_wm,
                   beta8_LH_yy_mw = beta8_LH_yy_mw,
                   beta8_HL_yy_ww = beta8_HL_yy_ww,
                   beta8_HL_yy_mm = beta8_HL_yy_mm,
                   beta8_HL_yy_wm = beta8_HL_yy_wm,
                   beta8_HL_yy_mw = beta8_HL_yy_mw,
                   
                   N_L_y_w = L.N_y_w,
                   N_L_y_m = L.N_y_m,
                   N_L_o_w = L.N_o_w,
                   N_L_o_m = L.N_o_m,
                   N_H_y_w = H.N_y_w,
                   N_H_y_m = H.N_y_m,
                   N_H_o_w = H.N_o_w,
                   N_H_o_m = H.N_o_m,

                   t0 = t0,
                   t1 = t1,
                   t2 = t2,
                   t3 = t3,
                   t4 = t4,
                   t5 = t5,
                   t6 = t6,
                   t7 = t7,
                   t8 = t8,
                   a1 = a1,
                   a2 = a2,
                   gamma = gamma
        )

        out <- ode(Y, times, func = "derivs", parms = parms,
                   dllname = "model8",
                   initfunc = "initmod", nout = 34)
     }

    state$L.S_y_w[(padding + 1):(padding + period)] = out[,2]
    state$L.E_y_w[(padding + 1):(padding + period)] = out[,3]+out[,4]
    state$L.I_y_w[(padding + 1):(padding + period)] = out[,5]
    state$L.R_y_w[(padding + 1):(padding + period)] = out[,6]
    
    state$L.S_y_m[(padding + 1):(padding + period)] = out[,7]
    state$L.E_y_m[(padding + 1):(padding + period)] = out[,8]+out[,9]
    state$L.I_y_m[(padding + 1):(padding + period)] = out[,10]
    state$L.R_y_m[(padding + 1):(padding + period)] = out[,11]
    
    state$L.S_o_w[(padding + 1):(padding + period)] = out[,12]
    state$L.E_o_w[(padding + 1):(padding + period)] = out[,13]+out[,14]
    state$L.I_o_w[(padding + 1):(padding + period)] = out[,15]
    state$L.R_o_w[(padding + 1):(padding + period)] = out[,16]
    
    state$L.S_o_m[(padding + 1):(padding + period)] = out[,17]
    state$L.E_o_m[(padding + 1):(padding + period)] = out[,18]+out[,19]
    state$L.I_o_m[(padding + 1):(padding + period)] = out[,20]
    state$L.R_o_m[(padding + 1):(padding + period)] = out[,21]
    
    state$H.S_y_w[(padding + 1):(padding + period)] = out[,22]
    state$H.E_y_w[(padding + 1):(padding + period)] = out[,23]+out[,24]
    state$H.I_y_w[(padding + 1):(padding + period)] = out[,25]
    state$H.R_y_w[(padding + 1):(padding + period)] = out[,26]
    
    state$H.S_y_m[(padding + 1):(padding + period)] = out[,27]
    state$H.E_y_m[(padding + 1):(padding + period)] = out[,28]+out[,29]
    state$H.I_y_m[(padding + 1):(padding + period)] = out[,30]
    state$H.R_y_m[(padding + 1):(padding + period)] = out[,31]
    
    state$H.S_o_w[(padding + 1):(padding + period)] = out[,32]
    state$H.E_o_w[(padding + 1):(padding + period)] = out[,33]+out[,34]
    state$H.I_o_w[(padding + 1):(padding + period)] = out[,35]
    state$H.R_o_w[(padding + 1):(padding + period)] = out[,36]
    
    state$H.S_o_m[(padding + 1):(padding + period)] = out[,37]
    state$H.E_o_m[(padding + 1):(padding + period)] = out[,38]+out[,39]
    state$H.I_o_m[(padding + 1):(padding + period)] = out[,40]
    state$H.R_o_m[(padding + 1):(padding + period)] = out[,41]
    
    state$Ro[(padding + 1):(padding + period)] = out[,42]
    state$Rt[(padding + 1):(padding + period)] = out[,43]
    
    state$R.LL_yy_ww[(padding+1):(padding+period)] = out[,44]
    state$R.LL_yy_mm[(padding+1):(padding+period)] = out[,45]
    state$R.LL_yy_wm[(padding+1):(padding+period)] = out[,46]
    state$R.LL_yy_mw[(padding+1):(padding+period)] = out[,47]
    state$R.LL_oo_ww[(padding+1):(padding+period)] = out[,48]
    state$R.LL_oo_mm[(padding+1):(padding+period)] = out[,49]
    state$R.LL_oo_wm[(padding+1):(padding+period)] = out[,50]
    state$R.LL_oo_mw[(padding+1):(padding+period)] = out[,51]
    state$R.LL_yo_ww[(padding+1):(padding+period)] = out[,52]
    state$R.LL_yo_mm[(padding+1):(padding+period)] = out[,53]
    state$R.LL_yo_wm[(padding+1):(padding+period)] = out[,54]
    state$R.LL_yo_mw[(padding+1):(padding+period)] = out[,55]
    state$R.HH_yy_ww[(padding+1):(padding+period)] = out[,56]
    state$R.HH_yy_mm[(padding+1):(padding+period)] = out[,57]
    state$R.HH_yy_wm[(padding+1):(padding+period)] = out[,58]
    state$R.HH_yy_mw[(padding+1):(padding+period)] = out[,59]
    state$R.HH_oo_ww[(padding+1):(padding+period)] = out[,60]
    state$R.HH_oo_mm[(padding+1):(padding+period)] = out[,61]
    state$R.HH_oo_wm[(padding+1):(padding+period)] = out[,62]
    state$R.HH_oo_mw[(padding+1):(padding+period)] = out[,63]
    state$R.HH_yo_ww[(padding+1):(padding+period)] = out[,64]
    state$R.HH_yo_mm[(padding+1):(padding+period)] = out[,65]
    state$R.HH_yo_wm[(padding+1):(padding+period)] = out[,66]
    state$R.HH_yo_mw[(padding+1):(padding+period)] = out[,67]
    state$R.LH_yy_ww[(padding+1):(padding+period)] = out[,68]
    state$R.LH_yy_mm[(padding+1):(padding+period)] = out[,69]
    state$R.LH_yy_wm[(padding+1):(padding+period)] = out[,70]
    state$R.LH_yy_mw[(padding+1):(padding+period)] = out[,71]
    state$R.HL_yy_ww[(padding+1):(padding+period)] = out[,72]
    state$R.HL_yy_mm[(padding+1):(padding+period)] = out[,73]
    state$R.HL_yy_wm[(padding+1):(padding+period)] = out[,74]
    state$R.HL_yy_mw[(padding+1):(padding+period)] = out[,75]
    
    
    s2 <- convolute(state$L.S_y_w, padding + 1, padding + period, died_cv_profile.y)
    state$L.died_y_w[(padding + 1):(padding + period)] = (L.N_y_w - s2) * died_rate_L.y_w
    state$L.deadi_y_w <- c(state$L.died_y_w[1], diff(state$L.died_y_w))
    
    s2 <- convolute(state$L.S_y_m, padding + 1, padding + period, died_cv_profile.y)
    state$L.died_y_m[(padding + 1):(padding + period)] = (L.N_y_m - s2) * died_rate_L.y_m
    state$L.deadi_y_m <- c(state$L.died_y_m[1], diff(state$L.died_y_m))
    
    s2 <- convolute(state$L.S_o_w, padding + 1, padding + period, died_cv_profile.o)
    state$L.died_o_w[(padding + 1):(padding + period)] = (L.N_o_w - s2) * died_rate_L.o_w
    state$L.deadi_o_w <- c(state$L.died_o_w[1], diff(state$L.died_o_w))
    
    s2 <- convolute(state$L.S_o_m, padding + 1, padding + period, died_cv_profile.o)
    state$L.died_o_m[(padding + 1):(padding + period)] = (L.N_o_m - s2) * died_rate_L.o_m
    state$L.deadi_o_m <- c(state$L.died_o_m[1], diff(state$L.died_o_m))
    
    s2 <- convolute(state$H.S_y_w, padding + 1, padding + period, died_cv_profile.y)
    state$H.died_y_w[(padding + 1):(padding + period)] = (H.N_y_w - s2) * died_rate_H.y_w
    state$H.deadi_y_w <- c(state$H.died_y_w[1], diff(state$H.died_y_w))
    
    s2 <- convolute(state$H.S_y_m, padding + 1, padding + period, died_cv_profile.y)
    state$H.died_y_m[(padding + 1):(padding + period)] = (H.N_y_m - s2) * died_rate_H.y_m
    state$H.deadi_y_m <- c(state$H.died_y_m[1], diff(state$H.died_y_m))
    
    s2 <- convolute(state$H.S_o_w, padding + 1, padding + period, died_cv_profile.o)
    state$H.died_o_w[(padding + 1):(padding + period)] = (H.N_o_w - s2) * died_rate_H.o_w
    state$H.deadi_o_w <- c(state$H.died_o_w[1], diff(state$H.died_o_w))
    
    s2 <- convolute(state$H.S_o_m, padding + 1, padding + period, died_cv_profile.o)
    state$H.died_o_m[(padding + 1):(padding + period)] = (H.N_o_m - s2) * died_rate_H.o_m
    state$H.deadi_o_m <- c(state$H.died_o_m[1], diff(state$H.died_o_m))
    
    
    state$padding <- padding
    state$offset <- data_offset

    state
}

transformParams <- function(params)
{
    params
}

invTransformParams <- function(posterior)
{
    posterior$Tinf <- 1/gamma

    posterior
}

calcNominalState <- function(state)
{
    state$L.died <- state$L.died_y_w + state$L.died_y_m + state$L.died_o_w + state$L.died_o_m 
    state$H.died <- state$H.died_y_w + state$H.died_y_m + state$H.died_o_w + state$H.died_o_m 
    
    state$L.deadi <- state$L.deadi_y_w + state$L.deadi_y_m + state$L.deadi_o_w + state$L.deadi_o_m 
    state$H.deadi <- state$H.deadi_y_w + state$H.deadi_y_m + state$H.deadi_o_w + state$H.deadi_o_m 
      
    state$died <- state$L.died + state$H.died    
    state$deadi <- state$L.deadi + state$H.deadi
    
    state$L.S <- state$L.S_y_w + state$L.S_y_m + state$L.S_o_w + state$L.S_o_m 
    state$H.S <- state$H.S_y_w + state$H.S_y_m + state$H.S_o_w + state$H.S_o_m
    state$L.I <- state$L.I_y_w + state$L.I_y_m + state$L.I_o_w + state$L.I_o_m 
    state$H.I <- state$H.I_y_w + state$H.I_y_m + state$H.I_o_w + state$H.I_o_m
    state$L.E <- state$L.E_y_w + state$L.E_y_m + state$L.E_o_w + state$L.E_o_m 
    state$H.E <- state$H.E_y_w + state$H.E_y_m + state$H.E_o_w + state$H.E_o_m
    
    state$L.R <- state$L.R_y_w + state$L.R_y_m + state$L.R_o_w + state$L.R_o_m 
    state$H.R <- state$H.R_y_w + state$H.R_y_m + state$H.R_o_w + state$H.R_o_m

    state$S <- state$L.S + state$H.S 
    state$R <- state$L.R + state$H.R 
    state$I <- state$L.I + state$H.I 
    state$E <- state$L.E + state$H.E 

    state
}

## log likelihood function for fitting this model to observed data:
##   L.dmorti, H.dmorti

calclogp <- function(params) {
    
  beta0_LL_yy_ww <- params[1]
  beta0_LL_yy_mm <- params[2]
  beta0_LL_yy_wm <- params[3]
  beta0_LL_yy_mw <- params[4]
  beta0_LL_oo_ww <- params[5]
  beta0_LL_oo_mm <- params[6]
  beta0_LL_oo_wm <- params[7]
  beta0_LL_oo_mw <- params[8]
  beta0_LL_yo_ww <- params[9]
  beta0_LL_yo_mm <- params[10]
  beta0_LL_yo_wm <- params[11]
  beta0_LL_yo_mw <- params[12]
  beta0_HH_yy_ww <- params[13]
  beta0_HH_yy_mm <- params[14]
  beta0_HH_yy_wm <- params[15]
  beta0_HH_yy_mw <- params[16]
  beta0_HH_oo_ww <- params[17]
  beta0_HH_oo_mm <- params[18]
  beta0_HH_oo_wm <- params[19]
  beta0_HH_oo_mw <- params[20]
  beta0_HH_yo_ww <- params[21]
  beta0_HH_yo_mm <- params[22]
  beta0_HH_yo_wm <- params[23]
  beta0_HH_yo_mw <- params[24]
  beta0_LH_yy_ww <- params[25]
  beta0_LH_yy_mm <- params[26]
  beta0_LH_yy_wm <- params[27]
  beta0_LH_yy_mw <- params[28]
  beta0_HL_yy_ww <- params[29]
  beta0_HL_yy_mm <- params[30]
  beta0_HL_yy_wm <- params[31]
  beta0_HL_yy_mw <- params[32]
  beta1_LL_yy_ww <- params[33]
  beta1_LL_yy_mm <- params[34]
  beta1_LL_yy_wm <- params[35]
  beta1_LL_yy_mw <- params[36]
  beta1_LL_oo_ww <- params[37]
  beta1_LL_oo_mm <- params[38]
  beta1_LL_oo_wm <- params[39]
  beta1_LL_oo_mw <- params[40]
  beta1_LL_yo_ww <- params[41]
  beta1_LL_yo_mm <- params[42]
  beta1_LL_yo_wm <- params[43]
  beta1_LL_yo_mw <- params[44]
  beta1_HH_yy_ww <- params[45]
  beta1_HH_yy_mm <- params[46]
  beta1_HH_yy_wm <- params[47]
  beta1_HH_yy_mw <- params[48]
  beta1_HH_oo_ww <- params[49]
  beta1_HH_oo_mm <- params[50]
  beta1_HH_oo_wm <- params[51]
  beta1_HH_oo_mw <- params[52]
  beta1_HH_yo_ww <- params[53]
  beta1_HH_yo_mm <- params[54]
  beta1_HH_yo_wm <- params[55]
  beta1_HH_yo_mw <- params[56]
  beta1_LH_yy_ww <- params[57]
  beta1_LH_yy_mm <- params[58]
  beta1_LH_yy_wm <- params[59]
  beta1_LH_yy_mw <- params[60]
  beta1_HL_yy_ww <- params[61]
  beta1_HL_yy_mm <- params[62]
  beta1_HL_yy_wm <- params[63]
  beta1_HL_yy_mw <- params[64]
  beta2_LL_yy_ww <- params[65]
  beta2_LL_yy_mm <- params[66]
  beta2_LL_yy_wm <- params[67]
  beta2_LL_yy_mw <- params[68]
  beta2_LL_oo_ww <- params[69]
  beta2_LL_oo_mm <- params[70]
  beta2_LL_oo_wm <- params[71]
  beta2_LL_oo_mw <- params[72]
  beta2_LL_yo_ww <- params[73]
  beta2_LL_yo_mm <- params[74]
  beta2_LL_yo_wm <- params[75]
  beta2_LL_yo_mw <- params[76]
  beta2_HH_yy_ww <- params[77]
  beta2_HH_yy_mm <- params[78]
  beta2_HH_yy_wm <- params[79]
  beta2_HH_yy_mw <- params[80]
  beta2_HH_oo_ww <- params[81]
  beta2_HH_oo_mm <- params[82]
  beta2_HH_oo_wm <- params[83]
  beta2_HH_oo_mw <- params[84]
  beta2_HH_yo_ww <- params[85]
  beta2_HH_yo_mm <- params[86]
  beta2_HH_yo_wm <- params[87]
  beta2_HH_yo_mw <- params[88]
  beta2_LH_yy_ww <- params[89]
  beta2_LH_yy_mm <- params[90]
  beta2_LH_yy_wm <- params[91]
  beta2_LH_yy_mw <- params[92]
  beta2_HL_yy_ww <- params[93]
  beta2_HL_yy_mm <- params[94]
  beta2_HL_yy_wm <- params[95]
  beta2_HL_yy_mw <- params[96]
  beta3_LL_yy_ww <- params[97]
  beta3_LL_yy_mm <- params[98]
  beta3_LL_yy_wm <- params[99]
  beta3_LL_yy_mw <- params[100]
  beta3_LL_oo_ww <- params[101]
  beta3_LL_oo_mm <- params[102]
  beta3_LL_oo_wm <- params[103]
  beta3_LL_oo_mw <- params[104]
  beta3_LL_yo_ww <- params[105]
  beta3_LL_yo_mm <- params[106]
  beta3_LL_yo_wm <- params[107]
  beta3_LL_yo_mw <- params[108]
  beta3_HH_yy_ww <- params[109]
  beta3_HH_yy_mm <- params[110]
  beta3_HH_yy_wm <- params[111]
  beta3_HH_yy_mw <- params[112]
  beta3_HH_oo_ww <- params[113]
  beta3_HH_oo_mm <- params[114]
  beta3_HH_oo_wm <- params[115]
  beta3_HH_oo_mw <- params[116]
  beta3_HH_yo_ww <- params[117]
  beta3_HH_yo_mm <- params[118]
  beta3_HH_yo_wm <- params[119]
  beta3_HH_yo_mw <- params[120]
  beta3_LH_yy_ww <- params[121]
  beta3_LH_yy_mm <- params[122]
  beta3_LH_yy_wm <- params[123]
  beta3_LH_yy_mw <- params[124]
  beta3_HL_yy_ww <- params[125]
  beta3_HL_yy_mm <- params[126]
  beta3_HL_yy_wm <- params[127]
  beta3_HL_yy_mw <- params[128]
  beta4_LL_yy_ww <- params[129]
  beta4_LL_yy_mm <- params[130]
  beta4_LL_yy_wm <- params[131]
  beta4_LL_yy_mw <- params[132]
  beta4_LL_oo_ww <- params[133]
  beta4_LL_oo_mm <- params[134]
  beta4_LL_oo_wm <- params[135]
  beta4_LL_oo_mw <- params[136]
  beta4_LL_yo_ww <- params[137]
  beta4_LL_yo_mm <- params[138]
  beta4_LL_yo_wm <- params[139]
  beta4_LL_yo_mw <- params[140]
  beta4_HH_yy_ww <- params[141]
  beta4_HH_yy_mm <- params[142]
  beta4_HH_yy_wm <- params[143]
  beta4_HH_yy_mw <- params[144]
  beta4_HH_oo_ww <- params[145]
  beta4_HH_oo_mm <- params[146]
  beta4_HH_oo_wm <- params[147]
  beta4_HH_oo_mw <- params[148]
  beta4_HH_yo_ww <- params[149]
  beta4_HH_yo_mm <- params[150]
  beta4_HH_yo_wm <- params[151]
  beta4_HH_yo_mw <- params[152]
  beta4_LH_yy_ww <- params[153]
  beta4_LH_yy_mm <- params[154]
  beta4_LH_yy_wm <- params[155]
  beta4_LH_yy_mw <- params[156]
  beta4_HL_yy_ww <- params[157]
  beta4_HL_yy_mm <- params[158]
  beta4_HL_yy_wm <- params[159]
  beta4_HL_yy_mw <- params[160]
  beta5_LL_yy_ww <- params[161]
  beta5_LL_yy_mm <- params[162]
  beta5_LL_yy_wm <- params[163]
  beta5_LL_yy_mw <- params[164]
  beta5_LL_oo_ww <- params[165]
  beta5_LL_oo_mm <- params[166]
  beta5_LL_oo_wm <- params[167]
  beta5_LL_oo_mw <- params[168]
  beta5_LL_yo_ww <- params[169]
  beta5_LL_yo_mm <- params[170]
  beta5_LL_yo_wm <- params[171]
  beta5_LL_yo_mw <- params[172]
  beta5_HH_yy_ww <- params[173]
  beta5_HH_yy_mm <- params[174]
  beta5_HH_yy_wm <- params[175]
  beta5_HH_yy_mw <- params[176]
  beta5_HH_oo_ww <- params[177]
  beta5_HH_oo_mm <- params[178]
  beta5_HH_oo_wm <- params[179]
  beta5_HH_oo_mw <- params[180]
  beta5_HH_yo_ww <- params[181]
  beta5_HH_yo_mm <- params[182]
  beta5_HH_yo_wm <- params[183]
  beta5_HH_yo_mw <- params[184]
  beta5_LH_yy_ww <- params[185]
  beta5_LH_yy_mm <- params[186]
  beta5_LH_yy_wm <- params[187]
  beta5_LH_yy_mw <- params[188]
  beta5_HL_yy_ww <- params[189]
  beta5_HL_yy_mm <- params[190]
  beta5_HL_yy_wm <- params[191]
  beta5_HL_yy_mw <- params[192]
  beta6_LL_yy_ww <- params[193]
  beta6_LL_yy_mm <- params[194]
  beta6_LL_yy_wm <- params[195]
  beta6_LL_yy_mw <- params[196]
  beta6_LL_oo_ww <- params[197]
  beta6_LL_oo_mm <- params[198]
  beta6_LL_oo_wm <- params[199]
  beta6_LL_oo_mw <- params[200]
  beta6_LL_yo_ww <- params[201]
  beta6_LL_yo_mm <- params[202]
  beta6_LL_yo_wm <- params[203]
  beta6_LL_yo_mw <- params[204]
  beta6_HH_yy_ww <- params[205]
  beta6_HH_yy_mm <- params[206]
  beta6_HH_yy_wm <- params[207]
  beta6_HH_yy_mw <- params[208]
  beta6_HH_oo_ww <- params[209]
  beta6_HH_oo_mm <- params[210]
  beta6_HH_oo_wm <- params[211]
  beta6_HH_oo_mw <- params[212]
  beta6_HH_yo_ww <- params[213]
  beta6_HH_yo_mm <- params[214]
  beta6_HH_yo_wm <- params[215]
  beta6_HH_yo_mw <- params[216]
  beta6_LH_yy_ww <- params[217]
  beta6_LH_yy_mm <- params[218]
  beta6_LH_yy_wm <- params[219]
  beta6_LH_yy_mw <- params[220]
  beta6_HL_yy_ww <- params[221]
  beta6_HL_yy_mm <- params[222]
  beta6_HL_yy_wm <- params[223]
  beta6_HL_yy_mw <- params[224]
  beta7_LL_yy_ww <- params[225]
  beta7_LL_yy_mm <- params[226]
  beta7_LL_yy_wm <- params[227]
  beta7_LL_yy_mw <- params[228]
  beta7_LL_oo_ww <- params[229]
  beta7_LL_oo_mm <- params[230]
  beta7_LL_oo_wm <- params[231]
  beta7_LL_oo_mw <- params[232]
  beta7_LL_yo_ww <- params[233]
  beta7_LL_yo_mm <- params[234]
  beta7_LL_yo_wm <- params[235]
  beta7_LL_yo_mw <- params[236]
  beta7_HH_yy_ww <- params[237]
  beta7_HH_yy_mm <- params[238]
  beta7_HH_yy_wm <- params[239]
  beta7_HH_yy_mw <- params[240]
  beta7_HH_oo_ww <- params[241]
  beta7_HH_oo_mm <- params[242]
  beta7_HH_oo_wm <- params[243]
  beta7_HH_oo_mw <- params[244]
  beta7_HH_yo_ww <- params[245]
  beta7_HH_yo_mm <- params[246]
  beta7_HH_yo_wm <- params[247]
  beta7_HH_yo_mw <- params[248]
  beta7_LH_yy_ww <- params[249]
  beta7_LH_yy_mm <- params[250]
  beta7_LH_yy_wm <- params[251]
  beta7_LH_yy_mw <- params[252]
  beta7_HL_yy_ww <- params[253]
  beta7_HL_yy_mm <- params[254]
  beta7_HL_yy_wm <- params[255]
  beta7_HL_yy_mw <- params[256]
  
  beta8_LL_yy_ww <- params[257]
  beta8_LL_yy_mm <- params[258]
  beta8_LL_yy_wm <- params[259]
  beta8_LL_yy_mw <- params[260]
  beta8_LL_oo_ww <- params[261]
  beta8_LL_oo_mm <- params[262]
  beta8_LL_oo_wm <- params[263]
  beta8_LL_oo_mw <- params[264]
  beta8_LL_yo_ww <- params[265]
  beta8_LL_yo_mm <- params[266]
  beta8_LL_yo_wm <- params[267]
  beta8_LL_yo_mw <- params[268]
  beta8_HH_yy_ww <- params[269]
  beta8_HH_yy_mm <- params[270]
  beta8_HH_yy_wm <- params[271]
  beta8_HH_yy_mw <- params[272]
  beta8_HH_oo_ww <- params[273]
  beta8_HH_oo_mm <- params[274]
  beta8_HH_oo_wm <- params[275]
  beta8_HH_oo_mw <- params[276]
  beta8_HH_yo_ww <- params[277]
  beta8_HH_yo_mm <- params[278]
  beta8_HH_yo_wm <- params[279]
  beta8_HH_yo_mw <- params[280]
  beta8_LH_yy_ww <- params[281]
  beta8_LH_yy_mm <- params[282]
  beta8_LH_yy_wm <- params[283]
  beta8_LH_yy_mw <- params[284]
  beta8_HL_yy_ww <- params[285]
  beta8_HL_yy_mm <- params[286]
  beta8_HL_yy_wm <- params[287]
  beta8_HL_yy_mw <- params[288]
  
  died_latency.y <- params[289]
  died_latency.o <- params[290]
  
  died_rate_L.y_w <- params[291]
  died_rate_L.y_m <- params[292]
  died_rate_L.o_w <- params[293]
  died_rate_L.o_m <- params[294]
  
  died_rate_H.y_w <- params[295]
  died_rate_H.y_m <- params[296]
  died_rate_H.o_w <- params[297]
  died_rate_H.o_m <- params[298]
  
  t0_morts <- params[299]
  DLsd <- params[300]
  
    

    logPriorP <- 0
    
    logPriorP <- logPriorP + dnorm(died_latency.y, mean=21, sd=4, log=T)
    logPriorP <- logPriorP + dnorm(died_latency.o, mean=19, sd=4, log=T)

    logPriorP <- logPriorP + dnorm(DLsd, mean=5, sd=1, log=T)
    
    logPriorP <- logPriorP + dbeta(died_rate_L.y_w, beta_ifr_y_w$alpha, beta_ifr_y_w$beta, log=T)
    logPriorP <- logPriorP + dbeta(died_rate_L.y_m, beta_ifr_y_m$alpha, beta_ifr_y_m$beta, log=T)
    logPriorP <- logPriorP + dbeta(died_rate_L.o_w, beta_ifr_o_w$alpha, beta_ifr_o_w$beta, log=T)
    logPriorP <- logPriorP + dbeta(died_rate_L.o_w, beta_ifr_o_m$alpha, beta_ifr_o_m$beta, log=T)
    logPriorP <- logPriorP + dbeta(died_rate_H.y_w, beta_ifr_y_w$alpha, beta_ifr_y_w$beta, log=T)
    logPriorP <- logPriorP + dbeta(died_rate_H.y_m, beta_ifr_y_m$alpha, beta_ifr_y_m$beta, log=T)
    logPriorP <- logPriorP + dbeta(died_rate_H.o_w, beta_ifr_o_w$alpha, beta_ifr_o_w$beta, log=T)
    logPriorP <- logPriorP + dbeta(died_rate_H.o_w, beta_ifr_o_m$alpha, beta_ifr_o_m$beta, log=T)
    
    logPriorP <- logPriorP + dnorm(beta0_LH_yy_mm - beta0_HL_yy_mm, mean=0, sd=gamma*0.1, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LH_yy_ww - beta0_HL_yy_ww, mean=0, sd=gamma*0.1, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LH_yy_mw - beta0_HL_yy_mw, mean=0, sd=gamma*0.1, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LH_yy_wm - beta0_HL_yy_wm, mean=0, sd=gamma*0.1, log=T) 
    
    logPriorP <- logPriorP + dnorm(beta0_LL_yy_ww - beta1_LL_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LL_yy_mm - beta1_LL_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LL_yy_wm - beta1_LL_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LL_yy_mw - beta1_LL_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LL_oo_ww - beta1_LL_oo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LL_oo_mm - beta1_LL_oo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LL_oo_wm - beta1_LL_oo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LL_oo_mw - beta1_LL_oo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LL_yo_ww - beta1_LL_yo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LL_yo_mm - beta1_LL_yo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LL_yo_wm - beta1_LL_yo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LL_yo_mw - beta1_LL_yo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_HH_yy_ww - beta1_HH_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_HH_yy_mm - beta1_HH_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_HH_yy_wm - beta1_HH_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_HH_yy_mw - beta1_HH_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_HH_oo_ww - beta1_HH_oo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_HH_oo_mm - beta1_HH_oo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_HH_oo_wm - beta1_HH_oo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_HH_oo_mw - beta1_HH_oo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_HH_yo_ww - beta1_HH_yo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_HH_yo_mm - beta1_HH_yo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_HH_yo_wm - beta1_HH_yo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_HH_yo_mw - beta1_HH_yo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LH_yy_ww - beta1_LH_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LH_yy_mm - beta1_LH_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LH_yy_wm - beta1_LH_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_LH_yy_mw - beta1_LH_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_HL_yy_ww - beta1_HL_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_HL_yy_mm - beta1_HL_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_HL_yy_wm - beta1_HL_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta0_HL_yy_mw - beta1_HL_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_LL_yy_ww - beta2_LL_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_LL_yy_mm - beta2_LL_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_LL_yy_wm - beta2_LL_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_LL_yy_mw - beta2_LL_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_LL_oo_ww - beta2_LL_oo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_LL_oo_mm - beta2_LL_oo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_LL_oo_wm - beta2_LL_oo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_LL_oo_mw - beta2_LL_oo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_LL_yo_ww - beta2_LL_yo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_LL_yo_mm - beta2_LL_yo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_LL_yo_wm - beta2_LL_yo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_LL_yo_mw - beta2_LL_yo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_HH_yy_ww - beta2_HH_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_HH_yy_mm - beta2_HH_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_HH_yy_wm - beta2_HH_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_HH_yy_mw - beta2_HH_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_HH_oo_ww - beta2_HH_oo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_HH_oo_mm - beta2_HH_oo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_HH_oo_wm - beta2_HH_oo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_HH_oo_mw - beta2_HH_oo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_HH_yo_ww - beta2_HH_yo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_HH_yo_mm - beta2_HH_yo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_HH_yo_wm - beta2_HH_yo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_HH_yo_mw - beta2_HH_yo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_LH_yy_ww - beta2_LH_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_LH_yy_mm - beta2_LH_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_LH_yy_wm - beta2_LH_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_LH_yy_mw - beta2_LH_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_HL_yy_ww - beta2_HL_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_HL_yy_mm - beta2_HL_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_HL_yy_wm - beta2_HL_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta1_HL_yy_mw - beta2_HL_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_LL_yy_ww - beta3_LL_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_LL_yy_mm - beta3_LL_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_LL_yy_wm - beta3_LL_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_LL_yy_mw - beta3_LL_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_LL_oo_ww - beta3_LL_oo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_LL_oo_mm - beta3_LL_oo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_LL_oo_wm - beta3_LL_oo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_LL_oo_mw - beta3_LL_oo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_LL_yo_ww - beta3_LL_yo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_LL_yo_mm - beta3_LL_yo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_LL_yo_wm - beta3_LL_yo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_LL_yo_mw - beta3_LL_yo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_HH_yy_ww - beta3_HH_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_HH_yy_mm - beta3_HH_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_HH_yy_wm - beta3_HH_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_HH_yy_mw - beta3_HH_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_HH_oo_ww - beta3_HH_oo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_HH_oo_mm - beta3_HH_oo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_HH_oo_wm - beta3_HH_oo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_HH_oo_mw - beta3_HH_oo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_HH_yo_ww - beta3_HH_yo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_HH_yo_mm - beta3_HH_yo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_HH_yo_wm - beta3_HH_yo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_HH_yo_mw - beta3_HH_yo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_LH_yy_ww - beta3_LH_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_LH_yy_mm - beta3_LH_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_LH_yy_wm - beta3_LH_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_LH_yy_mw - beta3_LH_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_HL_yy_ww - beta3_HL_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_HL_yy_mm - beta3_HL_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_HL_yy_wm - beta3_HL_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta2_HL_yy_mw - beta3_HL_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_LL_yy_ww - beta4_LL_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_LL_yy_mm - beta4_LL_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_LL_yy_wm - beta4_LL_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_LL_yy_mw - beta4_LL_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_LL_oo_ww - beta4_LL_oo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_LL_oo_mm - beta4_LL_oo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_LL_oo_wm - beta4_LL_oo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_LL_oo_mw - beta4_LL_oo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_LL_yo_ww - beta4_LL_yo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_LL_yo_mm - beta4_LL_yo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_LL_yo_wm - beta4_LL_yo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_LL_yo_mw - beta4_LL_yo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_HH_yy_ww - beta4_HH_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_HH_yy_mm - beta4_HH_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_HH_yy_wm - beta4_HH_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_HH_yy_mw - beta4_HH_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_HH_oo_ww - beta4_HH_oo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_HH_oo_mm - beta4_HH_oo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_HH_oo_wm - beta4_HH_oo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_HH_oo_mw - beta4_HH_oo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_HH_yo_ww - beta4_HH_yo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_HH_yo_mm - beta4_HH_yo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_HH_yo_wm - beta4_HH_yo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_HH_yo_mw - beta4_HH_yo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_LH_yy_ww - beta4_LH_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_LH_yy_mm - beta4_LH_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_LH_yy_wm - beta4_LH_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_LH_yy_mw - beta4_LH_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_HL_yy_ww - beta4_HL_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_HL_yy_mm - beta4_HL_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_HL_yy_wm - beta4_HL_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta3_HL_yy_mw - beta4_HL_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_LL_yy_ww - beta5_LL_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_LL_yy_mm - beta5_LL_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_LL_yy_wm - beta5_LL_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_LL_yy_mw - beta5_LL_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_LL_oo_ww - beta5_LL_oo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_LL_oo_mm - beta5_LL_oo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_LL_oo_wm - beta5_LL_oo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_LL_oo_mw - beta5_LL_oo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_LL_yo_ww - beta5_LL_yo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_LL_yo_mm - beta5_LL_yo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_LL_yo_wm - beta5_LL_yo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_LL_yo_mw - beta5_LL_yo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_HH_yy_ww - beta5_HH_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_HH_yy_mm - beta5_HH_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_HH_yy_wm - beta5_HH_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_HH_yy_mw - beta5_HH_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_HH_oo_ww - beta5_HH_oo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_HH_oo_mm - beta5_HH_oo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_HH_oo_wm - beta5_HH_oo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_HH_oo_mw - beta5_HH_oo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_HH_yo_ww - beta5_HH_yo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_HH_yo_mm - beta5_HH_yo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_HH_yo_wm - beta5_HH_yo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_HH_yo_mw - beta5_HH_yo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_LH_yy_ww - beta5_LH_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_LH_yy_mm - beta5_LH_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_LH_yy_wm - beta5_LH_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_LH_yy_mw - beta5_LH_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_HL_yy_ww - beta5_HL_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_HL_yy_mm - beta5_HL_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_HL_yy_wm - beta5_HL_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta4_HL_yy_mw - beta5_HL_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_LL_yy_ww - beta6_LL_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_LL_yy_mm - beta6_LL_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_LL_yy_wm - beta6_LL_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_LL_yy_mw - beta6_LL_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_LL_oo_ww - beta6_LL_oo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_LL_oo_mm - beta6_LL_oo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_LL_oo_wm - beta6_LL_oo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_LL_oo_mw - beta6_LL_oo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_LL_yo_ww - beta6_LL_yo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_LL_yo_mm - beta6_LL_yo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_LL_yo_wm - beta6_LL_yo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_LL_yo_mw - beta6_LL_yo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_HH_yy_ww - beta6_HH_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_HH_yy_mm - beta6_HH_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_HH_yy_wm - beta6_HH_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_HH_yy_mw - beta6_HH_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_HH_oo_ww - beta6_HH_oo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_HH_oo_mm - beta6_HH_oo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_HH_oo_wm - beta6_HH_oo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_HH_oo_mw - beta6_HH_oo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_HH_yo_ww - beta6_HH_yo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_HH_yo_mm - beta6_HH_yo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_HH_yo_wm - beta6_HH_yo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_HH_yo_mw - beta6_HH_yo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_LH_yy_ww - beta6_LH_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_LH_yy_mm - beta6_LH_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_LH_yy_wm - beta6_LH_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_LH_yy_mw - beta6_LH_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_HL_yy_ww - beta6_HL_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_HL_yy_mm - beta6_HL_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_HL_yy_wm - beta6_HL_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta5_HL_yy_mw - beta6_HL_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_LL_yy_ww - beta7_LL_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_LL_yy_mm - beta7_LL_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_LL_yy_wm - beta7_LL_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_LL_yy_mw - beta7_LL_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_LL_oo_ww - beta7_LL_oo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_LL_oo_mm - beta7_LL_oo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_LL_oo_wm - beta7_LL_oo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_LL_oo_mw - beta7_LL_oo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_LL_yo_ww - beta7_LL_yo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_LL_yo_mm - beta7_LL_yo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_LL_yo_wm - beta7_LL_yo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_LL_yo_mw - beta7_LL_yo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_HH_yy_ww - beta7_HH_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_HH_yy_mm - beta7_HH_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_HH_yy_wm - beta7_HH_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_HH_yy_mw - beta7_HH_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_HH_oo_ww - beta7_HH_oo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_HH_oo_mm - beta7_HH_oo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_HH_oo_wm - beta7_HH_oo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_HH_oo_mw - beta7_HH_oo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_HH_yo_ww - beta7_HH_yo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_HH_yo_mm - beta7_HH_yo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_HH_yo_wm - beta7_HH_yo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_HH_yo_mw - beta7_HH_yo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_LH_yy_ww - beta7_LH_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_LH_yy_mm - beta7_LH_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_LH_yy_wm - beta7_LH_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_LH_yy_mw - beta7_LH_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_HL_yy_ww - beta7_HL_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_HL_yy_mm - beta7_HL_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_HL_yy_wm - beta7_HL_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta6_HL_yy_mw - beta7_HL_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_LL_yy_ww - beta8_LL_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_LL_yy_mm - beta8_LL_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_LL_yy_wm - beta8_LL_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_LL_yy_mw - beta8_LL_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_LL_oo_ww - beta8_LL_oo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_LL_oo_mm - beta8_LL_oo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_LL_oo_wm - beta8_LL_oo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_LL_oo_mw - beta8_LL_oo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_LL_yo_ww - beta8_LL_yo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_LL_yo_mm - beta8_LL_yo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_LL_yo_wm - beta8_LL_yo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_LL_yo_mw - beta8_LL_yo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_HH_yy_ww - beta8_HH_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_HH_yy_mm - beta8_HH_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_HH_yy_wm - beta8_HH_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_HH_yy_mw - beta8_HH_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_HH_oo_ww - beta8_HH_oo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_HH_oo_mm - beta8_HH_oo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_HH_oo_wm - beta8_HH_oo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_HH_oo_mw - beta8_HH_oo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_HH_yo_ww - beta8_HH_yo_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_HH_yo_mm - beta8_HH_yo_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_HH_yo_wm - beta8_HH_yo_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_HH_yo_mw - beta8_HH_yo_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_LH_yy_ww - beta8_LH_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_LH_yy_mm - beta8_LH_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_LH_yy_wm - beta8_LH_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_LH_yy_mw - beta8_LH_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_HL_yy_ww - beta8_HL_yy_ww, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_HL_yy_mm - beta8_HL_yy_mm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_HL_yy_wm - beta8_HL_yy_wm, mean=0, sd=gamma*0.2, log=T) 
    logPriorP <- logPriorP + dnorm(beta7_HL_yy_mw - beta8_HL_yy_mw, mean=0, sd=gamma*0.2, log=T) 
    
    logPriorP
}

calclogl <- function(params, x) {
    state <<- calculateModel(params, FitTotalPeriod)

    if (state$offset == InvalidDataOffset)
        state$offset = 1

    dstart <- state$offset
    dend <- state$offset + length(L.dmorti_o_m) - 1

    if (dend > length(state$L.deadi_o_m)) {
        ##print("=========================== Increase FitTotalPeriod ===================")
        dend <- length(state$L.deadi_o_m)
        dstart <- dend - length(L.dmorti_o_m) + 1
    }

    if (dstart < 1) {
        ##print("=========================== Increase Padding ? ===================")
        dstart <- 1
        dend <- dstart + length(L.dmorti_o_m) - 1
    }

    
    
    L1.loglD <- sum(dnbinom(L.dmorti_y_w,
                           mu=pmax(0.1,state$L.deadi_y_w[dstart:dend]),
                           size=mort_nbinom_size1, log=T))
    
    L2.loglD <- sum(dnbinom(L.dmorti_y_m,
                           mu=pmax(0.1,state$L.deadi_y_m[dstart:dend]),
                           size=mort_nbinom_size1, log=T))
    
    L3.loglD <- sum(dnbinom(L.dmorti_o_w,
                            mu=pmax(0.1,state$L.deadi_o_w[dstart:dend]),
                            size=mort_nbinom_size1, log=T))
    
    L4.loglD <- sum(dnbinom(L.dmorti_o_m,
                            mu=pmax(0.1,state$L.deadi_o_m[dstart:dend]),
                            size=mort_nbinom_size1, log=T))
    
    H1.loglD <- sum(dnbinom(H.dmorti_y_w,
                            mu=pmax(0.1,state$H.deadi_y_w[dstart:dend]),
                            size=mort_nbinom_size1, log=T))
    
    H2.loglD <- sum(dnbinom(H.dmorti_y_m,
                            mu=pmax(0.1,state$H.deadi_y_m[dstart:dend]),
                            size=mort_nbinom_size1, log=T))
    
    H3.loglD <- sum(dnbinom(H.dmorti_o_w,
                            mu=pmax(0.1,state$H.deadi_o_w[dstart:dend]),
                            size=mort_nbinom_size1, log=T))
    
    H4.loglD <- sum(dnbinom(H.dmorti_o_m,
                            mu=pmax(0.1,state$H.deadi_o_m[dstart:dend]),
                            size=mort_nbinom_size1, log=T))
    
    
    
    L.loglsero <- sum(dnorm(sero_L,
                            mean= (( state$L.R_y_w[dstart+time_sero]+
                                       state$L.R_y_m[dstart+time_sero]+
                                       state$L.R_o_w[dstart+time_sero]+
                                       state$L.R_o_m[dstart+time_sero])/(L.N)),
                            sd=sdd(sero_L,1000), log=T))
    
    
    H.loglsero <- sum(dnorm(sero_H,
                            mean= (( state$H.R_y_w[dstart+time_sero]+
                                       state$H.R_y_m[dstart+time_sero]+
                                       state$H.R_o_w[dstart+time_sero]+
                                       state$H.R_o_m[dstart+time_sero])/(H.N)),
                            sd=sdd(sero_H,1000), log=T))
    
    
    Y.loglsero <- sum(dnorm(sero_y,
                            mean= ((  state$L.R_y_w[dstart+time_sero]+
                                       state$L.R_y_m[dstart+time_sero]+
                                        state$H.R_y_w[dstart+time_sero]+
                                        state$H.R_y_m[dstart+time_sero])/(NN.y)),
                            sd=sdd(sero_y,1000), log=T))
    
    
    O.loglsero <- sum(dnorm(sero_o,
                            mean= ((
                                      state$L.R_o_w[dstart+time_sero]+
                                      state$L.R_o_m[dstart+time_sero]+
                                      state$H.R_o_w[dstart+time_sero]+
                                      state$H.R_o_m[dstart+time_sero])/(NN.o)),
                            sd=sdd(sero_o,1000), log=T))
    
    W.loglsero <- sum(dnorm(sero_w,
                            mean= 
                              ((  state$L.R_y_w[dstart+time_sero]+
                                    state$H.R_y_w[dstart+time_sero]+
                                    state$L.R_o_w[dstart+time_sero]+
                                    state$H.R_o_w[dstart+time_sero])/(NN.w)),
                            sd=sdd(sero_w,1000), log=T))
    
    M.loglsero <- sum(dnorm(sero_m,
                            mean= 
                              ((  state$L.R_y_m[dstart+time_sero]+
                                    state$H.R_y_m[dstart+time_sero]+
                                    state$L.R_o_m[dstart+time_sero]+
                                    state$H.R_o_m[dstart+time_sero])/(NN.m)),
                            sd=sdd(sero_m,1000), log=T)) 
                                
  
    it <<- it + 1

    result <- L1.loglD + L2.loglD + L3.loglD  + L4.loglD +
              H1.loglD + H2.loglD + H3.loglD  + H4.loglD +
              L.loglsero +  H.loglsero +  Y.loglsero +  O.loglsero +  
              W.loglsero + M.loglsero
      
    
    if (it %% 1000 == 0) {
        print(params)
	print(c(it, result))
        state <<- calcNominalState(state)
	graphs()
    }

    result
}

fit.paramnames <- c(
  "beta0_LL_yy_ww",	"beta0_LL_yy_mm",	"beta0_LL_yy_wm",	"beta0_LL_yy_mw",	
  "beta0_LL_oo_ww",	"beta0_LL_oo_mm",	"beta0_LL_oo_wm",	"beta0_LL_oo_mw",	
  "beta0_LL_yo_ww",	"beta0_LL_yo_mm",	"beta0_LL_yo_wm",	"beta0_LL_yo_mw",	
  "beta0_HH_yy_ww",	"beta0_HH_yy_mm",	"beta0_HH_yy_wm",	"beta0_HH_yy_mw",	
  "beta0_HH_oo_ww",	"beta0_HH_oo_mm",	"beta0_HH_oo_wm",	"beta0_HH_oo_mw",	
  "beta0_HH_yo_ww",	"beta0_HH_yo_mm",	"beta0_HH_yo_wm",	"beta0_HH_yo_mw",	
  "beta0_LH_yy_ww",	"beta0_LH_yy_mm",	"beta0_LH_yy_wm",	"beta0_LH_yy_mw",	
  "beta0_HL_yy_ww",	"beta0_HL_yy_mm",	"beta0_HL_yy_wm",	"beta0_HL_yy_mw",	
  "beta1_LL_yy_ww",	"beta1_LL_yy_mm",	"beta1_LL_yy_wm",	"beta1_LL_yy_mw",	
  "beta1_LL_oo_ww",	"beta1_LL_oo_mm",	"beta1_LL_oo_wm",	"beta1_LL_oo_mw",	
  "beta1_LL_yo_ww",	"beta1_LL_yo_mm",	"beta1_LL_yo_wm",	"beta1_LL_yo_mw",	
  "beta1_HH_yy_ww",	"beta1_HH_yy_mm",	"beta1_HH_yy_wm",	"beta1_HH_yy_mw",	
  "beta1_HH_oo_ww",	"beta1_HH_oo_mm",	"beta1_HH_oo_wm",	"beta1_HH_oo_mw",	
  "beta1_HH_yo_ww",	"beta1_HH_yo_mm",	"beta1_HH_yo_wm",	"beta1_HH_yo_mw",	
  "beta1_LH_yy_ww",	"beta1_LH_yy_mm",	"beta1_LH_yy_wm",	"beta1_LH_yy_mw",	
  "beta1_HL_yy_ww",	"beta1_HL_yy_mm",	"beta1_HL_yy_wm",	"beta1_HL_yy_mw",	
  "beta2_LL_yy_ww",	"beta2_LL_yy_mm",	"beta2_LL_yy_wm",	"beta2_LL_yy_mw",	
  "beta2_LL_oo_ww",	"beta2_LL_oo_mm",	"beta2_LL_oo_wm",	"beta2_LL_oo_mw",	
  "beta2_LL_yo_ww",	"beta2_LL_yo_mm",	"beta2_LL_yo_wm",	"beta2_LL_yo_mw",	
  "beta2_HH_yy_ww",	"beta2_HH_yy_mm",	"beta2_HH_yy_wm",	"beta2_HH_yy_mw",	
  "beta2_HH_oo_ww",	"beta2_HH_oo_mm",	"beta2_HH_oo_wm",	"beta2_HH_oo_mw",	
  "beta2_HH_yo_ww",	"beta2_HH_yo_mm",	"beta2_HH_yo_wm",	"beta2_HH_yo_mw",	
  "beta2_LH_yy_ww",	"beta2_LH_yy_mm",	"beta2_LH_yy_wm",	"beta2_LH_yy_mw",	
  "beta2_HL_yy_ww",	"beta2_HL_yy_mm",	"beta2_HL_yy_wm",	"beta2_HL_yy_mw",	
  "beta3_LL_yy_ww",	"beta3_LL_yy_mm",	"beta3_LL_yy_wm",	"beta3_LL_yy_mw",	
  "beta3_LL_oo_ww",	"beta3_LL_oo_mm",	"beta3_LL_oo_wm",	"beta3_LL_oo_mw",	
  "beta3_LL_yo_ww",	"beta3_LL_yo_mm",	"beta3_LL_yo_wm",	"beta3_LL_yo_mw",	
  "beta3_HH_yy_ww",	"beta3_HH_yy_mm",	"beta3_HH_yy_wm",	"beta3_HH_yy_mw",	
  "beta3_HH_oo_ww",	"beta3_HH_oo_mm",	"beta3_HH_oo_wm",	"beta3_HH_oo_mw",	
  "beta3_HH_yo_ww",	"beta3_HH_yo_mm",	"beta3_HH_yo_wm",	"beta3_HH_yo_mw",	
  "beta3_LH_yy_ww",	"beta3_LH_yy_mm",	"beta3_LH_yy_wm",	"beta3_LH_yy_mw",	
  "beta3_HL_yy_ww",	"beta3_HL_yy_mm",	"beta3_HL_yy_wm",	"beta3_HL_yy_mw",	
  "beta4_LL_yy_ww",	"beta4_LL_yy_mm",	"beta4_LL_yy_wm",	"beta4_LL_yy_mw",	
  "beta4_LL_oo_ww",	"beta4_LL_oo_mm",	"beta4_LL_oo_wm",	"beta4_LL_oo_mw",	
  "beta4_LL_yo_ww",	"beta4_LL_yo_mm",	"beta4_LL_yo_wm",	"beta4_LL_yo_mw",	
  "beta4_HH_yy_ww",	"beta4_HH_yy_mm",	"beta4_HH_yy_wm",	"beta4_HH_yy_mw",	
  "beta4_HH_oo_ww",	"beta4_HH_oo_mm",	"beta4_HH_oo_wm",	"beta4_HH_oo_mw",	
  "beta4_HH_yo_ww",	"beta4_HH_yo_mm",	"beta4_HH_yo_wm",	"beta4_HH_yo_mw",	
  "beta4_LH_yy_ww",	"beta4_LH_yy_mm",	"beta4_LH_yy_wm",	"beta4_LH_yy_mw",	
  "beta4_HL_yy_ww",	"beta4_HL_yy_mm",	"beta4_HL_yy_wm",	"beta4_HL_yy_mw",	
  "beta5_LL_yy_ww",	"beta5_LL_yy_mm",	"beta5_LL_yy_wm",	"beta5_LL_yy_mw",	
  "beta5_LL_oo_ww",	"beta5_LL_oo_mm",	"beta5_LL_oo_wm",	"beta5_LL_oo_mw",	
  "beta5_LL_yo_ww",	"beta5_LL_yo_mm",	"beta5_LL_yo_wm",	"beta5_LL_yo_mw",	
  "beta5_HH_yy_ww",	"beta5_HH_yy_mm",	"beta5_HH_yy_wm",	"beta5_HH_yy_mw",	
  "beta5_HH_oo_ww",	"beta5_HH_oo_mm",	"beta5_HH_oo_wm",	"beta5_HH_oo_mw",	
  "beta5_HH_yo_ww",	"beta5_HH_yo_mm",	"beta5_HH_yo_wm",	"beta5_HH_yo_mw",	
  "beta5_LH_yy_ww",	"beta5_LH_yy_mm",	"beta5_LH_yy_wm",	"beta5_LH_yy_mw",	
  "beta5_HL_yy_ww",	"beta5_HL_yy_mm",	"beta5_HL_yy_wm",	"beta5_HL_yy_mw",	
  "beta6_LL_yy_ww",	"beta6_LL_yy_mm",	"beta6_LL_yy_wm",	"beta6_LL_yy_mw",	
  "beta6_LL_oo_ww",	"beta6_LL_oo_mm",	"beta6_LL_oo_wm",	"beta6_LL_oo_mw",	
  "beta6_LL_yo_ww",	"beta6_LL_yo_mm",	"beta6_LL_yo_wm",	"beta6_LL_yo_mw",	
  "beta6_HH_yy_ww",	"beta6_HH_yy_mm",	"beta6_HH_yy_wm",	"beta6_HH_yy_mw",	
  "beta6_HH_oo_ww",	"beta6_HH_oo_mm",	"beta6_HH_oo_wm",	"beta6_HH_oo_mw",	
  "beta6_HH_yo_ww",	"beta6_HH_yo_mm",	"beta6_HH_yo_wm",	"beta6_HH_yo_mw",	
  "beta6_LH_yy_ww",	"beta6_LH_yy_mm",	"beta6_LH_yy_wm",	"beta6_LH_yy_mw",	
  "beta6_HL_yy_ww",	"beta6_HL_yy_mm",	"beta6_HL_yy_wm",	"beta6_HL_yy_mw",	
  "beta7_LL_yy_ww",	"beta7_LL_yy_mm",	"beta7_LL_yy_wm",	"beta7_LL_yy_mw",	
  "beta7_LL_oo_ww",	"beta7_LL_oo_mm",	"beta7_LL_oo_wm",	"beta7_LL_oo_mw",	
  "beta7_LL_yo_ww",	"beta7_LL_yo_mm",	"beta7_LL_yo_wm",	"beta7_LL_yo_mw",	
  "beta7_HH_yy_ww",	"beta7_HH_yy_mm",	"beta7_HH_yy_wm",	"beta7_HH_yy_mw",	
  "beta7_HH_oo_ww",	"beta7_HH_oo_mm",	"beta7_HH_oo_wm",	"beta7_HH_oo_mw",	
  "beta7_HH_yo_ww",	"beta7_HH_yo_mm",	"beta7_HH_yo_wm",	"beta7_HH_yo_mw",	
  "beta7_LH_yy_ww",	"beta7_LH_yy_mm",	"beta7_LH_yy_wm",	"beta7_LH_yy_mw",	
  "beta7_HL_yy_ww",	"beta7_HL_yy_mm",	"beta7_HL_yy_wm",	"beta7_HL_yy_mw",	
  "beta8_LL_yy_ww",	"beta8_LL_yy_mm",	"beta8_LL_yy_wm",	"beta8_LL_yy_mw",	
  "beta8_LL_oo_ww",	"beta8_LL_oo_mm",	"beta8_LL_oo_wm",	"beta8_LL_oo_mw",	
  "beta8_LL_yo_ww",	"beta8_LL_yo_mm",	"beta8_LL_yo_wm",	"beta8_LL_yo_mw",	
  "beta8_HH_yy_ww",	"beta8_HH_yy_mm",	"beta8_HH_yy_wm",	"beta8_HH_yy_mw",	
  "beta8_HH_oo_ww",	"beta8_HH_oo_mm",	"beta8_HH_oo_wm",	"beta8_HH_oo_mw",	
  "beta8_HH_yo_ww",	"beta8_HH_yo_mm",	"beta8_HH_yo_wm",	"beta8_HH_yo_mw",	
  "beta8_LH_yy_ww",	"beta8_LH_yy_mm",	"beta8_LH_yy_wm",	"beta8_LH_yy_mw",	
  "beta8_HL_yy_ww",	"beta8_HL_yy_mm",	"beta8_HL_yy_wm",	"beta8_HL_yy_mw",
  "died_latency.y", "died_latency.o", 
  "died_rate_L.y_w", "died_rate_L.y_m", "died_rate_L.o_w", "died_rate_L.o_m", 
  "died_rate_H.y_w", "died_rate_H.y_m", "died_rate_H.o_w", "died_rate_H.o_m", 
  "t0_morts","DLsd")
    
init <- c(
  0.6*gamma, 0.6*gamma, 0.6*gamma, 0.6*gamma, 
  0.6*gamma, 0.6*gamma, 0.6*gamma, 0.6*gamma, 
  0.6*gamma, 0.6*gamma, 0.6*gamma, 0.6*gamma, 
  0.6*gamma, 0.6*gamma, 0.6*gamma, 0.6*gamma, 
  0.6*gamma, 0.6*gamma, 0.6*gamma, 0.6*gamma, 
  0.6*gamma, 0.6*gamma, 0.6*gamma, 0.6*gamma, 
  0.6*gamma, 0.6*gamma, 0.6*gamma, 0.6*gamma, 
  0.6*gamma, 0.6*gamma, 0.6*gamma, 0.6*gamma,
  
  0.3*gamma, 0.3*gamma, 0.3*gamma, 0.3*gamma, 
  0.3*gamma, 0.3*gamma, 0.3*gamma, 0.3*gamma, 
  0.3*gamma, 0.3*gamma, 0.3*gamma, 0.3*gamma, 
  0.3*gamma, 0.3*gamma, 0.3*gamma, 0.3*gamma, 
  0.3*gamma, 0.3*gamma, 0.3*gamma, 0.3*gamma, 
  0.3*gamma, 0.3*gamma, 0.3*gamma, 0.3*gamma, 
  0.3*gamma, 0.3*gamma, 0.3*gamma, 0.3*gamma, 
  0.3*gamma, 0.3*gamma, 0.3*gamma, 0.3*gamma,
  
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma,
  
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma,
  
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma,
  
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma,
  
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma,
  
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma,
  
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma, 
  0.2*gamma, 0.2*gamma, 0.2*gamma, 0.2*gamma,
  
  21,19,  
  0.003,  0.006, 0.03, 0.06,
  0.003,  0.006, 0.03, 0.06,
  2, 5)
  

df_params <- data.frame(name = fit.paramnames,
                        min = c(
                          0.1*gamma, 0.1*gamma, 0.1*gamma, 0.1*gamma, 
                          0.1*gamma, 0.1*gamma, 0.1*gamma, 0.1*gamma, 
                          0.1*gamma, 0.1*gamma, 0.1*gamma, 0.1*gamma, 
                          0.1*gamma, 0.1*gamma, 0.1*gamma, 0.1*gamma, 
                          0.1*gamma, 0.1*gamma, 0.1*gamma, 0.1*gamma, 
                          0.1*gamma, 0.1*gamma, 0.1*gamma, 0.1*gamma, 
                          0.1*gamma, 0.1*gamma, 0.1*gamma, 0.1*gamma, 
                          0.1*gamma, 0.1*gamma, 0.1*gamma, 0.1*gamma,
                          
                          0.05*gamma, 0.05*gamma, 0.05*gamma, 0.05*gamma, 
                          0.05*gamma, 0.05*gamma, 0.05*gamma, 0.05*gamma, 
                          0.05*gamma, 0.05*gamma, 0.05*gamma, 0.05*gamma, 
                          0.05*gamma, 0.05*gamma, 0.05*gamma, 0.05*gamma, 
                          0.05*gamma, 0.05*gamma, 0.05*gamma, 0.05*gamma, 
                          0.05*gamma, 0.05*gamma, 0.05*gamma, 0.05*gamma, 
                          0.05*gamma, 0.05*gamma, 0.05*gamma, 0.05*gamma, 
                          0.05*gamma, 0.05*gamma, 0.05*gamma, 0.05*gamma,
                          
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma,
                          
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma,
                          
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma,
                          
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma,
                          
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma,
                          
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma,
                          
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma, 
                          0.02*gamma, 0.02*gamma, 0.02*gamma, 0.02*gamma,
                          
                          16,14,
                          0.001,  0.002, 0.003, 0.006,
                          0.001,  0.002, 0.003, 0.006,
                          0, 2),
                        max = c(
                          2*gamma, 2*gamma, 2*gamma, 2*gamma, 
                          2*gamma, 2*gamma, 2*gamma, 2*gamma, 
                          2*gamma, 2*gamma, 2*gamma, 2*gamma, 
                          2*gamma, 2*gamma, 2*gamma, 2*gamma, 
                          2*gamma, 2*gamma, 2*gamma, 2*gamma, 
                          2*gamma, 2*gamma, 2*gamma, 2*gamma, 
                          2*gamma, 2*gamma, 2*gamma, 2*gamma, 
                          2*gamma, 2*gamma, 2*gamma, 2*gamma,
                          
                          1.5*gamma, 1.5*gamma, 1.5*gamma, 1.5*gamma, 
                          1.5*gamma, 1.5*gamma, 1.5*gamma, 1.5*gamma, 
                          1.5*gamma, 1.5*gamma, 1.5*gamma, 1.5*gamma, 
                          1.5*gamma, 1.5*gamma, 1.5*gamma, 1.5*gamma, 
                          1.5*gamma, 1.5*gamma, 1.5*gamma, 1.5*gamma, 
                          1.5*gamma, 1.5*gamma, 1.5*gamma, 1.5*gamma, 
                          1.5*gamma, 1.5*gamma, 1.5*gamma, 1.5*gamma, 
                          1.5*gamma, 1.5*gamma, 1.5*gamma, 1.5*gamma,
                          
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma,
                          
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma,
                          
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma,
                          
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma,
                          
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma,
                          
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma,
                          
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma, 
                          1*gamma, 1*gamma, 1*gamma, 1*gamma,
                          
                          24,22,
                          0.01,  0.01, 0.12, 0.12,
                          0.01,  0.01, 0.12, 0.12,
                          10, 9),
                        init = init)
