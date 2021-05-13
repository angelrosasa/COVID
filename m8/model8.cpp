#include <R.h>

#include <iostream>

static const int PARAM_N = 308;
static double parms[PARAM_N];


#define beta0_LL_yy_ww parms[0]
#define beta0_LL_yy_mm parms[1]
#define beta0_LL_yy_wm parms[2]
#define beta0_LL_yy_mw parms[3]

#define beta0_LL_oo_ww parms[4]
#define beta0_LL_oo_mm parms[5]
#define beta0_LL_oo_wm parms[6]
#define beta0_LL_oo_mw parms[7]

#define beta0_LL_yo_ww parms[8]
#define beta0_LL_yo_mm parms[9]
#define beta0_LL_yo_wm parms[10]
#define beta0_LL_yo_mw parms[11]

#define beta0_HH_yy_ww parms[12]
#define beta0_HH_yy_mm parms[13]
#define beta0_HH_yy_wm parms[14]
#define beta0_HH_yy_mw parms[15]

#define beta0_HH_oo_ww parms[16]
#define beta0_HH_oo_mm parms[17]
#define beta0_HH_oo_wm parms[18]
#define beta0_HH_oo_mw parms[19]

#define beta0_HH_yo_ww parms[20]
#define beta0_HH_yo_mm parms[21]
#define beta0_HH_yo_wm parms[22]
#define beta0_HH_yo_mw parms[23]

#define beta0_LH_yy_ww parms[24]
#define beta0_LH_yy_mm parms[25]
#define beta0_LH_yy_wm parms[26]
#define beta0_LH_yy_mw parms[27]

#define beta0_HL_yy_ww parms[28]
#define beta0_HL_yy_mm parms[29]
#define beta0_HL_yy_wm parms[30]
#define beta0_HL_yy_mw parms[31]


#define beta1_LL_yy_ww parms[32]
#define beta1_LL_yy_mm parms[33]
#define beta1_LL_yy_wm parms[34]
#define beta1_LL_yy_mw parms[35]

#define beta1_LL_oo_ww parms[36]
#define beta1_LL_oo_mm parms[37]
#define beta1_LL_oo_wm parms[38]
#define beta1_LL_oo_mw parms[39]

#define beta1_LL_yo_ww parms[40]
#define beta1_LL_yo_mm parms[41]
#define beta1_LL_yo_wm parms[42]
#define beta1_LL_yo_mw parms[43]


#define beta1_HH_yy_ww parms[44]
#define beta1_HH_yy_mm parms[45]
#define beta1_HH_yy_wm parms[46]
#define beta1_HH_yy_mw parms[47]

#define beta1_HH_oo_ww parms[48]
#define beta1_HH_oo_mm parms[49]
#define beta1_HH_oo_wm parms[50]
#define beta1_HH_oo_mw parms[51]

#define beta1_HH_yo_ww parms[52]
#define beta1_HH_yo_mm parms[53]
#define beta1_HH_yo_wm parms[54]
#define beta1_HH_yo_mw parms[55]

#define beta1_LH_yy_ww parms[56]
#define beta1_LH_yy_mm parms[57]
#define beta1_LH_yy_wm parms[58]
#define beta1_LH_yy_mw parms[59]

#define beta1_HL_yy_ww parms[60]
#define beta1_HL_yy_mm parms[61]
#define beta1_HL_yy_wm parms[62]
#define beta1_HL_yy_mw parms[63]


#define beta2_LL_yy_ww parms[64]
#define beta2_LL_yy_mm parms[65]
#define beta2_LL_yy_wm parms[66]
#define beta2_LL_yy_mw parms[67]

#define beta2_LL_oo_ww parms[68]
#define beta2_LL_oo_mm parms[69]
#define beta2_LL_oo_wm parms[70]
#define beta2_LL_oo_mw parms[71]

#define beta2_LL_yo_ww parms[72]
#define beta2_LL_yo_mm parms[73]
#define beta2_LL_yo_wm parms[74]
#define beta2_LL_yo_mw parms[75]


#define beta2_HH_yy_ww parms[76]
#define beta2_HH_yy_mm parms[77]
#define beta2_HH_yy_wm parms[78]
#define beta2_HH_yy_mw parms[79]

#define beta2_HH_oo_ww parms[80]
#define beta2_HH_oo_mm parms[81]
#define beta2_HH_oo_wm parms[82]
#define beta2_HH_oo_mw parms[83]

#define beta2_HH_yo_ww parms[84]
#define beta2_HH_yo_mm parms[85]
#define beta2_HH_yo_wm parms[86]
#define beta2_HH_yo_mw parms[87]

#define beta2_LH_yy_ww parms[88]
#define beta2_LH_yy_mm parms[89]
#define beta2_LH_yy_wm parms[90]
#define beta2_LH_yy_mw parms[91]

#define beta2_HL_yy_ww parms[92]
#define beta2_HL_yy_mm parms[93]
#define beta2_HL_yy_wm parms[94]
#define beta2_HL_yy_mw parms[95]


#define beta3_LL_yy_ww parms[96]
#define beta3_LL_yy_mm parms[97]
#define beta3_LL_yy_wm parms[98]
#define beta3_LL_yy_mw parms[99]

#define beta3_LL_oo_ww parms[100]
#define beta3_LL_oo_mm parms[101]
#define beta3_LL_oo_wm parms[102]
#define beta3_LL_oo_mw parms[103]

#define beta3_LL_yo_ww parms[104]
#define beta3_LL_yo_mm parms[105]
#define beta3_LL_yo_wm parms[106]
#define beta3_LL_yo_mw parms[107]


#define beta3_HH_yy_ww parms[108]
#define beta3_HH_yy_mm parms[109]
#define beta3_HH_yy_wm parms[110]
#define beta3_HH_yy_mw parms[111]

#define beta3_HH_oo_ww parms[112]
#define beta3_HH_oo_mm parms[113]
#define beta3_HH_oo_wm parms[114]
#define beta3_HH_oo_mw parms[115]

#define beta3_HH_yo_ww parms[116]
#define beta3_HH_yo_mm parms[117]
#define beta3_HH_yo_wm parms[118]
#define beta3_HH_yo_mw parms[119]

#define beta3_LH_yy_ww parms[120]
#define beta3_LH_yy_mm parms[121]
#define beta3_LH_yy_wm parms[122]
#define beta3_LH_yy_mw parms[123]

#define beta3_HL_yy_ww parms[124]
#define beta3_HL_yy_mm parms[125]
#define beta3_HL_yy_wm parms[126]
#define beta3_HL_yy_mw parms[127]


#define beta4_LL_yy_ww parms[128]
#define beta4_LL_yy_mm parms[129]
#define beta4_LL_yy_wm parms[130]
#define beta4_LL_yy_mw parms[131]

#define beta4_LL_oo_ww parms[132]
#define beta4_LL_oo_mm parms[133]
#define beta4_LL_oo_wm parms[134]
#define beta4_LL_oo_mw parms[135]

#define beta4_LL_yo_ww parms[136]
#define beta4_LL_yo_mm parms[137]
#define beta4_LL_yo_wm parms[138]
#define beta4_LL_yo_mw parms[139]


#define beta4_HH_yy_ww parms[140]
#define beta4_HH_yy_mm parms[141]
#define beta4_HH_yy_wm parms[142]
#define beta4_HH_yy_mw parms[143]

#define beta4_HH_oo_ww parms[144]
#define beta4_HH_oo_mm parms[145]
#define beta4_HH_oo_wm parms[146]
#define beta4_HH_oo_mw parms[147]

#define beta4_HH_yo_ww parms[148]
#define beta4_HH_yo_mm parms[149]
#define beta4_HH_yo_wm parms[150]
#define beta4_HH_yo_mw parms[151]

#define beta4_LH_yy_ww parms[152]
#define beta4_LH_yy_mm parms[153]
#define beta4_LH_yy_wm parms[154]
#define beta4_LH_yy_mw parms[155]

#define beta4_HL_yy_ww parms[156]
#define beta4_HL_yy_mm parms[157]
#define beta4_HL_yy_wm parms[158]
#define beta4_HL_yy_mw parms[159]


#define beta5_LL_yy_ww parms[160]
#define beta5_LL_yy_mm parms[161]
#define beta5_LL_yy_wm parms[162]
#define beta5_LL_yy_mw parms[163]

#define beta5_LL_oo_ww parms[164]
#define beta5_LL_oo_mm parms[165]
#define beta5_LL_oo_wm parms[166]
#define beta5_LL_oo_mw parms[167]

#define beta5_LL_yo_ww parms[168]
#define beta5_LL_yo_mm parms[169]
#define beta5_LL_yo_wm parms[170]
#define beta5_LL_yo_mw parms[171]


#define beta5_HH_yy_ww parms[172]
#define beta5_HH_yy_mm parms[173]
#define beta5_HH_yy_wm parms[174]
#define beta5_HH_yy_mw parms[175]

#define beta5_HH_oo_ww parms[176]
#define beta5_HH_oo_mm parms[177]
#define beta5_HH_oo_wm parms[178]
#define beta5_HH_oo_mw parms[179]

#define beta5_HH_yo_ww parms[180]
#define beta5_HH_yo_mm parms[181]
#define beta5_HH_yo_wm parms[182]
#define beta5_HH_yo_mw parms[183]

#define beta5_LH_yy_ww parms[184]
#define beta5_LH_yy_mm parms[185]
#define beta5_LH_yy_wm parms[186]
#define beta5_LH_yy_mw parms[187]

#define beta5_HL_yy_ww parms[188]
#define beta5_HL_yy_mm parms[189]
#define beta5_HL_yy_wm parms[190]
#define beta5_HL_yy_mw parms[191]


#define beta6_LL_yy_ww parms[192]
#define beta6_LL_yy_mm parms[193]
#define beta6_LL_yy_wm parms[194]
#define beta6_LL_yy_mw parms[195]

#define beta6_LL_oo_ww parms[196]
#define beta6_LL_oo_mm parms[197]
#define beta6_LL_oo_wm parms[198]
#define beta6_LL_oo_mw parms[199]

#define beta6_LL_yo_ww parms[200]
#define beta6_LL_yo_mm parms[201]
#define beta6_LL_yo_wm parms[202]
#define beta6_LL_yo_mw parms[203]


#define beta6_HH_yy_ww parms[204]
#define beta6_HH_yy_mm parms[205]
#define beta6_HH_yy_wm parms[206]
#define beta6_HH_yy_mw parms[207]

#define beta6_HH_oo_ww parms[208]
#define beta6_HH_oo_mm parms[209]
#define beta6_HH_oo_wm parms[210]
#define beta6_HH_oo_mw parms[211]

#define beta6_HH_yo_ww parms[212]
#define beta6_HH_yo_mm parms[213]
#define beta6_HH_yo_wm parms[214]
#define beta6_HH_yo_mw parms[215]

#define beta6_LH_yy_ww parms[216]
#define beta6_LH_yy_mm parms[217]
#define beta6_LH_yy_wm parms[218]
#define beta6_LH_yy_mw parms[219]

#define beta6_HL_yy_ww parms[220]
#define beta6_HL_yy_mm parms[221]
#define beta6_HL_yy_wm parms[222]
#define beta6_HL_yy_mw parms[223]


#define beta7_LL_yy_ww parms[224]
#define beta7_LL_yy_mm parms[225]
#define beta7_LL_yy_wm parms[226]
#define beta7_LL_yy_mw parms[227]

#define beta7_LL_oo_ww parms[228]
#define beta7_LL_oo_mm parms[229]
#define beta7_LL_oo_wm parms[230]
#define beta7_LL_oo_mw parms[231]

#define beta7_LL_yo_ww parms[232]
#define beta7_LL_yo_mm parms[233]
#define beta7_LL_yo_wm parms[234]
#define beta7_LL_yo_mw parms[235]


#define beta7_HH_yy_ww parms[236]
#define beta7_HH_yy_mm parms[237]
#define beta7_HH_yy_wm parms[238]
#define beta7_HH_yy_mw parms[239]

#define beta7_HH_oo_ww parms[240]
#define beta7_HH_oo_mm parms[241]
#define beta7_HH_oo_wm parms[242]
#define beta7_HH_oo_mw parms[243]

#define beta7_HH_yo_ww parms[244]
#define beta7_HH_yo_mm parms[245]
#define beta7_HH_yo_wm parms[246]
#define beta7_HH_yo_mw parms[247]

#define beta7_LH_yy_ww parms[248]
#define beta7_LH_yy_mm parms[249]
#define beta7_LH_yy_wm parms[250]
#define beta7_LH_yy_mw parms[251]

#define beta7_HL_yy_ww parms[252]
#define beta7_HL_yy_mm parms[253]
#define beta7_HL_yy_wm parms[254]
#define beta7_HL_yy_mw parms[255]

#define beta8_LL_yy_ww parms[256]
#define beta8_LL_yy_mm parms[257]
#define beta8_LL_yy_wm parms[258]
#define beta8_LL_yy_mw parms[259]

#define beta8_LL_oo_ww parms[260]
#define beta8_LL_oo_mm parms[261]
#define beta8_LL_oo_wm parms[262]
#define beta8_LL_oo_mw parms[263]

#define beta8_LL_yo_ww parms[264]
#define beta8_LL_yo_mm parms[265]
#define beta8_LL_yo_wm parms[266]
#define beta8_LL_yo_mw parms[267]


#define beta8_HH_yy_ww parms[268]
#define beta8_HH_yy_mm parms[269]
#define beta8_HH_yy_wm parms[270]
#define beta8_HH_yy_mw parms[271]

#define beta8_HH_oo_ww parms[272]
#define beta8_HH_oo_mm parms[273]
#define beta8_HH_oo_wm parms[274]
#define beta8_HH_oo_mw parms[275]

#define beta8_HH_yo_ww parms[276]
#define beta8_HH_yo_mm parms[277]
#define beta8_HH_yo_wm parms[278]
#define beta8_HH_yo_mw parms[279]

#define beta8_LH_yy_ww parms[280]
#define beta8_LH_yy_mm parms[281]
#define beta8_LH_yy_wm parms[282]
#define beta8_LH_yy_mw parms[283]

#define beta8_HL_yy_ww parms[284]
#define beta8_HL_yy_mm parms[285]
#define beta8_HL_yy_wm parms[286]
#define beta8_HL_yy_mw parms[287]

#define N_L_y_w parms[288]
#define N_L_y_m parms[289]
#define N_L_o_w parms[290]
#define N_L_o_m parms[291]

#define N_H_y_w parms[292]
#define N_H_y_m parms[293]
#define N_H_o_w parms[294]
#define N_H_o_m parms[295]

#define t0 parms[296] // Start of lockdown            beta0
#define t1 parms[297] // End of lockdown transition   beta1
#define t2 parms[298]
#define t3 parms[299]
#define t4 parms[300]
#define t5 parms[301]
#define t6 parms[302]
#define t7 parms[303]
#define t8 parms[304]

#define a1 parms[305]
#define a2 parms[306]

#define gamma parms[307]


#define S_L_y_w y[0]
#define E1_L_y_w y[1]
#define E2_L_y_w y[2]
#define I_L_y_w y[3]
#define R_L_y_w y[4]

#define S_L_y_m y[5]
#define E1_L_y_m y[6]
#define E2_L_y_m y[7]
#define I_L_y_m y[8]
#define R_L_y_m y[9]

#define S_L_o_w y[10]
#define E1_L_o_w y[11]
#define E2_L_o_w y[12]
#define I_L_o_w y[13]
#define R_L_o_w y[14]

#define S_L_o_m y[15]
#define E1_L_o_m y[16]
#define E2_L_o_m y[17]
#define I_L_o_m y[18]
#define R_L_o_m y[19]

#define S_H_y_w y[20]
#define E1_H_y_w y[21]
#define E2_H_y_w y[22]
#define I_H_y_w y[23]
#define R_H_y_w y[24]

#define S_H_y_m y[25]
#define E1_H_y_m y[26]
#define E2_H_y_m y[27]
#define I_H_y_m y[28]
#define R_H_y_m y[29]

#define S_H_o_w y[30]
#define E1_H_o_w y[31]
#define E2_H_o_w y[32]
#define I_H_o_w y[33]
#define R_H_o_w y[34]

#define S_H_o_m y[35]
#define E1_H_o_m y[36]
#define E2_H_o_m y[37]
#define I_H_o_m y[38]
#define R_H_o_m y[39]




/* initializer */
extern "C" {
  void initmod(void (* odeparms)(int *, double *))
  {
    int c=PARAM_N;
    odeparms(&c, parms);
  }
}

static double interpolate(double t,
			  double vt0, double vt1, double vt2, double vt3, 
			  double vt4, double vt5, double vt6, double vt7, double vt8)
{
  if (t > t8)
    return vt8;
  if (t > t7)
    return vt7 + (t - t7) / (t8 - t7) * (vt8 - vt7);
  else if (t > t6)
    return vt6 + (t - t6) / (t7 - t6) * (vt7 - vt6);
  else if (t > t5)
    return vt5 + (t - t5) / (t6 - t5) * (vt6 - vt5);
  else if (t > t4)
    return vt4 + (t - t4) / (t5 - t4) * (vt5 - vt4);
  else if (t > t3)
    return vt3 + (t - t3) / (t4 - t3) * (vt4 - vt3);
  else if (t > t2)
    return vt2 + (t - t2) / (t3 - t2) * (vt3 - vt2);
  else if (t > t1)
    return vt1 + (t - t1) / (t2 - t1) * (vt2 - vt1);
  else if (t > t0)
    return vt0 + (t - t0) / (t1 - t0) * (vt1 - vt0);
  else
    return vt0;
}

/* Derivatives and 1 output variable */
extern "C" {
  void derivs (int *neq, double *t, double *y, double *ydot,
	       double *yout, int *ip)
  {
    if (ip[0] < 34) error("nout should be at least 34");
    
    if (S_L_y_w < 0   || E1_L_y_w < 0   || E2_L_y_w < 0   || I_L_y_w < 0   || R_L_y_w < 0   ||
	S_L_y_w > N_L_y_w || E1_L_y_w > N_L_y_w || E2_L_y_w > N_L_y_w || I_L_y_w > N_L_y_w || R_L_y_w > N_L_y_w ||
	
	S_L_y_m < 0   || E1_L_y_m < 0 || E2_L_y_m < 0    || I_L_y_m < 0   || R_L_y_m < 0   ||
	S_L_y_m > N_L_y_m || E1_L_y_m > N_L_y_m || E2_L_y_m > N_L_y_m || I_L_y_m > N_L_y_m || R_L_y_m > N_L_y_m ||
	
	S_L_o_w < 0   || E1_L_o_w < 0 || E2_L_o_w < 0   || I_L_o_w < 0   || R_L_o_w < 0   ||
	S_L_o_w > N_L_o_w || E1_L_o_w > N_L_o_w  || E2_L_o_w > N_L_o_w  || I_L_o_w > N_L_o_w || R_L_o_w > N_L_o_w ||
	
	S_L_o_m < 0   || E1_L_o_m < 0 || E2_L_o_m < 0   || I_L_o_m < 0   || R_L_o_m < 0   ||
	S_L_o_m > N_L_o_m || E1_L_o_m > N_L_o_m || E2_L_o_m > N_L_o_m || I_L_o_m > N_L_o_m || R_L_o_m > N_L_o_m ||
	
	S_H_y_w < 0   || E1_H_y_w < 0   || E2_H_y_w < 0   || I_H_y_w < 0   || R_H_y_w < 0   ||
	S_H_y_w > N_H_y_w || E1_H_y_w > N_H_y_w || E2_H_y_w > N_H_y_w || I_H_y_w > N_H_y_w || R_H_y_w > N_H_y_w ||
	
	S_H_y_m < 0   || E1_H_y_m < 0   || E2_H_y_m < 0   || I_H_y_m < 0   || R_H_y_m < 0   ||
	S_H_y_m > N_H_y_m || E1_H_y_m > N_H_y_m || E2_H_y_m > N_H_y_m || I_H_y_m > N_H_y_m || R_H_y_m > N_H_y_m ||
	
	S_H_o_w < 0   || E1_H_o_w < 0   || E2_H_o_w < 0   || I_H_o_w < 0   || R_H_o_w < 0   ||
	S_H_o_w > N_H_o_w || E1_H_o_w > N_H_o_w || E2_H_o_w > N_H_o_w || I_H_o_w > N_H_o_w || R_H_o_w > N_H_o_w ||
	
	S_H_o_m < 0   || E1_H_o_m < 0   || E2_H_o_m < 0   || I_H_o_m < 0   || R_H_o_m < 0   ||
	S_H_o_m > N_H_o_m || E1_H_o_m > N_H_o_m || E2_H_o_m > N_H_o_m || I_H_o_m > N_H_o_m || R_H_o_m > N_H_o_m){
      
      std::cerr << "Oops ?" << std::endl;
      /* dS_L_y_w  */ ydot[0] = 0;
      /* dE1_L_y_w  */ ydot[1] = 0;
      /* dE2_L_y_w  */ ydot[2] = 0; 
      /* dI_L_y_w  */ ydot[3] = 0;
      /* dR_L_y_w  */ ydot[4] = 0;
       
       /* dS_L_y_m  */ ydot[5] = 0;
       /* dE1_L_y_m  */ ydot[6] = 0;
       /* dE2_L_y_m  */ ydot[7] = 0;
       /* dI_L_y_m  */ ydot[8] = 0;
       /* dR_L_y_m  */ ydot[9] = 0;
        
       /* dS_L_o_w  */ ydot[10] = 0;
       /* dE1_L_o_w  */ ydot[11] = 0;
      /* dE2_L_o_w  */ ydot[12] = 0;
       /* dI_L_o_w  */ ydot[13] = 0;
       /* dR_L_o_w  */ ydot[14] = 0;
       
         /* dS_L_o_m  */ ydot[15] = 0;
         /* dE1_L_o_m  */ ydot[16] = 0;
         /* dE2_L_o_m  */ ydot[17] = 0;  
         /* dI_L_o_m  */ ydot[18] = 0;
         /* dR_L_o_m  */ ydot[19] = 0;
        
        /* dS_H_y_w  */ ydot[20] = 0;
        /* dE1_H_y_w  */ ydot[21] = 0;
        /* dE2_H_y_w  */ ydot[22] = 0;
        /* dI_H_y_w  */ ydot[23] = 0;
        /* dR_H_y_w  */ ydot[24] = 0;
        
        /* dS_H_y_m  */ ydot[25] = 0;
        /* dE1_H_y_m  */ ydot[26] = 0;
        /* dE2_H_y_m  */ ydot[27] = 0;
        /* dI_H_y_m  */ ydot[28] = 0;
        /* dR_H_y_m  */ ydot[29] = 0;
        
        /* dS_H_o_w  */ ydot[30] = 0;
        /* dE1_H_o_w  */ ydot[31] = 0;
        /* dE2_H_o_w  */ ydot[32] = 0;
        /* dI_H_o_w  */ ydot[33] = 0;
        /* dR_H_o_w  */ ydot[34] = 0;
        
        /* dS_H_o_m  */ ydot[35] = 0;
        /* dE1_H_o_m  */ ydot[36] = 0;
        /* dE2_H_o_m  */ ydot[37] = 0;
        /* dI_H_o_m  */ ydot[38] = 0;
        /* dR_H_o_m  */ ydot[39] = 0; 
        
      return;
    }

    const double beta_LL_yy_ww = interpolate(*t,
                                				     beta0_LL_yy_ww, beta1_LL_yy_ww, beta2_LL_yy_ww, beta3_LL_yy_ww, 
                                				     beta4_LL_yy_ww, beta5_LL_yy_ww, beta6_LL_yy_ww, beta7_LL_yy_ww, beta8_LL_yy_ww);
    const double beta_LL_yy_mm = interpolate(*t,
                                              beta0_LL_yy_mm, beta1_LL_yy_mm, beta2_LL_yy_mm, beta3_LL_yy_mm, 
                                              beta4_LL_yy_mm, beta5_LL_yy_mm, beta6_LL_yy_mm, beta7_LL_yy_mm, beta8_LL_yy_mm);
    const double beta_LL_yy_wm = interpolate(*t,
                                              beta0_LL_yy_wm, beta1_LL_yy_wm, beta2_LL_yy_wm, beta3_LL_yy_wm, 
                                              beta4_LL_yy_wm, beta5_LL_yy_wm, beta6_LL_yy_wm, beta7_LL_yy_wm, beta8_LL_yy_wm);
    
    const double beta_LL_yy_mw = interpolate(*t,
                                             beta0_LL_yy_mw, beta1_LL_yy_mw, beta2_LL_yy_mw, beta3_LL_yy_mw, 
                                             beta4_LL_yy_mw, beta5_LL_yy_mw, beta6_LL_yy_mw, beta7_LL_yy_mw, beta8_LL_yy_mw);
    
    const double beta_LL_oo_ww = interpolate(*t,
                                             beta0_LL_oo_ww, beta1_LL_oo_ww, beta2_LL_oo_ww, beta3_LL_oo_ww, 
                                             beta4_LL_oo_ww, beta5_LL_oo_ww, beta6_LL_oo_ww, beta7_LL_oo_ww, beta8_LL_oo_ww);
    const double beta_LL_oo_mm = interpolate(*t,
                                             beta0_LL_oo_mm, beta1_LL_oo_mm, beta2_LL_oo_mm, beta3_LL_oo_mm, 
                                             beta4_LL_oo_mm, beta5_LL_oo_mm, beta6_LL_oo_mm, beta7_LL_oo_mm, beta8_LL_oo_mm);
    const double beta_LL_oo_wm = interpolate(*t,
                                             beta0_LL_oo_wm, beta1_LL_oo_wm, beta2_LL_oo_wm, beta3_LL_oo_wm, 
                                             beta4_LL_oo_wm, beta5_LL_oo_wm, beta6_LL_oo_wm, beta7_LL_oo_wm, beta8_LL_oo_wm);
    
    const double beta_LL_oo_mw = interpolate(*t,
                                             beta0_LL_oo_mw, beta1_LL_oo_mw, beta2_LL_oo_mw, beta3_LL_oo_mw, 
                                             beta4_LL_oo_mw, beta5_LL_oo_mw, beta6_LL_oo_mw, beta7_LL_oo_mw, beta8_LL_oo_mw);
    
    const double beta_LL_yo_ww = interpolate(*t,
                                             beta0_LL_yo_ww, beta1_LL_yo_ww, beta2_LL_yo_ww, beta3_LL_yo_ww, 
                                             beta4_LL_yo_ww, beta5_LL_yo_ww, beta6_LL_yo_ww, beta7_LL_yo_ww, beta8_LL_yo_ww);
    const double beta_LL_yo_mm = interpolate(*t,
                                             beta0_LL_yo_mm, beta1_LL_yo_mm, beta2_LL_yo_mm, beta3_LL_yo_mm, 
                                             beta4_LL_yo_mm, beta5_LL_yo_mm, beta6_LL_yo_mm, beta7_LL_yo_mm, beta8_LL_yo_mm);
    const double beta_LL_yo_wm = interpolate(*t,
                                             beta0_LL_yo_wm, beta1_LL_yo_wm, beta2_LL_yo_wm, beta3_LL_yo_wm, 
                                             beta4_LL_yo_wm, beta5_LL_yo_wm, beta6_LL_yo_wm, beta7_LL_yo_wm, beta8_LL_yo_wm);
    
    const double beta_LL_yo_mw = interpolate(*t,
                                             beta0_LL_yo_mw, beta1_LL_yo_mw, beta2_LL_yo_mw, beta3_LL_yo_mw, 
                                             beta4_LL_yo_mw, beta5_LL_yo_mw, beta6_LL_yo_mw, beta7_LL_yo_mw, beta8_LL_yo_mw);
    
    const double beta_HH_yy_ww = interpolate(*t,
                                             beta0_HH_yy_ww, beta1_HH_yy_ww, beta2_HH_yy_ww, beta3_HH_yy_ww, 
                                             beta4_HH_yy_ww, beta5_HH_yy_ww, beta6_HH_yy_ww, beta7_HH_yy_ww, beta8_HH_yy_ww);
    const double beta_HH_yy_mm = interpolate(*t,
                                             beta0_HH_yy_mm, beta1_HH_yy_mm, beta2_HH_yy_mm, beta3_HH_yy_mm, 
                                             beta4_HH_yy_mm, beta5_HH_yy_mm, beta6_HH_yy_mm, beta7_HH_yy_mm, beta8_HH_yy_mm);
    const double beta_HH_yy_wm = interpolate(*t,
                                             beta0_HH_yy_wm, beta1_HH_yy_wm, beta2_HH_yy_wm, beta3_HH_yy_wm, 
                                             beta4_HH_yy_wm, beta5_HH_yy_wm, beta6_HH_yy_wm, beta7_HH_yy_wm, beta8_HH_yy_wm);
    
    const double beta_HH_yy_mw = interpolate(*t,
                                             beta0_HH_yy_mw, beta1_HH_yy_mw, beta2_HH_yy_mw, beta3_HH_yy_mw, 
                                             beta4_HH_yy_mw, beta5_HH_yy_mw, beta6_HH_yy_mw, beta7_HH_yy_mw, beta8_HH_yy_mw);
    
    const double beta_HH_oo_ww = interpolate(*t,
                                             beta0_HH_oo_ww, beta1_HH_oo_ww, beta2_HH_oo_ww, beta3_HH_oo_ww, 
                                             beta4_HH_oo_ww, beta5_HH_oo_ww, beta6_HH_oo_ww, beta7_HH_oo_ww, beta8_HH_oo_ww);
    const double beta_HH_oo_mm = interpolate(*t,
                                             beta0_HH_oo_mm, beta1_HH_oo_mm, beta2_HH_oo_mm, beta3_HH_oo_mm, 
                                             beta4_HH_oo_mm, beta5_HH_oo_mm, beta6_HH_oo_mm, beta7_HH_oo_mm, beta8_HH_oo_mm);
    const double beta_HH_oo_wm = interpolate(*t,
                                             beta0_HH_oo_wm, beta1_HH_oo_wm, beta2_HH_oo_wm, beta3_HH_oo_wm, 
                                             beta4_HH_oo_wm, beta5_HH_oo_wm, beta6_HH_oo_wm, beta7_HH_oo_wm, beta8_HH_oo_wm);
    
    const double beta_HH_oo_mw = interpolate(*t,
                                             beta0_HH_oo_mw, beta1_HH_oo_mw, beta2_HH_oo_mw, beta3_HH_oo_mw, 
                                             beta4_HH_oo_mw, beta5_HH_oo_mw, beta6_HH_oo_mw, beta7_HH_oo_mw, beta8_HH_oo_mw);
    
    const double beta_HH_yo_ww = interpolate(*t,
                                             beta0_HH_yo_ww, beta1_HH_yo_ww, beta2_HH_yo_ww, beta3_HH_yo_ww, 
                                             beta4_HH_yo_ww, beta5_HH_yo_ww, beta6_HH_yo_ww, beta7_HH_yo_ww, beta8_HH_yo_ww);
    const double beta_HH_yo_mm = interpolate(*t,
                                             beta0_HH_yo_mm, beta1_HH_yo_mm, beta2_HH_yo_mm, beta3_HH_yo_mm, 
                                             beta4_HH_yo_mm, beta5_HH_yo_mm, beta6_HH_yo_mm, beta7_HH_yo_mm, beta8_HH_yo_mm);
    const double beta_HH_yo_wm = interpolate(*t,
                                             beta0_HH_yo_wm, beta1_HH_yo_wm, beta2_HH_yo_wm, beta3_HH_yo_wm, 
                                             beta4_HH_yo_wm, beta5_HH_yo_wm, beta6_HH_yo_wm, beta7_HH_yo_wm, beta8_HH_yo_wm);
    
    const double beta_HH_yo_mw = interpolate(*t,
                                             beta0_HH_yo_mw, beta1_HH_yo_mw, beta2_HH_yo_mw, beta3_HH_yo_mw, 
                                             beta4_HH_yo_mw, beta5_HH_yo_mw, beta6_HH_yo_mw, beta7_HH_yo_mw, beta8_HH_yo_mw);
    
    const double beta_LH_yy_ww = interpolate(*t,
                                             beta0_LH_yy_ww, beta1_LH_yy_ww, beta2_LH_yy_ww, beta3_LH_yy_ww, 
                                             beta4_LH_yy_ww, beta5_LH_yy_ww, beta6_LH_yy_ww, beta7_LH_yy_ww, beta8_LH_yy_ww);
    const double beta_LH_yy_wm = interpolate(*t,
                                             beta0_LH_yy_wm, beta1_LH_yy_wm, beta2_LH_yy_wm, beta3_LH_yy_wm, 
                                             beta4_LH_yy_wm, beta5_LH_yy_wm, beta6_LH_yy_wm, beta7_LH_yy_wm, beta8_LH_yy_wm);
    const double beta_LH_yy_mm = interpolate(*t,
                                             beta0_LH_yy_mm, beta1_LH_yy_mm, beta2_LH_yy_mm, beta3_LH_yy_mm, 
                                             beta4_LH_yy_mm, beta5_LH_yy_mm, beta6_LH_yy_mm, beta7_LH_yy_mm, beta8_LH_yy_mm);
    const double beta_LH_yy_mw = interpolate(*t,
                                             beta0_LH_yy_mw, beta1_LH_yy_mw, beta2_LH_yy_mw, beta3_LH_yy_mw, 
                                             beta4_LH_yy_mw, beta5_LH_yy_mw, beta6_LH_yy_mw, beta7_LH_yy_mw, beta8_LH_yy_mw);
    const double beta_HL_yy_ww = interpolate(*t,
                                             beta0_HL_yy_ww, beta1_HL_yy_ww, beta2_HL_yy_ww, beta3_HL_yy_ww, 
                                             beta4_HL_yy_ww, beta5_HL_yy_ww, beta6_HL_yy_ww, beta7_HL_yy_ww, beta8_HL_yy_ww);
    const double beta_HL_yy_wm = interpolate(*t,
                                             beta0_HL_yy_wm, beta1_HL_yy_wm, beta2_HL_yy_wm, beta3_HL_yy_wm, 
                                             beta4_HL_yy_wm, beta5_HL_yy_wm, beta6_HL_yy_wm, beta7_HL_yy_wm, beta8_HL_yy_wm);
    const double beta_HL_yy_mm = interpolate(*t,
                                             beta0_HL_yy_mm, beta1_HL_yy_mm, beta2_HL_yy_mm, beta3_HL_yy_mm, 
                                             beta4_HL_yy_mm, beta5_HL_yy_mm, beta6_HL_yy_mm, beta7_HL_yy_mm, beta8_HL_yy_mm);
    const double beta_HL_yy_mw = interpolate(*t,
                                             beta0_HL_yy_mw, beta1_HL_yy_mw, beta2_HL_yy_mw, beta3_HL_yy_mw, 
                                             beta4_HL_yy_mw, beta5_HL_yy_mw, beta6_HL_yy_mw, beta7_HL_yy_mw, beta8_HL_yy_mw);
    
    
    const double LL_yy_ww_infecting = (beta_LL_yy_ww * I_L_y_w) / N_L_y_w;
    const double LL_yy_mm_infecting = (beta_LL_yy_mm * I_L_y_m) / N_L_y_m;
    const double LL_yy_wm_infecting = (beta_LL_yy_wm * I_L_y_w) / N_L_y_w;
    const double LL_yy_mw_infecting = (beta_LL_yy_mw * I_L_y_m) / N_L_y_m;
    
    const double LL_oo_ww_infecting = (beta_LL_oo_ww * I_L_o_w) / N_L_o_w;
    const double LL_oo_mm_infecting = (beta_LL_oo_mm * I_L_o_m) / N_L_o_m;
    const double LL_oo_wm_infecting = (beta_LL_oo_wm * I_L_o_w) / N_L_o_w;
    const double LL_oo_mw_infecting = (beta_LL_oo_mw * I_L_o_m) / N_L_o_m;
    
    const double LL_yo_ww_infecting = (beta_LL_yo_ww * I_L_y_w) / N_L_y_w;
    const double LL_yo_mm_infecting = (beta_LL_yo_mm * I_L_y_m) / N_L_y_m;
    const double LL_yo_wm_infecting = (beta_LL_yo_wm * I_L_y_w) / N_L_y_w;
    const double LL_yo_mw_infecting = (beta_LL_yo_mw * I_L_y_m) / N_L_y_m;
    
    
    
    const double HH_yy_ww_infecting = (beta_HH_yy_ww * I_H_y_w) / N_H_y_w;
    const double HH_yy_mm_infecting = (beta_HH_yy_mm * I_H_y_m) / N_H_y_m;
    const double HH_yy_wm_infecting = (beta_HH_yy_wm * I_H_y_w) / N_H_y_w;
    const double HH_yy_mw_infecting = (beta_HH_yy_mw * I_H_y_m) / N_H_y_m;
    
    const double HH_oo_ww_infecting = (beta_HH_oo_ww * I_H_o_w) / N_H_o_w;
    const double HH_oo_mm_infecting = (beta_HH_oo_mm * I_H_o_m) / N_H_o_m;
    const double HH_oo_wm_infecting = (beta_HH_oo_wm * I_H_o_w) / N_H_o_w;
    const double HH_oo_mw_infecting = (beta_HH_oo_mw * I_H_o_m) / N_H_o_m;
    
    const double HH_yo_ww_infecting = (beta_HH_yo_ww * I_H_y_w) / N_H_y_w;
    const double HH_yo_mm_infecting = (beta_HH_yo_mm * I_H_y_m) / N_H_y_m;
    const double HH_yo_wm_infecting = (beta_HH_yo_wm * I_H_y_w) / N_H_y_w;
    const double HH_yo_mw_infecting = (beta_HH_yo_mw * I_H_y_m) / N_H_y_m;
    
    const double LH_yy_ww_infecting = (beta_LH_yy_ww * I_L_y_w) / N_L_y_w;
    const double LH_yy_mm_infecting = (beta_LH_yy_mm * I_L_y_m) / N_L_y_m;
    const double LH_yy_wm_infecting = (beta_LH_yy_wm * I_L_y_w) / N_L_y_w;
    const double LH_yy_mw_infecting = (beta_LH_yy_mw * I_L_y_m) / N_L_y_m;
    
    const double HL_yy_ww_infecting = (beta_HL_yy_ww * I_H_y_w) / N_H_y_w;
    const double HL_yy_mm_infecting = (beta_HL_yy_mm * I_H_y_m) / N_H_y_m;
    const double HL_yy_wm_infecting = (beta_HL_yy_wm * I_H_y_w) / N_H_y_w;
    const double HL_yy_mw_infecting = (beta_HL_yy_mw * I_H_y_m) / N_H_y_m;
    
    
    const double L_y_w_got_infected = (LL_yy_ww_infecting + LL_yy_mw_infecting +  HL_yy_ww_infecting +  HL_yy_mw_infecting) * S_L_y_w;
    const double L_y_w_got_latent = a1 * E1_L_y_w;
    const double L_y_w_got_infectious = a2 * E2_L_y_w;
    const double L_y_w_got_removed = gamma * I_L_y_w;
    
    const double L_y_m_got_infected = (LL_yy_mm_infecting + LL_yy_wm_infecting +  HL_yy_mm_infecting +  HL_yy_wm_infecting) * S_L_y_m;
    const double L_y_m_got_latent = a1 * E1_L_y_m;
    const double L_y_m_got_infectious = a2 * E2_L_y_m;
    const double L_y_m_got_removed = gamma * I_L_y_m;
    
    const double L_o_w_got_infected = (LL_oo_ww_infecting + LL_oo_mw_infecting +  LL_yo_ww_infecting +  LL_yo_mw_infecting) * S_L_o_w;
    const double L_o_w_got_latent = a1 * E1_L_o_w;
    const double L_o_w_got_infectious = a2 * E2_L_o_w;
    const double L_o_w_got_removed = gamma * I_L_o_w;
    
    const double L_o_m_got_infected = (LL_oo_mm_infecting + LL_oo_wm_infecting +  LL_yo_mm_infecting +  LL_yo_wm_infecting) * S_L_o_m;
    const double L_o_m_got_latent = a1 * E1_L_o_m;
    const double L_o_m_got_infectious = a2 * E2_L_o_m;
    const double L_o_m_got_removed = gamma * I_L_o_m;
    
    const double H_y_w_got_infected = (HH_yy_ww_infecting + HH_yy_mw_infecting +  HL_yy_ww_infecting +  HL_yy_mw_infecting) * S_H_y_w;
    const double H_y_w_got_latent = a1 * E1_H_y_w;
    const double H_y_w_got_infectious = a2 * E2_H_y_w;
    const double H_y_w_got_removed = gamma * I_H_y_w;
    
    const double H_y_m_got_infected = (HH_yy_mm_infecting + HH_yy_wm_infecting +  HL_yy_mm_infecting +  HL_yy_wm_infecting) * S_H_y_m;
    const double H_y_m_got_latent = a1 * E1_H_y_m;
    const double H_y_m_got_infectious = a2 * E2_H_y_m;
    const double H_y_m_got_removed = gamma * I_H_y_m;
    
    const double H_o_w_got_infected = (HH_oo_ww_infecting + HH_oo_mw_infecting +  HH_yo_ww_infecting +  HH_yo_mw_infecting) * S_H_o_w;
    const double H_o_w_got_latent = a1 * E1_H_o_w;
    const double H_o_w_got_infectious = a2 * E2_H_o_w;
    const double H_o_w_got_removed = gamma * I_H_o_w;
    
    const double H_o_m_got_infected = (HH_oo_mm_infecting + HH_oo_wm_infecting +  HH_yo_mm_infecting +  HH_yo_wm_infecting) * S_H_o_m;
    const double H_o_m_got_latent = a1 * E1_H_o_m;
    const double H_o_m_got_infectious = a2 * E2_H_o_m;
    const double H_o_m_got_removed = gamma * I_H_o_m;
    
    /* dS_y_w  */ ydot[0] = -L_y_w_got_infected;
    /* dE1_y_w  */ ydot[1] = L_y_w_got_infected - L_y_w_got_latent;
    /* dE2_y_w  */ ydot[2] = L_y_w_got_latent - L_y_w_got_infectious;
    /* dI_y_w  */ ydot[3] = L_y_w_got_infectious - L_y_w_got_removed;
    /* dR_y_w  */ ydot[4] = L_y_w_got_removed;
     
     /* dS_y_m  */ ydot[5] = -L_y_m_got_infected;
     /* dE1_y_m  */ ydot[6] = L_y_m_got_infected - L_y_m_got_latent;
     /* dE2_y_m  */ ydot[7] = L_y_m_got_latent - L_y_m_got_infectious;
     /* dI_y_m  */ ydot[8] = L_y_m_got_infectious - L_y_m_got_removed;
     /* dR_y_m  */ ydot[9] = L_y_m_got_removed; 
      
      /* dS_o_w  */ ydot[10] = -L_o_w_got_infected;
      /* dE1_o_w  */ ydot[11] = L_o_w_got_infected - L_o_w_got_latent;
      /* dE2_o_w  */ ydot[12] = L_o_w_got_latent - L_o_w_got_infectious;
      /* dI_o_w  */ ydot[13] = L_o_w_got_infectious - L_o_w_got_removed;
      /* dR_o_w  */ ydot[14] = L_o_w_got_removed;    
  
      /* dS_o_m  */ ydot[15] = -L_o_m_got_infected;
      /* dE1_o_m  */ ydot[16] = L_o_m_got_infected - L_o_m_got_latent;
      /* dE2_o_m  */ ydot[17] = L_o_m_got_latent - L_o_m_got_infectious;
      /* dI_o_m  */ ydot[18] = L_o_m_got_infectious - L_o_m_got_removed;
      /* dR_o_m  */ ydot[19] = L_o_m_got_removed; 
       
       /* dS_y_w  */ ydot[20] = -H_y_w_got_infected;
       /* dE1_y_w  */ ydot[21] = H_y_w_got_infected - H_y_w_got_latent;
       /* dE2_y_w  */ ydot[22] = H_y_w_got_latent - H_y_w_got_infectious;
       /* dI_y_w  */ ydot[23] = H_y_w_got_infectious - H_y_w_got_removed;
       /* dR_y_w  */ ydot[24] = H_y_w_got_removed;
       
       /* dS_y_m  */ ydot[25] = -H_y_m_got_infected;
       /* dE1_y_m  */ ydot[26] = H_y_m_got_infected - H_y_m_got_latent;
       /* dE2_y_m  */ ydot[27] = H_y_m_got_latent - H_y_m_got_infectious;
       /* dI_y_m  */ ydot[28] = H_y_m_got_infectious - H_y_m_got_removed;
       /* dR_y_m  */ ydot[29] = H_y_m_got_removed; 
       
       /* dS_o_w  */ ydot[30] = -H_o_w_got_infected;
       /* dE1_o_w  */ ydot[31] = H_o_w_got_infected - H_o_w_got_latent;
       /* dE2_o_w  */ ydot[32] = H_o_w_got_latent - H_o_w_got_infectious;
       /* dI_o_w  */ ydot[33] = H_o_w_got_infectious - H_o_w_got_removed;
       /* dR_o_w  */ ydot[34] = H_o_w_got_removed;    
       
       /* dS_o_m  */ ydot[35] = -H_o_m_got_infected;
       /* dE1_o_m  */ ydot[36] = H_o_m_got_infected - H_o_m_got_latent;
       /* dE2_o_m  */ ydot[37] = H_o_m_got_latent - H_o_m_got_infectious;
       /* dI_o_m  */ ydot[38] = H_o_m_got_infectious - H_o_m_got_removed;
       /* dR_o_m  */ ydot[39] = H_o_m_got_removed;   
       
    yout[0] = 1/gamma * (L_y_w_got_infected + L_y_m_got_infected + L_o_w_got_infected + L_o_m_got_infected +
                         H_y_w_got_infected + H_y_m_got_infected + H_o_w_got_infected + H_o_m_got_infected)/ 
                         (I_L_y_w + I_L_y_m + I_L_o_w + I_L_o_m + I_H_y_w + I_H_y_m + I_H_o_w + I_H_o_m);
    yout[1] = yout[0] * (N_L_y_w + N_L_y_m + N_L_o_w + N_L_o_m + N_H_y_w + N_H_y_m + N_H_o_w + N_H_o_m)/
                        (S_L_y_w + S_L_y_m + S_L_o_w + S_L_o_m + S_H_y_w + S_H_y_m + S_H_o_w + S_H_o_m);
    
    yout[2] = beta_LL_yy_ww/gamma;
    yout[3] = beta_LL_yy_mm/gamma;
    yout[4] = beta_LL_yy_wm/gamma;
    yout[5] = beta_LL_yy_mw/gamma;
    yout[6] = beta_LL_oo_ww/gamma;
    yout[7] = beta_LL_oo_mm/gamma;
    yout[8] = beta_LL_oo_wm/gamma;
    yout[9] = beta_LL_oo_mw/gamma;
    yout[10] = beta_LL_yo_ww/gamma;
    yout[11] = beta_LL_yo_mm/gamma;
    yout[12] = beta_LL_yo_wm/gamma;
    yout[13] = beta_LL_yo_mw/gamma;
    yout[14] = beta_HH_yy_ww/gamma;
    yout[15] = beta_HH_yy_mm/gamma;
    yout[16] = beta_HH_yy_wm/gamma;
    yout[17] = beta_HH_yy_mw/gamma;
    yout[18] = beta_HH_oo_ww/gamma;
    yout[19] = beta_HH_oo_mm/gamma;
    yout[20] = beta_HH_oo_wm/gamma;
    yout[21] = beta_HH_oo_mw/gamma;
    yout[22] = beta_HH_yo_ww/gamma;
    yout[23] = beta_HH_yo_mm/gamma;
    yout[24] = beta_HH_yo_wm/gamma;
    yout[25] = beta_HH_yo_mw/gamma;
    yout[26] = beta_LH_yy_ww/gamma;
    yout[27] = beta_LH_yy_mm/gamma;
    yout[28] = beta_LH_yy_wm/gamma;
    yout[29] = beta_LH_yy_mw/gamma;
    yout[30] = beta_HL_yy_ww/gamma;
    yout[31] = beta_HL_yy_mm/gamma;
    yout[32] = beta_HL_yy_wm/gamma;
    yout[33] = beta_HL_yy_mw/gamma;
    
  }
}
