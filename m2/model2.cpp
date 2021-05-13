#include <R.h>
#include <math.h>
#include <stdio.h>
#include <iostream>

static const int PARAM_N = 50;
static double parms[PARAM_N];

#define beta0_LL parms[0]
#define beta0_HH parms[1]
#define beta0_LH parms[2]
#define beta0_HL parms[3]

#define beta1_LL parms[4]
#define beta1_HH parms[5]
#define beta1_LH parms[6]
#define beta1_HL parms[7]

#define beta2_LL parms[8]
#define beta2_HH parms[9]
#define beta2_LH parms[10]
#define beta2_HL parms[11]

#define beta3_LL parms[12]
#define beta3_HH parms[13]
#define beta3_LH parms[14]
#define beta3_HL parms[15]

#define beta4_LL parms[16]
#define beta4_HH parms[17]
#define beta4_LH parms[18]
#define beta4_HL parms[19]

#define beta5_LL parms[20]
#define beta5_HH parms[21]
#define beta5_LH parms[22]
#define beta5_HL parms[23]

#define beta6_LL parms[24]
#define beta6_HH parms[25]
#define beta6_LH parms[26]
#define beta6_HL parms[27]

#define beta7_LL parms[28]
#define beta7_HH parms[29]
#define beta7_LH parms[30]
#define beta7_HL parms[31]

#define beta8_LL parms[32]
#define beta8_HH parms[33]
#define beta8_LH parms[34]
#define beta8_HL parms[35]

#define N_L parms[36]
#define N_H parms[37]

#define t0 parms[38] // Start of lockdown            beta0
#define t1 parms[39] // End of lockdown transition   beta1
#define t2 parms[40]
#define t3 parms[41]
#define t4 parms[42]
#define t5 parms[43]
#define t6 parms[44]
#define t7 parms[45]
#define t8 parms[46]

#define a1 parms[47]
#define a2 parms[48]
#define gamma parms[49]


#define S_L y[0]
#define E1_L y[1]
#define E2_L y[2]
#define I_L y[3]
#define R_L y[4]
#define S_H y[5]
#define E1_H y[6]
#define E2_H y[7]
#define I_H y[8]
#define R_H y[9]



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
    if (ip[0] < 7) error("nout should be at least 7");
    
    if (S_L < 0   || E1_L < 0   || E2_L < 0   ||I_L < 0   || R_L < 0   ||
	S_L > N_L || E1_L > N_L || E2_L > N_L || I_L > N_L || R_L > N_L ||
	S_H < 0   || E1_H < 0   ||  E2_H < 0   || I_H < 0   || R_H < 0   ||
	S_H > N_H || E1_H > N_H ||  E2_H > N_H || I_H > N_H || R_H > N_H) {
      std::cerr << "Oops ?" << std::endl;
      /* dS_L  */ ydot[0] = 0;
      /* dE1_L  */ ydot[1] = 0;
      /* dE2_L  */ ydot[2] = 0;
      /* dI_L  */ ydot[3] = 0;
      /* dR_L  */ ydot[4] = 0;
      /* dS_H  */ ydot[5] = 0;
      /* dE1_H  */ ydot[6] = 0;
      /* dE2_H  */ ydot[7] = 0;
      /* dI_H  */ ydot[8] = 0;
      /* dR_H  */ ydot[9] = 0;
      return;
    }

    
    const double beta_LL = interpolate(*t,
				     beta0_LL, beta1_LL, beta2_LL, beta3_LL, 
				     beta4_LL, beta5_LL, beta6_LL, beta7_LL, beta8_LL);
    const double beta_HH = interpolate(*t,
             beta0_HH, beta1_HH, beta2_HH, beta3_HH, 
             beta4_HH, beta5_HH, beta6_HH, beta7_HH, beta8_HH);
    const double beta_HL = interpolate(*t,
              beta0_HL, beta1_HL, beta2_HL, beta3_HL, 
              beta4_HL, beta5_HL, beta6_HL, beta7_HL, beta8_HL);
    const double beta_LH = interpolate(*t,
              beta0_LH, beta1_LH, beta2_LH, beta3_LH, 
              beta4_LH, beta5_LH, beta6_LH, beta7_LH, beta8_LH);
    
    
    const double LL_infecting = (beta_LL * I_L) / N_L;
    const double HH_infecting = (beta_HH * I_H) / N_H;
    const double LH_infecting = (beta_LH * I_L) / N_L;
    const double HL_infecting = (beta_HL * I_H) / N_H;

    const double L_got_infected = (LL_infecting + HL_infecting) * S_L;
    const double L_got_latent = a1 * E1_L;
    const double L_got_infectious = a2 * E2_L;
    const double L_got_removed = gamma * I_L;
    
    const double H_got_infected = (HH_infecting + LH_infecting) * S_H;
    const double H_got_latent = a1 * E1_H;
    const double H_got_infectious = a2 * E2_H;
    const double H_got_removed = gamma * I_H;
    
    /* dSy  */ ydot[0] = -L_got_infected;
    /* dSy  */ ydot[1] = L_got_infected - L_got_latent;
    /* dEy  */ ydot[2] = L_got_latent - L_got_infectious;
    /* dIy  */ ydot[3] = L_got_infectious - L_got_removed;
    /* dRy  */ ydot[4] = L_got_removed;

    /* dSm  */ ydot[5] = -H_got_infected;
    /* dSy  */ ydot[6] = H_got_infected - H_got_latent;
    /* dEy  */ ydot[7] = H_got_latent - H_got_infectious;
    /* dIm  */ ydot[8] = H_got_infectious - H_got_removed;
    /* dRm  */ ydot[9] = H_got_removed;

    yout[0] = 1/gamma * (L_got_infected + H_got_infected) / (I_L + I_H);  /* Ro  */
    yout[1] = yout[0] * (N_L + N_H) / (S_L + S_H); /* Rt  */
    yout[2] =  beta_LL/gamma;  /* Rt_LL  SC */
    yout[3] =  beta_HH/gamma;  /* Rt_HH  SC */
    yout[4] =  beta_LH/gamma;  /* Rt_LH  */
    yout[5] =  beta_HL/gamma;  /* Rt_HL  */
    yout[6] =  (yout[2]+yout[3] + sqrt(pow((yout[2]-yout[3]),2) + 4*yout[4]*yout[5]))/2;  /* Rt_2*/
    
    
  }
}
