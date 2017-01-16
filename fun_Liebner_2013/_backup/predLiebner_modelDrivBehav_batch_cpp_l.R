library(Rcpp)

cppFunction(
  '
  #include <cmath>

  List sim_asv(double v_sim1, 
               double u, 
               double delta, 
               double s_sim1, 
               double d0,
               double a_max,
               double b,
               double objpos,
               int j,
               double pos4carryout,
               NumericVector time_s_diff) {

  double a_sim;

  NumericVector v_sim( time_s_diff.length() );
  NumericVector s_sim( time_s_diff.length() );
  v_sim[0] = v_sim1;
  s_sim[0] = s_sim1;

  double gap_des = 0;
  double gap_act = 1;
  double dt = 0.01;
  double a = a_max * dt;

  if (j == 1 || j == 3 || pos4carryout <= objpos) {

      for(int i = 1; i < time_s_diff.length(); i++) {

        if ((j == 2) || (j == 4)) {
          gap_des = d0 + d0 * v_sim[i-1] + ( pow(v_sim[i-1], 2) / ( 2 * sqrt((a_max * b)) ) );
          gap_act = s_sim[i-1] - objpos;
        }
      
        a_sim = a * ( (1 - pow((v_sim[i-1] / u), delta)) - pow(( gap_des / gap_act), 2) );
        v_sim[i] = v_sim[i-1] + a_sim;
        s_sim[i] = s_sim[i-1] + v_sim[i-1] * dt;
      } 

      List ret;
      ret["v_sim"] = v_sim;
      ret["s_sim"] = s_sim;
      return ret;
      //return Rcpp::DataFrame::create( Named("speed_ms")= v_sim, Named("dist_m") = s_sim);

  } else {
      List ret;
      ret["speed_ms"] = R_NilValue;
      ret["dist_m"] = R_NilValue;
      return ret;
      //return Rcpp::DataFrame::create( Named("speed_ms")= R_NilValue, Named("dist_m") = R_NilValue);
    }


  }
  '
)