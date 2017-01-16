# rtools <- "C:\\RBuildTools\\3.3\\bin"
# gcc <- "C:\\RBuildTools\\3.3\\gcc-4.6.3\\bin"
# gcc32 <- "C:\\RBuildTools\\3.3\\gcc-4.6.3\\bin32"
# gcc64 <- "C:\\RBuildTools\\3.3\\gcc-4.6.3\\bin64"
# mingw <-"C:\\RBuildTools\\3.3\\mingw_64\\bin\\"
# path <- strsplit(Sys.getenv("PATH"), ";")[[1]]
# new_path <- c(rtools, gcc, path, mingw, gcc32, gcc64)
# new_path <- new_path[!duplicated(tolower(new_path))]
# Sys.setenv(PATH = paste(new_path, collapse = ";"))


# add_path(new_path)
# 
# 
# library(inline)
# src <- " double x = as<double>(xs);
#          double x = x^2;
#          return wrap(x);"
# 
# l <- cxxfunction(signature(xs = "double"), 
#                  body = src, 
#                  plugin = "Rcpp")




#  ------------------------------------------------------------------------




library(Rcpp)
cppfun <-
  '
  #include <cmath>
  List sim_asv(double v_sim, 
               double u, 
               double delta, 
               double s_sim, 
               double d0,
               double a_max,
               double b,
               double objpos,
               int j) {

  double a_sim;
  double gap_des = 0;
  double gap_act = 1;
  double dt = 0.01;
  double a = a_max * dt;

  for(int i = 0; i < 100; i++) {

    Rcout << "i = " << i << std::endl;

    if ((j == 2) || (j == 4)) {
      gap_des = d0 + d0 * v_sim + ( pow(v_sim, 2) / ( 2 * sqrt((a_max * b)) ) );
      gap_act = s_sim - objpos;
    }
  
    a_sim = a * ( (1 - pow((v_sim / u), delta)) - pow(( gap_des / gap_act), 2) );
    v_sim = v_sim + a_sim;
    s_sim = s_sim + v_sim * dt;
  }

  List ret;
  ret["v_sim"] = v_sim;
  ret["s_sim"] = s_sim;
  return ret;
  }
  '

cppFunction(cppfun)
