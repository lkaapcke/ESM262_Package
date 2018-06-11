#' Prediction of fish growth rate by water temperature
#'
#' This function predicts fish growth rate by temperature in Celsius.
#' @param T Temperature of the surrounding water in degrees celsius
#' @param a Zero order parameter
#' @param b First order parameter
#' @param c Second order parameter
#' @param d Third order parameter
#' @return G Specific growth rate
#' @examples fish_growth(T = 0, a = -0.2425, b = 0.1519, c = 0.0552, d = -0.002931)
#' @author Lauren Kaapcke
#' @references https://www.sciencedirect.com/science/article/abs/pii/S0044848607005182


fish_growth = function(T, a, b, c, d) {
  G = a + b*T + c*T**2 + d*T**3
  return(G)
}






