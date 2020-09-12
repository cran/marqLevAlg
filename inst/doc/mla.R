## ---- results='hide', echo=FALSE, include = FALSE-----------------------------
knitr::opts_chunk$set(cache = TRUE, comment = '', echo = TRUE)
library("marqLevAlg")
library("ggplot2")
library("viridis")
library("patchwork")

## -----------------------------------------------------------------------------
Y <- dataEx$Y
X <- as.matrix(cbind(1, dataEx[, c("t", "X1", "X3")], 
                     dataEx$t * dataEx$X1))
ni <- as.numeric(table(dataEx$i))

## -----------------------------------------------------------------------------
binit <- c(0, 0, 0, 0, 0, 1, 1)

## -----------------------------------------------------------------------------
estim <- marqLevAlg(b = binit, fn = loglikLMM, minimize = FALSE, 
                    X = X, Y = Y, ni = ni)
estim

## -----------------------------------------------------------------------------
summary(estim, loglik = TRUE)

## ---- warning=FALSE-----------------------------------------------------------
estim2 <- marqLevAlg(b = binit, fn = loglikLMM, minimize = FALSE, 
                     nproc = 2, X = X, Y = Y, ni = ni)

## -----------------------------------------------------------------------------
estim3 <- marqLevAlg(b = binit, fn = loglikLMM, gr = gradLMM, 
                     minimize = FALSE, X = X, Y = Y, ni = ni)

## ----echo=FALSE,results='asis'------------------------------------------------
res <- function(x, core, gradient){
  res <- data.frame(core = core, gradient = gradient, loglik = x$fn, 
                    iterations = x$ni,
                    criterion1 = x$ca,
                    criterion2 = x$cb,
                    criterion3 = x$rdm)
  rownames(res) <- paste("Object ", deparse(substitute(x)), sep = "")
  colnames(res) <- c("Number of cores", "Analytical gradient", "Objective Function", "Number of iterations", "Parameter Stability", "Likelihood stability", "RDM")
  return(t(res))
}
library("xtable")
print(xtable(cbind(res(estim, 1, "no"), res(estim2, 2, "no"), res(estim3, 1, "yes")), digits = matrix(c(rep(0, 7), 0, 0, 2, 0, -1, -1, -1, 0, 0, 2, 0, -1, -1, -1, 0, 0, 2, 0, -1, -1, -1), 7, 4), label = "tab:fit",
             caption = "Summary of the estimation process of a linear mixed model using marqLevAlg function run either in sequential mode with numerical gradient calculation (object estim), parallel mode with numerical gradient calculation (object estim2), or sequential mode with analytical gradient calculation (object estim3).", align = c("l", "r", "r", "r")), comment = FALSE)

## ----echo=FALSE,results='asis'------------------------------------------------
coef <- function(x){
  coef <- cbind(x$b, sqrt(x$v[c(1, 3, 6, 10, 15, 21, 28)]))
  colnames(coef) <- c(paste("Coef (", deparse(substitute(x)), ")", sep = ""),
                  paste("SE (", deparse(substitute(x)), ")", sep = ""))
  rownames(coef) <- paste("Parameter", 1:7)  
  return(round(coef, digits = 4))
}

addtorow <- list()
addtorow$pos <- list(-1, 0)
addtorow$command <- c("\\hline & \\multicolumn{2}{r}{Object estim} & \\multicolumn{2}{r}{Object estim2} & \\multicolumn{2}{r}{Object estim3} \\\\", " & Coef & SE &  Coef & SE &  Coef & SE \\\\")

print(xtable(cbind(coef(estim), coef(estim2), coef(estim3)),digits = 4,  label = "tab:estim",
             caption = "Estimates (Coef) and standard error (SE) of the parameters of a linear mixed model fitted using marqLevAlg function run either in sequential mode with numerical gradient calculation (object estim), parallel mode with numerical gradient calculation (object estim2), or sequential mode with analytical gradient calculation (object estim3)."), comment = FALSE, add.to.row = addtorow, include.colnames = FALSE)

## ---- echo=FALSE--------------------------------------------------------------
structure(list(i = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 
3), class = c(2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3), X1 = c(0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), X2 = c(0.647220467290558, 
0.647220467290558, 0.647220467290558, 0.647220467290558, 0.647220467290558, 
0.395484594958106, 0.395484594958106, 0.395484594958106, 0.395484594958106, 
0.395484594958106, 1.06608365829076, 1.06608365829076, 1.06608365829076, 
1.06608365829076, 1.06608365829076), X3 = c(43.4292026597267, 
43.4292026597267, 43.4292026597267, 43.4292026597267, 43.4292026597267, 
43.4605985147139, 43.4605985147139, 43.4605985147139, 43.4605985147139, 
43.4605985147139, 42.0805667757557, 42.0805667757557, 42.0805667757557, 
42.0805667757557, 42.0805667757557), t = c(0, 1, 2, 3, 4, 0, 
1, 2, 3, 4, 0, 1, 2, 3, 4), Ycens = c(61.1063246971846, 60.7698831687246, 
58.7261691593818, 56.7601531082037, 54.0455763709346, 37.9530151094344, 
34.4865956424182, 31.3967930421455, 27.8142715666862, NA, 51.6087691810814, 
53.8067070658917, 51.118399417372, 50.6433122329987, 50.8787321079207
), tsurv = c(20, 20, 20, 20, 20, 3.76314836600959, 3.76314836600959, 
3.76314836600959, 3.76314836600959, 3.76314836600959, 15.3969576458208, 
15.3969576458208, 15.3969576458208, 15.3969576458208, 15.3969576458208
), event = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), row.names = c(1L, 
2L, 3L, 4L, 5L, 22L, 23L, 24L, 25L, 26L, 43L, 44L, 45L, 46L, 
47L), class = "data.frame")

## ---- echo=FALSE, results='hide'----------------------------------------------
load("res_JM_hlme_CInLPN.RData")

table1 <- cbind(c(40, 16, 40, round(temps_JM_nhs5000_analyDeriv[1, 1]), round(res_JM_nhs5000_analyDeriv[-1, 1], 2)),
  c(40, 16, 860, round(temps_JM_nhs5000_numeriDeriv[1, 1]), round(res_JM_nhs5000_numeriDeriv[-1, 1], 2)),
  c(10, 30, 65, round(temps_hlme_G1[1, 1]), round(res_hlme_G1[-1, 1], 2)),
  c(18, 30, 189, round(temps_hlme_G2[1, 1]), round(res_hlme_G2[-1, 1], 2)),
  c(26, 30, 377, round(temps_hlme_G3[1, 1]), round(res_hlme_G3[-1, 1], 2)),
  c(34, 30, 629, round(temps_hlme_G4[1, 1]), round(res_hlme_G4[-1, 1], 2)),
  c(27, 13, 405, round(temps_CInLPN[1, 1]), round(res_CInLPN[-1, 1], 2)))
rownames(table1) <- c("Number of parameters", "Number of iterations", "Number of elements in foreach loop", "Sequential time (seconds)",  "Speed up with 2 cores", "Speed up with 3 cores", "Speed up with 4 cores", "Speed up with 6 cores", "Speed up with 8 cores", "Speed up with 10 cores", "Speed up with 15 cores", "Speed up with 20 cores", "Speed up with 25 cores", "Speed up with 30 cores")

## ----perf, results='asis', echo=FALSE-----------------------------------------
library("xtable")
addtorow <- list()
addtorow$pos <- list(-1, 0)
addtorow$command <- c("\\hline & \\multicolumn{2}{c}{JM} & \\multicolumn{4}{c}{hlme} & CInLPN \\\\", " & analytic & numeric & G=1 & G=2 & G=3 & G=4 & \\\\")
dig <- rbind(matrix(0, 4, 8), matrix(2, 10, 8))
print(xtable(table1, align=c("l", rep("r", 7)), digits = dig, label = "tab:perf",
             caption = "Estimation process characteristics for the 3 different programs (JM, hlme and CInLPN). Analytic and Numeric refer to the analytical and numerical computations of the gradient in JM; G refers to the number of latent classes."), 
      size = "\\small",
      comment = FALSE, add.to.row = addtorow, include.colnames = FALSE)

## ----speedup, echo=FALSE, results='hide', fig.width=11, fig.height=5.5, out.width='100%', fig.cap="Speed up performances for the 3 different programs (JM, hlme and CInLPN). Analytic and numeric refer to the analytical and numerical computations of the gradient in JM. The number of parameters was  40 for JM; 10, 18, 26, 34 for hlme with 1, 2, 3, 4 classes, respectively; 27 for CInLPN."----

pJM <- ggplot(data = cbind.data.frame("ncores" = rep(c(1, 2, 3, 4, 6, 8, 10, 15, 20, 25, 30), 2),
                               "speedup" = c(res_JM_nhs5000_numeriDeriv[, 1],
                                             res_JM_nhs5000_analyDeriv[, 1]),
                               "method" = factor(rep(c("Numeric gradient", "Analytical gradient"), each = 11), 
                                                 levels = c("Numeric gradient", "Analytical gradient"), ordered = TRUE),
                               "lowbound" = c(res_JM_nhs5000_numeriDeriv[, 1] - 1.96 * res_JM_nhs5000_numeriDeriv[, 2],
                                              res_JM_nhs5000_analyDeriv[, 1] - 1.96 * res_JM_nhs5000_analyDeriv[, 2]),
                               "upbound" = c(res_JM_nhs5000_numeriDeriv[, 1] + 1.96 * res_JM_nhs5000_numeriDeriv[, 2], 
                                             res_JM_nhs5000_analyDeriv[, 1] + 1.96 * res_JM_nhs5000_analyDeriv[, 2])
                               ),  
       aes(x = ncores)) +
  geom_ribbon(aes(ymin = lowbound, ymax = upbound, fill = method), alpha = 0.4) +
  geom_point(aes(y = speedup, colour = method)) +
  geom_line(aes(y = speedup, colour = method)) +
  theme_classic() +
  scale_x_continuous(minor_breaks = c(1, 2, 3, 4, 6, 8, 10, 15, 20, 25, 30), breaks = c(1, 5, 10, 15, 20, 25, 30)) +
  theme(panel.grid.major.y = element_line(), 
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(size = 16),
        axis.text  = element_text(size = 14),
        title = element_text(size = 17, face = "bold"),
        legend.margin = margin(-30, 10, 10, 10),
        legend.position = "bottom", legend.direction = "vertical",
        legend.text = element_text(size = 14)) +
  xlab("Number of cores") +
  ylab("Speed-up") +
  scale_color_manual("", values = rev(viridis::inferno(5)[3:4])) +
  scale_fill_manual("", values = rev(viridis::inferno(5)[3:4])) +
  ggtitle("JM") +
  guides(fill="none") +
  NULL

phlme <- ggplot(data = cbind.data.frame("ncores" = rep(c(1, 2, 3, 4, 6, 8, 10, 15, 20, 25, 30), 4),
                               "speedup" = c(res_hlme_G1[, 1], res_hlme_G2[, 1],
                                             res_hlme_G3[, 1], res_hlme_G4[, 1]),
                               "nbclass" = factor(rep(c("1 class", "2 classes", "3 classes", "4 classes"), each = 11), 
                                                 levels = c("1 class", "2 classes", "3 classes", "4 classes"), ordered = TRUE),
                               "lowbound" = c(res_hlme_G1[, 1] - 1.96 * res_hlme_G1[, 2], res_hlme_G2[, 1] - 1.96 * res_hlme_G2[, 2],
                                              res_hlme_G3[, 1] - 1.96 * res_hlme_G3[, 2], res_hlme_G4[, 1] - 1.96 * res_hlme_G4[, 2]),
                               "upbound" = c(res_hlme_G1[, 1] + 1.96 * res_hlme_G1[, 2], res_hlme_G2[, 1] + 1.96 * res_hlme_G2[, 2],
                                              res_hlme_G3[, 1] + 1.96 * res_hlme_G3[, 2], res_hlme_G4[, 1] + 1.96 * res_hlme_G4[, 2])
                               ),  
       aes(x = ncores)) +
  geom_ribbon(aes(ymin = lowbound, ymax = upbound, fill = nbclass), alpha = 0.4) +
  geom_point(aes(y = speedup, colour = nbclass)) +
  geom_line(aes(y = speedup, colour = nbclass)) +
  theme_classic() +
  scale_x_continuous(minor_breaks = c(1, 2, 3, 4, 6, 8, 10, 15, 20, 25, 30), breaks = c(1, 5, 10, 15, 20, 25, 30)) +
  theme(panel.grid.major.y = element_line(), 
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(size = 16),
        axis.text  = element_text(size = 14),
        title = element_text(size = 17, face = "bold"),
        legend.margin = margin(-30, 10, 10, 10),
        legend.position = "bottom", legend.direction = "vertical",
        legend.text = element_text(size = 14)) +
  xlab("Number of cores") +
  ylab("Speed-up") +
  scale_color_manual("", values = rev(viridis::viridis(5)[1:4])) +
  scale_fill_manual("", values = rev(viridis::viridis(5)[1:4])) +
  ggtitle("hlme") +
  guides(fill = "none", color = guide_legend(ncol = 2)) +
  NULL
  

pCInLPN <- ggplot(data = cbind.data.frame("ncores" = rep(c(1, 2, 3, 4, 6, 8, 10, 15, 20, 25, 30), 1),
                               "speedup" = res_CInLPN[, 1],
                               "lowbound" = res_CInLPN[, 1] - 1.96 * res_CInLPN[, 2],
                               "upbound" = res_CInLPN[, 1] + 1.96 * res_CInLPN[, 2]
                               ),  
       aes(x = ncores)) +
  geom_ribbon(aes(ymin = lowbound, ymax = upbound, fill = "95% Conf. Int."), alpha = 0.4) +
  geom_point(aes(y = speedup, color = "95% Conf. Int.")) +
  geom_line(aes(y = speedup, color = "95% Conf. Int.")) +
  theme_classic() +
  scale_x_continuous(minor_breaks = c(1, 2, 3, 4, 6, 8, 10, 15, 20, 25, 30), breaks = c(1, 5, 10, 15, 20, 25, 30)) +
  theme(panel.grid.major.y = element_line(), 
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(size = 16),
        axis.text  = element_text(size = 14),
        title = element_text(size = 17, face = "bold"),
        legend.margin = margin(-10, 10, 10, 10),
        legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  scale_color_manual("", values = "black") +
  scale_fill_manual("", values = "black") +
  xlab("Number of cores") +
  ylab("Speed-up") +
  ggtitle("CInLPN") +
  NULL

(pJM + ylim(0, 18)) + (phlme + ylim(0, 18)) + (pCInLPN + ylim(0, 18))

## ----prothro, results='asis', echo=FALSE, message = FALSE---------------------
resbis <- structure(list(association = c("value", "value", "value", "value", 
"value", "value", "value", "value", "value", "slope", "slope", 
"slope", "slope", "slope", "slope", "slope", "slope", "slope", 
"both", "both", "both", "both", "both", "both", "both", "both", 
"both"), algorithm = c("BFGS", "BFGS", "BFGS", "EM", "EM", "EM", 
"MLA", "MLA", "MLA", "BFGS", "BFGS", "BFGS", "EM", "EM", "EM", 
"MLA", "MLA", "MLA", "BFGS", "BFGS", "BFGS", "EM", "EM", "EM", 
"MLA", "MLA", "MLA"), scaling = c("1", "0.1", "10", "1", "0.1", 
"10", "1", "0.1", "10", "1", "0.1", "10", "1", "0.1", "10", "1", 
"0.1", "10", "1", "0.1", "10", "1", "0.1", "10", "1", "0.1", 
"10"), LogLik = c(-13958.552128322, -7123.83366523364, -20795.6100064946, 
-13957.9100947483, -7123.64664242897, -20792.0165904447, -13957.6874738687, 
-7123.61491786343, -20791.7600408212, -13961.410640618, -7127.15852472177, 
-20814.9746430314, -13960.6949103499, -7126.61992438705, -20794.7771562731, 
-13960.688478504, -7126.61592251244, -20794.7610340869, -13951.5986192875, 
-7115.74930093756, -20799.3213382741, -13949.8221932589, -7115.36653280077, 
-20784.5370063629, -13949.4174388403, -7115.34488283373, -20783.4899958775
), scaledLogLik = c(-13958.552128322, -13957.90622124, -13961.5374504883, 
-13957.9100947483, -13957.7191984353, -13957.9440344384, -13957.6874738687, 
-13957.6874738698, -13957.6874848149, -13961.410640618, -13961.2310807281, 
-13980.9020870251, -13960.6949103499, -13960.6924803934, -13960.7046002668, 
-13960.688478504, -13960.6884785188, -13960.6884780806, -13951.5986192875, 
-13949.8218569439, -13965.2487822678, -13949.8221932589, -13949.4390888071, 
-13950.4644503565, -13949.4174388403, -13949.4174388401, -13949.4174398712
), value = c(-3.73061827287826, -0.0141539186930607, -9.27594324673535, 
-0.29330877747947, 0.140534649953885, -0.591117664733939, 0, 
-0.000142338309824958, -0.00140558371611392, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, 15.9710405532139, 2.65507149201079, 40.3077266989259, 
4.10228091592489, 1.68353683871255, 10.6659991378836, 0, -0.000170669856949751, 
0.00419338230974572), slope = c(NA, NA, NA, NA, NA, NA, NA, NA, 
NA, -1.84843620399159, -1.3696438283896, -13.9825813428088, 0.179854683531865, 
0.0260101270184876, 0.0817324481422286, 0, 0.000565059170089405, 
0.00258486458745339, -28.1732598640996, -4.62818306775129, -95.2646725814592, 
-7.22395309527721, -3.66247190442162, -16.3091118268196, 0, 0.000488761569032907, 
-0.00982741352301927), iterations = c(120, 490, 91, 66, 104, 
62, 7, 5, 15, 251, 391, 444, 169, 208, 156, 10, 10, 14, 164, 
502, 52, 159, 156, 142, 10, 10, 22), time = c(29.319, 117.203, 
18.3049999999998, 57.641, 89.976, 61.098, 36.0789999999997, 26.7719999999999, 
72.5709999999999, 52.4590000000001, 78.7809999999999, 86.6110000000003, 
143.2, 156.799, 138.045, 46.0369999999998, 46.3680000000004, 
63.634, 40.1879999999999, 133.824, 10.8450000000003, 179.708, 
148.234, 197.16, 51.2360000000003, 53.3180000000002, 118.374)), row.names = c(1L, 
2L, 3L, 19L, 20L, 21L, 10L, 11L, 12L, 4L, 5L, 6L, 22L, 23L, 24L, 
13L, 14L, 15L, 7L, 8L, 9L, 25L, 26L, 27L, 16L, 17L, 18L), class = "data.frame")

addtorow <- list()
addtorow$pos <- list(-1, 0)
addtorow$command <- c("\\hline  Nature of & Algorithm & Scaling & Rescaled log-& Variation of& Variation of& Number of & Time in\\\\",
  " dependency & & factor & likelihood & value (\\%) & slope (\\%) & iterations & seconds \\\\")

print(xtable(resbis[, -c(4)], align = rep("r", 9), digits = c(1, 2, 2, 1, 2, 2, 2, 0, 2), label = "tab:prothro",
             caption = "Comparison of the convergence obtained by MLA, BFGS and EM algorithms for the estimation of a joint model for prothrobin repeated marker (scaled by 1, 0.1 or 10) and time to death when considering a dependency on the current level of prothrobin ('value') or the current slope ('slope') or both ('both'). All the models converged correctly according to the algorithm outputs. We report the final log-likelihood rescaled to scaling factor 1 (for comparison), the  percentage of variation of the association parameters ('value' and 'slope' columns) compared to the one obtained with the overall maximum likelihood with scaling 1, the number of iterations and the running time in seconds. "),
      comment = FALSE, include.rownames = FALSE, include.colnames = FALSE, add.to.row = addtorow, hline.after = c(0, nrow(resbis)), size = "footnotesize")

