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
table4 <- structure(list(dep = c("value", "value", "value", "value", "value", 
"value", "value", "value", "value", "value", "value", "value", 
"slope", "slope", "slope", "slope", "slope", "slope", "slope", 
"slope", "slope", "slope", "slope", "slope", "both", "both", 
"both", "both", "both", "both", "both", "both", "both", "both", 
"both", "both"), algorithm = c("BFGS", "BFGS", "BFGS", "LBFGSB", 
"LBFGSB", "LBFGSB", "EM", "EM", "EM", "marq", "marq", "marq", 
"BFGS", "BFGS", "BFGS", "LBFGSB", "LBFGSB", "LBFGSB", "EM", "EM", 
"EM", "marq", "marq", "marq", "BFGS", "BFGS", "BFGS", "LBFGSB", 
"LBFGSB", "LBFGSB", "EM", "EM", "EM", "marq", "marq", "marq"), 
    scaling = c("1", "0.1", "10", "1", "0.1", "10", "1", "0.1", 
    "10", "1", "0.1", "10", "1", "0.1", "10", "1", "0.1", "10", 
    "1", "0.1", "10", "1", "0.1", "10", "1", "0.1", "10", "1", 
    "0.1", "10", "1", "0.1", "10", "1", "0.1", "10"), loglik = c(-13958.552128322, 
    -13957.90622124, -13961.5374504883, -13958.4111218853, -13957.6945196808, 
    NA, -13957.9100947483, -13957.7191984353, -13957.9440344384, 
    -13957.6874738687, -13957.6874738698, -13957.6874848149, 
    -13961.410640618, -13961.2310807281, -13980.9020870251, -13960.6947231745, 
    -13960.6973319216, -13962.5562003806, -13960.6949103499, 
    -13960.6924803934, -13960.7046002668, -13960.688478504, -13960.6884785188, 
    -13960.6884780806, -13951.5986192875, -13949.8218569439, 
    -13965.2487822678, -13950.0395615584, -13949.4178398458, 
    -13985.7183851268, -13949.8221932589, -13949.4390888071, 
    -13950.4644503565, -13949.4174388403, -13949.4174388401, 
    -13949.4174398712), value = c(-3.73061827287826, -0.0141539186930607, 
    -9.27594324673535, -3.55740885028042, -0.106768062409848, 
    NA, -0.29330877747947, 0.140534649953885, -0.591117664733939, 
    0, -0.000142338309807917, -0.00140558371611392, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, 15.9710405532139, 2.65507149201079, 
    40.3077266989259, -1.66621615271708, -0.014837370092208, 
    67.325397659912, 4.10228091592489, 1.68353683871255, 10.6659991378836, 
    0, -0.000170669856949751, 0.00419338230974572), slope = c(NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, -1.84843620399159, 
    -1.3696438283896, -13.9825813428088, -0.149643379518137, 
    -0.267844491004789, -2.86911721220401, 0.179854683531865, 
    0.0260101270184876, 0.0817324481422286, 0, 0.000565059170089405, 
    0.00258486458745339, -28.1732598640996, -4.62818306775129, 
    -95.2646725814592, 7.10251223145208, 0.381619058329372, -147.296646571411, 
    -7.22395309527721, -3.66247190442162, -16.3091118268196, 
    0, 0.000488761569032907, -0.00982741352301927), iterations = c(120, 
    490, 91, 289, 244, NA, 66, 104, 62, 7, 5, 15, 251, 391, 444, 
    266, 206, 823, 169, 208, 156, 10, 10, 14, 164, 502, 52, 800, 
    411, 18, 159, 156, 142, 10, 10, 22), time = c(29.319, 117.203, 
    18.3049999999998, 80.644, 67.396999999999, NA, 57.64, 89.98, 
    61.1, 36.0789999999997, 26.7719999999999, 72.5709999999999, 
    52.4590000000001, 78.7809999999999, 86.6110000000003, 59.6179999999999, 
    46.8199999999997, 179.600999999999, 143.2, 156.8, 138.04, 
    46.0369999999998, 46.3680000000004, 63.634, 40.1879999999999, 
    133.824, 10.8450000000003, 177.559, 91.6859999999997, 7.68400000000111, 
    179.71, 148.23, 197.16, 51.2360000000003, 53.3180000000002, 
    118.374)), row.names = c(NA, -36L), class = "data.frame")

addtorow <- list()
addtorow$pos <- list(-1, 0)
addtorow$command <- c("\\hline  Nature of & Algorithm & Scaling & Rescaled log-& Variation of& Variation of& Number of & Time in\\\\",
  " dependency & & factor & likelihood & value (\\%) & slope (\\%) & iterations & seconds \\\\")

print(xtable(table4, align = rep("r", 9), digits = c(1, 2, 2, 1, 2, 2, 2, 0, 2), label = "tab:prothro",
             caption = "Comparison of the convergence obtained by MLA, BFGS, L-BFGS-B and EM algorithms for the estimation of a joint model for prothrobin repeated marker (scaled by 1, 0.1 or 10) and time to death when considering a dependency on the current level of prothrobin ('value') or the current slope ('slope') or both ('both'). All the models converged correctly according to the algorithm outputs. We report the final log-likelihood rescaled to scaling factor 1 (for comparison), the  percentage of variation of the association parameters ('value' and 'slope' columns) compared to the one obtained with the overall maximum likelihood with scaling 1, the number of iterations and the running time in seconds. "),
      comment = FALSE, include.rownames = FALSE, include.colnames = FALSE, add.to.row = addtorow, hline.after = c(0, nrow(table4)), size = "footnotesize")

## ----table35ex, results='asis', echo=FALSE------------------------------------
res35 <- structure(c(0, 48.9842, 0, 0, 0, 124.362, 0, 0.00821487, 1.12793e-08, 
87.9458, 0, 0, 0, 0, 0.000307505, 85822.2, 5.46489e-05, 0.00565565, 
0.0401377, 0.00228767, 0, 0, 2.24997e-05, 9.37629e-06, 0, 0, 
0, 0, 0, 0, 0, 55, 24.6268656716418, 26.1269035532995, 0, 1.87618871877924e-12, 
48.98425367924, 0.000199128321049547, NA, 4.14317254580169e-09, 
124.362182355615, 1.13631729762697e-13, 0.00821487730665518, 
1.17468761108584e-08, NA, 3.38219856564872e-16, 5.4312102712489e-19, 
1.1743985190355e-07, 2.42444029642598e-09, 0.0003075056187365, 
85822.2016263563, 7.61203879381678e-05, 1.65238528179872e-05, 
0.0401377363254047, 0.00228767005384086, 1.87618871877924e-11, 
2.29157031370395e-08, 2.42209886146193e-05, 9.65998354126513e-06, 
4.37806606239881e-14, 2.11438270538369e-05, 1.60901421938586e-09, 
2.14800842450392e-05, 1.31364828665149e-17, 1.29064737058101e-16, 
1.62956548532985e-13, 55, NA, NA, 8.35595331968899e-08, 8.82524109672275e-08, 
48.9842546935397, 1.83335474057457e-06, 18271.0159051812, 1.05520907738501e-08, 
124.362204017805, 4.57009423839009e-05, 0.00821516454802443, 
1.12793459159837e-08, 75393.9659406469, 3.67333642595554e-08, 
5.42227525207928e-06, 0.000209435347333077, 7.85491025410823, 
0.000307505630501291, 85822.234058694, 0.000170546100925534, 
NA, NA, NA, NA, NA, 2.99801340663387e-05, 9.78218148821597e-06, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 9.59495564376983e-18, 
48.9842536792694, NA, 2.70327936402721e-18, 2.19271340244521e-17, 
2020, 1.68355971775731e-24, 0.00821487777594039, 1.12793276961832e-08, 
87.9475796104573, NA, 2.34530820183238e-18, 5.69463859377807e-12, 
2.32803453449134e-27, 0.000307505636771332, 85822.2016263948, 
5.46572759433473e-05, NA, 0.0401377362940383, 0.00228767059855416, 
1.1893148975986e-17, 2.43176875116543e-12, 2.24998399746028e-05, 
9.38623759503698e-06, 5.38580348117675e-24, 5.65488656566107e-06, 
1.9618799083239e-19, 2.2784304098615e-18, 4.6666118897051e-18, 
0.712527909909443, 2.68002493809021, 55, 24.6268656716418, 26.1269035532995, 
9.84920930370854e-18, NA, NA, NA, NA, 3.93563638720758e-12, 2019.99999999998, 
1.10735833513877e-12, NA, 1.12848417647101e-08, NA, NA, NA, NA, 
NA, NA, 85822.2016263563, NA, NA, NA, NA, 1.76218812831914e-11, 
6.87587025121152e-09, NA, 9.48100590055563e-06, 3.49131163666423e-16, 
5.6551961239364e-06, 4.49315958784311e-09, NA, 9.9158643506394e-12, 
5.53540480822016e-13, 3.07621820456769, 55.0000000000089, 24.6268656716418, 
26.1269035532995, 1.7287601134023e-13, 2.26757730748798e-13, 
48.98425367924, 0.135188173053538, 2.67318949581686e-13, 5.307639825275e-16, 
NaN, 2.20396592803802e-12, 0.00821487730659092, 1.17672147196962e-08, 
112123.437420832, 1.19657837212447e-13, 5.22580214770706e-12, 
5.31786401687472e-09, 2.90763098419414e-12, 0.000307505621833634, 
85822.2016328264, 7.69590012080293e-05, 0.0056556499292764, 0.0401381601297482, 
0.00228767385880551, 1.8822193777221e-15, 3.27760934205846e-08, 
2.4241662319424e-05, 9.76688350552292e-06, 6.19009291565613e-29, 
5.65746195666815e-06, 4.40024298018673e-07, 1.30211792719787e-06, 
2.50182345412151e-13, 7.92708598940424e-10, 1.09992042024496e-10, 
55, 24.6268656716418, 26.1269035532995, 6.93472874538534e-12, 
2.26757730748798e-13, 48.98425367924, 0.135188173053538, 2.67318949581686e-13, 
5.307639825275e-16, NaN, 2.20396592803802e-12, 0.00821487730659092, 
1.17672147196962e-08, 112123.437420832, 1.19657837212447e-13, 
5.22580214770706e-12, 5.31786401687472e-09, 2.90763098419414e-12, 
0.000307505621833634, 85822.2016328264, 7.69590012080293e-05, 
0.0056556499292764, 0.0401381601297482, 0.00228767385880551, 
1.8822193777221e-15, 3.27760934205846e-08, 2.4241662319424e-05, 
9.76688350552292e-06, 6.19009291565613e-29, 5.65746195666815e-06, 
4.40024298018673e-07, 1.30211792719787e-06, 2.50182345412151e-13, 
7.92708598940424e-10, 1.09992042024496e-10, 55, 24.6268656716418, 
26.1269035532995, 6.93472874538534e-12, 4.29181601069864e-22, 
48.98425367924, NA, 2.435572005319e-15, 7.16313558591394e-25, 
124.362182355615, 2.20496266766302e-21, 0.00821487730657919, 
1.12793276963448e-08, NA, 1.30253053352066e-22, 1.88513240030002e-24, 
6.41610074452284e-65, 4.87365404836244e-19, 0.000307505603849238, 
85822.2016263564, 5.46489469748393e-05, 0.0056556499255002, 0.040137736293548, 
0.00228767005355251, 2.19383106122565e-19, NA, 2.24997750089994e-05, 
NA, 4.28526660285727e-25, 5.65488655145258e-06, 2.15800983612517e-23, 
3.40619377023087e-21, 6.52598135156146e-18, 1.08847592592226e-16, 
4.88039713600863e-21, 55, 24.6268656716418, 26.1269035532995, 
5.81645784452508e-15), .Dim = c(35L, 8L), .Dimnames = list(c("rosen", 
"freud_roth", "powell_bs", "brown_bs", "beale", "jenn_samp", 
"helical", "bard", "gauss", "meyer", "gulf", "box_3d", "powell_s", 
"wood", "kow_osb", "brown_den", "osborne_1", "biggs_exp6", "osborne_2", 
"watson", "ex_rosen", "ex_powell", "penalty_1", "penalty_2", 
"var_dim", "trigon", "brown_al", "disc_bv", "disc_ie", "broyden_tri", 
"broyden_band", "linfun_fr", "linfun_r1", "linfun_r1z", "chebyquad"
), c("solution", "marqLevAlg", "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", 
"optimParallel", "nlminb")))

deci <- matrix(3, 35, 8)
deci[16, ] <- 1
deci[4, 3] <- 0
deci[10, 3] <- 0
deci[6, 4:5] <- 0
deci[10, 6:7] <- 0

print(xtable(round(sweep(res35[,-c(1)], 1, res35[,1]),5), digits=deci, align = c("l", rep("r", 7)), label="tab:table35ex", caption="Absolute bias, for each of the 35 problems, between the real solution and the final optimum value at convergence point for 7 different algorithms : marqLevAlg, Nelder-Mead, BFGS, CG, L-BFGS-B, optimParallel and nlminb. An empty case means that the algorithm did not converged."), comment = FALSE, table.placement="h", floating=TRUE)

## ----figure35ex, echo=FALSE, results='hide', fig.width=11, fig.height=5.5, out.width='100%', fig.cap="Log-scaled bias between real solution and final optimum value at convergence point for 7 different algorithms : marqLevAlg, Nelder-Mead, BFGS, CG, L-BFGS-B, optimParallel and nlminb"----
boxplot(log(1+round(sweep(res35[,-c(1)], 1, res35[,1]),5)), ylab="log(1 + bias)")

## ----exnlmrt, results="asis", echo=FALSE--------------------------------------
res <- structure(c(8.92959881985513, 2.58727739533182, 2.5872773952842, 
2.58727739529232, 2.58727739528418, 2.58727739632938, 2.58727739529231, 
8.81335261136904e-20, 1731.04695, 13047.28143, 3522.30897, 7762.82791, 
3615.17351, 16343.82488, 3421.8311, 1871.21691, 8.92959881985513, 
NA, 2.58727754468569, 2.58727739554295, 2.58727789179372, 2.58727757727614, 
2.58727739529978, 1.00457614105069e-10, 775.15143, 155910.19108, 
34805.04655, 12019.77276, 9357.71489, 27588.33633, 13454.96472, 
983.13773), .Dim = c(8L, 4L), .Dimnames = list(c("One parameter problem", 
"Hobbs problem unscaled - start1", "Hobbs problem unscaled - easy", 
"Hobbs problem scaled - start1", "Hobbs problem scaled - easy", 
"Hobbs problem scaled - hard", "Hobbs problem scaled - start1 - gradient", 
"Gabor Grothendieck problem"), c("objective function", "runtime", 
"objective function", "runtime")))


addtorow <- list()
addtorow$pos <- list(-1)
addtorow$command <- c("\\hline   & \\multicolumn{2}{c}{nlxb/nlfb} & \\multicolumn{2}{c}{marqLevAlg}\\\\")

print(xtable(res, digits=c(2,4,0,4,0), align=c("l", rep("r",4)), caption="Final objective function value and runtimes (in microseconds) of least squares problems solved with nlmrt and marqLevAlg packages.", label="tab:exnlmrt"), comment=FALSE, add.to.row=addtorow)

## ----exminpack, results="asis", echo=FALSE, message=FALSE---------------------
res <- structure(c(0.798577959615689, 79237.0726183538, 79237.0726183538, 
312.6977, 2373.57642, 2388.32664, 0.798577959615706, 79237.0726183537, 
79237.0726183563, 7061.93807, 38584.34017, 20306.92131), .Dim = 3:4, .Dimnames = list(
    c("Example1", "Example2", "Example2 - gradient"), c("objective function", 
    "runtime", "objective function", "runtime")))


addtorow <- list()
addtorow$pos <- list(-1)
addtorow$command <- c("\\hline   & \\multicolumn{2}{c}{nls.lm} & \\multicolumn{2}{c}{marqLevAlg}\\\\")

print(xtable(res, digits=matrix(c(2,2,2, 4,0,0, 0,0,0, 4,0,0, 0,0,0), 3, 5), align=c("l", rep("r",4)), caption="Final objective function value and runtimes (in microseconds) of least squares problems solved with minpack.lm and marqLevAlg packages.", label="tab:exminpack"), comment=FALSE, add.to.row=addtorow)

## ----figureVI, echo=FALSE, results='hide', fig.width=11, fig.height=5.5, out.width='100%', fig.cap="Final value of the objective function at convergence for marqLevAlg algorithm and for nls.lm algorithm according to the type of convergence criterion met (1 for objective function, 2 for parameters, 3 for both). Are only reported the runs that converged, and results are in the log scale so that small differences are not blurred by some extreme differences. "----
ok1 <- 0.906342494714588
ok2 <- 0.0572998430141287
ok3 <- 0.973063973063973
okm <- 1

h1 <- structure(list(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 
28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 
44, 45), counts = c(5155L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L), density = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), mids = c(0.5, 1.5, 2.5, 
3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 
15.5, 16.5, 17.5, 18.5, 19.5, 20.5, 21.5, 22.5, 23.5, 24.5, 25.5, 
26.5, 27.5, 28.5, 29.5, 30.5, 31.5, 32.5, 33.5, 34.5, 35.5, 36.5, 
37.5, 38.5, 39.5, 40.5, 41.5, 42.5, 43.5, 44.5), xname = "log(1 + fmla[which(convmla == 1)])", 
    equidist = TRUE), class = "histogram")

h2 <- structure(list(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 
28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 
44, 45), counts = c(4287L, 0L, 0L, 0L, 0L, 0L, 443L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L), density = c(0.906342494714588, 0, 0, 0, 0, 0, 
0.0936575052854123, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0), mids = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 
9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5, 17.5, 18.5, 19.5, 
20.5, 21.5, 22.5, 23.5, 24.5, 25.5, 26.5, 27.5, 28.5, 29.5, 30.5, 
31.5, 32.5, 33.5, 34.5, 35.5, 36.5, 37.5, 38.5, 39.5, 40.5, 41.5, 
42.5, 43.5, 44.5), xname = "log(1 + fnls[which(convnls == 1)])", 
    equidist = TRUE), class = "histogram")

h3 <- structure(list(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 
28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 
44, 45), counts = c(73L, 0L, 0L, 0L, 0L, 0L, 18L, 10L, 14L, 19L, 
37L, 37L, 41L, 39L, 43L, 49L, 40L, 31L, 38L, 35L, 40L, 53L, 45L, 
37L, 41L, 34L, 35L, 41L, 40L, 42L, 45L, 28L, 44L, 45L, 30L, 28L, 
28L, 27L, 26L, 16L, 16L, 8L, 1L, 0L, 0L), density = c(0.0572998430141287, 
0, 0, 0, 0, 0, 0.0141287284144427, 0.00784929356357928, 0.010989010989011, 
0.0149136577708006, 0.0290423861852433, 0.0290423861852433, 0.032182103610675, 
0.0306122448979592, 0.0337519623233909, 0.0384615384615385, 0.0313971742543171, 
0.0243328100470958, 0.0298273155416013, 0.0274725274725275, 0.0313971742543171, 
0.0416012558869702, 0.0353218210361068, 0.0290423861852433, 0.032182103610675, 
0.0266875981161695, 0.0274725274725275, 0.032182103610675, 0.0313971742543171, 
0.032967032967033, 0.0353218210361068, 0.021978021978022, 0.0345368916797488, 
0.0353218210361068, 0.0235478806907378, 0.021978021978022, 0.021978021978022, 
0.0211930926216641, 0.0204081632653061, 0.0125588697017268, 0.0125588697017268, 
0.00627943485086342, 0.000784929356357928, 0, 0), mids = c(0.5, 
1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 
13.5, 14.5, 15.5, 16.5, 17.5, 18.5, 19.5, 20.5, 21.5, 22.5, 23.5, 
24.5, 25.5, 26.5, 27.5, 28.5, 29.5, 30.5, 31.5, 32.5, 33.5, 34.5, 
35.5, 36.5, 37.5, 38.5, 39.5, 40.5, 41.5, 42.5, 43.5, 44.5), 
    xname = "log(1 + fnls[which(convnls == 2)])", equidist = TRUE), class = "histogram")

h4 <- structure(list(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 
28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 
44, 45), counts = c(578L, 0L, 0L, 0L, 0L, 0L, 16L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L), density = c(0.973063973063973, 0, 0, 0, 0, 0, 0.0269360269360269, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), mids = c(0.5, 
1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 
13.5, 14.5, 15.5, 16.5, 17.5, 18.5, 19.5, 20.5, 21.5, 22.5, 23.5, 
24.5, 25.5, 26.5, 27.5, 28.5, 29.5, 30.5, 31.5, 32.5, 33.5, 34.5, 
35.5, 36.5, 37.5, 38.5, 39.5, 40.5, 41.5, 42.5, 43.5, 44.5), 
    xname = "log(1 + fnls[which(convnls == 3)])", equidist = TRUE), class = "histogram")


par(mfrow=c(2,2), mgp=c(2,0.5,0), mar=c(5,4,2,2), tcl=-0.5, xpd=NA)
plot(h1, col="grey", ylim=c(0,1), freq=FALSE, xlab="log(1+f)", main="marqLevAlg - istop=1 (N=5155)")
text(0.7, okm+0.05, paste(round(okm*100),"%", sep=""))
plot(h2, col="grey", ylim=c(0,1), freq=FALSE, xlab="log(1+f)", main="nls.lm - info=1 (N=4730)")
text(0.7, ok1+0.05, paste(round(ok1*100,1),"%", sep=""))
text(6.6, 1-ok1+0.05, paste(round((1-ok1)*100,1),"%", sep=""))
plot(h3, col="grey", ylim=c(0,1), freq=FALSE, xlab="log(1+f)", main="nls.lm - info=2 (N=1274)")
text(0.7, ok2+0.05, paste(round(ok2*100,1),"%", sep=""))
plot(h4, col="grey", ylim=c(0, 1), freq=FALSE, xlab="log(1+f)", main="nls.lm - info=3 (N=594)")
text(0.7, ok3+0.05, paste(round(ok3*100,1),"%", sep=""))
text(6.5, 1-ok3+0.05, paste(round((1-ok3)*100,1),"%", sep=""))

## ----figureWild, echo=FALSE, results='hide', fig.width=11, fig.height=5.5, out.width='100%', fig.cap="The Wild function of the help page of optim function. Global minimum appears in red."----
fw <- function (x){ 10*sin(0.3*x)*sin(1.3*x^2) + 0.00001*x^4 + 0.2*x+80}
plot(fw, -50, 50, n = 1000, ylab="fw")
points(-15.81515, fw(-15.81515), col=2, lwd=3, pch=3)

## ----gridsearch, results="asis", echo=FALSE-----------------------------------
resSANN <- optim(50, fw, method = "SANN",
                 control = list(maxit = 20000, temp = 20, parscale = 20))
res200 <- sapply(seq(-50,50, length.out=200), function(x){
    z <- mla(b=x, fn=fw)
    res <- c(NA, NA)
    if(z$istop==1) res <- c(z$fn.value, z$b)
    return(res)}) 

res <- cbind(c(resSANN$value, resSANN$par), res200[,which.min(res200[1,])] )
colnames(res) <- c("SANN", "grid search MLA")
rownames(res) <- c("minimum", "param")

## ----resgridsearch, results="asis", echo=FALSE--------------------------------
print(xtable(res, digits=4, align=c("l", "r", "r"), caption="Optimization results on the Wild function with algorithm SANN and MLA", label="tab:resgridsearch"), comment=FALSE, table.placement="h!")

