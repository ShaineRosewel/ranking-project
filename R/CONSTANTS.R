# Sys.setenv(PATH = paste(Sys.getenv("PATH"), "/usr/bin", "/bin", sep = ":"))
LOW <- list(CHAR = "Coverage", NAME = "Low")
MED <- list(CHAR = "Coverage", NAME = "Moderate")
HIGH <- list(CHAR = "Coverage", NAME = "High")

COVERAGE <- list(CHAR = "coverage", NAME = "Coverage")
T1 <- list(CHAR = "t1", MATHNAME = "$T_1$")
T2 <- list(CHAR = "t2", MATHNAME = "$T_2$")
T3 <- list(CHAR = "t3", MATHNAME = "$T_3$")

ORDERED <- list(CHAR = "Coverage", NAME = "Ordered")
UNORDERED <- list(CHAR = "Coverage", NAME = "Unordered")

BLOCK_DIAGONAL <- list(CHAR = "Coverage", NAME = "Block Diagonal", MATHNAME = "$\\mathbf{R}_{{block}}$", GGNAME = "$\\textbf{R}_{block}$")
EQUICORRELATED <- list(CHAR = "Coverage", NAME = "Equicorrelated", MATHNAME = "$\\mathbf{R}_{{eq}}$", GGNAME = "$\\textbf{R}_{eq}$")

NONRANK <- list(RAWCHAR = "nonrankbased", SHORTNAME = 'Proposed Unordered', MATHNAME = "$\\mathfrak{R}_1$", GGNAME = "$\u211c_1$")
BONF <- list(RAWCHAR = "bonferroni", SHORTNAME = 'KWW-Bonf', MATHNAME = "$\\mathfrak{R}_{Bonf}$", GGNAME = "$\u211c_{Bonf}$")
IND <- list(RAWCHAR = "independent", SHORTNAME = 'KWW-Ind', MATHNAME = "$\\mathfrak{R}_{Ind}$", GGNAME = "$\u211c_{Ind}$")
ASYMP <- list(RAWCHAR = "rankbased_asymptotic", SHORTNAME = 'Proposed Asymp', MATHNAME = "$\\mathfrak{R}_{2,Asymp}$", GGNAME = "$\u211c_{Asymp}$")
BOOT <- list(RAWCHAR = "rankbased_level2bs", SHORTNAME ='Proposed Boot', MATHNAME = "$\\mathfrak{R}_{2,Boot}$", GGNAME = "$\u211c_{2, Boot}$")

CORR_COEFF <- list(CHAR = "r", MATHNAME = '$\\rho$')

CORR_MATRIX <- list(CHAR = 'R', MATHNAME = "$\\textbf{R}$")