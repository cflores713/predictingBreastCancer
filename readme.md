\documentclass[12pt]{article}
\usepackage{lingmacros}
\usepackage{tikz}
\usetikzlibrary{arrows}
\usetikzlibrary{graphs}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{xcolor}
\usepackage{empheq}
\usepackage{graphicx}
\usepackage{hyperref}

\tikzset{
  every node/.style={draw=black},
  red/.style={draw=red},
  blue/.style={draw=blue},
  purple/.style={draw=purple},
  green/.style={draw=green},
  violet/.style={draw=violet},
}

\newcommand{\boxedeq}[2]{\begin{empheq}[box={\fboxsep=6pt\fbox}]{align}\label{#1}#2\end{empheq}}

\newcommand\tab[1][1cm]{\hspace*{#1}}

\begin{document}
\noindent
\begin{center}
	{\large
	\textbf{ \\ Using Radial SVM Models to Predict Breast Cancer Diagnoses From Fine Needle Aspiration Biopsy Data}
	\\ } \textbf{Carlos Flores, May 3, 2019}\\
	Computer Science Department, University of Houston, Houston, TX 77004
	\\cflores28@uh.edu\\
	\includegraphics[scale=.25]{/Users/Carlos/Desktop/MLProject2/fna.jpg}
	\includegraphics[scale=.32]{/Users/Carlos/Desktop/MLProject2/kmScale30.png}
\end{center}
\tab
\href{https://commons.wikimedia.org/wiki/File:Breast_fibroadenoma_by_fine_needle_aspiration_(1)_DG_stain.jpg}{Source: Wikipedia Commons (Permission is granted under the terms of the GNU Free Documentation License).}\\

\noindent
\begin{center}
	\textbf{Abstract}
\end{center}
Recent studies have found that women with a history of a false-positive mammogram results may be at increased risk of developing subsequent breast cancer, but the origin of these regularities has remained opaque.$^{[1]}$ I analyzed the physical characteristics of biopsy data to develop various successful predictive models needed to for such regularities to emerge. The result is a Radial SVM model to help radiologists and pathologists to investigate abnormal cellular growth irrespective of preconceived demographic biases. The model efficiently leverages statistical information by training only on the feature space of the physical bio-markers. Therefore, creating a meaningful substructure with significantly less computational demand given the reduced dimensionality. Two other models, KMeans and KNN, performed similarly and give us greater insight into the emergent regularities.\\

\noindent
{\scriptsize $[1]$ Henderson LM, Hubbard RA, Sprague BL, Zhu W, and Kerlikowske K: "Increased Risk of Developing Breast Cancer after a False-Positive Screening Mammogram", Cancer Epidemiol Biomarkers Prev, December 1 2015 (24) (12) 1882-1889; DOI: 10.1158/1055-9965.EPI-15-0623 {\color{blue} \href{http://cebp.aacrjournals.org/content/24/12/1882.full?sid=c5f1ad0f-4c35-46cc-aeeb-49db070d9426}{link}}
}

\end{document}}