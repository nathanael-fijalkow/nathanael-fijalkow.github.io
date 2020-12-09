%\ProvidesClass{mybook}[2006/10/06 TP-Caml]
\NeedsTeXFormat{LaTeX2e}[1995/06/01]
\LoadClass[10pt]{article}

% packages
\usepackage[ec]{aeguill} 
%\usepackage[utf8]{inputenc}
\usepackage{bookman}
\usepackage[rflt]{floatflt}
\usepackage{amssymb,amsmath,amsthm}
\usepackage{graphicx}
\usepackage{gastex}
\usepackage[usenames]{color}


% ============================================================================
% Commandes
% ============================================================================

\def\mynote#1{{\sf $\clubsuit$ #1 $\clubsuit$}}

\newcommand{\F}{\mathcal{F}}
\newcommand{\R}{\mathbb{R}}

\newcommand{\ar}{[|}
\newcommand{\ra}{|]}

\newcommand{\li}{[}
\newcommand{\il}{]}

\newcommand{\concat}{$\colon\colon$}
\newcommand{\interval}[1]{[ #1 ]}
\newcommand{\caml}{\texttt{caml}}

% ============================================================================
% Dimensions
% ============================================================================
% Dimensions de la page
\oddsidemargin    -0.7in
\evensidemargin   -1in
\textwidth      7.5in
\headheight       1.3in
\topmargin        -2in
\textheight=10in

\newcommand{\mymail}{nathanael.fijalkow@gmail.com}
\newcommand{\mypage}{http://www.liafa.jussieu.fr/$\sim$nath/}

\newcommand{\set}[1]{\{ #1 \}}
\newcommand{\N}{\mathbb{N}}
\newcommand{\VE}{V_{Eve}}
\newcommand{\VA}{V_{Adam}}
\newcommand{\W}{\mathcal{W}}
\newcommand{\WE}{\W_{Eve}}
\newcommand{\WA}{\W_{Adam}}
\newcommand{\A}{\mathcal{A}}
\newcommand{\Attr}{\mathrm{Attr}}
\newcommand{\rg}{\mathrm{rg}}
\newcommand{\Succ}{\mathrm{Succ}}
\newcommand{\Pred}{\mathrm{Pred}}

% ============================================================================
% Page de titre
% ============================================================================

\def\@tp{}
\newcommand{\tp}[2]{\def\@tp{#1}\title{#2}}

\def\maketitle%
{\noindent{\Large\sc TP \@tp --- \@title}%
  \hfill%
  \par\hbox{\texttt{\mypage{}tp\@tp/tp\@tp.pdf}}\par%
  \vskip -8pt%
  \noindent\hrulefill\par%
  {Les questions sont les bienvenues et peuvent \^etre envoy\'ees
   \`a \texttt{\mymail}.}
  \vskip 6pt}

% ============================================================================
% Gestion de chapitres, sections...
% ============================================================================
   
% Num�ros de section
\def\thesection {\arabic{section}}

% Aspect de section
\def\section#1{%
  %\ifhmode\par\fi
  \removelastskip
  \vskip 5ex\goodbreak
  \refstepcounter{section}%
  %\hbox to \hsize{\hss\vbox{\advance\hsize by -1cm%
   \noindent%
   {\leavevmode\Large\raggedright%
    \thesection\ \sc#1}\par% 
   \noindent\vskip -\parskip\vskip -2ex\hrulefill%
   %}}
  \nobreak\vskip 1ex\nobreak%
  }
  
% Aspect de sous-section
\def\subsection#1{%
  \removelastskip%
  \vskip 4ex\goodbreak%
  \refstepcounter{subsection}%
   \noindent%
   {\leavevmode\large\raggedright%
    \thesubsection\ \sc#1}\par%
  \nobreak\vskip 1ex\nobreak%
  }


\def\subsubsection#1{%
  \removelastskip!%
  \vskip 4ex\goodbreak%
  \hbox to \hsize{\hss\vbox{\advance\hsize by 0.0cm
      \noindent
      {\leavevmode\large\raggedright#1}\par%
      }}%
  \nobreak\vskip 2ex\nobreak%
  }
  
% ============================================================================
% Environnements 
% ============================================================================

% Bo�te pour mettre du code
\newsavebox{\thecode}

% Bo�te d'image
\newcommand{\sideimage}[1]{%
  \begin{floatingtable}{%
      \begin{minipage}{3.21in}%
        {\includegraphics[width=3.2in]{{#1}}}%
      \end{minipage}%
}\end{floatingtable}}

% Deux images
\newcommand{\twoimages}[2]{%
  \begin{center}%
    \hfill%
    {\includegraphics[width=3.2in]{{#1}}}%
    \hfill%
    {\includegraphics[width=3.2in]{{#2}}}%
    \hfill%
  \end{center}}

% Du code non format�
\newcommand{\code}[1]{\hbox{\tt #1}}

% Bo�te de code
\newenvironment{codebox}
  {\noindent
    \vspace{2pt}%
   \begin{lrbox}{\thecode}%
   \begin{minipage}{3in}%
   \tt\small%
   }
  {\end{minipage}
   \end{lrbox}%
   \begin{floatingtable}{
   \fbox{%
    \usebox{\thecode}
    }\vspace{2pt}}
    \end{floatingtable}
   }
% Ligne de code non format�
\newenvironment{codeline}
  {\begin{codebox}}
  {\end{codebox}}

% Du code format�.
\newenvironment{codesection}
  {%
   \begin{codebox}%
   \hfill\\
   \vspace{-24pt}%
   \begin{tabbing}%
   \phantom{aa}\=\phantom{aa}\=\phantom{aa}\=%
   \phantom{aa}\=\phantom{aa}\=\phantom{aa}\=\kill%
  }
  {\end{tabbing}%%
   \end{codebox}}
   
% Le num�ro de la question
\newcounter{nbquest}

% Une question
\newenvironment{question}
  {\stepcounter{nbquest}%
    \vspace{8pt}\par$\triangleright$ {\bf Question \arabic{nbquest}.} }
  {$\triangleleft$\par\vspace{8pt}}