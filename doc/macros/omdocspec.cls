%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of the LaTeX sources of the OMDoc 1.8 specifiation
% Copyright (c) 2007 Michael Kohlhase
% This work is licensed by the Creative Commons Share-Alike license
% see http://creativecommons.org/licenses/by-sa/2.5/ for details
% The source original is at https://github.com/KWARC/OMDoc/doc/macros
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\NeedsTeXFormat{LaTeX2e}[1999/12/01]
\ProvidesClass{omdocspec}[2007/7/11 OMDoc Specification Class]
\DeclareOption{extrefs}{\PassOptionsToPackage{\CurrentOption}{sref}}
\DeclareOption{report}{\PassOptionsToPackage{\CurrentOption}{omdoc}}
\DeclareOption{book}{\PassOptionsToPackage{\CurrentOption}{omdoc}}
\ProcessOptions
%\LoadClass[envcountsame,envcountchap]{svmono}
\RequirePackage{fancyhdr}
\LoadClass[twoside,titlepage,a4paper]{report}
\RequirePackage{a4wide}
\RequirePackage{makeidx}         % allows index generation
\RequirePackage{graphicx}        % standard LaTeX graphics tool
                             % when including figure files
\RequirePackage{multicol}        % used for the two-column index
%\RequirePackage[bottom]{footmisc}% places footnotes at page bottom
% end Springer recommendation
\RequirePackage{array}
\RequirePackage{longtable}
\RequirePackage{moreverb}
\RequirePackage{amssymb}
\RequirePackage{amstext}
\RequirePackage{url}
\RequirePackage{wrapfig}
\RequirePackage{makeidx}
\RequirePackage{wasysym}
\RequirePackage{textcomp} %standard
\RequirePackage[hyper]{acronyms}
\RequirePackage{ded}
\RequirePackage{myref}
\RequirePackage{xmlindex} % ../macros
\RequirePackage{tikz}
\usetikzlibrary{shapes}
\usetikzlibrary{arrows}
\RequirePackage{alltt}
\RequirePackage{dcm}
\RequirePackage{omdoc}
\RequirePackage{statements}
\RequirePackage{paralist}
\RequirePackage{gitinfo2}

\newenvironment{presonly}{}{}

\fancyhead[RE,LO]{\omdoc}\fancyhead[LE,RO]{page~\thepage}
\pagestyle{fancyplain}

\RequirePackage[a4paper=true,  bookmarks=true, linkcolor=black,
citecolor=black,urlcolor=black,colorlinks=true,
breaklinks=true, bookmarksopen=true]{hyperref}


%%%%%%%%%%%%%%%%%%%%%%%%%%%% listings %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\RequirePackage[savemem]{listings} % assuming ../macros is early on TEXINPUTS path
\RequirePackage{lstomdoc}
\lstset{float=htb,columns=flexible,frame=lines,language=[1.6]OMDoc,basicstyle=\scriptsize,
        numbers=left,stepnumber=5,numberstyle=\tiny,showstringspaces=false}

%% to number the listings in sequence with figures
%\let\c@lstlisting\c@figure

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modules %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\def\@@module#1#2#3{% module name, DTD/Schema/spec
\index{#1 (#3)!#2 Module}%
\index{#2 Module!#1 (#3)}%
\index{#3!#2 Module #1}#1}   
\def\METAmodule#1{\@@module{DC}{#1}{Metadata}} 
\def\MOBJmodule#1{\@@module{MOBJ}{#1}{Mathematical Objects}} 
\def\MTXTmodule#1{\@@module{MTXT}{#1}{Mathematical Text}} 
\def\STmodule#1{\@@module{ST}{#1}{Mathematical Statements}} 
\def\ADTmodule#1{\@@module{ADT}{#1}{Abstract Data Types}} 
\def\PFmodule#1{\@@module{PF}{#1}{Proofs and Arguments}} 
\def\CTHmodule#1{\@@module{CTH}{#1}{Complex Theories}} 
\def\DGmodule#1{\@@module{DG}{#1}{Development Graphs}} 
\def\EXTmodule#1{\@@module{EXT}{#1}{Extensions}} 
\def\PRESmodule#1{\@@module{PRES}{#1}{Presentation}} 
\def\QUIZmodule#1{\@@module{QUIZ}{#1}{Exercises}} 
\def\DOCmodule#1{\@@module{DOC}{#1}{Document Structure}} 
\def\RTmodule#1{\@@module{RT}{#1}{Rich Text Structure}} 

%=====================================================
%fette Buchstaben

\newcommand{\bA}{{\bf A}}\newcommand{\bB}{{\bf B}}
\newcommand{\bC}{{\bf C}}\newcommand{\bE}{{\bf E}}
\newcommand{\bF}{{\bf F}}\newcommand{\bG}{{\bf G}}
\newcommand{\bH}{{\bf H}}\newcommand{\bI}{{\bf I}}
\newcommand{\bJ}{{\bf J}}\newcommand{\bK}{{\bf K}}
\newcommand{\bL}{{\bf L}}\newcommand{\bO}{{\bf O}}
\newcommand{\bP}{{\bf P}}\newcommand{\bQ}{{\bf Q}}
\newcommand{\bS}{{\bf S}}\newcommand{\bT}{{\bf T}}
\newcommand{\bU}{{\bf U}}\newcommand{\bV}{{\bf V}}
\newcommand{\bW}{{\bf W}}\newcommand{\bX}{{\bf X}}
\newcommand{\bY}{{\bf Y}}\newcommand{\bZ}{{\bf Z}}
\newcommand{\bN}{{\bf N}}\newcommand{\bM}{{\bf M}}
\newcommand{\bD}{{\bf D}}\newcommand{\bR}{{\bf R}}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% specal abbreviations for \bf and \cal math symbols

\def\cA{{\cal A}}\def\cB{{\cal B}}\def\cC{{\cal C}}
\def\cD{{\cal D}}\def\cE{{\cal E}}\def\cF{{\cal F}}
\def\cG{{\cal G}}\def\cH{{\cal H}}\def\cI{{\cal I}}
\def\cJ{{\cal J}}\def\cK{{\cal K}}\def\cL{{\cal L}}
\def\cM{{\cal M}}\def\cN{{\cal N}}\def\cO{{\cal O}}
\def\cP{{\cal P}}\def\cQ{{\cal Q}}\def\cR{{\cal R}}
\def\cS{{\cal S}}\def\cT{{\cal T}}\def\cU{{\cal U}}
\def\cV{{\cal V}}\def\cW{{\cal W}}\def\cX{{\cal X}}
\def\cY{{\cal Y}}\def\cZ{{\cal Z}}

%=====================================================
% blackboard Buchstaben

\newcommand{\bbA}{{\mathbb{A}}}\newcommand{\bbB}{{\mathbb{B}}}
\newcommand{\bbC}{{\mathbb{C}}}\newcommand{\bbE}{{\mathbb{E}}}
\newcommand{\bbF}{{\mathbb{F}}}\newcommand{\bbG}{{\mathbb{G}}}
\newcommand{\bbH}{{\mathbb{H}}}\newcommand{\bbI}{{\mathbb{I}}}
\newcommand{\bbJ}{{\mathbb{J}}}\newcommand{\bbK}{{\mathbb{K}}}
\newcommand{\bbL}{{\mathbb{L}}}\newcommand{\bbO}{{\mathbb{O}}}
\newcommand{\bbP}{{\mathbb{P}}}\newcommand{\bbQ}{{\mathbb{Q}}}
\newcommand{\bbS}{{\mathbb{S}}}\newcommand{\bbT}{{\mathbb{T}}}
\newcommand{\bbU}{{\mathbb{U}}}\newcommand{\bbV}{{\mathbb{V}}}
\newcommand{\bbW}{{\mathbb{W}}}\newcommand{\bbX}{{\mathbb{X}}}
\newcommand{\bbY}{{\mathbb{Y}}}\newcommand{\bbZ}{{\mathbb{Z}}}
\newcommand{\bbN}{{\mathbb{N}}}\newcommand{\bbM}{{\mathbb{M}}}
\newcommand{\bbD}{{\mathbb{D}}}\newcommand{\bbR}{{\mathbb{R}}}

\def\mobjabbr{OMOBJ |m:math |legacy}
 \def\sdot{.}\def\deq{\colon=}

\def\boexchen{\hbox{\vrule\vbox{\hrule width 6 pt\vskip 6pt\hrule}\vrule}}
\def\kasten{\hfil\null\nobreak\hfill\boexchen\par\smallskip}
\def\seen#1{seen #1}

\def\@endpart{\par\strut\vspace*{4cm}\par\noindent}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Project descriptions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newenvironment{projectdescription}%
{\clearpage\bgroup\newenvironment{pabstract}{\paragraph{Abstract}}{cl}}%
{\egroup}

\def\pinstN#1{$^{#1}$}
\def\and{, }
\def\pauthors#1{Authors & #1\\}
\def\pinstituteN#1#2{&$^{#1}$  #2\\}
\def\pinstitute#1{& #1\\}

\newenvironment{project}[2]% label, project home page
{\gdef\theprojectlabel{sec:#1}%
\begin{center}\begin{tabular}{|l|p{8cm}|}\hline Project Home & \footnotesize\url{#2}\\\hline}
{\hline\end{tabular}\label{\theprojectlabel}\end{center}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The sets of numbers 

\def\NN{\bbN} % natural numbers
\def\PP{\bbP} % positive natural numbers
\def\ZZ{\bbZ} % integers
\def\RR{\bbR} % real numbers
\def\CC{\bbC} % complex numbers
\def\QQ{\bbQ} % rational numbers

\def\Set#1{\left\{#1\right\}}\def\set#1{\{#1\}}
\def\Setst#1#2{\left\{#1\right\left|#2\}}\def\setst#1#2{\{#1\bigl|#2\}}
\def\allcdot#1#2{{\forall{#1}.{#2}}}
\def\excdot#1#2{{\exists{#1}.{#2}}}
\def\exucdot#1#2{{\exists^1{#1}.{#2}}}
\def\thatcdot#1#2{{\tau{#1}.{#2}}}
\def\implies{\Rightarrow}
\def\webpageat{web page at }
\def\July{ July}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Systems from the 

\def\fp{$\mathcal{FP}$}
\def\java{\scsys{JAVA}}
\def\verifun{{\mbox{{\Large $\checkmark $}\hspace{-0.23cm}\texttt{eriFun} \hspace{-0.2cm}} }}
\newcommand{\MathDox}{\scsys{MathDox}}
\newcommand{\DocBook}{{\scsys{DocBook}}}
\newcommand{\Gap}{{\scsys{Gap}}}
\def\@stex{{\raisebox{-.5ex}S\kern-.5ex\TeX}}
\def\stex{\@stex\index{sTeX@\protect\@stex}}
\def\sTeX{\stex}
\def\latexml{\scsys{LaTeXML}}
\def\sentido{\scsys{Sentido}}
\def\algebra{\scsys{Algebra}}
\def\theorema{\scsys{Theorema}}
\def\maxima{\scsys{Maxima}}
\def\yacas{\scsys{Yacas}}
\def\maple{\scsys{Maple{\texttrademark}}}
\def\mathematica{\scsys{Mathematica{\textsuperscript{\textregistered}}}}
\def\openoffice{\scsys{OpenOffice}}
\newcommand{\MMISS}{\scsys{MMiSS}}
\def\mmiss{\MMISS}
\def\mmlsearch{{\scshape{MathWebSearch}}}
\def\google{{\scshape{Google}}}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sharing between report and book

\def\report{book}

\def\llquote#1{\ensuremath{\langle\kern-.25em\langle\hbox{\sl{#1}}\rangle\kern-.25em\rangle}}

\newenvironment{changelog}% the module
{\begin{center}\footnotesize\begin{longtable}{|l|c|p{7.1cm}|l|}\hline
  element & state & comments & cf.\\\hline\hline} 
{\end{longtable}\end{center}}

%specializing the specification head for the DCMetadata environment
\def\dcm@spechead%
{\begin{titlepage}
    \null\vfil\vskip 60\p@
    \begin{center}
    {\LARGE \dcm@title \par}\vskip 3em%
    {\large
     \lineskip .75em%
      \begin{tabular}[t]{c}%
        \dcm@creators
      \end{tabular}\par}%
      \vskip 1.5em%
    {\large \dcm@date \par}%       % Set date in \large size.
    \vskip 2em
  \end{center}\vskip2em
  \begin{quote}
    {\bf Abstract:}\dcm@abstract
  \end{quote}
  \vskip 2em\par\vfil\noindent
  {\small\noindent\dcm@rights\dcm@license}
  \end{titlepage}}

%specializing the specification head for the DCMetadata environment
\def\dcm@bookhead%
{\begin{titlepage}
    \null\vfil\vskip 60\p@
    \begin{center}
    {\LARGE \dcm@title \par}\vskip 3em%
    {\large
     \lineskip .75em%
      \begin{tabular}[t]{c}%
        \dcm@creators
      \end{tabular}\par}%
      \vskip 1.5em%
    {\large \dcm@date \par}%       % Set date in \large size.
    \vskip 2em
  \end{center}
\end{titlepage}}

\def\tracticket#1{\href{https://trac.omdoc.org/OMDoc/ticket/#1}{Ticket #1} in the OMDoc TRAC}
\def\psom#1{\mbox{\fbox{\ensuremath{#1}}}}

%%%%%%%% svmono compatibility 
\def\subtitle{}
\def\frontmatter{}
\def\mainmatter{}
\def\backmatter{}

%%%%%%%%%%%%%%% purely presentational stuff
\def\presem{\em}
\def\presbf{\bf}

\def\twin#1#2{\@twin{#1}{#2}}
\def\atwin#1#2{\@atwin{#1}{#2}}
\newcommand{\eldef}[2][]{% keyval, element
\def\ns@elt{}\setkeys{ns}{#1}\definiendum{\ns@elt{#2}}
\sref@label{Definition ????}{eldef.\ns@elt #2}
\ifmargins\marginpar{\fbox{\snippet{\ns@elt #2}}}\fi%
\index{#2@{\snippet{\ns@elt #2}! element}}%
\index{\ns@elt #2@{\snippet{\ns@elt #2}! element}}}

%%% Local Variables: 
%%% mode: latex
%%% TeX-master: "omdoc"
%%% End:
 

% LocalWords:  pdf PDFLaTeX gz jpg bb gunzip tmp rm ps png TEXINPUTS htb CDM PF
% LocalWords:  basicstyle indexstyle stepnumber numberstyle showstringspaces DG
% LocalWords:  MOBJ MTXT ADT CTH PRES RT fette Buchstaben specal cl omdoc OMOBJ
% LocalWords:  lstlisting FP eriFun MathDox DocBook sTeX LaTeXML Sentido Maxima
% LocalWords:  Theorema Yacas OpenOffice MMiSS MathWebSearch Google
