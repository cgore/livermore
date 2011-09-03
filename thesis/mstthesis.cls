%% Last update: November 19, 2007, by Christopher Mark Gore
%% <http://www.cgore.com>
%% <chris-gore@earthlink.net>
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Last update: Dec. 6, 2005 by IE. 
%%%%%%%%%%%%%%%%%%%%%
%% 
%% This is file `umrthsis.cls', for use at the University of Missouri-Rolla,
%% consistent with the thesis format approved in 1998.
%%
%% Version 1.1
%%
%% It is based on the standard LaTeX report class.
%% Written by James Friend, UMR, taken loosely from a thesis format
%% originally written by Harriet Borton (ITS), April 1996 and
%% umrthesis.sty (LaTeX 2.09), 1994.
%%
%% Last modified:  March 30, 1998 
%% 
%% 
%% This file requires caption2.sty, graphicx.sty,
%% lscape.sty and ulem.sty.
%% If this file came to you without those files, COMPLAIN!
%% 

%%%%%%%%%%%%%%%%%%%%%
%
% tweaked by Ilker Ersoy to comply with the document "SPECIFICATIONS
% FOR THESES AND DISSERTATIONS (T/D), UNIVERSITY OF MISSOURI-ROLLA,
% GRADUATE STUDIES, Revised 08-13-04"
%
%  changes: 
%  - subsection headings; no underlining, bold, uppercase.
%    (ulem.sty probably not necessary any more.)
%  - subsubsection etc. headings; no underlining, bold, lowercase. 
%  - dot separation in TOC.
%  - heading indentation in TOC (section number boxes).
%  - equations numbered sequentially (no section numbers, except appendices?).
%  - changed page margins so that page numbers are 1/2" from top.
%  - reduced widow/orphan penalties to avoid frantic typsetting. 
%  - made sure specialheads are uppercase.
%  - made sure sections are uppercase.
%  - made sure the whole thesis is double-spaced except the bibliography.
%  - made sure author name is uppercase in title page, but NOT in copyright page. 
%  - Copyright page corrected.
%  - page numbering in list of figures corrected.
%  - subsubsection headings followed by a period. 
%  - The periods after sections, subsections etc. defined properly. 
%
%   subsubsection headings should be capitalized manually. 
%
%  Word of advice: latex some times messes up page numbers, make sure 
%  you check TOC, LOF and LOT after running latex three times!
%
%  Word of advice 2: if a subsection starts on a new page, latex leaves 
%  some extra space before the heading increasing the upper margin, which 
%  is not desirable by the School of Arts and Sciences. 
%  Quick remedy: do NOT start a subsection on a new page (play with \looseness).
%
%  Word of advice 3: if ((latexperience<2yrs).and.(defense<2mos)) then   
%                        ret=useMSWord(goodluck)
%                    else
%                        ret=useLatex()  
%                    endif
%
%  Fall 2005 IE
%
%%%%%%%%%%%%%%%%%%%%%

\NeedsTeXFormat{LaTeX2e}
%\ProvidesClass{umrthsis}[1998/03/30 University of Missouri--Rolla]
%\ProvidesClass{umrthesis2}[2007/11/19 University of Missouri -- Rolla]
\ProvidesClass{mstthesis}[2008/01/28 Missouri University of Science and Technology]
\RequirePackage[]{ulem}
%% DON'T "RequirePackage" caption2.sty here!
\RequirePackage[]{graphicx}
\RequirePackage[]{lscape}

%% No Computer Modern fonts for us.
\RequirePackage{times}
\RequirePackage{mathptmx}
\DeclareMathAlphabet{\mathpzc}{OT1}{pzc}{m}{it}

%% We want the AMS styles.
\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{amsthm}

%% Different symbols than the default.
\RequirePackage{marvosym}

%  Note that the setspace package is built in (code included near the end of 
%  this file) to provide "line-and-a half spacing" (1.4 by default) and also
%  the singlespace environment. 

% UMR option chap:
\newif\ifchap  % true for chap option
  \chapfalse   % false by default
\DeclareOption{chap}{\chaptrue} % option to print "Chapter" at each new chapter
\newcommand\docsize{}  % to allow 10pt or 11pt to be specified as option
\typeout{MS\&T Thesis format 2008, Updated by Christopher Mark Gore, updated by IE, written by James Friend}
\DeclareOption{10pt}{\renewcommand\docsize{10pt}}
\DeclareOption{11pt}{\renewcommand\docsize{11pt}}
\DeclareOption{12pt}{\renewcommand\docsize{12pt}}
%  Prepare to load the standard report class (12pt):
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}
\ExecuteOptions{12pt}         % define 12pt as the default doc size
\ProcessOptions
\LoadClass[\docsize]{report}  % load report.cls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%  The following sections are revisions or additions to report.cls
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand{\emspace}{\hspace*{1em}}
%  FOOTNOTES:   make them continuously numbered throughout document
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% define command that can undo having footnotes reset with each chapter:
% (taken from removefr.sty by Donald Arseneau) 
\def\@removefromreset#1#2{\let\@tempb\@elt
   \expandafter\let\expandafter\@tempa\csname c@#1\endcsname
   \def\@elt##1{\expandafter\ifx\csname c@##1\endcsname\@tempa\else
         \noexpand\@elt{##1}\fi}%
   \expandafter\edef\csname cl@#2\endcsname{\csname cl@#2\endcsname}%
   \let\@elt\@tempb}
% use the command \@removefromreset to undo the \@addtoreset in report.cls:
\@removefromreset{footnote}{chapter}
%
% define command to allow people to reset footnote counter at will: 
\def\resetfootnote{\setcounter{footnote}{0}}  % definition to reset footnote


%%
%% These are the time and date handling functions.
%% This assumes English language and the Christian calendar.
%% This also assumes your computer has the correct time.
%%
\newcount\hour
\hour=\time
\divide\hour by 60
%%
\newcount\minute
\minute=\hour
\multiply\minute by 60
\minute=-\minute
\advance\minute by \time
%%
\newcommand{\clock}{%
	\number\hour:%
	\ifnum\minute<10
		0%
	\fi
	\number\minute
}
%%
\newcommand{\clockampm}{%
   \count1=\hour
   \ifnum\hour>12
      \advance\count1 by -12
   \fi
   \ifnum\hour<1
      \count1=12
   \fi
   \number\count1:%
   \ifnum\minute<10
      0%
   \fi
   \number\minute%
   \ifnum\hour>11
      ~pm%
   \else
      ~am%
   \fi
}
%%
\newcommand{\monthname}{%
	\ifcase\month\or
		January\or
		February\or
		March\or
		April\or
		May\or
		June\or
		July\or
		August\or
		September\or
		October\or
		November\or
		December\fi
}
%%
\renewcommand{\today}{\monthname~\number\day,~\number\year}
%%
\newcommand{\timestamp}{\clockampm,~\today}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PAGE LAYOUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% use \setlength here. IE

% SIDE MARGINS:
\if@twoside                 % Values for two-sided printing:
  \setlength{\oddsidemargin}{0.5in}    %   Left margin on odd-numbered pages.
  \setlength{\evensidemargin}{0.0in}   %   Left margin on even-numbered pages.
  \setlength{\marginparwidth}{0.5in}   %   Width of marginal notes.
\else                       % Values for one-sided printing:
  \setlength{\oddsidemargin}{0.5in}    %   Note that \oddsidemargin = \evensidemargin
  \setlength{\evensidemargin}{0.5in}
  \setlength{\marginparwidth}{0.5in}
\fi
\setlength{\marginparsep}{0.125in}     % Horizontal space between outer margin and
                                       % marginal note
\setlength{\textwidth}{6.0in}          % width of text
 
% VERTICAL SPACING:
                            % Top of page:
\setlength{\topmargin}{-0.7in}         %    distance from top of page to running head
\setlength{\headheight}{0.25in}        %    Height of box containing running head.
\setlength{\headsep}{0.45in}           %    Space between running head and text.
\setlength{\textheight}{9.0in}         %    space for text
\setlength{\footskip}{0.5in}           %    Distance from baseline of box containing foot
                                       %    to baseline of last line of text.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            SECTION HEADINGS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

\newcommand\chaptersize{\normalsize}
\newcommand\sectionsize{\normalsize}
\newcommand\subsectionsize{\normalsize}
\newcommand\subsubsectionsize{\normalsize}
\newcounter{firstchapter}
\setcounter{firstchapter}{0}
\newcounter{inappendix}
\setcounter{inappendix}{0}

\setcounter{secnumdepth}{4}    % Number subsubsections in the chapters
\setcounter{tocdepth}{4}       % Put subsubsections in the table of contents

% fixed the periods. IE
\renewcommand{\thesection}{\arabic{section}}
\renewcommand{\thesubsection}{\thesection.\arabic{subsection}}
\renewcommand{\thesubsubsection}{\thesubsection.\arabic{subsubsection}}
\renewcommand{\theparagraph}{\thesubsubsection.\arabic{paragraph}}
\@addtoreset{figure}{section}
\@addtoreset{table}{section}
%\@addtoreset{equation}{section} IE

% DON'T Print "CHAPTER" if chap option is specified:
\ifchap
  \renewcommand\@chapapp{}
\else
  \renewcommand\@chapapp{}
\fi

\def\specialhead#1{%   GENERAL HEADING WITHOUT A NUMBER (for abstract, etc.)
     \chapter*{\centering \uppercase{#1} \@mkboth{#1}{#1}}
      \thispagestyle{myheadings}
     \addcontentsline{toc}{chapter}{\uppercase{#1}}}

\renewcommand\chapter{\if@openright\cleardoublepage\else\clearpage\fi
                    \thispagestyle{plain}%
                    \global\@topnum\z@
                    \secdef\@chapter\@schapter}

\def\@chapter[#1]#2{\ifnum\c@firstchapter=0    % start of rpi added stuff
                      \if@twoside\cleardoublepage\suppressfloats[t]\fi
                      \pagenumbering{arabic} 
                      \setcounter{firstchapter}{1}
                    \fi
                    \renewcommand{\thepage}{\arabic{page}}
                    \thispagestyle{myheadings}
                    \pagestyle{myheadings}      % end of rpi added stuff
                    \ifnum \c@secnumdepth >\m@ne
                         \refstepcounter{chapter}%
                         \typeout{\@chapapp\space\thechapter.}%
                         \addcontentsline{toc}{chapter}%
                                   {\protect\numberline{\thechapter.}#1}%
                    \else
                      \addcontentsline{toc}{chapter}{#1}%
                    \fi
                    \chaptermark{#1}% 
                    \if@twocolumn
                      \@topnewpage[\@makechapterhead{#2}]%
                    \else
                      \@makechapterhead{#2}%
                      \@afterheading
                    \fi}
\def\@makechapterhead#1{%
  %\vspace*{0\p@}%
  {\parindent \z@ \raggedright \centering \normalfont \chaptersize
    \ifnum \c@secnumdepth >\m@ne
         \bfseries \@chapapp{} \thechapter . #1    % print "Chapter" and number
    \fi
    \vskip 36\p@
  }}

\def\@makeschapterhead#1{%    heading for chapter* command (no numbering)
  %\vspace*{0\p@}%
  {\parindent \z@ \raggedright \centering
    \normalfont  \chaptersize 
    \interlinepenalty\@M
    \bfseries  #1\par\nobreak
    \vskip 36\p@
  }}


% proper section defs. IE
% was it OK to reuse firstchapter here ?? IE

\renewcommand\section{\if@openright\cleardoublepage\else\clearpage\fi
                    \thispagestyle{myheadings}%
                    \ifnum\c@firstchapter=0\setcounter{firstchapter}{1}% If it's the
                       \pagenumbering{arabic}\thispagestyle{empty}
                    \fi%  first section, 
                    \global\@topnum\z@%   suppress the page number, and prevent renumbering
		    \@afterindenttrue
                    \secdef\@section\@ssection}
		   
\def\@section[#1]#2{\ifnum \c@secnumdepth >\m@ne
                     \refstepcounter{section}%
                     \addcontentsline{toc}{section}{\protect\numberline{\thesection .}\uppercase{#1}}%
                    \fi
		    \sectionmark{#1}
                    \@makesectionhead{#1}
		 %%   \@afterheading
                   }

\def\@ssection[#1]#2{\@makessectionhead{#1}
                     \@afterheading}	

\def\@makesectionhead#1{\vspace*{\baselineskip}%
            \parindent \z@
            \centerline{
            \ifnum \c@secnumdepth >\m@ne
              \textbf{\thesection .}\space
            \fi
            \normalfont\sectionsize\textbf{\uppercase{#1}} 
             } \par \nobreak \parindent 28\p@
	     \vskip 36\p@}
\def\@makessectionhead#1{\vspace*{\baselineskip}%
            \centerline{
            \normalfont\sectionsize\textbf{\uppercase{#1}} 
             } \par \nobreak \parindent 28\p@
	     \vskip 36\p@}



\renewcommand\subsection{\secdef\@subsection\@ssubsection}

\def\@subsection[#1]#2{\ifnum \c@secnumdepth >\m@ne
                       \refstepcounter{subsection}%
                      
\addcontentsline{toc}{subsection}{\protect\numberline{\thesubsection .}\uppercase{#1}}%
                     \fi
                     \@makesubsectionhead{#1}
                     }

\def\@ssubsection[#1]#2{\@makessubsectionhead{#1}}

\def\@makesubsectionhead#1{\vspace*{\baselineskip}%
           {\parindent \z@ \raggedright \bfseries 
            \ifnum \c@secnumdepth >\m@ne
              \ifnum\c@inappendix=0\@chapapp\space\fi\thesubsection .\space
            \fi
             \uppercase{#1}\par\nobreak           
             }}
\def\@makessubsectionhead#1{\vspace*{\baselineskip}%
            \bfseries  \uppercase{#1}\par\nobreak   
             }

\renewcommand\subsubsection{\secdef\@subsubsection\@ssubsubsection}

\def\@subsubsection[#1]#2{\ifnum \c@secnumdepth >\m@ne
                       \refstepcounter{subsubsection}%
                       \addcontentsline{toc}{subsubsection}{\protect\numberline{\thesubsubsection .}#1}%
                     \fi
                     \@makesubsubsectionhead{#1}
                     }

\def\@ssubsubsection[#1]#2{\@makessubsubsectionhead{#1}}

\def\@makesubsubsectionhead#1{%
           {\parindent 28\p@  \bfseries
            \ifnum \c@secnumdepth >\m@ne
            \ifnum\c@inappendix=0\@chapapp\space\fi\thesubsubsection .\space
            \fi
            \bfseries #1.\emspace\nobreak            
             }}

\def\@makessubsubsectionhead#1{%
            \parindent 28\p@
            \bfseries #1.\emspace\nobreak            
             }

\renewcommand\paragraph{\secdef\@paragraph\@sparagraph}

\def\@paragraph[#1]#2{\ifnum \c@secnumdepth >\m@ne
                       \refstepcounter{paragraph}%
                       \addcontentsline{toc}{paragraph}{\protect\numberline{\theparagraph .}#1}%
                     \fi
                     \@makeparagraphhead{#1}
                     }

\def\@sparagraph[#1]#2{\@makesparagraphhead{#1}}

\def\@makeparagraphhead#1{%
           {\parindent 28\p@  \bfseries
            \ifnum \c@secnumdepth >\m@ne
            \ifnum\c@inappendix=0\@chapapp\space\fi\theparagraph .\space
            \fi
            \bfseries #1\emspace\nobreak            
             }}

\def\@makesparagraphhead#1{%
            \parindent 28\p@
            \bfseries #1\emspace\nobreak            
             }


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  APPENDIX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\renewcommand\appendix{%
  \setcounter{inappendix}{1}%
  \setcounter{chapter}{0}%
  \setcounter{section}{0}%
  \setcounter{tocdepth}{2}% Only put sections in the table of contents
  \renewcommand{\thesection}{\Alph{section}}
  \renewcommand{\theequation}{\@Alph\c@section.\@arabic\c@equation}
  \renewcommand\@chapapp{\appendixname}%
  \renewcommand\thechapter{\@Alph\c@chapter}
  \renewcommand\section{\if@openright\cleardoublepage\else\clearpage\fi
           \thispagestyle{empty}%   suppress the page number
           \global\@topnum\z@
           \secdef\@appendx\@sappendx}% redefine the section format
                       }
% \renewcommand{\thesubsection}{\thesection\arabic{subsection}.}
% \renewcommand{\thesubsubsection}{\thesubsection\arabic{subsubsection}.}
% \renewcommand{\theparagraph}{\thesubsubsection\arabic{paragraph}.}


\def\@appendx[#1]#2{\ifnum \c@secnumdepth >\m@ne
                        \refstepcounter{section}%
                        \setcounter{equation}{0}%
                     \fi
                     \@makeappendixhead{#1}
                     }

\def\@sappendx[#1]#2{\@makesappendixhead{#1}}

\def\@makeappendixhead#1{%
           {\centering\vspace*{4in}
            \ifnum \c@secnumdepth >\m@ne
            \@chapapp\space \thesection\space
            \fi
            \par\vspace*{8pt} #1
                     \ifnum \c@section=1
                        \addtocontents{toc}{%
                        \parindent0pt\vskip12pt APPENDICES}~%toc entry
                     \fi
                     \addcontentsline{toc}{section}{\protect\numberline{\thesection .}#1}%
                     \clearpage  }          
             }

\def\@makesappendixhead#1{\centering\vspace*{4in} #1 \clearpage}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  FIGURES and TABLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%control float placement:
%
\setcounter{topnumber}{50}
\renewcommand\topfraction{.9}
\setcounter{bottomnumber}{50}
\renewcommand\bottomfraction{.9}
\setcounter{totalnumber}{50}
\renewcommand\textfraction{0.1}
\renewcommand\floatpagefraction{.9}
\setcounter{dbltopnumber}{5}
\renewcommand\dbltopfraction{.9}
\renewcommand\dblfloatpagefraction{.9}

%control float/float and float/text spacing
\setlength\floatsep    {48\p@ \@plus 4\p@ \@minus 8\p@}
\setlength\textfloatsep{48\p@ \@plus 4\p@ \@minus 8\p@}
\setlength\intextsep   {48\p@ \@plus 6\p@ \@minus 8\p@}


\long\def\@makecaption#1#2{%
  \vskip\abovecaptionskip
  \sbox\@tempboxa{#1.~~#2}
  \ifdim \wd\@tempboxa >\hsize
   {#1.~~#2}\par
  \else
    \global \@minipagefalse
    \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
  \fi
  \vskip\belowcaptionskip}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is a newer version of the algorithms package than is present in
%% the default installation from Ubuntu 7.10 ``Gutsy Gibbon''.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage[boxed,section]{algorithms/algorithm}
\RequirePackage[noend]{algorithms/algorithmic}
%% The title for the list of algorithms needs to match the others.
\renewcommand{\listalgorithmname}{LIST OF ALGORITHMS}
\algsetup{indent=2em,
   linenosize=\small,
   linenodelimiter=.}
\newcommand{\algorithmicinput}{\textbf{Input:}}
\newcommand{\INPUT}{\item[\algorithmicinput]}
\newcommand{\LET}{\STATE \textbf{let}\ }
\newcommand{\LETARROW}[1]{\LET #1 $\leftarrow$}
\newcommand{\PUSH}[2]{\STATE \textbf{push} #1 \textbf{onto} #2}
\renewcommand{\algorithmiccomment}[1]{\textit{~//~#1}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  TABLE of CONTENTS,  LIST OF TABLES,  LIST OF FIGURES 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\renewcommand\tableofcontents{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\contentsname
        \@mkboth{%
           \MakeUppercase\contentsname}{\MakeUppercase\contentsname}}%
    \vskip -1em \begin{singlespace}      % singlespacing
    \thispagestyle{myheadings}
    \@starttoc{toc}%
    \if@restonecol\twocolumn\fi
    \end{singlespace}
    }
 
\def\@dotsep{1} %% and fixed the rest. IE

\renewcommand*\l@chapter{\pagebreak[3]\vskip 10pt plus 1pt minus 1pt
                         \@dottedtocline{0}{0em}{0em}}
\renewcommand*\l@section{\vskip 6pt plus 1pt minus 1pt
                         \@dottedtocline{1}{1em}{1.2em}}
\renewcommand*\l@subsection{\vskip 4pt minus 1pt
                         \@dottedtocline{2}{2.3em}{2em}}
\renewcommand*\l@subsubsection{\ifnum\c@tocdepth>2 \vskip 3pt minus 1pt \fi
                         \@dottedtocline{3}{4.4em}{2.8em}}
\renewcommand*\l@paragraph{\ifnum\c@tocdepth>3 \vskip 2pt minus 1pt \fi
                         \@dottedtocline{4}{7.2em}{3.6em}}
 
\renewcommand\listoffigures{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\centering\listfigurename  % center it
      \@mkboth{\MakeUppercase\listfigurename}%
              {\MakeUppercase\listfigurename}}%
    \addcontentsline{toc}{chapter}{\listfigurename}  % add lof to toc
    \vskip -1em \begin{singlespace}  % singlespacing
    \thispagestyle{myheadings}\pagestyle{myheadings}
    \@starttoc{lof}%
    \if@restonecol\twocolumn\fi
    \end{singlespace}
    }

\renewcommand\listoftables{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\centering\listtablename  % center it
      \@mkboth{\MakeUppercase\listtablename}%
              {\MakeUppercase\listtablename}}%
    \addcontentsline{toc}{chapter}{\listtablename}  % add lot to toc
    \vskip -1em \begin{singlespace}  % singlespacing
    \@starttoc{lot}%
    \if@restonecol\twocolumn\fi
    \end{singlespace}
    \thispagestyle{myheadings}
    }

%\renewcommand\listofalgorithms{%
%    \if@twocolumn
%      \@restonecoltrue\onecolumn
%    \else
%      \@restonecolfalse
%    \fi
%    \chapter*{\centering\listalgorithmname  % center it
%      \@mkboth{\MakeUppercase\listalgorithmname}%
%              {\MakeUppercase\listalgorithmname}}%
%    \addcontentsline{toc}{chapter}{\listalgorithmname}  % add loa to toc
%    \vskip -1em \begin{singlespace}  % singlespacing
%    \thispagestyle{myheadings}
%    \@starttoc{loa}%
%    \if@restonecol\twocolumn\fi
%    \end{singlespace}
%    }

\newcommand\nomenclature{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\centering\listsymbolname  % center it
      \@mkboth{\MakeUppercase\listsymbolname}%
              {\MakeUppercase\listsymbolname}}%
    \addcontentsline{toc}{chapter}{\listsymbolname}  % add lot to toc
    \vskip -1em \begin{singlespace}  % singlespacing
    \@starttoc{los}%
    \if@restonecol\twocolumn\fi
    \end{singlespace}
    \thispagestyle{myheadings}
    }


% add symbol to los and in text with optional definition in the los
 \def\symbol{\@ifnextchar[{\@symboldef}{\@symbolnodef}}
 \def\@symboldef[#1]#2{\addcontentsline{los}{symbol}
    {\protect\makebox[1in][l]{#2} #1\dotfill}}
 \def\@symbolnodef#1{#1\addcontentsline{los}{symbol}{#1}}

% define the "symbol" list of symbols line
\newcommand{\l@symbol}[1]{\vskip 10pt plus 1pt minus 1pt
                          \noindent #1\dotfill\ }

\renewcommand*\l@figure{\vskip 10pt plus 1pt minus 1pt
                       \@dottedtocline{1}{0em}{2.8em}}
\let\l@table\l@figure

\let\rpicaption\caption
\let\lrpicaption\caption


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  BIBLIOGRAPHY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RPI def: for use in making an unnumbered bibliography with hanging indents
\def\bibentry{\vskip10pt\par\noindent\hangindent=40pt\frenchspacing}

% report definition modified for no automatic heading and use ragged right
\newcommand{\bibalign}{\raggedright}

\renewenvironment{thebibliography}[1]
     {\bibalign\frenchspacing
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \sloppy\clubpenalty4000\widowpenalty4000%
      \sfcode`\.=\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  FOR RPI TITLEPAGE, ABSTR. TITLEPAGE & COPYRIGHT PAGE 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \def\thesistitle#1{\gdef\@thesistitle{#1}}
 \def\author#1{\gdef\@author{#1}}
 \def\degree#1{\gdef\@degree{#1}}
 \def\department#1{\gdef\@department{#1}}
 \newcount\numcomm \numcomm=1
 \def\thesistype#1{\numcomm=#1}
 \def\thadviser#1{\gdef\@thadviser{#1}} 
 \def\projadviser#1{\gdef\@projadviser{#1}} \projadviser{*}
 \def\cothadviser#1{\gdef\@cothadviser{#1}} \cothadviser{*} 
 \def\coprojadviser#1{\gdef\@coprojadviser{#1}} \coprojadviser{*}
 \def\doctype{\if \@projadviser *Thesis \else Project \fi}
 \def\adviser{\if \@projadviser *\@thadviser \else \@projadviser\fi}
 \def\coadviser{\if \@coprojadviser *\@cothadviser\else \@coprojadviser\fi}
 \def\memberone#1{\gdef\@memberone{#1}}
 \def\membertwo#1{\gdef\@membertwo{#1}}
 \def\memberthree#1{\gdef\@memberthree{#1}}
 \def\memberfour#1{\gdef\@memberfour{#1}}
 \def\memberfive#1{\gdef\@memberfive{#1}}
 \def\membersix#1{\gdef\@membersix{#1}}
 \def\copyrightyear#1{\gdef\@copyrightyear{#1}} \copyrightyear{\the\year}
 \def\graddate#1{\gdef\@graddate{#1}}

\newcount\numcount



\def\titlepage{%

  \ifnum\numcomm<2 \typeout{---------------Masters Thesis----------}

     \thispagestyle{empty}
     \begin{singlespace}
     \begin{center}
         \parindent=0pt
         \vspace*{0.125in}
        \begin{doublespace} 
        {\uppercase\expandafter{\@thesistitle}}\\ [24pt]
         by \\ [24pt]
         \MakeUppercase\@author\\ [24pt]
         \end{doublespace}
         A \uppercase\expandafter{\doctype} \\[12pt]
         Presented to the Faculty of the Graduate School of the\\ [18pt]
         MISSOURI UNIVERSITY OF SCIENCE AND TECHNOLOGY\\ [18pt]
         in Partial Fulfillment of the Requirements for the Degree\\[24pt]
         \uppercase\expandafter{\@degree}~IN~\uppercase\expandafter{\@department} \\ [24pt]
         \@graddate\\[18pt]
         Approved by\\
     \vspace*{0.375in}
     \end{center}
     \if \coadviser *  % no coadviser
         \makebox[2.75in]{\hrulefill} \hspace*{\fill}
              \makebox[2.75in]{\hrulefill} \\
         \makebox[2.75in][l]{\adviser, Advisor} \hspace*{\fill}
              \makebox[2.75in][l]{\@memberone} \\
         \vspace*{0.375in}
         \begin{center}
          \makebox[2.75in]{\hrulefill} \\
               \makebox[2.75in][c]{\@membertwo} \\
         \end{center}
         \end{singlespace}
         \newpage
         \pagestyle{myheadings}
         \pagenumbering{roman}
         \setcounter{page}{2}
     \else
         \makebox[2.75in]{\hrulefill} \hspace*{\fill}
              \makebox[2.75in]{\hrulefill} \\
         \makebox[2.75in][l]{\adviser, Co-advisor} \hspace*{\fill}
              \makebox[2.75in][l]{\@memberone, Co-advisor} \\
         \vspace*{0.375in}
         \begin{center}
          \makebox[2.75in]{\hrulefill} \\
               \makebox[2.75in][c]{\@membertwo} \\
         \end{center}
         \end{singlespace}
         \newpage
         \pagestyle{myheadings}
         \pagenumbering{roman}
         \setcounter{page}{2}
     \fi

  \else  \typeout{---------------Doctoral Thesis----------}

     \thispagestyle{empty}
     \begin{singlespace}
     \begin{center}
         \parindent=0pt
         \vspace*{0.375in}
        \begin{doublespace} 
        {\uppercase\expandafter{\@thesistitle}}\\ [20pt]
         by \\ [20pt]
         \MakeUppercase\@author \\ [20pt]
         \end{doublespace}
         A DISSERTATION \\[18pt]
         Presented to the Faculty of the Graduate School of the\\ [18pt]
         UNIVERSITY OF MISSOURI--ROLLA\\ [18pt]
         in Partial Fulfillment of the Requirements for the Degree\\[24pt]
         \uppercase\expandafter{\@degree}\\[18pt]
         in\\[18pt]
         \uppercase\expandafter{\@department} \\[18pt]
         \@graddate\\
     \vspace*{0.3in}
     \end{center}
     \if \coadviser *  % no coadviser
         \makebox[2.75in]{\hrulefill} \hspace*{\fill}
              \makebox[2.75in]{\hrulefill} \\
         \makebox[2.75in][l]{\adviser, Advisor} \hspace*{\fill}
              \makebox[2.75in][l]{\@memberone} \\
         \vspace*{0.175in}
         \mbox{ } \\
         \makebox[2.75in]{\hrulefill} \hspace*{\fill}
              \makebox[2.75in]{\hrulefill} \\
         \makebox[2.75in][l]{\@membertwo} \hspace*{\fill}
              \makebox[2.75in][l]{\@memberthree} \\
         \vspace*{0.075in}
         \begin{center}
              \makebox[2.75in]{\hrulefill} \\
                   \makebox[2.75in][c]{\@memberfour} \\
         \end{center}
     \else
         \makebox[2.75in]{\hrulefill} \hspace*{\fill}
              \makebox[2.75in]{\hrulefill} \\
         \makebox[2.75in][l]{\adviser, Co-advisor} \hspace*{\fill}
              \makebox[2.75in][l]{\@memberone, Co-advisor} \\
         \vspace*{0.175in}
         \mbox{ } \\
         \makebox[2.75in]{\hrulefill} \hspace*{\fill}
              \makebox[2.75in]{\hrulefill} \\
         \makebox[2.75in][l]{\@membertwo} \hspace*{\fill}
              \makebox[2.75in][l]{\@memberthree} \\
         \vspace*{0.075in}
         \begin{center}
              \makebox[2.75in]{\hrulefill} \\
                   \makebox[2.75in][c]{\@memberfour} \\
         \end{center}
     \fi
     \end{singlespace}
     \newpage
     \pagestyle{myheadings}
     \pagenumbering{roman}
     \setcounter{page}{2}

  \fi
  }

%%%% 
 \def\abstitlepage{%
     \if@twoside\thispagestyle{empty}\cleardoublepage\fi
     \thispagestyle{empty}
     \begin{singlespace}
     \begin{center}
         \parindent=0pt
         \vglue 24pt plus 24pt minus 12pt
         {\uppercase\expandafter{\@thesistitle}}\\ [11pt]
         By \\ [11pt]
         \MakeUppercase\@author\\ [11pt]
         An Abstract of a Thesis Submitted to the Graduate\\ [7pt]
         Faculty of Rensselaer Polytechnic Institute\\ [7pt]
         in Partial Fulfillment of the\\ [7pt]
         Requirements for the Degree of\\ [7pt]
         \uppercase\expandafter{\@degree}\\ [7pt]
         Major Subject:~~\@department\\  [7pt]
         The original of the complete thesis is on file\\
         in the Rensselaer Polytechnic Institute Library\\
%       \vskip 36pt plus 2pt minus 12pt
       \vfill
       \sigblockabs 
       \vskip 36pt plus 2pt minus 12pt 
        Rensselaer Polytechnic Institute\\
        Troy, New York\\ [12pt]
         \@graddate
     \end{center}
     \end{singlespace}
     \newpage
     \pagestyle{myheadings}
     \pagenumbering{roman}
     \setcounter{page}{2}}
 
\def\copyrightpage{
    \hbox{ }
    \thispagestyle{empty}
    \vfill
    \begin{center}
    Copyright \copyright\ \@copyrightyear \\
    \@author \\
    All Rights Reserved \\ [12pt]
    \end{center}
    \clearpage}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The CODE FROM SETSPACE.STY:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\def\setspace@size{%
  \ifx \@currsize \normalsize
    \@normalsize
  \else
    \@currsize
  \fi
}

\def\doublespacing{%
  \ifcase \@ptsize \relax % 10pt
    \def \baselinestretch {1.667}%
  \or % 11pt
    \def \baselinestretch {1.618}%
  \or % 12pt
    \def \baselinestretch {1.667}%  Was 1.655 before (JRF)
  \fi
  \setspace@size
}

\def\onehalfspacing{%
  \ifcase \@ptsize \relax % 10pt
    \def \baselinestretch {1.25}%
  \or % 11pt
    \def \baselinestretch {1.213}%
  \or % 12pt
    \def \baselinestretch {1.241}%
  \fi
  \setspace@size
}

\def\singlespacing{%
  \def \baselinestretch {1}%
  \setspace@size
  \vskip \baselineskip  % Correction for coming into singlespace
}

\def\setstretch#1{%
  \renewcommand{\baselinestretch}{#1}}

%---Stretch the baseline BEFORE calculating the strut size. This improves
%   spacing below tabular environments etc., probably...
%   Comments are welcomed.

\def\@setsize#1#2#3#4{%
  % Modified 1993.04.07--GDG per KPC
  \@nomath#1%
  \let\@currsize#1%
  \baselineskip #2%
  \baselineskip \baselinestretch\baselineskip
  \parskip \baselinestretch\parskip
  \setbox\strutbox \hbox{%
    \vrule height.7\baselineskip depth.3\baselineskip width\z@}%
  \skip\footins \baselinestretch\skip\footins
  \normalbaselineskip\baselineskip#3#4}

%---Increase the space between last line of text and footnote rule.
%\skip\footins 20pt plus4pt minus4pt

%---Reset baselinestretch within floats and footnotes.

% GT:  This is where the conflict with the combination of the color
% package and the figure environment used to occur.

\let\latex@xfloat=\@xfloat
\def\@xfloat #1[#2]{%
  \latex@xfloat #1[#2]%
  \def \baselinestretch {1}\@normalsize  %%  GT: need \@normalsize?
  \normalsize
}

\long\def\@footnotetext#1{%
  \insert\footins{%
    \def\baselinestretch {1}%
    \reset@font\footnotesize
    \interlinepenalty\interfootnotelinepenalty
    \splittopskip\footnotesep
    \splitmaxdepth \dp\strutbox \floatingpenalty \@MM
    \hsize\columnwidth
    \@parboxrestore
    \protected@edef\@currentlabel{%
      \csname p@footnote\endcsname\@thefnmark}%
    \color@begingroup
      \@makefntext{%
        \rule\z@\footnotesep\ignorespaces#1\@finalstrut\strutbox}%
    \color@endgroup}}

\long\def\@mpfootnotetext#1{%
  \global\setbox\@mpfootins\vbox{%
    \unvbox \@mpfootins
    \def\baselinestretch {1}%
    \reset@font\footnotesize
    \hsize\columnwidth
    \@parboxrestore
    \protected@edef\@currentlabel{%
      \csname p@mpfootnote\endcsname\@thefnmark}%
    \color@begingroup
      \@makefntext{%
       \rule\z@\footnotesep\ignorespaces#1\@finalstrut\strutbox}%
   \color@endgroup}}

% A single spaced quote (say) is done by surrounding singlespace with quote.
%
\def\singlespace{%
  \vskip \baselineskip
  \def\baselinestretch {1}%
  \setspace@size
  \vskip -\baselineskip
}

\def\endsinglespace{\par}

%  spacing, doublespace and onehalfspace all are meant to INCREASE the
%  spacing (i.e. calling onehalfspace from within doublespace will not
%  produce a graceful transition between spacings)
%
% Next two definitions fixed for consistency with TeX 3.x

\def\spacing#1{%
  \par
  \begingroup             % moved from \endspacing by PGBR 29-1-91
    \def\baselinestretch {#1}%
    \setspace@size
}

\def\endspacing{%
    \par
    \vskip \parskip
    \vskip \baselineskip
  \endgroup
  \vskip -\parskip
  \vskip -\baselineskip
}

% one and a half spacing is 1.5 x pt size
\def\onehalfspace{%
  \ifcase \@ptsize \relax  % 10pt
    \spacing{1.25}%
  \or % 11pt
    \spacing{1.213}%
  \or % 12pt
    \spacing{1.241}%
  \fi
}

\let\endonehalfspace=\endspacing

% double spacing is 2 x pt size
\def\doublespace{%
  \ifcase \@ptsize \relax % 10pt
    \spacing{1.667}%
  \or % 11pt
    \spacing{1.618}%
  \or % 12pt
    \spacing{1.667}%  Was 1.655 before (JRF)
  \fi
}

\let\enddoublespace=\endspacing

% GT:  EMSH chose to omit display math part that follows.
% She wrote (see above) that the "altered spacing before and after displayed
% equations ... just looked too much".
%
% Fix up spacing before and after displayed math
% (arraystretch seems to do a fine job for inside LaTeX displayed math,
% since array and eqnarray seem to be affected as expected).
% Changing \baselinestretch and doing a font change also works if done here,
% but then you have to change @setsize to remove the call to @nomath)
%
\everydisplay\expandafter{%
  \the\everydisplay
  \abovedisplayskip \baselinestretch\abovedisplayskip
  \belowdisplayskip \abovedisplayskip
  \abovedisplayshortskip \baselinestretch\abovedisplayshortskip
  \belowdisplayshortskip \baselinestretch\belowdisplayshortskip
}
%%%%%%%%%%%%%%%%%%%%%%  End of setspace.sty %%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOME INITIALIZATIONS:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% make the following names uppercase:
\renewcommand\contentsname{TABLE OF CONTENTS}
\renewcommand\listfigurename{LIST OF ILLUSTRATIONS}
\renewcommand\listtablename{LIST OF TABLES}
\newcommand\listsymbolname{NOMENCLATURE}
\renewcommand\bibname{BIBLIOGRAPHY}
\renewcommand\indexname{INDEX}
\renewcommand\partname{PART}
\renewcommand\chaptername{SECTION}
\renewcommand\appendixname{APPENDIX}
\renewcommand\abstractname{ABSTRACT}

\renewcommand\chaptersize{\normalsize}
\renewcommand\sectionsize{\normalsize}
\renewcommand\subsectionsize{\normalsize}
\renewcommand\subsubsectionsize{\normalsize}

% fixed the following. IE
\renewcommand{\thechapter}{\arabic{chapter}}
%\renewcommand\theequation
%  {\thesection\@arabic\c@equation} IE
\renewcommand\theequation
  {\@arabic\c@equation}
\renewcommand\thefigure
     {\thesection.\@arabic\c@figure}
\renewcommand\thetable
     {\thesection.\@arabic\c@table}

\raggedbottom
%\widowpenalty 10000    % completely discourage widow lines
%\clubpenalty 10000     % completely discourage club (orphan) lines
%\brokenpenalty 10000   % completely discourage hyphenation across pages
% IE
\widowpenalty 4000 
\clubpenalty 4000  
\brokenpenalty 4000

\setstretch{1.4}        % at 11 or 12pt this is line-and-a-half spacing
\setlength{\parindent}{28pt} % Width of paragraph indentation
                        % Don't change this number unless you change the values in 
                        % the definitions for the subsubsections! (28\p@)

\markboth{}{}           % Do not include chapter titles in headers;
\pagestyle{myheadings}  % include just page numbers in upper right
\normalem

%\renewcommand{\bibalign}{\raggedright}  %  bibliography is ragged right
\renewcommand{\bibalign}{}  %  bibliography is justified

\endinput
%% 
%% End of file.
