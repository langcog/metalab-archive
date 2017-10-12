%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
MIT Press >>Open Mind<< Journal LaTeX Package
Amy Hendrickson
amykaren@mit.edu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

21 Files and what they are used for:

%%%%%%%%%%%%%%%%%%
Basic macro file:
%%%%%%%%%%%%%%%%%%

stjour.cls   
  Used: \documentclass[OpenMind]{stjour}
  Or, for double spaced manuscript form: 
        \documentclass[manuscript]{stjour}
        \journalname{Open Mind} 
        (\journalname{} only needed when [manuscript] option is used.)

%%%%%%%%%%%%%%%%%%
Graphics files:
%%%%%%%%%%%%%%%%%%

fig1.eps/.pdf For use in OpenMindSample. 
              Figure sample file in .eps form for dvips, and .pdf
	      form for pdflatex. 

colophon.pdf/.eps 
Cross-Mark.pdf/.eps 
OPMI\_logo.pdf/.eps
openaccess3.pdf/.eps
             All files needed for formatting first page of article.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Sample Files}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

OpenMindSample.tex/.pdf Sample file to see the LaTeX commands
in use, and to compare with the resulting typeset document

OpenMindSample.bbl    Sample bibliography file made with BibTeX

bibsamp.bib           Sample bibliography database file for use with
                      BibTeX to use with OpenMindSample.

ManuscriptSample.tex/.pdf Sample files to show manuscript option in
                          use.
                     \documentclass[manuscript]{stjour}
ManuscriptSample.bbl       Sample .bbl file to build bibliography in ManuscriptSample

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Template File
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

OpenMindTemplate.tex  For authors to copy and rename  when making
                      their own article.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Documentation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

OpenMindDocs.pdf

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

OpenMindReadme.txt   this file