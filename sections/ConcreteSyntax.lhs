\let\Bbbk\undefined  % https://github.com/kosmikus/lhs2tex/issues/82

%include polycode.fmt
%include formatting.fmt

%if False
\begin{code}
module ConcreteSyntax where

\end{code}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Abstract vs. Concrete Syntax of Notional Machines}
\label{sec:ConcreteSyntax}

%\begin{wrapfigure}{r}{.2\textwidth}
\begin{figure}
    \centering
    \begin{minipage}{0.2\textwidth}
    \centering
    \begin{diagram}[width=\textwidth]
import NotionalMachines.Util.Diagrams
import NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs

dia = (diagramWithError . diagram)
      "(\\f. \\x. f (f x))"
    \end{diagram}
    % small vertical fill
    \begin{minipage}{0.7\textwidth}
    \vspace{0.25cm}
    % textwidth is defined by the minipage
    \centering
    \includegraphics[width=0.6\textwidth]{images/alligators/alligators-numeral-2-website.png}
    \end{minipage}
    \end{minipage}
    \caption{Multiple concrete representations.}
    %\caption{Different concrete representations of the same Alligator family in the \nmName{Alligator} \nm{}.}
    \label{fig:two-alligator-representations}
\end{figure}
%\end{wrapfigure}

% - general idea
%The \nmName{Alligator} \nm{} we have seen uses concrete images to represent alligators and eggs.
%
When reasoning about a \nm{},
it's important to distinguish between the data types that represents the \nm{} (the information present in |A_NM| and |B_NM|)
and how the \nm{} is visualized.
This difference is akin to the distinction between the concrete syntax and the abstract syntax of a language.
Like a programming language, a \nm{} has a concrete and an abstract syntax as well.
We also refer to those as concrete and abstract representations of a \nm{}.
%
Notice that the concrete representation of a \nm{} may not only be a diagram or image that can be depicted on paper
but it could also be made of physical objects
or enacted with students.
%
In fact, many \nms{} are ludic in nature or are built around a metaphor, so the concrete representation of a \nm{} is very important.

%
Figure~\ref{fig:concrete-abstract} shows
the different layers at play here.
%
On the programming language side, the |parse| function converts from
the concrete
to the abstract syntax
of the language.
The function $\alpha$ maps from
language constructs to \nm{} constructs.
On the \nm{} side, the function |toDiagram| maps from the abstract representation of the \nm{}
%
to its concrete representation, e.g., in the form of an actual diagram.
%
In the case of \nmName{Alligator},
we use the \emph{diagrams} library~\cite{yatesDiagramsFunctionalEDSL2015, yorgeyMonoidsThemeVariations2012} to construct the concrete representation
so |toDiagram| produces a value of type
|Diagram B|,
where the type parameter |B| (for Backend) determines the output format of the diagram (e.g. SVG).
%
In fact,
the depictions of Alligator Eggs shown here
are generated
by calls to our artifact
that are
embedded directly into the paper.


By decoupling the abstract from the concrete representation, a notional machine can have multiple concrete representations.
Alligator Eggs, for example, also describes another concrete syntax that it calls ``Schematic Form''.
This concrete representation is suitable for working with the \nm{} using pencil and paper.
%
Figure~\ref{fig:two-alligator-representations} shows
the Church numeral two (the term $\texttt{\abs{f}{\abs{x}{\app{f}{(\app{f}{x})}}}}$),
represented
using both concrete representations.
%
In the schematic representation,
colors are presented with variable names.
An alligator is drawn as a line ending with a \texttt{<} for a mouth,
and is preceded by a variable name corresponding to its color.
An old alligator is drawn with a line without a mouth.
An egg is drawn just with the variable name corresponding to its color.

%\subsection{Designing a Concrete Representation}
%
%When designing a concrete representation,
%we should strive for making the information contained in the abstract representation aparent and unambiguous.
%%
%For instance, in the graphical representation of \nmName{Alligator}
%let's focus on the \emph{size} of the depiction of alligators and eggs.
%%
%The original
%Alligator Eggs web site
%doesn't prescribe a size for alligators and eggs,
%and
%the examples
%are
%inconsistent about the size of the pieces:
%sometimes
%reduce the size of pieces that are guarded by other pieces
%and sometimes do not.
%%
%But
%the relative size of pieces is important
%because their relative position determines the relationship between them.
%%
%Depending on the size of the pieces, it may not be obvious who is supposed to eat whom.
%%
%For example,
%Figure~\ref{fig:alligator-scaled-not-scaled} shows
%two variations of
%the same
%board state.
%%
%On the left, all pieces have the same size.
%Because of that,
%it looks like
%the light blue alligator,
%guarded by the green alligator,
%should eat
%the gray alligator,
%guarded by the pink alligator.
%%
%This confusion can be solved by scaling the sizes of alligators.
%In the concrete representation shown on the right of Figure~\ref{fig:alligator-scaled-not-scaled},
%pieces are resized proportionally depending on their relationship with other pieces.
%%
%The width of the pieces directly under an alligator adds up to 90\% of the width of the alligator guarding them
%and
%the height of the pieces (alligators and eggs)
%on the same level are the same.
%%
%The effect in this case is that
%it is easier to see that
%the topmost alligator of each family is the one that can eat another family.
%%
%
%Interestingly,
%looking at the schematic concrete representation (Figure~\ref{fig:two-alligator-representations}), 
%we see that the author was aware of the importance of the relative size of pieces,
%drawing
%each
%alligator with a width smaller or equal than
%the alligator protecting it.
%%


%\begin{figure}
%    \centering
%    \begin{diagram}[width=\textwidth]
%import NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs (str2NM)
%import NotionalMachines.Machine.AlligatorEggs.Diagram
%import NotionalMachines.Util.Diagrams
%import DiagramHelpers (compareAlligators)
%
%alligatorFixedWidths = mapM (toDiagram' (def { _widths = constWidths })) . str2NM
%--alligatorWrongWidths = mapM (toDiagram' (def { _widths = equalWidths })) . str2NM
%adjustedAlligators = mapM toDiagram . str2NM
%
%dia = compareAlligators alligatorFixedWidths adjustedAlligators
%      "(\\a.(\\b.b) (\\c.c) (\\d.d)) (\\e.(\\f.f) e)"
%    \end{diagram}
%    \caption{Confusion due to suboptimal concrete representation in the \nmName{Alligator} \nm{}.}
%    \label{fig:alligator-scaled-not-scaled}
%\end{figure}

