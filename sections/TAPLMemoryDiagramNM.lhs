\let\Bbbk\undefined  % https://github.com/kosmikus/lhs2tex/issues/82

%include polycode.fmt
%include formatting.fmt

%if False
\begin{code}
module TAPLMemoryDiagramNM where

import Data.Map (Map)

newtype DLocation a = DLoc a
  deriving (Eq, Show, Ord)

type Name = String
\end{code}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NM x Lang x Op:
%   - (08)  (TAPLMem/Store, TypedLambdaRef, alloc)
%   - (09)  (TAPLMem/Store, TypedLambdaRef, deref)
%   - (10)  (TAPLMem/Store, TypedLambdaRef, assign)
\subsection{A Monomorphic Notional Machine to Reason About State}
\label{sec:State}

% \begin{meta}
% \begin{itemize}
%     \item An important use of NMs is to reason about state \done
%     \item Mention notionalmachines.github.io statistics \done
%     \item Notable example: TAPL. Explain the context in the book \done
%     \item Show example \done
%     \item TAPL pages 155--156 suggest the use of this diagram to reason about references... Exercise 13.1.1 asks the reader to draw one \done
%     \item Although we understand informally the use of this diagram in this context... how can we know what a correct diagram would be in general for any given program? What is the language of this diagram and what is the relationship between this language and lambda ref? (meta: have we talked at this point about NM as a language?). As we construct this NM and the corresponding commuting diagram relating it to lambda ref we are forced to answer some of these questions and as a result reveal essential aspects about references. \done
%     \item Explain how we modeled it \done
%     \item How can we construct a commuting diagram with lambda ref? \done
%     \item Talk about the problem of names... names in the TAPL diagram come from the REPL like name binding that is outside the language... \done
%     \item Let's try to do it in the language \done
%     \item Show example of code \done
%     \item But to build that we would have to know at any given point what are the names that refer to what values, but we don't have that in our first implementation of the interpreter because it does substitution directly. \done
%     \item Solution... write another interpreter... which means that the commutation diagram is a "there exists an $f_{\text{PL}}$ such that... etc"... so the construction of the specific function that makes the diagram commute is important and it may not be trivial and the fact that a given implementation doesn't allow you to construct the diagram doesn't make the NM necessarily unsound \done
%     \item Show example of tracing the program with corresponding diagrams at each step \done
%     \item Interesting consequence is that stores map location to values which themselves may contain location explicitated by the corresponding circular diagram \done
% \end{itemize}
% \end{meta}

A common use of \nms{} is in the context of reasoning about state.
In fact, 16 of the \numOfNMs{} \nms{} in the dataset by~\citet{fincherNotionalMachinesComputing2020}
focus on either
References, Variables, or Arrays
(see Section~\ref{chr:Evaluation}).
An example of
the use of a visual notation to represent state
can
also be found in
TAPL~\citep[p.~155]{pierceTypesProgrammingLanguages2002}.
%
In Chapter 13 (``References''), the book
extends the simply typed lambda-calculus with references (a language we will refer to as \plName{TypedLambdaRef}%
\footnote{The syntax and reduction rules for \plName{TypedLambdaRef} are reproduced in
%Appendix~\ref{sec:language-definition-typedlambdaref}
the appendix provided as supplementary material
.}).
It explains references and aliasing by introducing a visual notation to highlight the difference between a \emph{reference}
and the \emph{cell} in the store that is pointed to by that reference.
We will refer to this notation,
which we will develop into a notional machine,
as \nmName{TAPLMemoryDiagram}.
In this notation, references are represented as arrows and cells are represented as rounded rectangles containing the representation of the value contained in the cell.
%
Before designing the \nm{},
we need to see the context in which this notation is used in the book.

The book first uses this notation to explain the effect of creating a reference.
It shows that when we reduce the term $\ \refr{13}\ $ we obtain a reference (a store location) to a store cell containing \texttt{13}.
The book then represents the result of binding the name \texttt{r} to such a reference with the following diagram:

\begin{center}
\includegraphics[scale=0.25]{images/tapl-references/TAPL-ReferenceDiagram-r.pdf}
\end{center}

In the book,
this operation is written as \texttt{r = \refr{13}},
but as we will see in the next Section,
%
this form of name binding (\texttt{name = term})
exists only in a REPL-like context which is not part of the language.

The book continues
explaining that
we can
``make a copy of \texttt{r}''
by binding its value to another variable
\texttt{s}
(with \texttt{s = r})
and
shows the resulting diagram:

\begin{center}
\includegraphics[scale=0.25]{images/tapl-references/TAPL-ReferenceDiagram-rs.pdf}
\end{center}


The book then explains that
one can verify that both names refer to the same cell by \emph{assigning} a new value to \texttt{s} and reading this value using \texttt{r}
(for example, the term $\texttt{\seq{\assign{s}{82}}{\deref{r}}}$ would evaluate to \texttt{82}).
Right after,
the book suggests to the reader an exercise to
``draw a similar diagram showing the effects of evaluating the expressions
$\texttt{a = \Tuple{\refr{0}, \refr{0}}}$ and $\texttt{b = \app{(\tyabs{x}{\Ref{Nat}}{\Tuple{x, x}})}{(\refr{0})}}$.''
%
Although we understand informally the use of this diagram in this context,
how can we know what a correct diagram would be in general for any given program?
This is what we aim to achieve by designing a \nm{} based on this notation.


%\subsubsection{Designing a \NM{}}
Let's see how we would turn that kind of diagram into a sound \nm{}.
%
We want to construct a commutative diagram
where |A_PL| is an abstract representation of the state of a \plName{TypedLambdaRef} program execution,
|A_NM| is an abstract representation of the diagram presented in the book,
and |f_PL| is an operation that affects the state of the store during program execution.

In a first attempt, let's choose |f_PL| to be an evaluation step and
|A_PL| to be modeled as close as possible to the presentation of a \plName{TypedLambdaRef} program as described in the book.
In that case, |A_PL| is the program's abstract syntax tree together with a \emph{store}, a mapping from a location (a reference) to a value.
 
%problem 1: name = value is outside the language
\subsubsection{Problem: Beyond the Language}
The first challenge is that the name-binding mechanism used in the examples above (written as \texttt{name = term})
exists only in a REPL-like context in the book used for the convenience of referring to terms by name.
It
is actually not part of the language (\plName{TypedLambdaRef})
so it is not present in this representation of |A_PL| and
as a result it cannot be mapped to |A_NM| (the \nm{}).
%
We will avoid this problem by avoiding this name-binding notation entirely and
writing corresponding examples fully in the language.
The only mechanism actually in the language to bind names is by applying a lambda to a term.
Let's see how we can write a term
to express the behavior described in the example the book uses to introduce the diagram (shown earlier),
where we:
\begin{enumerate}
    \item Bind \texttt{r} to the result of evaluating $\texttt{\refr{13}}$
    \item Bind \texttt{s} to the result of evaluating \texttt{r}
    \item Assign the new value \texttt{82} to \texttt{s}
    \item Read this new value using \texttt{r}
\end{enumerate}

\noindent
Using only the constructs in the language, we express this with the following term:

$$\texttt{\app{(\tyabs{r}{\Ref{Nat}}{\app{(\tyabs{s}{\Ref{Nat}}{\seq{\assign{s}{82}}{\deref{r}}})}{r}})}{(\refr{13})}}$$
 
%problem 2: direct substitution
\subsubsection{Problem: Direct Substitution}
The problem now is that if we model |A_PL| and evaluation as described in the book,
the result of reducing a term $\app{(\abs{\texttt{x}}{t_1})}{t_2}$
is the term obtained by replacing all free occurrences of \texttt{x} in $t_1$ by $t_2$ (modulo alpha-conversion),
so we don't actually keep track of name binding information.
What we have in |A_PL| at each step is an abstract syntax tree and a store,
but we have no information about which names are bound to which values
because the names were already substituted in the abstract syntax tree.
%
That would be enough
to do Exercise 13.1.1, for example, whose solution is shown in Figure~\ref{fig:lamdaref-exercise}.
But the absence of
explicit information mapping names to values
makes it less suitable
to talk about aliasing,
because even though a term
%
\begin{wrapfigure}{r}{.6\textwidth}
%\begin{figure}
    \centering
    \includegraphics[width=.6\textwidth]{images/tapl-references/LambdaRefTrace.pdf}
    \caption{Using \nmName{TAPLMemoryDiagram} to trace the evaluation of $\texttt{\app{(\tyabs{r}{\Ref{Nat}}{\app{(\tyabs{s}{\Ref{Nat}}{\seq{\assign{s}{82}}{\deref{r}}})}{r}})}{(\refr{13})}}$}
    \label{fig:lamdaref-trace}
%\end{figure}
\end{wrapfigure}
%
may contain, at any given point during evaluation,
multiple occurrences of the same location,
it is not possible to know if these locations correspond to different names,
and one may need to trace several reduction steps back to find out when a name was substituted by a location.

We need to change |A_PL| and \texttt{step} to capture this information,
keeping not only a store but an explicit name environment that maps names to values, and
only substituting the corresponding value when we evaluate a variable.
Like the definition of application in terms of substitution,
we have to be careful to avoid variable capture by generating fresh names when needed.

\begin{figure}
    \centering
    \includegraphics[width=.7\textwidth]{images/tapl-references/TAPL-ReferenceDiagram-Exercise-1311.pdf}
    \caption{\nmName{TAPLMemoryDiagram} for \plName{TypedLambdaRef} for TAPL Exercise 13.1.1}
    \label{fig:lamdaref-exercise}
\end{figure}


\subsubsection{Illustrative Example}

Figure~\ref{fig:lamdaref-trace} shows two variations of the \nm{} being used to explain the evaluation of the term we had described before.
It shows the state after each reduction step,
on the left without an explicit name environment and on the right with an explicit name environment.
Between each step, a line with the name of the applied reduction rule is shown.
Notice that
the representation with a name environment requires extra name lookup steps.

In both variations,
the representation of the program (the term) being evaluated appears first (with gray background).
Each term is actually an abstract syntax tree, which we represent here, like in the book, with a linearized textual form.
Location terms
are represented as arrows starting from where they appear in the abstract syntax tree and ending in the store cell they refer to.

The naming environment is shown as a table from variable names to terms.
Store cells also contain terms.
This means
the textual representation of terms that
appear both inside name environments and inside cells
may also contain arrows to other cells.


\subsubsection{Commutative Diagram}

Similar to the abstract representation of the program execution,
the abstract representation of the \nm{} contains three parts:
one for the program being evaluated, one for the name environment, and one for the store.

\begin{minipage}[b]{\textwidth}
\begin{code}
data TAPLMemoryDiagram l = TAPLMemoryDiagram {
    memDiaTerm    :: DTerm l,
    memDiaNameEnv :: Map Name (DTerm l),
    memDiaStore   :: Map (DLocation l) (DTerm l) }

data DTerm l  =  Leaf String
              |  Branch [DTerm l]
              |  TLoc (DLocation l)
\end{code}
\end{minipage}

The type |DLocation| corresponds to arrow destinations (arrow endpoints).
A term is represented as a rose tree of |String|s augmented with a case for location.

The concrete representation of a |DTerm| can be in linearized text form or as a tree akin to that shown
in Section~\ref{sec:ExpTree}.
The representation of the nodes in a |DTerm| tree that are |TLoc| are shown as arrow starting points.
These arrows end in the cell corresponding to the |DLocation| in each |TLoc|.
%
The concrete representation of the store relates
the visual position of each cell with the |DLocation| of each cell.
That leads to the commutative diagram in Figure~\ref{fig:commutativeDiagramTAPLMemoryDiagram},
where
% This explanation is enough for ICFP but should we make it more detailed?
we use the symbol $\circ_M$ to denote monadic function composition (the fish operator \texttt{<=<} in Haskell).

% bisim :: Bisimulation (Term, StateRacket)
%                       (Maybe (Term, StateRacket))
%                       (TAPLMemoryDiagram Location)
%                       (Maybe (TAPLMemoryDiagram Location))
% bisim = MkBisim { fLang  = step
%                 , fNM    = fmap toNM . step <=< fromNM
%                 , alphaA = toNM
%                 , alphaB = fmap toNM }

\begin{figure}

\[
\begin{tikzcd}
|TAPLMemoryDiagram| \arrow[rrrr, "|fmap alpha|\; \circ\; |step|\; \circ_M\; |alpha_circ|", dashed]
                    \arrow[ddd,  "|alpha_circ|", shift left, dotted]
& & & &
|Maybe TAPLMemoryDiagram| \\
\\
\\
|(TermUL, EnvUL, StoreUL)| \arrow[rrrr, "|step|"]
                           \arrow[uuu,  "|alpha|", shift left]
& & & &
|Maybe (TermUL, EnvUL, StoreUL)| \arrow[uuu, "|fmap alpha|", dashed]
\end{tikzcd}
\]

    \caption{Instantiation of the commutative diagram in Figure~\ref{fig:commutativeDiagram} for the notional machine \nmName{TAPLMemoryDiagram} and the programming language \plName{TypedLambdaRef}.}
    \label{fig:commutativeDiagramTAPLMemoryDiagram}
\end{figure}

% The process of reworking |A_PL| and |f_PL| to expose an explicit name environment
% shows that
% if a given choice of types (|A_PL|) and functions (|f_PL|)
% doesn't allow for the construction of the commutative diagram for a \nm{}, that doesn't mean the \nm{} is necessarily unsound.
% The soundness of a given \nm{} is predicated on the existence
% of an |A_PL| and |f_PL| compatible with the semantics of the language,
% but
% the construction of the specific types and functions that make the diagram commute
% may not be trivial.

% \begin{figure}
%     \centering
%     %\includegraphics[width=\textwidth]{TAPL-ReferenceDiagram.pdf}
%     %\includegraphics[width=\textwidth]{TAPL-ReferenceDiagram-Variant.pdf}
%     \includegraphics[width=\textwidth]{images/TAPL-ReferenceDiagram-Variant2.pdf}
%     \caption{TAPL Memory Diagrams to explain allocation, dereferencing, and assignment}
%     \label{fig:tapl_ref}
% \end{figure}

