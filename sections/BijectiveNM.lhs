\let\Bbbk\undefined  % https://github.com/kosmikus/lhs2tex/issues/82

%include polycode.fmt
%include formatting.fmt

%if False
\begin{code}
module BijectiveNM where

data TermUL = App TermUL TermUL
            | Lambda Name TermUL
            | Var Name
type Name = String

-- or ExpAsTree?
data ExpTree = Box String
             | BinaryBox ExpTree ExpTree
             | LambdaBox String ExpTree

langToNM :: TermUL -> ExpTree
langToNM (Var name)      = Box name
langToNM (Lambda name e) = LambdaBox name (langToNM e)
langToNM (App e1 e2)     = BinaryBox (langToNM e1) (langToNM e2)

nmToLang :: ExpTree -> TermUL
nmToLang (Box name)         = Var name
nmToLang (LambdaBox name e) = Lambda name (nmToLang e)
nmToLang (BinaryBox e1 e2)  = App (nmToLang e1) (nmToLang e2)

step :: TermUL -> TermUL
step = undefined

type A_PL = TermUL
type B_PL = TermUL
type A_NM = ExpTree
type B_NM = ExpTree
alpha_A :: A_PL -> A_NM
alpha_A = langToNM
alpha_A_inv :: A_NM -> A_PL
alpha_A_inv = nmToLang
alpha_B :: B_PL -> B_NM
alpha_B = alpha_A
f_PL = step
\end{code}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NM x Lang x Op:
%% ExpTree x UntypedLambda x eval
%\section{Bijective Notional Machines: a simple example}
\subsection{Isomorphic Notional Machines}
%\section{Notional Machines as Bisimulations}
\label{sec:ExpTree}

%
% Example: state the triplet ExpTree x UntypedLambda x eval
As a first straightforward example, let's look at a \nm{} for teaching how evaluation works in the untyped lambda-calculus
(we will refer to this language as \plName{UntypedLambda}%
\footnote{The syntax and reduction rules for \plName{UntypedLambda} are reproduced in
%Appendix~\ref{sec:language-definition-untypedlambda}
the appendix provided as supplementary material
.}).
While most research papers discuss the lambda-calculus using its textual representation,
textbooks sometimes illustrate it using tree diagrams~\citep[p.~54]{pierceTypesProgrammingLanguages2002}.
We use this as an opportunity to define a simple \nm{} which we call \nmName{ExpTree}.%

% \begin{table}
% \centering
% \begin{tabular}{|l||l|c|}
% \hline
% Terms & Programming Language & Notional Machine \\
% \hline
% \hline
% variable    & $x$            & \begin{tabular}{c}\includegraphics[scale=0.4]{images/var.pdf}\end{tabular} \\
% \hline
% abstraction & $\lambda x. t$ & \begin{tabular}{c}\includegraphics[scale=0.4]{images/abs.pdf}\end{tabular} \\
% \hline
% application & $t\ t$         & \begin{tabular}{c}\includegraphics[scale=0.4]{images/app.pdf}\end{tabular} \\
% \hline
% \end{tabular}
% \caption{\nmName{ExpTree}: Lambda-Calculus Expressions as Trees}
% \label{tab:lambdaTrees}
% \end{table}

\subsubsection{Illustrative Example}
Figure~\ref{fig:takeFirst} uses \nmName{ExpTree} to demonstrate the evaluation of a specific lambda expression,
which happens in two reduction steps.
The top of the figure shows the terms in the traditional textual representation of the programming language,
while the bottom shows the terms as a tree.



\subsubsection{Soundness via Commutative Diagrams}
\label{sec:soundnessDefinition}

\begin{figure}

\[
\begin{tikzcd}
|A_NM| \arrow{rrr}{|f_NM|}                        & & & |B_NM| \\
\\
\\
|A_PL| \arrow{rrr}{|f_PL|} \arrow{uuu}{|alpha_A|} & & & |B_PL| \arrow{uuu}{|alpha_B|}
\end{tikzcd}
\]

\begin{equation}
|alpha_B . f_PL == f_NM . alpha_A|
\end{equation}

    \caption{Soundness condition for notional machines shown as a commutative diagram and in algebraic form.}
    \label{fig:commutativeDiagram}
\end{figure}

In general, a notional machine is sound if the diagram in Figure~\ref{fig:commutativeDiagram} commutes.
We call the commutativity of this diagram the \emph{soundness condition} for a notional machine.
In this diagram, the vertices are types and the edges are functions.
We will explain the diagram while instantiating it for this illustrative example
(the result of this instantiation is shown in Figure~\ref{fig:commutativeDiagramExpTree}).

The bottom layer (|A_PL|, |f_PL|, |B_PL|) represents the aspect of a programming language%
\footnote{%
Although we refer to the bottom layer of the diagram as the programming language layer
and we restrict ourselves to analyzing aspects of the syntax and semantics of programming languages,
for which we have well-established formalizations,
that is not an intrinsic restriction of the approach.
In principle,
the bottom level of the diagram can be whatever aspects of programs or programming the notional machine is focused on.
}
we want to focus on.
% concept?
%The triple $A_{PL}$, $f_{PL}$, and $B_{PL}$ together completely describe the aspect of the semantics of the programming language we want to represent.
%
%At the programming language level,
|A_PL| is an abstract representation of a program in that language.
In our example, that is the abstract syntax of \plName{UntypedLambda} (given by the type |TermUL|).
%
The function |f_PL| is an operation the notional machine is focusing on.
In our example, that would be |step|, a function that performs a reduction step in the evaluation of a program according to the operational semantics of the language,
which in this case also produces a value of type |TermUL|.
%

The top layer of the diagram (|A_NM|, |f_NM|, |B_NM|) represents the notional machine.
%
%At the notional machine level,
|A_NM| is an abstract representation of the notional machine (its abstract syntax).
In our simple example, that is a type |ExpTree| trivially isomorphic to |TermUL| via a simple renaming of constructors.
%
The function |f_NM| is an operation on the notional machine which should correspond to |f_PL|.
%
Connecting the bottom layer to the top layer, there are the functions |alpha_A| and |alpha_B| from the abstract representation of a program in the programming language to the abstract representation of the notional machine.
|alpha| is also called an abstraction function.
%
%Note that the types $A_{PL}$ and $B_{PL}$ may be the same (as may be the case with the types $A_{NM}$ and $B_{NM}$). If $f_{PL}$ calculates the type of an expression, $A_{PL}$ would be the type of expressions, whereas $B_{PL}$ would be the type of types. $f_{PL}$ is often a function that performs small-step operational semantics steps, but it can be any kind of action we want to represent.


\begin{definition} %[Fibration]
Given the notional machine
(|A_NM|, |B_NM|, |f_NM :: A_NM -> B_NM|),
focused on the aspect of a programming language
given by
(|A_PL|, |B_PL|, |f_PL :: A_PL -> B_PL|),
the notional machine is \emph{sound} iff
there exist two functions |alpha_A :: A_PL -> A_NM| and |alpha_B :: B_PL -> B_NM| such that
|alpha_B . f_PL == f_NM . alpha_A|.
\end{definition}


If the abstract representation of the programming language (|A_PL|)
is isomorphic to the abstract representation of the notional machine (|A_NM|),
we can construct an inverse mapping |alpha_A_inv| such that |alpha_A_inv . alpha_A == id == alpha_A . alpha_A_inv|.
In that case,
we can always define a correct-by-construction operation |f_NM| on |A_NM| in terms of an operation |f_PL| on |A_PL|:

\begin{code}
f_NM :: A_NM -> B_NM
f_NM = alpha_B . f_PL . alpha_A_inv
\end{code}

\noindent 
In such cases,
the diagram always commutes and therefore the notional machine is sound:

\begin{equation}
|f_NM . alpha_A == alpha_B . f_PL . alpha_A_inv . alpha_A == alpha_B . f_PL|
\end{equation}

Instantiating the commutative diagram for \nmName{ExpTree} and \plName{UntypedLambda} yields the diagram in Figure~\ref{fig:commutativeDiagramExpTree}.
A dashed line indicates a function that is implemented
in terms of the other functions in the diagram and/or standard primitives.

\begin{figure}

\[
\begin{tikzcd}
|ExpTree| \arrow[rrr, "|alpha|\; \circ\; |step|\; \circ\; |alpha_inv|", dashed]
          \arrow[ddd, "|alpha_inv|", shift left]
& & &
|ExpTree| \\
\\
\\
|TermUL| \arrow{rrr}{|step|}
         \arrow[uuu, "|alpha|", shift left]
& & &
|TermUL| \arrow{uuu}{|alpha|}
\end{tikzcd}
\]

    \caption{Instantiation of the commutative diagram in Figure~\ref{fig:commutativeDiagram} for the notional machine \nmName{ExpTree} and the programming language \plName{UntypedLambda}.}
    \label{fig:commutativeDiagramExpTree}
\end{figure}

We call these isomorphic notional machines because they are isomorphic to the
aspect of the programming language they focus on
(a condition sometimes called \emph{strong simulation}~\cite{milnerAlgebraicDefinitionSimulation1971}).
Of course
that's a rather strong condition
and
not every \nm{} is isomorphic
so
throughout the next sections we will move further away from this simple example,
arriving at other instantiations of this commutative diagram.

%%%% From: Refactoring pattern matching
% It is obvious that if ↵ and ↵ are each other’s inverses (a situation sometimes called strong simulation [12]), the computation law is equivalent to the promotion condition. However, requiring them to be inverses restricts the abstraction function to be bijective, which is rather a strong condition. In fact, one-sided invertibility is sufficient in this case. Specifically, the ↵ function should be a right inverse of ↵ – that is, ↵ ↵ ⌘ id; it is not necessary for ↵ also to be a left inverse of ↵.
