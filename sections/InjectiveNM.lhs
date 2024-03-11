\let\Bbbk\undefined  % https://github.com/kosmikus/lhs2tex/issues/82

%include polycode.fmt
%include formatting.fmt

%options ghci -fglasgow-exts

%if False
\begin{code}
module InjectiveNM where

import Data.Set (Set)

------------------------
-- Abstract representation of the Language
------------------------
data TermUL = App TermUL TermUL
            | Lambda Name TermUL
            | Var Name
type Name = String

step :: TermUL -> TermUL
step = undefined

------------------------
-- Abstract representation of the NM
------------------------
data ExpTutorDiagram = ExpTutorDiagram { nodes :: Set Node
                                       , edges :: Set Edge
                                       , root  :: Maybe Node
                                       }
data Node = Node { nodePlug :: Plug
                 , typ      :: Maybe Type
                 , content  :: [NodeContentElem]
                 }
data NodeContentElem = C String
                     | NameDef String
                     | NameUse String
                     | Hole Plug -- (Maybe Type)
newtype Plug = Plug (Int, Int)
data Edge = Edge Plug Plug
type Type = String

------------------------
-- Commuting square relating Lang and NM
------------------------
langToNM :: TermUL -> ExpTutorDiagram
langToNM = undefined

nmToLang :: ExpTutorDiagram -> Maybe TermUL
nmToLang = undefined

type A_PL = TermUL
type B_PL = TermUL
type A_NM = ExpTutorDiagram
type B'_NM = ExpTutorDiagram
type B_NM = Maybe B'_NM
alpha_A :: A_PL -> A_NM
alpha_A = langToNM
alpha_A_circ :: A_NM -> Maybe A_PL
alpha_A_circ = nmToLang
alpha'_B :: B_PL -> B'_NM
alpha'_B = alpha_A
f_PL = step

alpha = langToNM
alpha_circ = nmToLang
\end{code}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NM x Lang x Op:
%% ExpTutorDiagram x UntypedLambda x eval
% \section{Deliberately Allowing Student's Mistakes}
%\section{Expression Trees revisited}
% \section{Monomorphic Notional Machines: expression trees revisited}
% \section{Monomorphic Notional Machines: Deliberately Allowing Student's Mistakes}
\subsection{Monomorphic Notional Machines}
%\section{Left-inversible Notional Machines}
%\section{Injective Notional Machines}
%\section{Notional Machines as Simulations}
\label{sec:ExpTutor}

% \begin{meta}
% \begin{itemize}
%     \item But sometimes lang and nm are not isomorphic... \done
%     \item Explain Expression Tutor \done
%     \item Show an example \done
%     \item Explain why its not an isomorphism... why is this a good idea here. \done
%     \item Show an example \done
%     \item Even if it's not an isomorphism there is a partial inverse (left inverse)
%     \item With a partial inverse it is still possible to build a correct-by-construction operation on NMs that is built in terms of alpha.
%     \item build commuting diagram \done
%     \item Explain the corresponding commuting diagram
%     \item let's use equational reasoning to show the diagram indeed commutes \done
%     \item Here it is particularly useful to describe the operation on nm in terms of the operation on lang because the tool is language indipendent. that makes it actually a family (or a parametrized) nm that can be instantiated for a given language.
%     \item insight: for nms with invertible (or partially invertible) alphas, we are reducing the problem of constructing the operation on nms (or checking its correctness) to the problem of constructing a inverse (or partial inverse) of $\alpha$. in other words: the first two examples of NM (in general bijective and injective) dont check the correctness of step but the "correctness" of the relationship between the two data structures.
% \end{itemize}
% \end{meta}

\Nms{} can also serve as the basis for so-called ``visual program simulation''~\citep{sorvaStudentsWaysExperiencing2013} activities,
where students manually construct representations of the program execution.
This effort often is supported by tools, such as interactive diagram editors, that scaffold the student's activity.
Obviously, instructors will want to see their students creating correct representations.
However, to prevent students from blindly following
a path to a solution prescribed by the tool,
the visual program simulation environment 
should also allow \emph{incorrect} representations.

% what is ET
ExpressionTutor\footnote{\href{http://expressiontutor.org}{expressiontutor.org}} is such an educational
tool to teach the structure, typing, and evaluation of expressions in programming courses.
In ExpressionTutor,
students can interactively construct expression tree diagrams given a source code expression.
% detail it
The tool is language agnostic so each node can be freely constructed (by the instructor or the student)
to represent nodes of the abstract syntax tree of any language.
Nodes can contain any number of holes that can be used to connect nodes to each other.
Each hole corresponds to a place in an abstract syntax tree node where an expression would go.
% why (in what ways) does it allow for mistakes?
Nodes can be connected in a variety of ways,
deliberately admitting incorrect structures
that not only may not be valid abstract syntax trees of a given programming language
but may not even be trees.
Even the root node (labeled with a star) has to be explicitly labeled by the student,
so it is not guaranteed to exist in every diagram.
%
% mistakes expressed in commutative diagram

We define the notional machine \nmName{ExpTutorDiagram}, which models the behavior of ExpressionTutor.
The fact that
ExpressionTutor
allows students to construct incorrect expression tree diagrams
means that
the abstraction function $\alpha$ is not bijective,
as was the case of \nmName{ExpTree}'s $\alpha$.
Such incorrect diagrams do not correspond to programs,
thus $\alpha$ is deliberately not surjective.

%(λx. x x) (λx. x x)
\begin{figure}
    \centering
    \begin{tabular}{c}
        $\texttt{\app{(\abs{x}{\app{x}{x}})}{(\abs{x}{\app{x}{x}})}}$ \\
        \includegraphics[scale=0.13]{images/expression-tutor/omegaBroken1} \hspace{10mm}
        \includegraphics[scale=0.13]{images/expression-tutor/omegaBroken2} \hspace{10mm}
        \includegraphics[scale=0.13]{images/expression-tutor/omegaBroken3}
    \end{tabular}
    \caption{The omega combinator in \plName{UntypedLambda} (top)
             and (incorrect) representations in \nmName{ExpTutorDiagram} \nm{} (bottom).}
    \label{fig:omegaBroken}
\end{figure}

\subsubsection{Illustrative Example}
Figure~\ref{fig:omegaBroken} uses \nmName{ExpTutorDiagram} to represent the omega combinator.
The top shows the textual form on the level of the programming language.
Below that are three different incorrect representations students could produce.
The left tree collapses the
applications
(the terms $\texttt{\app{x}{x}}$)
into the enclosing lambda abstraction.
The middle tree similarly does this,
but it preserves the structure of the lambda abstraction node,
while violating the well-formedness of the tree
by plugging two children into the same hole.
The right tree shows a different problem,
where the definition of the name is pulled out of the lambda abstraction
and shown as a name use.

\subsubsection{Commutative Diagram}

In general,
if the mapping |alpha_A|, from the abstract representation of the programming language (|A_PL|)
to the abstract representation of the notional machine (|A_NM|), is an injective but non-surjective function,
we can still define the operations on |A_NM| in terms of the operations on |A_PL|.
For this we define a function \eval{:t alpha_A_circ}
to be a left inverse of |alpha_A|
such that |alpha_A_circ . alpha_A == return|
(we use |return| and |fmap| to refer to the |unit| and |map| operations on monads).
Here we modeled the left inverse using a |Maybe| but another monad could be used, for example, to capture information about the values of type |A_NM| that do not have a corresponding value in |A_PL|.
%
The top-right vertex of the square (|B_NM|) in this case is the type |Maybe B'_NM|
and the mapping |alpha_B| can be implemented in terms of a mapping \eval{:t alpha'_B} like so:

\begin{code}
alpha_B :: B_PL -> B_NM
alpha_B = return . alpha'_B
\end{code}

\noindent 
Using the left inverse |alpha_A_circ| and |alpha'_B|,
we define the operation on |A_NM| as follows:

\begin{code}
f_NM :: A_NM -> B_NM
f_NM = fmap alpha'_B . fmap f_PL . alpha_A_circ
\end{code}

\noindent
This square commutes like so:

\def\commentbegin{\quad\{\ }
\def\commentend{\}}
\begin{minipage}[b]{0.48\textwidth}
\begin{spec}
    f_NM . alpha_A
==  {- definition of |f_NM| -}
    fmap alpha'_B . fmap f_PL . alpha_A_circ . alpha_A
==  {- |alpha_A_circ| is left inverse of |alpha_A| -}
    fmap alpha'_B . fmap f_PL . return
==  {- third monad law -}
    fmap alpha'_B . return . f_PL
\end{spec}
\end{minipage}
\begin{minipage}[b]{0ex}{\rule[0ex]{0.5pt}{1.3in}}
\end{minipage}
\hspace{1ex}
\begin{minipage}[b]{0.3\textwidth}\setlength\mathindent{2ex}
\begin{spec}
    alpha_B . f_PL
==  {- definition of |alpha_B| -}
    return . alpha'_B . f_PL
==  {- third monad law -}
    fmap alpha'_B . return . f_PL
\end{spec}
\end{minipage}

We can use this result to instantiate the commutative diagram of Figure~\ref{fig:commutativeDiagram} for \nmName{ExpTutorDiagram} and \plName{UntypedLambda},
shown in Figure~\ref{fig:commutativeDiagramExpTutorDiagramComplete}.
% describe the structure (A_NM)
% summary
|A_PL| is defined to be the type |ExpTutorDiagram|,
which essentially implements a graph.
% details
Each node has a top plug and any number of holes, which contain plugs.
Edges connect plugs.
That allows for a lot of flexibility in the way nodes can be connected.
% question: show code?

% describe the structure (A_PL)
%
% it's a family of NMs also because of types etc...
%
The ExpressionTutor tool is language agnostic
but we can only talk about soundness of a \nm{} with respect to some language
and some aspect of that language. % or formalized computation
%
In this case,
we say the tool implements
a family of notional machines,
each one
for a given aspect of focus and a given programming language.
%
We consider here a \nm{}
focused on evaluation
and
the programming language again \plName{UntypedLambda} (denoted again by the type |TermUL|),
with |f_PL| equal to |step|.%


% \[
% \begin{tikzcd}
% \operatorname{ExpTutorDia} \arrow[rrr, "\operatorname{fmap} (\alpha\; \circ\; \operatorname{step})\; \circ\; \alpha^{\circ}", blue] \arrow[ddd, "\alpha^{\circ}", shift left, dotted] & & & \operatorname{Maybe\ ExpTutorDia} \\
%                                                           \\
%                                                           \\
% \operatorname{Term_{U\lambda}} \arrow[rrr, "\operatorname{step}"] \arrow[uuu, "\alpha", shift left] & & & \operatorname{Term_{U\lambda}} \arrow[uuu, "\operatorname{return}\circ\, \alpha", blue]
%
% \\
% \end{tikzcd}
% \]

\begin{figure}

%% v3
\[
\begin{tikzcd}[row sep=small, column sep=small]
|ExpTutorDiagram| \arrow[rr,  "|fmap|\; |alpha|\; \circ\; |fmap step|\; \circ\; |alpha_circ|", dashed]
                  \arrow[ddddr, "|alpha_circ|", shift left]
& &
|Maybe ExpTutorDiagram|
& \\
\\
\\
\\
& |Maybe TermUL| \arrow[rr, "|fmap step|" above left, dashed]
& &
|Maybe TermUL| \arrow[uuuul, "|fmap alpha|" above right, dashed] \\
|TermUL|       \arrow[rr,  "|step|"]
               \arrow[uuuuu, "|alpha|"]
               \arrow[ur,   "|return|", dashed]
& &
|TermUL| \arrow[uuuuu, "|fmap alpha|\; \circ\; |return|" above right, dashed, crossing over]
         \arrow[ur,   "|return|", dashed]
\end{tikzcd}
\]

    \caption{Instantiation of the commutative diagram in Figure~\ref{fig:commutativeDiagram} for the notional machine \nmName{ExpTutorDiagram}}
    \label{fig:commutativeDiagramExpTutorDiagramComplete}
\end{figure}

% describe alpha
The construction of a mapping \eval{:t alpha} is straightforward
because
a term |t :: TermUL| forms a tree
and from it
we can always construct a corresponding ExpressionTutor diagram |d :: ExpTutorDiagram| (a graph).
For each possible term in |TermUL|,
we need to define a pattern for the content of the corresponding |ExpTutorDiagram| node which will help the student identify the kind of node.
%
The construction of the left inverse mapping \eval{:t alpha_circ} requires more care.
We need to make sure that 
the diagram forms a proper tree
and that the pattern formed by the contents of each |ExpTutorDiagram| node
corresponds to a possible |TermUL| node,
besides making sure that they are connected in a way that indeed corresponds to a valid |TermUL| tree.
%Using pattern synonyms~\citep{pickeringPatternSynonyms2016},
%we can make sure that
%the same patterns used to determine the contents of an |ExpTutorDiagram| node for a given |TermUL| node (used in the implementation of |alpha|)
%are also used to implement the left inverse mapping |alpha_circ|.

% Throughout the paper,
In the next section,
we construct another commutative diagram where |f_NM| is defined using |f_PL| and a left inverse mapping |alpha_A_circ|.
To emphasize that point and simplify the diagrams,
we will depict the left inverse in the diagram as a dotted line pointing from |A_NM| to |A_PL|
(even though |alpha_A_circ| is of type |A_NM -> Maybe A_PL| and not |A_NM -> A_PL|)
and omit the path via |Maybe A_PL|
as shown in Figure~\ref{fig:commutativeDiagramExpTutorDiagramSimplified}.

\begin{figure}

%% v2
\[
\begin{tikzcd}
|ExpTutorDiagram| \arrow[rrrr, "|fmap alpha|\; \circ\; |fmap step|\; \circ\; |alpha_circ|", dashed]
                  \arrow[ddd,  "|alpha_circ|", shift left, dotted]
& & & &
|Maybe ExpTutorDiagram| \\
\\
\\
|TermUL| \arrow[rrrr, "|step|"]
         \arrow[uuu, "|alpha|", shift left]
& & & &
|TermUL| \arrow[uuu, "|return|\; \circ\; |alpha|", dashed]
\end{tikzcd}
\]

    \caption{Simplified version of the commutative diagram for \nmName{ExpTutorDiagram} shown in Figure~\ref{fig:commutativeDiagramExpTutorDiagramComplete}.}
    \label{fig:commutativeDiagramExpTutorDiagramSimplified}
\end{figure}

We call these monomorphic notional machines because
there is a monomorphism (injective homomorphism)
between the notional machine and the
aspect of the programming language it focuses on.
%
This is the case here by design, to allow students to make mistakes by constructing
wrong diagrams that don't correspond to programs.
In general, this will be the case whenever there are values of
\ensuremath{\Conid{A}_{\Conid{NM}}}
(the abstract syntax of the notional machine)
that have no correspondence in the abstract representation of the
language (\ensuremath{\Conid{A}_{\Conid{PL}}}).
That's often the case in memory diagrams~\citep{hollidayCS1AssessmentUsing2004, daltonAutomatedConstructionMemory2010, dragonMemoryDiagramsConsistant2016a}
(notional machines used to show the relationship between programs and memory)
because they typically allow for the representation of memory states
that cannot be produced by legal programs.
%
We show an example of such a notional machine in the next section.

