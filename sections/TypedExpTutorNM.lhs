\let\Bbbk\undefined  % https://github.com/kosmikus/lhs2tex/issues/82

%include polycode.fmt
%include formatting.fmt

%if False
\begin{code}
module TypedExpTutorNM where

import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Monad ((<=<))


data Term_TyArith -- Booleans
          = Tru
          | Fls
          | If Term_TyArith Term_TyArith Term_TyArith
          -- Arithmetic Expressions
          | Zero
          | Succ Term_TyArith
          | Pred Term_TyArith
          | IsZero Term_TyArith

data Type_TyArith = TyBool | TyNat

typeof :: Term_TyArith -> Maybe Type_TyArith
typeof e = undefined

--------------------
-- Expression Tutor
--------------------
data ExpTutorDiagram = ExpTutorDiagram { nodes :: Set Node
                                       , edges :: Set Edge
                                       , root  :: Maybe Node
                                       }

data Node = Node { nodePlug :: Plug
                 , typ      :: Maybe Type_ExpTutor
                   -- , value :: ExpTutorDiagram
                 , content  :: [NodeContentElem]
                 }

data NodeContentElem = C String
                     | NameDef String
                     | NameUse String
                     | Hole Plug -- (Maybe Type)

newtype Plug = Plug (Int, Int)

data Edge = Edge Plug Plug

newtype Type_ExpTutor = MkTy String


------------------------
-- Lang Types to NM Type representation and back
------------------------

alpha_Term :: Term_TyArith -> ExpTutorDiagram
alpha_Term = undefined

alpha_Term_circ :: ExpTutorDiagram -> Maybe Term_TyArith
alpha_Term_circ = undefined

alpha_Type :: Type_TyArith -> Type_ExpTutor
alpha_Type = undefined

\end{code}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NM x Lang x Op:
%   - (05)  (ET,            TypedArith,     eval)
%   - (06)  (ET,            TypedArith,     typeof)
%   - (07)  (ET,            TypedArith,     annotateType)
\subsection{A Monomorphic Notional Machine to Reason About Types}
\label{sec:Typing}

So far we have seen examples of commutative diagrams where |f_PL| is |step|
(a function that performs a reduction step)
but in principle, |f_PL| could be any operation on |A_PL| that is the focus of a given \nm{}.
%
Let's look at an example of \nm{} where we do not focus on evaluating but on typing an expression.
The language this \nm{} focuses on is \plName{TypedArith}\footnote{The syntax and typing rules for \plName{TypedArith} are reproduced in Appendix~\ref{sec:language-definition-typedarith}.}, a language of typed arithmetic expression, which is the simplest typed language introduced in TAPL~\citep[p.~91]{pierceTypesProgrammingLanguages2002}.
We describe two designs.

In the first design,
the data type used for the abstract representation of the \nm{} (|A_NM|) is |ExpTutorDiagram|, used in Section~\ref{sec:ExpTutor}.
We represent a program in \plName{TypedArith} with the type |Term_TyArith|
and the operation we focus on (|f_PL|) is |typeof :: Term_TyArith -> Maybe Type_TyArith|,
a function that gives the type of a term
(for simplicity we use a |Maybe| here to capture the cases where a term is not well-typed).

We then have two abstraction functions:

\begin{spec}
alpha_Term :: Term_TyArith -> ExpTutorDiagram
alpha_Type :: Type_TyArith -> Type_ExpTutor
\end{spec}

%
The implementation of |f_TyArith|,
the \nm{} operation (|f_NM|) that produces a \nm{}-level representation of maybe the type of a term,
is analogous to what is shown
in Figure~\ref{fig:commutativeDiagramTAPLMemoryDiagram}
because
\begin{enumerate*}[label=(\arabic*)]
    \item
the abstraction function (|alpha_Term|) has a left inverse (|alpha_Term_circ|)
and
    \item |f_PL| produces a Maybe.
\end{enumerate*}

\begin{code}
f_TyArith :: ExpTutorDiagram -> Maybe Type_ExpTutor
f_TyArith = fmap alpha_Type . typeof <=< alpha_Term_circ
\end{code}

% -- Ask for the type of a diagram not annotated with types
% typeOfBisim :: Bisimulation Term (Maybe TypedArith.Type) ExpTutorDiagram (Maybe ET.Type)
% typeOfBisim = MkBisim { fLang  = _fLang
%                       , fNM    = fmap tyToNM . _fLang <=< fromNM
%                       , alphaA = toNM
%                       , alphaB = fmap tyToNM }
%   where _fLang :: Term -> Maybe TypedArith.Type
%         _fLang = eitherToMaybe . typeof

%\begin{figure}
%
%\[
%\begin{tikzcd}
%|ExpTutorDiagram| \arrow[rrrrr, "|fmap alpha_Type|\; \circ\; |typeof|\; \circ_M\; |alpha_Term_circ|", dashed]
%                  \arrow[ddd,   "|alpha_Term_circ|"                                                 , shift left, dotted]
%& & & & &
%|Maybe Type_ExpTutor| \\
%\\
%\\
%|Term_TyArith| \arrow[rrrrr, "|typeof|"]
%               \arrow[uuu,   "|alpha_Term|", shift left]
%& & & & &
%|Maybe Type_TyArith| \arrow[uuu, "|fmap alpha_Type|", dashed] \\
%\end{tikzcd}
%\]
%
%    \caption{First attempt at instantiating the commutative diagram in Figure~\ref{fig:commutativeDiagram} for a notional machine that focuses on type-checking using the programming language \plName{TypedArith}.}
%    \label{fig:commutativeDiagramTypedArith-v1}
%\end{figure}

As is,
a student may benefit from the \nm{}'s representation of the program's abstract syntax tree and that may be helpful to reason about typing
but the \nm{} does not expose to the student the inner workings of the process of typing a term.

The second design,
represented in the diagram in Figure~\ref{fig:commutativeDiagramTypedArith-v2},
tackles this issue by enriching the \nm{} in a way that allows it to go step-by-step through the typing algorithm.
The idea is that |f_NM| now does not produce a type but gradually labels each subtree with its type as part of the process of typing a term.
For this,
|A_NM| here is |TyExpTreeDiagram|,
which differs from |ExpTreeDiagram| by adding to each node a possible type label.
We still want to write |f_NM| in terms of |f_PL|.
The key insight that enables this is
to change |f_PL| from |typeof| to |typeof1|.
%
The difference between |typeof| and |typeof1| is akin to the difference between big-step and small-step semantics:
|typeof1| applies a single typing rule at a time.
%
As a result,
we have to augment our representation of a program by bundling each term with a possible type
(captured in type |TyTerm_TyArith|).
The abstraction function and its left inverse are updated accordingly.
%A concrete representation of
The resulting \nm{} is depicted in Figure~\ref{fig:TypedArithExample}. 


% -- Annotate diagram with types
% annotateTypeBisim :: Bisimulation TypedTerm (Maybe TypedTerm) ExpTutorDiagram (Maybe ExpTutorDiagram)
% annotateTypeBisim = MkBisim { fLang  = _fLang
%                             , fNM    = fmap toNM . _fLang <=< fromNM
%                             , alphaA = toNM
%                             , alphaB = fmap toNM }
%   where _fLang :: TypedTerm -> Maybe TypedTerm
%         _fLang = eitherToMaybe . typeof1

\begin{figure}[h]

\[
\begin{tikzcd}
|TyExpTutorDiagram| \arrow[rrrrr, "|fmap alpha_TyTerm|\; \circ\; |typeof1|\; \circ_M\; |alpha_TyTerm_circ|", dashed]
                  \arrow[ddd,   "|alpha_TyTerm_circ|"                                                     , shift left, dotted]
& & & & &
|Maybe TyExpTutorDiagram| \\
\\
\\
|TyTerm_TyArith| \arrow[rrrrr, "|typeof1|"]
                 \arrow[uuu,   "|alpha_TyTerm|", shift left]
& & & & &
|Maybe TyTerm_TyArith| \arrow[uuu, "|fmap alpha_TyTerm|", dashed]
\end{tikzcd}
\]

    \caption{Instantiation of the commutative diagram in Figure~\ref{fig:commutativeDiagram} for a notional machine focused on type-checking programs in \plName{TypedArith}. The notional machine exposes the inner workings of the typing algorithm.}
    \label{fig:commutativeDiagramTypedArith-v2}
\end{figure}

\begin{figure}[h]
    \centering
    \includegraphics[width=.35\textwidth]{images/expression-tutor/TypedArithExample1.png}
    \hspace{1cm}
    \includegraphics[width=.35\textwidth]{images/expression-tutor/TypedArithExample1Next.png}
    % https://expressiontutor.org/activity/do?task=84891ae1-5133-451b-a37e-0aa81ed0f8f2
    \caption{One step in the notional machine \nmName{TypedExpTutorDiagram} as it types the term $\ift{\iszerot{\zero}}{\succt{\zero}}{\succt{\succt{\zero}}}$ in the language \plName{TypedArith}.}
    \label{fig:TypedArithExample}
\end{figure}

Interestingly,
given an expression |e|,
once we label all nodes in the ExpressionTutor diagram of |e| with their types,
the depiction of the resulting diagram is similar to the typing derivation tree of |e|.

Note that types are themselves trees but here we're representing them in a simplified form as textual labels because
the primary goal of ExpressionTutor is to represent the structure of terms, not the structure of types.



