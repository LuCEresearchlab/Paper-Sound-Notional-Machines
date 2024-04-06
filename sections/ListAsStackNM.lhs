\let\Bbbk\undefined  % https://github.com/kosmikus/lhs2tex/issues/82

%include polycode.fmt
%include formatting.fmt

%options ghci -fglasgow-exts

%if False
\begin{code}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFoldable #-}

module ListAsStackNM where


-- Data Structure (List)

-- data List a = Empty | Cons a (List a)
--   deriving (Show, Foldable)

-- data List a where
--   Empty :: List a
--   Cons :: a -> (List a) -> List a
--   deriving (Show, Foldable)
-- 
-- uncons :: List a -> Maybe (a, List a)
-- uncons Empty = Nothing
-- uncons (Cons x xs) = Just (x, xs)


-- Notional Machine (NM)

-- newtype Box = Box String
--   deriving (Show, Eq)
-- 
-- data Stack = Pallet
--           -- ^ A stack that is empty and only has the pallet.
--            | Stack Box Stack
--           -- ^ Stacking a box on top of a stack.
--   deriving (Show, Eq)

-- | When trying to pick up a box,
-- either there is only the pallet, in which case we get nothing,
-- or there is a box on top of a stack, in which case we get the box
-- and are left with the rest of the stack.
--
-- >>> pickUp (Stack (Box 1) Pallet)
-- Just (Box 1,Pallet)
-- >>> pickUp Pallet
-- Nothing
--pickUp :: Stack -> Maybe (Box, Stack)
--pickUp Pallet      = Nothing
--pickUp (Stack box stack) = Just (box, stack)


-- Bisimulation

langToNM :: Show a => [a] -> Stack
-- langToNM []     = Pallet
-- langToNM (x:xs) = Stack (Box (show x)) (langToNM xs)
langToNM = foldr (Stack . Box . show) Pallet


-- nilBisim :: Show a => Bisimulation () [a] () Stack
-- nilBisim = MkBisim { fLang = const empty
--                    , fNM = const Pallet
--                    , alphaA = id
--                    , alphaB = langToNM
--                    }
-- 
-- consBisim :: Show a => Bisimulation (a, [a]) [a] (Box, Stack) Stack
-- consBisim = MkBisim { fLang = uncurry cons
--                     , fNM = uncurry Stack
--                     , alphaA = bimap (Box . show) langToNM
--                     , alphaB = langToNM
--                     }
-- 
-- unconsBisim :: Show a => Bisimulation [a] (Maybe (a, [a])) Stack (Maybe (Box, Stack))
-- unconsBisim = MkBisim { fLang = uncons
--                       , fNM = pickUp
--                       , alphaA = langToNM
--                       , alphaB = fmap (bimap (Box . show) langToNM)
--                       }

\end{code}
%endif

% todo
% - why uncons

%% NM x Lang x Op:
%% ListAsStack x List x (cons, head, tail, etc.)
\subsection{Notional Machines for Data Structures}
\label{sec:ListAsStack}

Although most
notional machines focus on the semantics of programming language constructs,
some notional machines 
instead focus on data structures.
%
% Let's see how we can use our framework to reason about them...
%
One such notional machine,
which we model in this section,
is the ``List as Stack of Boxes'' (Figure~\ref{fig:nm-list-as-stack}),
described by~\citet{duboulayHowWorkLOGO1976} and included in the dataset of notional machines analyzed by~\citet{fincherNotionalMachinesComputing2020}.

%\subsubsection{Illustrative Example}

% saves about 20 lines of text
\begin{wrapfigure}{r}{.50\textwidth}
% \begin{figure}[h]
    \centering
    \begin{tabular}{c}
        \includegraphics[width=.50\textwidth]{images/nm-definition-cards/nm-list-as-stack}
    \end{tabular}
     \caption{The ``List as Stack of Boxes'' \nm{} as described by~\citet{duboulayHowWorkLOGO1976}.}
    \label{fig:nm-list-as-stack}
% \end{figure}
\end{wrapfigure}

%Let's see what is conceptually different
%between approaching a notional machine focused on a data structure.
%When we presented
For notional machines
focussing on the dynamic semantics of a language (\nmName{ExpTree}, \nmName{ExpTree}, \nmName{TAPLMemoryDiagram}),
the type |A_PL| represents a program in the language under focus
(and in the case of \nmName{TAPLMemoryDiagram}, additional information needed to evaluate the program)
and the function |f_PL| performs an evaluation step.
%
In the case of
\nmName{TypedExpTutorDiagram},
which focusses on type-checking (the static semantics of \plName{TypedArith}),
|A_PL| also represents a program in that language and |f_PL| performed type-checking.
%
Now we will model a notional machine focussing on a data structure, so |A_PL| represents that data structure and there are several |f_PL| functions, one for each operation supported by that data structure.
The type |A_NM| can be seen as an abstraction of |A_PL| and it should provide corresponding operations.
The commutation of the diagram demonstrates the correctness of this abstraction.

%\subsubsection{Commutative Diagram}

%Consider,
%for example,
Modelling
%the \nm{}
``List as Stack of Boxes''
%described by~\citet{duboulayHowWorkLOGO1976} (Figure~\ref{fig:nm-list-as-stack} shows the original illustration).
%Modeling it
requires some adaptations to the original description:
\begin{enumerate}
% - partiality of ops
\item
To avoid
%issues caused by the
partiality of the operations typically used to access the head and tail of a list (FIRST and REST in the original
%\nm{}
description),
we define a list using three operations: |Empty| and |Cons| to construct a list and \mbox{|uncons :: List a -> Maybe (a, List a)|} to deconstruct it.
That change also has a covenient representation in the \nm{}.
% - Show instance
\item
In the stack of boxes, each value is shown as a |String| in a box,
which means we can only represent lists of values for which we can create a |String| representation.
% - empty stack
\item
The original description
wasn't explicit about the
%\nm{}
representation of an empty list.
We need a corresponding empty stack of boxes that can be treated as a stack and not just the absence of boxes. For that we will use a pallet (used to hold boxes in storage).
%
The original description
%of the \nm{}
mentions a pallet in two contexts:
\begin{enumerate}
\item
``boxes are stacked on a pallet so that they can be picked up as one stack''.
That's an important part of the \nm{}'s behavior but we also need to be able to pick up a box from the top of the stack;
\item
``The beginning of a list, the top of the stack, is marked with [ and
the end of the list, the pallet is marked with ]''.
Here there's an asymmetry between the end of the list, represented with a pallet, and the beginning of the list, which has no representation in the notional machine.
Our denotation of pallet is different: it is the representation of an empty list.
\end{enumerate}
%
\end{enumerate}
%
The result is that we can construct lists either with a pallet
or by stacking a box on top of a stack,
which must have eventually a pallet at the end.
To deconstruct it, we can pick up
a box from the top of the stack
(\mbox{|pickUp :: Stack -> Maybe (Box, Stack)|}).
When trying to pick up a box,
either there is only the pallet, in which case we get nothing,
or there is a box on top of a stack, in which case
we are left with the box and the rest of the stack.

The process of formalizing the \nm{},
making it precise,
once again helped to identify the issues and improve the \nm{}.


%if False
\def\commentbegin{\quad\{\ }
\def\commentend{\}}
\begin{minipage}[b]{0.48\textwidth}
\begin{code}
data List a where
  Empty :: List a
  Cons :: a -> (List a) -> List a

uncons :: List a -> Maybe (a, List a)
uncons Empty = Nothing
uncons (Cons x xs) = Just (x, xs)
\end{code}
\end{minipage}
\begin{minipage}[b]{0ex}{\rule[0ex]{0.5pt}{1.5in}}
\end{minipage}
\hspace{1ex}
\begin{minipage}[b]{0.3\textwidth}\setlength\mathindent{2ex}
\begin{code}
newtype Box = Box String

data Stack where
  Pallet :: Stack
  Stack :: Box -> Stack -> Stack

pickUp :: Stack -> Maybe (Box, Stack)
pickUp Pallet      = Nothing
pickUp (Stack box rest) = Just (box, rest)
\end{code}
\end{minipage}
%endif


