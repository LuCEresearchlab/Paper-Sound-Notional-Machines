\let\Bbbk\undefined  % https://github.com/kosmikus/lhs2tex/issues/82

%include polycode.fmt
%include formatting.fmt

%options ghci -fglasgow-exts

%if False
\begin{code}
{-# LANGUAGE ScopedTypeVariables   #-}

module AlligatorNM where

-- import Control.Monad.State.Lazy (State, evalState, get, put)

data TermUL = App TermUL TermUL
            | Lambda Name TermUL
            | Var Name
type Name = String

newtype Color = MkColor { colorContent :: Double }

nameToColor :: Name -> Color
nameToColor = undefined
\end{code}
%endif

%% NM x Lang x Op:
%% Alligator x UntypedLambda x eval
\subsection{Informal Description}
\label{sec:AlligatorEggs}

% \begin{meta}
% \begin{itemize}
%     \item Describe it:
%     \begin{itemize}
%         \item pieces \done
%         \item rules \done
%         \item guessing game \done
%     \end{itemize}
%     \item Show examples \done
%     \item Describe the way it claims to relate to the language \done
%     \item Show how we don't have an isomorphism betweeen the abstract Lang and the abstract NM, but not all is lost... \done
%     \item What are the differences?
%     \begin{itemize}
%           \item not all parens are old allis \done
%           \item Alligators (lambdas) don't have to have an egg (variable use) \done
%           \item Evaluation strategy: "But that yellow alligator sure is hungry, and there's a tasty red egg infront of her. Here we go again..." The original instruction describe reduction under a lambda abstraction. We had to prevent that so the reduction strategy is the same as the one implemented by the lambda calculus interpreter (call-by-value). But attention, reduction under old alligations is necessary. \done
%     \end{itemize}
%     \item eggs can't be alone because only lambdas are values \done
%     \item NM can still be consistent if diagram commutes... \done
%     \item Build the commutation diagram \done
%     \item Explain need for deBruijn \done
%     \item Revert code to implementation following original description and run it to reveal the problem \done
%     \item Explain the problem: one of the cases of name clashes described in TAPL fails here
%     "But if she was guarding any eggs of the same color, each of those eggs hatches into what she ate." \done
%     %(\x.\x.x) a -> \x.a WRONG! the inner x is bound by the innermost lambda
%     \item Show example of the problem \done
%     \item Propose solution: propose a refinement of the recoloring rule that avoids color clashes \done
%     \item approach to reasoning... from proof to property-based testing \done
    % \item fix code and update text accordingly
    % \item talk about full beta reduction
% \end{itemize}
% \end{meta}


% one line summary
Alligator Eggs\footnote{\url{http://worrydream.com/AlligatorEggs/}} is a game
conceived by Bret Victor to introduce the lambda-calculus in a playful way.
It is essentially a \nm{} for the untyped lambda-calculus.
% detailed description (should follow tightly the implementation)
%
The game has three kinds of pieces and is guided by three rules.

\subsubsection{Pieces}
The pieces are \emph{hungry alligators}, \emph{old alligators}, and \emph{eggs}.
Old alligators are white, while hungry alligators and eggs are colored with colors other than white.
The pieces are placed in a plane and their relative position with respect to each other determines their relationship.
All pieces placed under an alligator are said to be guarded by that alligator.
An alligator together with the pieces that may be guarded by it form a family.
Families placed to the right of another family may be eaten by the guardian of the family on the left, depending on the applicability of the gameplay rules.
Every egg must be guarded by an alligator with the same color (this must be a hungry alligator because eggs cannot be white).

\subsubsection{Rules}
There are three rules that determine the ``evolution of families" over time:
% In every round, each rule is applied to the first family (the topmost leftmost family on the plane), unless specified otherwise.

\begin{description}
\item[Eating rule]
          % notice the "there is"... but which one eats first? eval strategy to be discussed later
          If there is a family guarded by a hungry alligator in the plane and there is a family or egg to its right, then the hungry alligator eats the entire family (or egg) to its right and the pieces of the eaten family are removed.
          The alligator that ate the pieces dies and the eggs that were guarded by this alligator and that have the same color of this alligator are hatched and are replaced by a copy of what was eaten by the alligator.
\item[Color rule]
          Before a hungry alligator $A$ can eat a family $B$, if a color appears both in $A$'s proteges and in $B$, then that color is changed in one of the families to another color different from the colors already present in these families.
\item[Old age rule]
          If an old alligator is guarding only one egg or one family (which itself may be composed of multiple families), then the old alligator dies and is removed.
\end{description}

%\subsubsection{Gameplay}
%A few suggestions of gameplay are provided.
%% guessing game
%An option would be to build a series of puzzles that challenge the player to find out, given a set of families organized in the plane, what should be the color of some of the pieces for the families to evolve into another given family.
%%
%Similarly, the player could be asked to devise a family that when fed $X$ produces $Y$.
%%
%These puzzles could be embedded into a board game, where the player needs to solve puzzles to make progress.

\subsubsection{Relation to the Untyped Lambda-Calculus}
According to their description, the way \nmName{Alligator} relates to the untyped lambda-calculus is as follows:
``A hungry alligator is a lambda abstraction,
an old alligator is parentheses,
and eggs are variables.
The eating rule corresponds to beta-reduction.
The color rule corresponds to (over-cautious) alpha-conversion.
The old age rule says that if a pair of parentheses contains a single term, the parentheses can be removed''.
%
Although very close, this relation is not completely accurate.
We will identify the limitations and propose solutions.


\subsection{Illustrative Example}
%\todo build another example because egss can't be alone.
Figure~\ref{fig:alligator-diagram} shows a representation of
the evaluation of the lambda-calculus term
$\texttt{\app{(\abs{t}{\app{(\abs{f}{t})}{a}})}{(\abs{b}{b})}}$
using the \nmName{Alligator} \nm{}.
% Note: this is equivalent to the Expression Tree example from Figure~\ref{fig:takeFirst}
Step 0 shows
the alligator families corresponding to the term.
%
In the first step,
the red alligator eats
the family made of the purple alligator and the purple egg.
As a result,
the eaten family disappears,
the red egg guarded by the red alligator hatches and is replaced by the family that was eaten,
and
the red alligator disappears (dies).
%
In the second step,
the grey alligator eats the green egg.
As a result,
the grey alligator dies
and
no eggs are hatched
because it was not guarding any grey eggs.
We are left with the purple family.


\begin{figure}
    \centering
    \begin{diagram}[width=0.9\textwidth]
import Diagrams.Prelude hiding (trace)

import NotionalMachines.Lang.UntypedLambda.Main (Exp (..), parse)
import NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs (diagramTrace)
import NotionalMachines.Meta.Steppable (Steppable (trace))
import NotionalMachines.Util.Diagrams (diagramWithError)
import NotionalMachines.Examples.Diagrams (langAndNMTrace)

t = langAndNMTrace 0.05 diagramTrace (fmap trace . parse)

dia = (diagramWithError . t)
      "(\\t. (\\f. t) a) (\\b. b)"
      -- "(\\t. \\f. t) a b"
    \end{diagram}
    \caption{Evaluation of $\texttt{\app{(\abs{t}{\app{(\abs{f}{t})}{a}})}{(\abs{b}{b})}}$ in the untyped lambda calculus (top) and \nmName{Alligator} \nm{} (bottom).}
    \label{fig:alligator-diagram}
\end{figure}


\subsection{Commutative Diagram}

To build a commutative diagram for \nmName{Alligator},
we need to build the abstract representation of the \nm{} |A_NM|, which corresponds to the game pieces and the game board,
the abstraction function |alpha :: TermUL -> A_NM|, and
an |f_NM| function, which correspond to the rules that guide the evolution of alligator families.
%
% What are the differences?
First, we look more precisely at the game pieces
and their relationship with |TermUL| to model |A_NM|.
%
%Although |A_NM| and |TermUL| are not isomorphic,
%that in itself doesn't prevent the construction of a commutative diagram.

\begin{description}
    \item[Eggs]
        An egg corresponds to a variable use and its color corresponds to the variable name.

    \item[Hungry alligators]
        % - Alligators (lambdas) don't have to have an egg (variable use)
        A hungry alligator somewhat corresponds to a lambda abstraction with
        its color corresponding to the name of the variable introduced by the lambda (a variable definition)
        and the pieces guarded by the hungry alligator corresponding to the body of the lambda abstraction.
        But differently from a lambda abstraction, a hungry alligator doesn't have to be guarding any pieces, which has no direct correspondence with the lambda calculus because a lambda abstraction cannot have an empty body.

    \item[Old alligators]
        % - not all parens are old allis
        An old alligator somewhat corresponds to parentheses but not exactly.
        The lambda abstraction in the term $\texttt{\app{\app{(\abs{t}{\abs{f}{t}})}{a}}{b}}$ requires parentheses because conventionally the body of a lambda abstraction extends as far to the right as possible, so without the parentheses its body would be $\texttt{\app{\app{t}{a}}{b}}$ instead of $\texttt{t}$.
        However the corresponding alligator families shown in Figure~\ref{fig:alligator-diagram} don't require an old alligator.
        On the other hand, if we want to represent the term $\texttt{\app{a}{(\app{b}{c})}}$, then we need an old alligator.
        Figure~\ref{fig:OldAlligator} shows an example of a term that requires an old alligator.
        %Like parentheses, old alligators are used to disambiguate an abstract syntax tree.
\end{description}

\begin{wrapfigure}{r}{.35\textwidth}
% \begin{figure}
\centering
\begin{diagram}[width=.35\textwidth]
import NotionalMachines.Util.Diagrams
import NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs

dia = (diagramWithError . diagram)
      "(\\a.y) ((\\b.b) c)"
\end{diagram}
% \caption{Example of family with old alligator.}
\caption{Old alligator in $\texttt{\app{(\abs{a}{y})}{(\app{(\abs{b}{b})}{c})}}$}
\label{fig:OldAlligator}
% \end{figure}
\end{wrapfigure}

% what about App
Now let's look at the terms of the untyped lambda-calculus.
If hungry alligators are lambda abstractions and eggs are variables then what is an application?
Applications are formed by the placement of pieces on the game board.
When an alligator family or egg (corresponding to a term \texttt{$t_1$}) is placed to the left of another family or egg (corresponding to a term \texttt{$t_2$}),
then this corresponds to the term \texttt{$t_1$} applied to \texttt{$t_2$} (in lambda calculus represented as $\app{t_1}{t_2}$).

% - eggs can't be alone because only lambdas are values
Notice that because 
every egg must be guarded by a hungry alligator with the same color,
strictly speaking, an egg cannot appear all by itself.
That corresponds to the fact that
in the untyped lambda-calculus
only lambda terms are values,
so a term cannot have unbounds variables.
Textbooks, of course, often use examples with unbound variables but these are actually metavariables that stand for an arbitrary term.
So,
for convenience,
we will consider that an egg by itself also forms a family.

We can then model an alligator family as the type |AlligatorFamily|,
and a game board as simply a list of alligator families.
The result is
the commutative diagram shown in Figure~\ref{fig:commutativeDiagramAlligator-v1}.

\begin{spec}
data AlligatorFamily  =  HungryAlligator Color [AlligatorFamily]
                      |  OldAlligator [AlligatorFamily]
                      |  Egg Color
\end{spec}
% \begin{code}
% type Board = [AlligatorFamily]
% \end{code}

The abstraction function |alpha| relies on some function \eval{:t nameToColor}.

\begin{code}
alpha ::  TermUL                 ->  [AlligatorFamily]
alpha     (Var name)             =   [Egg (nameToColor name)]
alpha     (Lambda name e)        =   [HungryAlligator (nameToColor name) (alpha e)]
alpha     (App e1 e2@(App _ _))  =   alpha e1 ++ [OldAlligator (alpha e2)]
alpha     (App e1 e2)            =   alpha e1 ++ alpha e2
\end{code}



\subsubsection{From Proof to Property-Based Testing}
% connect with what we've seen so far. where are we?
% So far we have seen several examples of how we can cast a \nm{} as a simulation of some aspect of a corresponding programming language
% and use this construction to reason about the soundness of a \nm{}.
% Approach 1: tests
% In practice, this reasoning was guided by a corresponding implementation for each \nm{} discussed so far,
% which include property-based testing that help to check, among other things, that the corresponding simulation diagrams indeed commute.
%
The commutativity of the diagrams presented in Chapter~\ref{chr:Modeling} was demonstrated using equational reasoning.
% The commutative diagrams in section~\ref{sec:Modeling} and section~\ref{sec:ExpTutor} were demonstrated to indeed commute using equational reasoning.
% In section~\ref{sec:Modeling} and section~\ref{sec:ExpTutor}, equational reasoning was used to demonstrate the commutativity of the diagrams in each section.
Here instead, we implement the elements that constitute the commutative diagram and 
use property-based testing to test if the diagram commutes.
This approach is less formal and it doesn't prove the \nm{} correct,
but it is lightweight and potentially more attractive to users that are not familiar with equational reasoning or mechanised proofs.
We will see here that, despite its limitations, this approach can go a long way in revealing issues with a \nm{}.
% Explain need for deBruijn (in fact the original descriptions antecipates the chanllenge of comparing alligator families)
The idea is that
a generator generates terms |t_i :: TermUL| and checks that |(f_NM . alpha) t_i == (alpha . step) t_i|.

\begin{figure}

% v0
\[
\begin{tikzcd}
|[AlligatorFamily]| \arrow[rr, "|f_NM|"]
& &
|[AlligatorFamily]| \\
\\
|TermUL| \arrow[rr, "|step|"]
         \arrow[uu, "|alpha|"]
& &
|TermUL| \arrow[uu, "|alpha|"]
\end{tikzcd}
\]

    \caption{First attempt at instantiating the commutative diagram in Figure~\ref{fig:commutativeDiagram} for the notional machine \nmName{Alligator}.}
    \label{fig:commutativeDiagramAlligator-v1}
\end{figure}


\subsubsection{de Bruijn Alligators}
The first challenge is that we need to compare values of type |[AlligatorFamily]| that were produced using |f_NM| with values produced using |step|.
As we have seen, the colors in |AlligatorFamily| correspond to variable names
but the way |step| generates fresh names (which then are turned into colors)
may be different from the way |f_NM| will generate fresh colors.
%
In fact, the original description of \nmName{Alligator} anticipates the challenge of comparing alligator families.
In the description of possible gameplays, they clarify that to compare alligator families we need to take into account that families with the same "color pattern" are equivalent.
% "For this to work well, we'd need a better color rule, to explain that families with different colors in the same "pattern" are equivalent."
%
%
% From TAPL:
% de Bruijn representation
% Nameless terms are also sometimes called de Bruijn terms
% numeric variables in them are called de Bruijn indices
% This can be accomplished by replacing named variables by natural numbers, where the number k stands for “the variable bound by the k’th enclosing λ.” For example, the ordinary term λx.x corresponds to the nameless term λ.0, while λx.λy. x (y x) corresponds to λ.λ. 1 (0 1)
% each (closed) ordinary term has just one de Bruijn representation
% two ordinary terms are equivalent modulo renaming of bound variables iff they have the same de Bruijn representation
%
This can be achieved by
using a \emph{de Bruijn representation}~\citep{debruijnLambdaCalculusNotation1972}
of Alligators.
We turn |AlligatorFamily| into |AlligatorFamilyF Color| and
before comparing families we transform them into |AlligatorFamilyF Int| following the de Bruijn indexing scheme.
%
The commutative diagram
we are moving towards
is shown in Figure~\ref{fig:commutativeDiagramAlligator-v2}.

%if False
\begin{code}
data AlligatorFamilyF a = HungryAlligator a [AlligatorFamilyF a]
                        | OldAlligator [AlligatorFamilyF a]
                        | Egg a
  deriving (Eq)

type AlligatorFamily = AlligatorFamilyF Color
\end{code}
%endif

\begin{figure}

% % More detailed explaination of the commutation diagram (types and functions) later??
% v2
\[
\begin{tikzcd}
|[AlligatorFamilyF Color]| \arrow[r, "|f_NM|"]
&
|[AlligatorFamilyF Color]| \arrow[r, "|deBruijn|"]
&
|[AlligatorFamilyF Int]|
\\
& &
|[AlligatorFamilyF Color]| \arrow[u, "|deBruijn|"] \\
\\
\\
|TermUL| \arrow[rr, "|eval|"]
         \arrow[uuuu, "|alpha|"]
& &
|TermUL| \arrow[uuu,  "|alpha|"]
\end{tikzcd}
\]

    \caption{Second attempt at instantiating the commutative diagram in Figure~\ref{fig:commutativeDiagram} for the notional machine \nmName{Alligator}.}
    \label{fig:commutativeDiagramAlligator-v2}
\end{figure}

\subsubsection{Evaluation Strategy}
% now we have the setup to run the tests
With this setup in place, the next step is to implement |f_NM| in terms of the game rules.
%
% do the rules correspond to eval things?
%
% problem with eating rule
The eating rule (together with the color rule) somewhat corresponds to beta-reduction
but under what evaluation strategy?
% Evaluation strategy: "But that yellow alligator sure is hungry, and there's a tasty red egg infront of her. Here we go again..." The original instruction describe reduction under a lambda abstraction. We had to prevent that so the reduction strategy is the same as the one implemented by the lambda calculus interpreter (call-by-value). But attention, reduction under old alligations is necessary.
The choice of evaluation strategy turns out to affect not only the eating rule but also the old age rule.
According to the original description, any hungry alligator that has something to eat can eat
and one of the original examples shows a hungry alligator eating an egg even when they are under another hungry alligator.
That would correspond to a \emph{full beta-reduction} evaluation strategy
but we will stick to a call-by-value lambda-calculus interpreter so we will adapt
the rules accordingly.
% todo: what if we changed the interpreter instead?
% problem with old alli
The old age rule has to be augmented
to trigger the evolution of
\begin{enumerate*}[label=]
\item an old alligator family that follows a topmost leftmost hungry alligator and
\item families under a topmost leftmost old alligator.
\end{enumerate*}
% so when do we eat?
The eating rule should be triggered
only for the topmost leftmost hungry alligator,
unless it is followed by an old alligator (in which case the augmented old age rule applies).
% todo:
% if we implement the rules as originally described we can use property-based testing to see what are the cases that fail and understand from there what are the rules that have to be changed to make them behave consistently with the call-by-value lambda-calculus.
%
%if False
\begin{code}
-- The eating rule says that if there are some families side-by-side, the
-- top-left alligator eats the family to her right.
eatingRule :: (Enum a, Eq a) => [AlligatorFamilyF a] -> [AlligatorFamilyF a]
eatingRule ((HungryAlligator c proteges):family:rest) = map hatch proteges ++ rest
  where hatch (Egg c1)                | c == c1 = family
        hatch (HungryAlligator c1 ys)           = HungryAlligator c1 (map hatch ys)
        -- hatch (HungryAlligator c1 ys) | c /= c1 = HungryAlligator c1 (map hatch ys) -- NM fix
        hatch (OldAlligator ys)                 = OldAlligator (map hatch ys)
        hatch protege                           = protege
eatingRule families = families

-- When an old alligator is just guarding a single thing, it dies.
oldAgeRule :: (Eq a, Enum a) => [AlligatorFamilyF a] -> [AlligatorFamilyF a]
oldAgeRule (OldAlligator        [] : rest) = rest
oldAgeRule (OldAlligator [protege] : rest) = protege : rest
oldAgeRule (OldAlligator proteges  : rest) = OldAlligator (evolve proteges) : rest -- needed in CBV
oldAgeRule (a : rest@(OldAlligator _:_))   = a : evolve rest -- needed in CBV
oldAgeRule families                        = families
\end{code}
%endif
%
% problem with recoloring rule
The color rule plays an important role in the correct behavior of the eating rule as a correspondence to beta-reduction.
That's because indeed
"the color rule corresponds to (over-cautious) alpha-conversion",
so it is responsible for avoiding variable capture.

%if False
\begin{code}
-- "If an alligator is about to eat a family, and there's a color that appears
-- in both families, we need to change that color in one family to something
-- else."
-- Change the colors of `family` that also appear in `a` to colors that don't
-- appear in neither family.
colorRule :: (Enum a, Eq a) => [AlligatorFamilyF a] -> [AlligatorFamilyF a]
colorRule (a@(HungryAlligator _ _):family:rest) = a:recolor a family:rest
colorRule families                              = families

recolor :: forall a. (Enum a, Eq a) => AlligatorFamilyF a -> AlligatorFamilyF a -> AlligatorFamilyF a
recolor = undefined
-- recolor a1 a2 = evalState (mapM go a2) ([], toEnum 0)
--   where -- The state contains:
--         -- * A mapping of color substitutions
--         -- * The next candidate color for a substitution (a counter)
--         go :: a -> State ([(a, a)], a) a
--         go c | c `notElem` a1 = return c
--              | otherwise = do (subs, next) <- get
--                               case lookup c subs of
--                                 Just newC -> return newC
--                                 Nothing -> let newC = nextNotIn [a1, a2] next
--                                            in do put ((c, newC):subs, succ newC)
--                                                  return newC
--           where
--             nextNotIn :: (Enum a, Eq a) => [AlligatorFamilyF a] -> a -> a
--             nextNotIn as = until (\x -> all (notElem x) as) succ
\end{code}
%endif

With all the rules implemented, we can define
a function |evolve|
that applies them in sequence.
We will then use |evolve| in the definition of |f_NM|.
%
% | Taking a step consists of using the first rule that applies (order matters).
% TODO: talk about the fact that evolve doesn't correspond to step from lambda
% (e.g. oldAlligator rule, :asciiTrace (\a.a) ((\b.b) c)). Talk about non-lock-step simulation.
%if False
\begin{code}
evolve :: (Enum a, Eq a) => [AlligatorFamilyF a] -> [AlligatorFamilyF a]
evolve = applyRules [oldAgeRule, colorRule, eatingRule]
  where  applyRules [] a                    =  a
         applyRules (f:fs) a  |  f a == a   =  applyRules fs a
                              |  otherwise  =  f a
\end{code}
%endif
%
% - talk about lock-step simulation in therefore |f_PL /= step|
One application of |evolve| corresponds to one step in the notional machine layer but
that step doesn't correspond to a step in the programming language layer.
For example,
The main action of the old age rule
(to remove old alligators)
doesn't have a correspondence in the reduction of terms in \plName{UntypedLambda}.
In terms of simulation theory,
in this case the simulation of the programming language by the notional machine is not lock-step.
To adapt
our property-based testing approach,
instead of making |f_PL| equal to |step|,
we will simply reduce the term all the way to a value
(leading to the use of |eval| as |f_PL| in Figure~\ref{fig:commutativeDiagramAlligator-v2})
and correspondingly define |f_NM| to be the successive applications of |evolve| until we reach a fixpoint.


\subsection{Problem: Substitution of Bound Variables}
% should we explore the question "in what order should the rules be applied?"
Now we have all the building blocks of the commutative diagram.
We can put them together by 
running the property-based tests to try to uncover issues in the diagram
and indeed we do.
%
According to the eating rule, after eating, a hungry alligator dies and if it was guarding
any eggs of the same color, each of those eggs hatches into what was eaten.
%
So
the family corresponding to
$\texttt{\app{(\abs{a}{(\abs{a}{a})})}{t}}$,
for example,
would evolve to
the family corresponding to
$\texttt{\abs{a}{t}}$ instead of $\texttt{\abs{a}{a}}$,
as shown in Figure~\ref{fig:alligator-issue}. 
%
This issue
corresponds to a well-known pitfall in substitution:
we cannot substitute \emph{bound} occurrences of a variable, only the ones that are \emph{free}.
%we can only substitute \emph{free} occurrences of a variable.
%we cannot substitute bound variables
%only free variables can be substituted.
%


\begin{figure}
\begin{diagram}[width=0.60\textwidth]
import NotionalMachines.Lang.UntypedLambda.Main
import NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs
import NotionalMachines.Machine.AlligatorEggs.Main
import NotionalMachines.Machine.AlligatorEggs.Diagram
import NotionalMachines.Util.Diagrams
import NotionalMachines.Meta.Steppable

wrongAlligatorDiagram :: IO (Diagram B)
wrongAlligatorDiagram = do
        font <- fontMono
        let e2 = step e1
        a1 <- (alligator . langToNm) e1
        a3 <- (alligator . wrongEvolve . langToNm) e1
        a2 <- (alligator . langToNm) e2
        return (vertices [t font (unparse e2) # named "B_PL",
                          a2 # named "B_NM1",
                          a3 # named "B_NM2",
                          a1 # named "A_NM",
                          t font (unparse e1) # named "A_PL"]
               # c "A_PL" "A_NM" 0.05 (label alpha)
               # c "A_NM" "B_NM2" 0.06 (label fnm)
               # c "A_PL" "B_PL" 0.06 (label fpl)
               # c "B_PL" "B_NM1" 0.05 (label alpha))

  where c = connectOutside'' (def { _headGap = small,
                                    _tailGap = small,
                                    _shaftStyle = mempty # lw thin,
                                    _headLength = local 0.03})

        label s = text s # fontSize (local 0.05)
               <> rect 0.3 0.12 # lw none

        t font s = d <> rect (width d) (height d) # lw none
            where d = text'' font black 0.1 s # centerXY
        alligator fs = (fmap ( centerXY
                             . sized (mkWidth (0.3 * fromIntegral (length fs))))
                     . toDiagram) fs

        vertices = atPoints [p2 (w,0), p2 (w,m*h), p2 (m*w,h), p2 (0,h), p2 (0,0)]
            where w = 1.5
                  h = 0.7
                  m = 0.7

        --e1 = App (Lambda "t" (Lambda "t" (Var "t"))) (Lambda "a" (Var "a"))
        e1 = App (Lambda "a" (Lambda "a" (Var "a"))) (Var "t")

        alpha = "$\\alpha$"
        math a s = "\\ensuremath{\\Conid{" ++ a ++ "}_{\\Conid{" ++ s ++ "}}}"
        anm = math "A" "NM"
        bnm = math "B" "NM"
        fnm = math "f" "NM"
        apl = math "A" "PL"
        bpl = math "B" "PL"
        fpl = math "f" "PL"

dia = wrongAlligatorDiagram
\end{diagram}
    \caption{Unsoundness: \emph{bound} occurrences of a variable should not be substituted.}
    \label{fig:alligator-issue}
\end{figure}


% solution
The issue can be solved in one of the following ways:
\begin{enumerate}[label=(\arabic*)]
\item
Refining the description of the eating rule,
changing
``if she was guarding any eggs of the same color, each of those eggs hatches into what she ate''
into
``if she was guarding any eggs of the same color \textbf{that are not guarded by a closer alligator with the same color}, each of those eggs hatches into what she ate'';
\item
Restricting all colors of hungry alligators in a family to be distinct;
\item
Defining colors to correspond to de Bruijn indices instead of names.
This means that not only colors wouldn't be repeated in the same family but also that every family would use the same "color scheme" for structurally equivalent terms.
\end{enumerate}



% * Implementing the rules of the game as originally described and testing
%   them with the commutation test reveals a mistake in the substitution (eating
%   rule + color rule). The color rule is enough to avoid variable capture, but
%   eating doesn't explain what to do with bound variables. Consider the
%   following example:
%
%     a-----------< a-<
%      i-----< a-<   a
%       i b-<   a
%          a
%
%   We apply the color rule changing the color of the alligator family that is about to be eatten obtaining:
%
%     a-----------< c-<
%      i-----< a-<   c
%       i b-<   a
%          a
%
% In the language of alligators, a family could be composed of multiple hungry alligators with the same color, who is guarding an egg of this color?
%
%
%  Alligators
%    commutation proof:                                                                                                           FAIL (0.04s)
%        ✗ commutation proof failed at test/Spec.hs:139:3
%          after 8 tests and 9 shrinks.
%
%              ┏━━ test/Spec.hs ━━━
%          136 ┃ isEquivalentTo :: (Eq a, Show a, Show e) => Gen e -> (e -> a) -> (e -> a) -> Property
%          137 ┃ isEquivalentTo g f f' = prop $ do
%          138 ┃   e <- forAll g
%              ┃   │ App
%              ┃   │   (Lambda
%              ┃   │      "a"
%              ┃   │      (App
%              ┃   │         (Lambda "g" (App (Var "g") (Lambda "b" (Var "a"))))
%              ┃   │         (Lambda "a" (Var "a"))))
%              ┃   │   (Lambda "a" (Var "a"))
%          139 ┃   f e === f' e
%              ┃   ^^^^^^^^^^^^
%              ┃   │ ━━━ Failed (- lhs) (+ rhs) ━━━
%              ┃   │ - Just [ HungryAlligator 0 [ HungryAlligator 0 [ Egg 0 ] ] ]
%              ┃   │ + Just [ HungryAlligator 0 [ Egg 0 ] ]
%
%          This failure can be reproduced by running:
%          > recheck (Size 7) (Seed 11253128347395396264 14482578956410378323) commutation proof
%
%      Use '--hedgehog-replay "Size 7 Seed 11253128347395396264 14482578956410378323"' to reproduce.

%%%%%%
% Lambda> :trace (\a. (\g. g (\b.a)) (\a.a)) (\a.a)
% [ (\a.(\g.g (\b.a)) (\a.a)) (\a.a)
% , (\g.g (\b.(\a.a))) (\a.a)
% , (\a.a) (\b.(\a.a))
% , (\b.(\a.a)) ]
%
% Wrong:
% Alligator> :asciiTrace (\a. (\g. g (\b.a)) (\a.a)) (\a.a)
% a-----------< a-<
%  g-----< a-<   a 
%   g b-<   a      
%      a           
% 
% g-------< a---<
%  g b---<   c-< 
%     c-<     c  
%      c         
% 
% a---< b---<
%  d-<   c-< 
%   d     c  
% 
% d-<
%  d
%
% [ (\a.(\g.g (\b.a)) (\a.a)) (\a.a)
% , (\g.g (\b.(\c.c))) (\a.(\c.c))
% , (\a.(\d.d)) (\b.(\c.c))
% , (\d.d)
%%%%%%

%    Alligators
%      commutation proof:                                                                                                           FAIL (0.08s)
%          ✗ commutation proof failed at test/Spec.hs:139:7
%            after 9 tests and 8 shrinks.
%
%                ┏━━ test/Spec.hs ━━━
%            136 ┃ isEquivalentTo :: (Eq a, Show a, Show e) => Gen e -> (e -> a) -> (e -> a) -> Property
%            137 ┃ isEquivalentTo g f f' = prop $ do
%            138 ┃   e <- forAll g
%                ┃   │ￔ� App
%                ┃   │ￔ�   (Lambda
%                ┃   │ￔ�      "a"
%                ┃   │ￔ�      (App
%                ┃   │ￔ�         (Lambda "b" (Lambda "c" (App (Var "b") (Var "a"))))
%                ┃   │ￔ�         (Lambda "a" (Var "a"))))
%                ┃   │ￔ�   (Lambda "a" (Var "a"))
%            139 ┃   f e === f' e
%                ┃   ^^^^^^^^^^^^
%                ┃   │ￔ� ━━━ Failed (- lhs) (+ rhs) ━━━
%                ┃   │ￔ�   Just [
%                ┃   │ￔ�       HungryAlligator
%                ┃   │ￔ�         0 [
%                ┃   │ￔ� -         HungryAlligator 0 [ Egg 0 ]
%                ┃   │ￔ� +         HungryAlligator 0 [ HungryAlligator 0 [ Egg 0 ] ]
%                ┃   │ￔ�         , HungryAlligator 0 [ Egg 0 ]
%                ┃   │ￔ�         ]
%                ┃   │ￔ�     ]
%
%            This failure can be reproduced by running:
%            > recheck (Size 8) (Seed 5604705324055738461 17944592572627097659) commutation proof
%
%        Use '--hedgehog-replay "Size 8 Seed 5604705324055738461 17944592572627097659"' to reproduce.
%
%        Use -p '/Alligators.commutation proof/' to rerun this test only.

%%%%%%
% Alligator> :trace (\a. (\b. (\c. b a)) (\a.a)) (\a.a)
% [ (\a.(\b.(\c.b a)) (\a.a)) (\a.a)
% , (\b.(\c.b (\a.a))) (\a.a)
% , (\c.(\a.a) (\a.a)) ]
%
% Alligator> :asciiTrace (\a. (\b. (\c. b a)) (\a.a)) (\a.a)
% a-----------< a-<
%  b-----< a-<   a 
%   c---<   a      
%    b a           
% 
% b-------< a---<
%  c-----<   d-< 
%   b d-<     d  
%      d         
% 
% c---------<
%  a---< d-<
%   e-<   d 
%    e
%
% [ (\a.(\b.(\c.b a)) (\a.a)) (\a.a)
% , (\b.(\c.b (\d.d))) (\a.(\d.d))
% , (\c.(\a.(\e.e)) (\d.d)) ]
%%%%%%

