\let\Bbbk\undefined  % https://github.com/kosmikus/lhs2tex/issues/82

%include polycode.fmt
%include formatting.fmt

%options ghci -fglasgow-exts

%if False
\begin{code}
{-# LANGUAGE ScopedTypeVariables   #-}

module ArrayAsParkingSpotsNM where

\end{code}
%endif

%% NM x Lang x Op:
%% ArrayAsParkingSpots x Java x (allocate, read, write, etc.)
\subsection{A Notional Machines for Arrays}
\label{sec:ArrayAsParkingSpots}

% -- Connect with introduction
With the techniques we have developed so far,
we can turn again to
the notional machine
"Array as Row of Parking Spaces is Parking Lot",
presented in Section~\ref{sec:UnsoundNotionalMachines}.
%
There we considered Java as the underlying programming language
and already identified the first issue,
caused by the difference between
the representation of
arrays of primitive types
and
arrays of reference types.
%
% --- two problems:
Here,
we will make the notional machine more precise
and in the process resolve two problems.

% -- array as data structure
One approach would be to
model the subset of Java needed for the notional machine to work,
but Java is a fairly complex language.
Instead we
start by following the approach
taken in the last section:
we model the PL layer as an idealized array
and the operations it supports.
%
An array supports three operations:
\emph{allocating} an array of a given type and a given length,
\emph{reading} an element at a given index, and
\emph{writing} an element to a given index.

% -- problem: empty spaces, primitive types versus reference types
\subsubsection{Problem: reference types versus primitive types}

\begin{figure*}[h]
    \centering
    \includegraphics[width=\textwidth]{images/alligators/NM-concrete-abstract-abstract-concrete.pdf}
    \caption{Both program and \nm{} have abstract and concrete representations.}
    \label{fig:concrete-abstract}
\end{figure*}


One of the appeals of the notional machine is that
we would represent
a newly allocated array of objects
as an empty parking lot,
because
it
does not contain "valid" values (their slots contain \emph{null}).
%We want to keep this appeal so
The first issue is
%immediately revealed
that
%because
arrays of primitive types are really different
in that,
when newly allocated,
their slots already contain valid values.
These arrays would be represented as fully booked parking lots
with slots that can never be empty!
%
% -- solution: accept reality, communicate to the students and use it as a learning opportunity
Here,
instead of changing the design of the notional machine,
we can use %this distinction (
this misfit in the metaphor
%),
as a learning opportunity
and explicitly discuss it with students.
%
% -- problem: representation of the values, boxed values versus unboxed values
There is still a decision to be made about how exactly the values in the array are going to be represented.
We could represent a value as a car with the string representation of the value drawn on its roof,
but representing both unboxed and boxed versions of an integer with the same string could be confusing.
Instead,
we could opt for representing values of reference types as cars of a different color, for example.
Of course an array can also contain other arrays,
which are themselves objects,
and that complicates the picture.

\subsubsection{Problem: representing multiple arrays}
To represent multiple arrays,
and the relationship between them,
we cannot
model the PL layer simply as an idealized array%
\footnote{
The representation of lists shown in Section~\ref{sec:ListAsStack} is not suitable to represent lists that contain lists.
}.
Instead,
we use the programming language
%here is
\plName{LambdaRef}
augmented with arrays.
% - not entire language
We first used this language
in Section~\ref{sec:State},
where the \nmName{TAPLMemoryDiagram} notional machine represented all constructs of the language
as well as the program's state as it ran (its memory): the store and name environment.
%
Here %instead,
we want to represent only arrays
and values
so
the notional machine layer contains,
besides the parking lot,
essentially
a sequence of statements equivalent to the occurrences of all terms in the program that manipulate
arrays
(an array allocation, array access, and assignment of a value to an array slot)
as they happen when the program runs.
This way we can ignore all other terms in the language
and aspects of the program's memory as it runs.
Besides the information present in the array-manipulating terms themselves,
we need one more thing:
% -- problem: multiple arrays, a problem of identity
to be able to uniquely identify each array
(e.g.~with its location, or address).
We annotate each parking lot with the corresponding location
(for example using an @@ and the location identifier)
which we use to identify the parking lot when we
write to
(or read from)
its slots.

% -- solution: use add signs
We can then represent values of reference type using their locations.
We could instead use arrows, like we have done in \nmName{TAPLMemoryDiagram},
but that would not work for $\refr{}$'s because here we are only representing arrays.


