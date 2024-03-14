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

\todo Missing topics:
\begin{itemize}
\item Connect with introduction
\item One way to approach the problem would be to
model the subset of Java needed for the notional machine to work
but Java is a fairly complex language
so instead will approach this problem
in a way similar to the one taken in the last section:
we'll treat arrays as a data structure (a datatype) and the operations it supports.
\end{itemize}


