\let\Bbbk\undefined  % https://github.com/kosmikus/lhs2tex/issues/82

%include polycode.fmt
%include formatting.fmt

%options ghci -fglasgow-exts

%if False
\begin{code}
{-# LANGUAGE ScopedTypeVariables   #-}

module ListAsStackNM where

\end{code}
%endif

%% NM x Lang x Op:
%% ListAsStack x List x (cons, head, tail, etc.)
\subsection{Notional Machines for Data Structures}
\label{sec:ListAsStack}

Some notional machines are not focused on the semantics of programming language constructs but instead are focused on data structures.
Let's see how we can use our framework to reason about them...
