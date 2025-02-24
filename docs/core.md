# Core Language (modulo changes, new features and bugs)

The core language is work-in-progress:

The provided core language (see [`Recalc.Semantics`][recalc-semantics])
implements a simple, dependently typed functional programming language
based on LambdaPi[^1] which is extended by cell references.

## Syntax

Cell references (and ranges) are similar to Excel:

\begin{aligned}
\mathfrak{m} \
  \mathrel{::=} & 1 \mid 2 \mid \dots \\
\mathfrak{n} \
  \mathrel{::=} & \text{A} \mid \text{B} \mid
      \dots \mid \text{AA} \mid \dots \\
\alpha
  \ \mathrel{::=} & \ \mathfrak{n}\ \mathfrak{m}
          \mid \mathfrak{n}\ \mathfrak{m}\ \textbf{:}\ \mathfrak{n}\ \mathfrak{m} \\
\\
\textit{sheet-id}
  \ \mathrel{::=} &
    \ \textit{simple-sheet-name}
    \\ \mid &
    \ \texttt{'} \textit{sheet-name} \texttt{'}
    \\ \mid &
    \ \texttt{[}\ \textit{simple-uri}\ \texttt{]} \textit{simple-sheet-name}
    \\ \mid &
    \ \texttt{'[}\ \textit{uri}\ \texttt{]} \textit{sheet-name} \texttt{'} \\
\\
\xi
  \ \mathrel{::=} & \ \Bigl[\textit{sheet-id}\ \textbf{!}\Bigr]\ \alpha
\end{aligned}

Where \(\textit{uri}\) and \(\textit{sheet-name}\) are printable strings and
characters `'[]\` need escaping with `\`. The corresponding \(\textit{simple-}\)
prefixed versions may only contain alpha-numeric characters and `._~`.

Terms and types share the syntax:

\begin{align}
\text{f}, \text{x}, \sigma, \tau \ \mathrel{::=}
          & \ \textit{literal}
  \\ \mid & \ \text{v}
  \\ \mid & \ \texttt{\\} \text{v}\ \texttt{->}\ \text{x}
  \\ \mid & \ \texttt{*}
  \\ \mid & \ \texttt{(} \text{v}\texttt{:}\ \sigma \texttt{) -> } \tau
  \\ \mid & \ \xi
  \\ \mid & \ \text{f}\ \text{x}
\end{align}

where \(\textit{literal}\) includes signed decimals, `int`, `bool`, `false`,
and `true`. Identifiers are case-insensitive.

When a function type ignores its argument it can be written as
\(\sigma \texttt{ -> } \tau\).

## Semantics

The typing judgements and evaluation rules follow directly the `LambdaPi`
implementation. The only difference is how cell references (and ranges) are
treated.

<!-- FIXME: write down -->

<!-- Footnotes & References -->
  [^1]: Andres Löh, Conor McBride, Wouter Swierstra. [*A Tutorial Implementation of a Dependently Typed Lambda Calculus*](https://dl.acm.org/doi/10.5555/1883634.1883637).

  [recalc-semantics]: ./haddock/Recalc-Semantics.html
