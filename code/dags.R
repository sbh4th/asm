# total effect
\begin{tikzpicture}[shorten > = 1pt, line width=1pt]
\tikzstyle{every node} = [rectangle, fill=white, draw=none]
\node (x) at (-1,0) [align=center] {$X$};
\node (t) at  (2,0) [align=center] {Policy ($T$)};
\node (y) at  (5.5,0) [align=center] {BP ($Y$)};
\foreach \from/\to in {x/t, t/y}
  \draw [->] (\from) -- (\to);
\foreach \from/\to in {t/y}
  \draw [->] (\from) -- (\to);
  \draw [->] (x.north) to [bend left=35] (y.north);
\end{tikzpicture}

# CDE with unmeasured
\begin{tikzpicture}[shorten > = 1pt, line width=1pt]
\tikzstyle{every node} = [rectangle, fill=white, draw=none]
\node (x) at (-1,0) [align=center] {$X$};
\node (t) at  (1,0) [align=center] {Policy ($T$)};
\node (mp) at  (3.5,1.2) [align=center] {$PM_{2.5}$\\($M_{1}$)};
\node (mt) at (3.5,-1.2) [align=left] {Temp\\{$(M_{2})$}};
\node (y) at  (5.5,0) [align=center] {BP ($Y$)};
\node (w) at (1.5,-1.5) [align=center] {{$W$}};
\node (u) at (5, -1.5) [align=center] {\textcolor{gray!30}{$U$}};
\foreach \from/\to in {x/t, t/mp, t/mt, mt/y, t/mp, t/y, w/mp, w/mt, mp/y}
  \draw [->] (\from) -- (\to);
\draw [->] (x.north) to [bend left=60] (y.north);
\draw [->] (x.north east) to [bend left=25] (mp.west);
\draw [->] (x.south) to [bend right=45] (mt);
\draw [->] (w.south) to [bend right=45] (y);
\foreach \from/\to in {u/y, u/mt, u/mp}
  \draw [->, color=gray!30] (\from) -- (\to);
\draw [->, color=gray!30] (u.south) to [bend left=45] (t.south);
\end{tikzpicture}

# CDE

\begin{tikzpicture}[shorten > = 1pt, line width=1pt]
\tikzstyle{every node} = [rectangle, fill=white, draw=none]
\node (x) at (-1,0) [align=center] {$X$};
\node (t) at  (1,0) [align=center] {Policy ($T$)};
\node (mp) at  (3.5,1.2) [align=center] {$PM_{2.5}$\\($M_{1}$)};
\node (mt) at (3.5,-1.2) [align=left] {Temp\\{$(M_{2})$}};
\node (y) at  (5.5,0) [align=center] {BP ($Y$)};
\node (w) at (1.5,-1.5) [align=center] {{$W$}};
% \node (u) at (4.5, -1.5) [align=center] {\textcolor{gray!30}{$U$}};
\foreach \from/\to in {x/t, t/mp, t/mt, mt/y, t/mp, t/y, w/mp, w/mt}
  \draw [->] (\from) -- (\to);
\draw [->] (x.north) to [bend left=60] (y.north);
\draw [->] (x.north east) to [bend left=25] (mp.west);
\draw [->] (x.south) to [bend right=45] (mt);
\draw [->] (w.south) to [bend right=45] (y);
\end{tikzpicture}

# dag with CDE shown
\begin{tikzpicture}[shorten > = 1pt, line width=1pt]
\tikzstyle{every node} = [rectangle, fill=white, draw=none]
\node (x) at (-1,0) [align=center] {$X$};
\node (t) at  (1,0) [align=center] {Policy ($T$)};
\node (mp) at  (3.5,1.2) [align=center] {$PM_{2.5}$\\($M_{1}$)};
\node (mt) at (3.5,-1.2) [align=left] {Temp\\{$(M_{2})$}};
\node (y) at  (5.5,0) [align=center] {BP ($Y$)};
\node (w) at (1.5,-1.5) [align=center] {{$W$}};
% \node (u) at (4.5, -1.5) [align=center] {\textcolor{gray!30}{$U$}};
\foreach \from/\to in {x/t, mt/y, w/mp, w/mt}
  \draw [->, color=gray!70] (\from) -- (\to);
\foreach \from/\to in {t/mt, t/mp}
  \draw [->, color=red] (\from) -- (\to);
\foreach \from/\to in {t/y}
  \draw [->, color=red!50!blue!50] (\from) -- (\to);
\draw [->, color=gray!70] (x.north) to [bend left=60] (y.north);
\draw [->, color=gray!70] (x.north east) to [bend left=25] (mp.west);
\draw [->, color=gray!70] (x.south) to [bend right=45] (mt);
\draw [->, color=gray!70] (w.south) to [bend right=45] (y);
\end{tikzpicture}

# DAG for mediators
\begin{tikzpicture}[shorten > = 1pt, line width=1pt]
\tikzstyle{every node} = [rectangle, fill=white, draw=none]
\node (x) at (-1,0) [align=center] {$X$};
\node (t) at  (1,0) [align=center] {Policy ($T$)};
\node (mp) at  (3.5,1.2) [align=center] {$PM_{2.5}$\\($M_{1}$)};
\node (mt) at (3.5,-1.2) [align=left] {Temp\\{$(M_{2})$}};
\node (y) at  (5.5,0) [align=center] {BP ($Y$)};
\node (w) at (1.5,-1.5) [align=center] {{$W$}};
% \node (u) at (4.5, -1.5) [align=center] {\textcolor{gray!30}{$U$}};
\foreach \from/\to in {x/t, mt/y, w/mp, w/mt, mp/y, t/y}
  \draw [->, color=gray!70] (\from) -- (\to);
\foreach \from/\to in {t/mt, t/mp}
  \draw [->, color=red] (\from) -- (\to);
\draw [->, color=gray!70] (x.north) to [bend left=60] (y.north);
\draw [->, color=gray!70] (x.north east) to [bend left=25] (mp.west);
\draw [->, color=gray!70] (x.south) to [bend right=45] (mt);
\draw [->, color=gray!70] (w.south) to [bend right=45] (y);
\end{tikzpicture}