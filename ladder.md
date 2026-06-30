Let $I = \{ (a, b) \mid a \in Int, b \in Int, a <= b \}$.
A Multisegment is a mulitset of elements in $I$.

For $\alpha \in Multisegment$,
let $\alpha \in Ladder \iff << \in Linord(\alpha)$
where $(a1, b1) << (a2, b2) \iff a1 < a2 \land b1 < b2$.

For $MS \in Multisegment, s \in MS$,
let $depth(s, MS) = max \{ |l| \mid l \in Ladder, l \subseteq MS, s = min l \} - 1$.

Lemma:
For $s1 \in MS, s2 \in MS, s1 << s2$,
follows $depth(s1, MS) < depth(s2, MS)$.
