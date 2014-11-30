%opcode(id3, A * (B * C), A * (B * C)).
%opcode(id4, A * (B * (C * D)), A * (B * (C * D))).
%opcode(idL3, (A * B) * C, (A * B) * C).
%opcode(idM4, (A * B) * (C * D), (A * B) * (C * D)).
opcode(l, A * (B * C), (A * B) * C).
opcode(r, (A * B) * C, A * (B * C)).
opcode(w, A * (B * C), B * (A * C)).
opcode(z, A * (B * (C * D)), A * (C * (B * D))).

:- dynamic found/1.
found([z, w, z]). % wzw
found([l, z, r, w]). %wlzr
found([z, r, w, l]). %wlzr
found([r, w, l, z]). %wlzr

pair(Op1 ,Op2, TyL, TyR) :- opcode(Op1, TyL, Ty), opcode(Op2, Ty, TyR).

sequence(2, [Op1,Op2], TyL, TyR) :- pair(Op1 ,Op2, TyL, TyR).
sequence(N, [Op|Ops], TyL, TyR) :- N > 2, Nm1 is N - 1, opcode(Op, TyL, Ty), sequence(Nm1, Ops, Ty, TyR).

sublist(L, Seq) :- prefix(L, Seq).
sublist(L, [_|Seq]) :- sublist(L, Seq).

sublist_any([L|_], Seq) :- sublist(L, Seq).
sublist_any([_|Ls], Seq) :- sublist_any(Ls, Seq).

reduced(Seq) :- findall(L, found(L), Ls), sublist_any(Ls, Seq), !, fail.
reduced(_).

seqopn(N, Seq, Op) :- sequence(N, Seq, STyL, STyR), reduced(Seq), opcode(Op, TyL, TyR), reduced(Seq), subsumes_term(STyL, TyL), STyL = TyL, subsumes_term(STyR, TyR), asserta(found(Seq)).

seqop(N, Seq, Op) :- N >= 2, Nm1 is N - 1, seqop(Nm1, Seq, Op).
seqop(N, Seq, Op) :- seqopn(N, Seq, Op).

idseqn(N, Seq) :- sequence(N, Seq, TyL, TyR), reduced(Seq), TyL == TyR, asserta(found(Seq)).

idseq(N, Seq) :- N >= 2, Nm1 is N - 1, idseq(Nm1, Seq).
idseq(N, Seq) :- idseqn(N, Seq).

seqseqn(N, Seq1, Seq2) :- sequence(N, Seq1, S1TyL, S1TyR), reduced(Seq1), sequence(N, Seq2, S2TyL, S2TyR), dif(Seq1, Seq2), reduced(Seq1), subsumes_term(S1TyL, S2TyL), S1TyL = S2TyL, subsumes_term(S1TyR, S2TyR), asserta(found(Seq1)).

seqseq(N, Seq1, Seq2) :- N >= 2, Nm1 is N - 1, seqseq(Nm1, Seq1, Seq2).
seqseq(N, Seq1, Seq2) :- seqseqn(N, Seq1, Seq2).
