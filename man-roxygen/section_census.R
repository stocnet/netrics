#' @section Dyad census:
#'   The dyad census counts each pair of nodes by the ties between them,
#'   using the MAN labels of Holland and Leinhardt (1970):
#'   - `Mutual`: a tie in both directions
#'   - `Asymmetric`: a tie in one direction only
#'   - `Null`: no tie
#'
#'   In an undirected or two-mode network every tie is mutual,
#'   so only `Mutual` and `Null` are reported.
#' @section Triad census:
#'   The triad census counts each set of three nodes by the ties among them,
#'   using the labels of Davis and Leinhardt (1967) and Holland and Leinhardt
#'   (1970).
#'   The three digits give the number of mutual, asymmetric, and null dyads
#'   in the triad, and a letter tells apart triads with the same digits:
#'   D(own), U(p), C(yclic), or T(ransitive).
#'   - `003`: no ties
#'   - `012`: one asymmetric tie, A->B
#'   - `102`: one mutual tie, A<->B
#'   - `021D`: A<-B->C, one node sending to the two others
#'   - `021U`: A->B<-C, one node receiving from the two others
#'   - `021C`: A->B->C, a chain
#'   - `111D`: A<->B<-C
#'   - `111U`: A<->B->C
#'   - `030T`: A->B<-C and A->C, a transitive triad
#'   - `030C`: A<-B<-C and A->C, a cycle
#'   - `201`: A<->B<->C, with A and C not tied
#'   - `120D`: A<-B->C and A<->C
#'   - `120U`: A->B<-C and A<->C
#'   - `120C`: A->B->C and A<->C
#'   - `210`: A->B<->C and A<->C
#'   - `300`: A<->B<->C and A<->C, all ties mutual
#'
#'   In an undirected network every tie is read as mutual,
#'   so only `003`, `102`, `201`, and `300` can occur.
#' @section Tetrad census:
#'   The tetrad census counts each set of four nodes by the ties among them.
#'   Ties are read as undirected. Each label gives a letter whose shape
#'   recalls the motif, and the number of nodes:
#'   - `E4` (co-K4): no ties
#'   - `I4` (co-diamond): one tie
#'   - `H4` (co-C4): two ties that share no node
#'   - `L4` (co-paw): two ties that share a node, and an isolate
#'   - `D4` (co-claw): a triangle, and an isolate
#'   - `U4` (P4): three ties in a line
#'   - `Y4` (claw): three ties from one node
#'   - `P4` (paw): a triangle, with one more tie to the fourth node
#'   - `C4` (C4): four ties in a cycle
#'   - `Z4` (diamond): a cycle with one diagonal
#'   - `X4` (K4): all six ties
#'
#'   See also \href{https://www.graphclasses.org/smallgraphs.html}{graph classes}
#'   and McMillan and Felmlee (2020).
#' @section Signed networks:
#'   Where the network is signed, dyads and triads are split by the signs of
#'   their ties.
#'   Each label is followed by the signs of the motif's ties, e.g. `300_++-`
#'   for a triangle with one negative tie, or `Mutual_+-` for a mutual dyad
#'   whose two ties differ in sign.
#'   The ties are listed in a fixed order over the motif's positions
#'   (1-2, 1-3, 2-1, 2-3, 3-1, 3-2 in a directed triad;
#'   1-2, 1-3, 2-3 in an undirected one), skipping the pairs that are not
#'   tied, with the positions ordered so that ties, and then positive ties,
#'   come as early as they can.
#'   A pair holding both a positive and a negative tie counts as negative.
#'   The tetrad census sets signs aside.
#'
#'   Only the motifs that occur are reported.
#'   Summing the variants of a motif gives the unsigned census.
#'   Use [manynet::to_unsigned()] first for a coarser census.
#' @section Multiplex and multilevel networks:
#'   Where the network holds several one-mode layers, dyads and triads are
#'   split by the layers of their ties.
#'   Each layer is given a letter, the first letter of its name where these
#'   differ, and the letters are kept in the `"layers"` attribute of the result.
#'   Each tie lists the letters of the layers that carry it, separated from the
#'   next tie by a `.`, in the order described for signed networks.
#'   A lowercase letter marks a positive tie and an uppercase letter a
#'   negative one, so `201_f.ft` is a two-path, one of whose ties is in the
#'   `friends` layer, and the other in both the `friends` and `tasks` layers.
#'   Only the motifs that occur are reported.
#'   Summing the variants of a motif gives the census of all layers together.
#'   Use [manynet::to_uniplex()] first for a census of one layer.
#'   The tetrad census counts every tie whatever its layer.
#'
#'   Where the network holds exactly one one-mode layer and one two-mode
#'   layer, such as `fict_marvel`, the triad census is instead the
#'   multilevel census of Hollway et al. (2017), over triads of two nodes of
#'   the first mode and one of the second.
#'   Its ten motifs are labelled by two digits: first, how many of the two
#'   first-mode nodes are tied to the second-mode node (2, 1, or 0);
#'   second, the tie between the two first-mode nodes
#'   (2 mutual, 1 asymmetric, 0 none):
#'   `22`, `21`, `20`, `12`, `11D`, `11U`, `10`, `02`, `01`, `00`.
#'   So `21` counts pairs tied in one direction that share an affiliation.
#'   Where only one of the pair is affiliated and the tie is asymmetric,
#'   `11D` has the affiliated node sending the tie, and `11U` receiving it.
#'   This census sets weights and signs aside, and takes no `attribute`.
#'   Passing the one-mode and two-mode networks separately still works,
#'   but is deprecated.
#'   Other combinations of one-mode and two-mode layers are not yet available.
#' @section Attributes:
#'   Where an `attribute` is given, each motif is further split by how its
#'   nodes compare on that attribute, after any signs or layers.
#'   The comparison is given for each pair of positions in turn
#'   (1-2 in a dyad; 1-2, 1-3, 2-3 in a triad;
#'   1-2, 1-3, 1-4, 2-3, 2-4, 3-4 in a tetrad), using the same order of
#'   positions as the ties.
#'   For a categorical attribute (character, factor, or logical),
#'   `=` marks a pair that shares a category and `!` a pair that does not,
#'   so `021D_!!=` is a node sending ties to two nodes that share a category
#'   other than its own.
#'   Missing values are a category of their own.
#'   For an ordered attribute (numeric, or an ordered factor),
#'   `<`, `=`, and `>` mark whether the first of the pair is lower than,
#'   equal to, or higher than the second, so `021D_>><` is a node sending ties
#'   to two nodes with lower values than its own.
#'   Missing values cannot be compared, so are not allowed.
#'   A numeric attribute that codes categories should be passed through
#'   `as.factor()` first.
#'
#'   Since only these comparisons are kept, an attribute may have any number
#'   of categories or values.
#'   Only the motifs that occur are reported, and summing the variants of a
#'   motif gives the census without the attribute.
#'   To see which categories take part in which motifs, take a node census
#'   and summarise its rows by the attribute.
