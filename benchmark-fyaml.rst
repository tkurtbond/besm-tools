=======================================================
Benchmark: yaml egg vs. slibfyaml egg for ``besm2-rst``
=======================================================

``besm2-rst`` can load its YAML input using either the ``yaml`` egg
(the default) or the ``slibfyaml`` egg, selected with the
``-f``/``--fyaml`` command line option. This note records a
correctness and performance comparison between the two, produced with
the ``fyaml``, ``compare-fyaml``, and ``benchmark-fyaml`` targets added
to ``GNUmakefile``.

``(slibfyaml scheme)`` vs. other slibfyaml modules
===================================================

The ``slibfyaml`` egg is a binding to ``libfyaml`` built around a
handle/tree-based document API: parsing gives you opaque document/node
handles, and modules ``(slibfyaml thin)``, ``(slibfyaml nodes)``,
``(slibfyaml documents)``, and ``(slibfyaml documents streams)``
expose that API directly. Using them means walking the tree yourself
with accessor and iterator procedures (``node-kind``,
``node-iterate-pairs``/``-items``, ``node-boolean?``/``node-integer?``/
etc.), and being responsible for handle lifetime -- destroying
documents and streams yourself. This mirrors the design of the
author's Ada ``alibfyaml`` binding, and lets you decode an arbitrary
subtree (not just a whole document root) or pull documents one at a
time from a stream, without ever converting parts you do not need.

``(slibfyaml scheme)`` is different: it is a pure convenience layer,
built on top of the handle-based core with no new FFI, that
**eagerly decodes a whole document into plain native Scheme data in
one pass**. Its ``load-port``/``load-file``/``load-string`` (and the
``node->scheme`` helper they are built from) recursively turn mappings
into alists, sequences into lists, and scalars into typed values
(boolean/integer/float/string/null), producing one flat Scheme value
per document -- the same "decode everything up front" style as the
``yaml``/``libyaml`` eggs. That is why it is the module ``besm2-rst``'s
``-f``/``--fyaml`` option actually calls (via ``fyaml-load-entities``,
wrapping ``load-port``): it is a drop-in replacement for the ``yaml``
egg's loader, not a rewrite to the handle-based style.

So the split is: ``(slibfyaml scheme)`` gives an eager, whole-document
decode into ordinary Scheme data, while every other module in the egg
gives a lazy, handle-based traversal that the caller drives by hand.

Correctness
===========

``make compare-fyaml`` builds every ``besm2-rst`` output variant
(plain, terse, TBL, and the Unicode-MINUS-SIGN variants) twice, once
with each egg, and diffs the pairs. All ten pairs, built from the two
2E files in ``test-data/``, are byte-for-byte identical. This also
held for the larger synthetic files used below (1, 100, 500, and 1500
entities).

Small test-data files
======================

The repository's two 2E test-data files (``enyon-boase-2e.yaml`` and
``FV2021-Coleopteran-2e.yaml``) each describe a single entity. Each
was run 200 times per egg (``make BENCH_N=200 benchmark-fyaml``);
figures below are wall-clock ("real") time per run.

+--------------------------+------+----------+------------+----------------+
|Test file                 |Runs  |yaml egg  |slibfyaml   |slibfyaml vs.   |
|                          |      |(per run) |egg         |yaml            |
|                          |      |          |(per run)   |                |
+==========================+======+==========+============+================+
|enyon-boase-2e.yaml       |200   |22.7 ms   |22.8 ms     |~0% (noise)     |
+--------------------------+------+----------+------------+----------------+
|FV2021-Coleopteran-2e.yaml|200   |21.3 ms   |20.8 ms     |~2.7% faster    |
+--------------------------+------+----------+------------+----------------+

At this size, process startup dominates and the two eggs are not
meaningfully distinguishable.

Synthetic large files
======================

To see whether the gap widens with input size, synthetic 2E files
were built by repeating the single entity in ``enyon-boase-2e.yaml``
100, 500, and 1500 times (these files are not part of the repository;
they were generated only to run this benchmark). Each was run several
times per egg with plain ``besm2-rst -s`` / ``besm2-rst -s -f``.

+--------+----+-----------+-----------+----------+-----------+-------------+
|Entities|Runs|yaml egg   |slibfyaml  |yaml egg  |slibfyaml  |slibfyaml    |
|        |    |(s / run)  |egg        |(ms /     |egg (ms /  |faster by    |
|        |    |           |(s / run)  |entity)   |entity)    |             |
+========+====+===========+===========+==========+===========+=============+
|1       |200 |0.022      |0.022      |22.0      |22.0       |~0% (noise)  |
+--------+----+-----------+-----------+----------+-----------+-------------+
|100     |10  |0.760      |0.711      |7.60      |7.11       |6.4%         |
+--------+----+-----------+-----------+----------+-----------+-------------+
|500     |5   |3.810      |3.530      |7.62      |7.06       |7.3%         |
+--------+----+-----------+-----------+----------+-----------+-------------+
|1500    |3   |11.420     |10.563     |7.61      |7.04       |7.5%         |
+--------+----+-----------+-----------+----------+-----------+-------------+

Conclusion
==========

- Output is identical between the two eggs at every size tested.
- On realistic (single-entity) files the two eggs perform the same;
  differences are within measurement noise.
- Once the input is large enough for parsing cost to dominate,
  ``slibfyaml`` is consistently **6-8% faster** than the ``yaml``
  egg, and that percentage holds steady as the input grows: both
  eggs scale linearly with the number of entities (roughly 7.6
  ms/entity for ``yaml`` vs. 7.1 ms/entity for ``slibfyaml``). The
  gap is a modest, constant-factor win rather than one that grows
  disproportionately at scale.
