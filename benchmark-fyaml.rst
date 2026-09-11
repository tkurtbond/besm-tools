====================================================================
Benchmark: yaml egg vs. slibfyaml egg vs. besm2-rst-f for besm2-rst
====================================================================

``besm2-rst`` can load its YAML input using either the ``yaml`` egg
(the default) or the ``slibfyaml`` egg, selected with the
``-f``/``--fyaml`` command line option. ``besm2-rst-f`` is a separate
program -- a copy of ``besm2-rst`` refactored to walk slibfyaml's own
handle/tree-based document API directly, rather than going through
either of those two whole-document loaders. This note records a
correctness and performance comparison between all three, produced
with the ``fyaml``/``treefyaml``, ``compare-fyaml``/
``compare-treefyaml``, and ``benchmark-fyaml`` targets added to
``GNUmakefile``.

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
``besm2-rst-f`` is built directly on this layer -- entities, and their
stats/attributes/defects/skills, stay node handles until a leaf scalar
is actually needed, at which point it is decoded with a typed accessor
(``node-string-value``/``node-integer-value``/``node-boolean-value``).

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

So the split is: ``(slibfyaml scheme)`` (used by ``besm2-rst
--fyaml``) gives an eager, whole-document decode into ordinary Scheme
data, the same style the ``yaml`` egg (``besm2-rst``'s default) uses;
every other module in the egg -- what ``besm2-rst-f`` uses -- gives a
lazy, handle-based traversal that the caller drives by hand.

Correctness
===========

``make compare-fyaml`` builds every ``besm2-rst`` output variant
(plain, terse, TBL, and the Unicode-MINUS-SIGN variants) twice, once
with each egg, and diffs the pairs; ``make compare-treefyaml`` does the
same between ``besm2-rst``'s default output and ``besm2-rst-f``. All
pairs, built from the two 2E files in ``test-data/``, are byte-for-byte
identical in both comparisons. This also held for the larger synthetic
files used below (1, 100, 500, and 1500 entities).

Small test-data files
======================

The repository's two 2E test-data files (``enyon-boase-2e.yaml`` and
``FV2021-Coleopteran-2e.yaml``) each describe a single entity, run
``$(BENCH_N)`` times per program (``make benchmark-fyaml``, overriding
``BENCH_N`` on the command line to change the repetition count).
Figures below are wall-clock ("real") time per run.

.. include:: build/benchmark-fyaml-small.gen.rst

At this size, process startup dominates.

Synthetic large files
======================

To see whether the gap widens with input size, ``make benchmark-fyaml``
also generates synthetic 2E files (``build/synthetic-N.yaml``, not part
of the repository) by repeating the single entity in
``enyon-boase-2e.yaml`` 1, 100, 500, and 1500 times, and runs each with
plain ``besm2-rst -s`` / ``besm2-rst -s -f`` / ``besm2-rst-f -s``
(fewer times as the file grows, so the whole sweep stays quick).

.. include:: build/benchmark-fyaml-large.gen.rst

Conclusion
==========

The two tables above are generated (by ``benchmark-report.py``, from
``build/benchmark-fyaml.out``) each time the benchmark is rerun, so the
specific figures will drift between runs and machines; read them for
shape, not for the exact numbers quoted here at the time this section
was last written by hand:

- Output is identical across all three programs at every size tested.
- On realistic (single-entity) files the three programs are close
  enough to call a wash; ``besm2-rst-f`` in particular pays a fixed
  per-process cost (loading two extra slibfyaml extensions,
  ``(slibfyaml documents)`` and ``(slibfyaml nodes)``, plus explicit
  document construction/teardown) that a single-entity run cannot
  amortize away, so it can measure slightly *slower* than the other
  two here even though it wins decisively once the input is large
  enough to amortize that fixed cost.
- Once the input is large enough for parsing/decoding cost to
  dominate, both slibfyaml-backed programs are consistently faster
  than the ``yaml`` egg, and ``besm2-rst-f``'s handle/tree traversal
  -- decoding only the scalars this program actually reads, rather
  than materializing the whole document into alists up front the way
  ``(slibfyaml scheme)`` does -- is faster still. Both percentages
  hold roughly steady as the input grows, since all three programs
  scale linearly with the number of entities: this is a modest,
  constant-factor win rather than one that grows disproportionately
  at scale.
