====================================================================
Benchmark: YAML-loading and entity-record strategies for besm2-rst
====================================================================

``besm2-rst`` can load its YAML input using either the ``yaml`` egg
(the default) or the ``slibfyaml`` egg, selected with the
``-f``/``--fyaml`` command line option. ``besm2-rst-f`` is a separate
program -- a copy of ``besm2-rst`` refactored to walk slibfyaml's own
handle/tree-based document API directly, rather than going through
either of those two whole-document loaders. ``besm2-rst-e`` and
``besm2-rst-f-e`` are ``besm2-rst`` and ``besm2-rst-f`` again, each
further refactored to decode every entity exactly once into a shared
record (``besm-entities.scm``) instead of re-deriving the same fields
separately in each of the four output backends. This note records a
correctness and performance comparison between all five, produced with
the ``fyaml``, ``treefyaml``, ``entity``, and ``entitytree`` targets,
the ``compare-fyaml``, ``compare-treefyaml``, ``compare-entity``, and
``compare-entitytree`` targets, and the ``benchmark-fyaml`` target,
all added to ``GNUmakefile``.

Throughout this note each program is referred to by a short code:

``yaml``
    ``besm2-rst``, the default: loads via the ``yaml`` egg.
``fyaml``
    ``besm2-rst -f``/``--fyaml``: loads via ``(slibfyaml scheme)``'s
    eager whole-document decode.
``tree``
    ``besm2-rst-f``: walks slibfyaml's handle/tree API directly,
    decoding only the scalars each output backend actually reads.
``entity``
    ``besm2-rst-e``: ``besm2-rst`` (still ``yaml``-egg-loaded by
    default) refactored onto ``besm-entities``' shared record.
``etree``
    ``besm2-rst-f-e``: ``besm2-rst-f`` (still handle/tree-loaded)
    refactored the same way.

Comparing ``entity`` against ``yaml``, and ``etree`` against ``tree``,
isolates the shared-record refactor's own cost from the choice of YAML
loader, since each pair differs only in that one respect.

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
same between ``besm2-rst``'s default output and ``besm2-rst-f``;
``make compare-entity`` and ``make compare-entitytree`` do the same
again for ``besm2-rst-e`` and ``besm2-rst-f-e`` against those same
``yaml``-egg baselines. All pairs, built from the two 2E files in
``test-data/``, are byte-for-byte identical in all four comparisons.
This also held for the larger synthetic files used below (1, 100, 500,
and 1500 entities).

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
``enyon-boase-2e.yaml`` 1, 100, 500, and 1500 times, and runs each of
the five programs against them (fewer times as the file grows, so the
whole sweep stays quick).

.. include:: build/benchmark-fyaml-large.gen.rst

Conclusion
==========

The tables above are generated (by ``benchmark-report.py``, from
``build/benchmark-fyaml.out``) each time the benchmark is rerun, so the
specific figures will drift between runs and machines; read them for
shape, not for the exact numbers quoted here at the time this section
was last written by hand:

- Output is identical across all five programs at every size tested.
- On realistic (single-entity) files all five are close enough to call
  a wash; ``tree`` and ``etree`` in particular pay a fixed per-process
  cost (loading two extra slibfyaml extensions, ``(slibfyaml
  documents)`` and ``(slibfyaml nodes)``, plus explicit document
  construction/teardown) that a single-entity run cannot amortize
  away, so they can measure slightly *slower* than the ``yaml``-egg
  loaders here even though they win decisively once the input is large
  enough to amortize that fixed cost.
- Once the input is large enough for parsing/decoding cost to
  dominate, both slibfyaml-backed loaders (``fyaml``, ``tree``,
  ``etree``) are consistently faster than the ``yaml`` egg (``yaml``,
  ``entity``), and the handle/tree traversal (``tree``, ``etree``) --
  decoding only the scalars each output backend actually reads, rather
  than materializing the whole document into alists up front the way
  ``(slibfyaml scheme)`` does -- is faster still. These percentages
  hold roughly steady as the input grows, since all five
  programs scale linearly with the number of entities: this is a
  modest, constant-factor win rather than one that grows
  disproportionately at scale.
- ``entity`` vs. ``yaml`` and ``etree`` vs. ``tree`` isolate the
  shared-record refactor's own cost: both pairs track each other
  closely at every size, so decoding each entity once into a
  ``besm-entities`` record up front -- rather than re-deriving the
  same fields separately in each of the four output backends -- is
  effectively free; the refactor is a code-sharing win with no
  measurable performance cost.
