## Release v0.17.0

* Port to ppx_hardcaml.
* Add `Counter_div_mod`, a counter module with quotient and remainder outputs.
* Add `Datapath_register`, a generic module with arbitrary interfaces with AXI Stream style handshake.
* Add iterative non-restoring `Divider` module with support for signed and unsigned operands.
* Fix for `Fast_fifo` in non-cut-through mode when read and writing on same cycle.
* Add optional pipelined enable signal to `pipelined_priority_tree_select`.
* Expose used level in `Stack` implementation.  Also assert read and write bit widths are equal.

## Release v0.16.0

* Fix bug in counterpart tap generation in LFSR implementation.
* Add sexp, enumerate deriving annotations to types in Cordic allowing them to
  be used in command line parameter specifications etc.
* Add new `Stack` module
* [Fast_fifo] documentation fixes.
