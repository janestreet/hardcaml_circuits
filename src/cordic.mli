(** Coordinate rotation digital computer.

    CORDIC is an iterative shift-add algorithm for computing trig and hyperbolic functions
    like sin, cosh, atan etc.

    Generally it requires 3 adders, 2 barrel shifters and a fairly small lookup table.

    {b Gain}

    The [x] and [y] outputs from the CORDIC are (usually) scaled by the CORDIC gain. The
    exact value of the gain depends on the number of iterations performed, but tends
    towards about 1.647.

    For some functions the gain can be adjusted for by altering input constants. Where it
    cannot, and it's a problem, it should be removed with a constant multiplier.

    {b Hyperbolic mode}

    In hyperbolic mode the standard iteration scheme needs to be adjusted. Generally the
    iterations run from [0,1,...,(iters-1)]. In hyperbolic mode the iterations are
    [1,2,..,4,4,...13,13,...]. That is they start at 1 and iterations [3k+1] (starting
    with k=4, then 13, 40, 121 etc) are repeated.

    The hardware designs require a fixed number of iterations regardless of the mode.
    Therefore the number of iterations specified is exactly the number run regardless of
    mode (indices are modified internally in hyperbolic mode). Some care might need to be
    taken to not stop processing in hyperbolic mode on one of the double iterations to
    ensure convergence. *)

include Cordic_intf.Cordic (** @inline *)
