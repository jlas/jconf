(defun number-sequence (from &optional to inc)
  "Return a sequence of numbers from FROM to TO (both inclusive) as a list.
INC is the increment used between numbers in the sequence and defaults to 1.
So, the Nth element of the list is \(+ FROM \(* N INC)) where N counts from
zero.  TO is only included if there is an N for which TO = FROM + N * INC.
If TO is nil or numerically equal to FROM, return \(FROM).
If INC is positive and TO is less than FROM, or INC is negative
and TO is larger than FROM, return nil.
If INC is zero and TO is neither nil nor numerically equal to
FROM, signal an error.

This function is primarily designed for integer arguments.
Nevertheless, FROM, TO and INC can be integer or float.  However,
floating point arithmetic is inexact.  For instance, depending on
the machine, it may quite well happen that
\(number-sequence 0.4 0.6 0.2) returns the one element list \(0.4),
whereas \(number-sequence 0.4 0.8 0.2) returns a list with three
elements.  Thus, if some of the arguments are floats and one wants
to make sure that TO is included, one may have to explicitly write
TO as \(+ FROM \(* N INC)) or use a variable whose value was
computed with this exact expression.  Alternatively, you can,
of course, also replace TO with a slightly larger value
\(or a slightly more negative value if INC is negative)."
  (if (or (not to) (= from to))
      (list from)
    (or inc (setq inc 1))
    (when (zerop inc) (error "The increment can not be zero"))
    (let (seq (n 0) (next from))
      (if (> inc 0)
          (while (<= next to)
            (setq seq (cons next seq)
                  n (1+ n)
                  next (+ from (* n inc))))
        (while (>= next to)
          (setq seq (cons next seq)
                n (1+ n)
                next (+ from (* n inc)))))
      (nreverse seq))))
