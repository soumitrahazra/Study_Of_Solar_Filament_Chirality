;-------------------------------------------------------------
;+
; NAME:
;       DT_TM_FROMYMDS
; PURPOSE:
;       Convert year, month, day, second to a date/time string.
; CALLING SEQUENCE:
;       dt = dt_tm_fromymds(y,m,d,s)
; INPUTS:
;       y,m,d = year, month, day numbers.   in
;       s = second into day.                in
; KEYWORD PARAMETERS:
; OUTPUTS:
;       dt = date/time string.      out
; COMMON BLOCKS:
; NOTES:
;       This is a wrapper that combines simply the two calls ymds2js and
;       dt_tm_fromjs into a single call
;       
;       See also js2ymds, dt_tm_fromjs, dt_tm_tojs, jscheck.
; MODIFICATION HISTORY:
;       Pietro N. BVernasconi JHU/APL March 1, 2004. Combining two routines
;       written by Ray Sterner.
;
; Copyright (C) 2004, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
function dt_tm_fromymds, y, m, d, s, help=hlp

if (n_params(0) lt 4) or keyword_set(hlp) then begin
    print,' Convert to year, month, day, second to "Julian Second".'
    print,' js = dt_tm_fromymds(y,m,d,s)'
    print,'   y,m,d = year, month, day numbers.   in'
    print,'   s = second into day.                in'
    print,'   dt = date/time string.      out'
    print,' '
    print,' See also js2ymds, dt_tm_fromjs, dt_tm_tojs, jscheck.'
    return, -1
endif

return,dt_tm_fromjs(ymds2js( y, m, d, s))

end
