;-------------------------------------------------------------
;+
; NAME:
;       HMS2SEC
; PURPOSE:
;       From Hour, Minutes and Seconds compute number of seconds.
;
; CALLING SEQUENCE:
;       secs = hms2sec(h,m,s)
; INPUTS:
;       h = hour of the day     (0 to 23).       in
;       m = minures in the hour (0 to 59).       in
;       s = seconds in hour     (0 to 59).       in
; KEYWORD PARAMETERS:
; OUTPUTS:
;       secs = number of seconds in the day.   out
; NOTES:
;       If one input parameter is negative it will be automatically
;       converted to positive. Also if the value is above the allowed
;       range, it will be resetted to the highest allowable value.
;
; MODIFICATION HISTORY:
;       Pietro N. Bernasconi --- Created.
;       Johns Hopkins University Applied Physics Laboratory.
;
; Copyright (C) 2004, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
	function hms2sec, ih, im, is,  help=hlp
 
	if (n_params(0) LT 3) or keyword_set(hlp) then begin
	  print,' From Hour, Minutes and Seconds compute number of seconds.'
	  print,' secs = hms2sec(h,m,s)'
	  print,'   h = hour of the day     (0 to 23).       in'
	  print,'   m = minures in the hour (0 to 59).       in'
	  print,'   s = seconds in hour     (0 to 59).       in'
	  print,'   secs = number of seconds in the day.     out'
	  return, -1
	endif
 
	h = long(abs(ih))
	m = long(abs(im))
	s = long(abs(is))

        if (h gt 23) then h=23d0
        if (m gt 59) then m=59d0
        if (s gt 59) then s=59d0

        return,s+(m+h*60)*60

	end
