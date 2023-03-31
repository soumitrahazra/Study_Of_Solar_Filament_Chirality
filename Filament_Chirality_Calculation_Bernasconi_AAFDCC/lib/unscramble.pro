;******************************************************************************
;***									    ***
	function unscramble,input,zero
;***									    ***
;******************************************************************************
;+
; NAME:
;	UNSCRAMBLE
;
; PURPOSE:
;	Unscrambles an arbitrary object after a Fourier transformation such
;	that frequency 0 is in the center and not in the lower left edge.
;
; CALLING SEQUENCE:
;	Result = unscramble(Array [,Zero])
;
; ARGUMENTS:
;	Array = 1- or 2-dim, any type except string
;
; OUTPUTS:
;	Result = unscrambled array
;	Zero   = position of 0 frequency, integer (optional)
;
; MODIFICATION HISTORY:
;	Written by Pietro N. Bernasconi: 8 Nov 1993
;	-> 16 Mar 1995: Also for bidimensional arrays (images), test
;			to check the input's dimention
;-
;******************************************************************************

;--- Determination of array size, check for 1/2-dim, creation of output array
array=float(reform(input))
out=array
sz=size(out)
if (sz(0) gt 2) then message,'Wrong dimension of input array.'

;--- Determination of the zero frequency position
if (sz(0) eq 2) then zero=[sz(1)/2,sz(2)/2] else zero=sz(1)/2

;--- Performs unscramble
if (sz(0) eq 1) then begin
  ;-- unscramble for 1-dim array ----
  if (sz(1)/2.-sz(1)/2 eq 0.) then begin
    ; unscramble for array with even number of elements
    out(0:zero-1)=array(zero:*)
    out(zero:*)=array(0:zero-1)
  endif else begin
    ; unscramble for array with odd number of elements
    out(0:zero-1)=array(zero+1:*)
    out(zero:*)=array(0:zero)
  endelse
endif else begin
  ;-- unscramble for 2-dim array ----
  if (sz(1)/2.-sz(1)/2 eq 0.) then begin
    if (sz(2)/2.-sz(2)/2 eq 0.) then begin
      ;-- unscramble for array with even elements in  x & y direc
;;print,' even even'
      out(0:zero(0)-1,0:zero(1)-1)=array(zero(0):*,zero(1):*)
      out(zero(0):*,zero(1):*)=array(0:zero(0)-1,0:zero(1)-1)
      out(0:zero(0)-1,zero(1):*)=array(zero(0):*,0:zero(1)-1)
      out(zero(0):*,0:zero(1)-1)=array(0:zero(0)-1,zero(1):*)
    endif else begin
      ;-- unscramble for array with even elements in  x and odd in y direc
;;print,' even odd'
      out(0:zero(0)-1,0:zero(1)-1)=array(zero(0):*,zero(1)+1:*)
      out(zero(0):*,zero(1):*)=array(0:zero(0)-1,0:zero(1))     
      out(0:zero(0)-1,zero(1):*)=array(zero(0):*,0:zero(1))
      out(zero(0):*,0:zero(1)-1)=array(0:zero(0)-1,zero(1)+1:*)
    endelse
  endif else begin
    if (sz(2)/2.-sz(2)/2 eq 0.) then begin
      ;-- unscramble for array with odd elements in  x and even in y direc
;;print,' odd even'
      out(0:zero(0)-1,0:zero(1)-1)=array(zero(0)+1:*,zero(1):*)
      out(zero(0):*,zero(1):*)=array(0:zero(0),0:zero(1)-1)
      out(0:zero(0)-1,zero(1):*)=array(zero(0)+1:*,0:zero(1)-1)
      out(zero(0):*,0:zero(1)-1)=array(0:zero(0),zero(1):*)
    endif else begin
      ;-- unscramble for array with odd elements in  x & y direc
;;print,' odd odd'
      out(0:zero(0)-1,0:zero(1)-1)=array(zero(0)+1:*,zero(1)+1:*)
      out(zero(0):*,zero(1):*)=array(0:zero(0),0:zero(1))
      out(0:zero(0)-1,zero(1):*)=array(zero(0)+1:*,0:zero(1))
      out(zero(0):*,0:zero(1)-1)=array(0:zero(0),zero(1)+1:*)
    endelse
  endelse
endelse

return,out

end
;******************************************************************************
