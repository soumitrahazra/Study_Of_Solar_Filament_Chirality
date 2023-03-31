;******************************************************************************
;***                                                                        ***
	function sign_pb,in,ONE4ZERO=one40
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;	SIGN_PB
;
; PURPOSE:
;	This function returns the signum of the imput variable. Returns 0
;	if input value is 0.
;
; CALLING SEQUENCE:
;	result = sign_pb(variable, [/ONE4ZERO])
;
; INPUTS:
;	variable = any type: variable of which the signum has to be
;			     calculated
;
; OPTIONAL INPUT PARAMETERS:
;	None
;
; KEYWORD PARAMETERS:
;	ONE4ZERO = Flag : If set then returns a 1 if the value is zero
;
; OUTPUTS:
;	result = integer(same size as variable): returns 1 if the value of
;			the imput variable is positive, 0 if it is zero,
;			-1 otherwise.
;
; OPTIONAL OUTPUT PARAMETERS:
;	None
;
; PROCEDURE:
;	result = variable / abs(variable)
;
; MODIFICATION HISTORY:
;	Written by Pietro N. Bernasconi (APL), May 26, 2000
;	-> Mar 26, 01 by PNB: added keyword ONE4ZERO
;-
;******************************************************************************
if not(keyword_set(one40)) then one40=0

sz=size(in)

if (sz(0) eq 0) then begin
  if (in eq 0) then begin
    if one40 then return,1 else return,0
  endif else return,in/abs(in)
endif else begin
  w=where(in eq 0)
  szw=size(w)
  if (szw(0) ne 0) then in(w)=1
  out=in/abs(in)
  if (szw(0) ne 0) then out(w)=0
endelse

return,out
end
;******************************************************************************
