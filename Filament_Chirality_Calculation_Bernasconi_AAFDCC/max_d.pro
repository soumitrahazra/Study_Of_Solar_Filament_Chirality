;******************************************************************************
;***                                                                        ***
    function max_d,array,min_sub,MIN=minv,NAN=nan,DIMENSION=dim
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    MAX_D
;
; PURPOSE:
;    The MAX function returns the value of the largest element of Array. 
;    The type of the result is the same as that of Array.
;
; CALLING SEQUENCE:
;    Result = max_d( Array [, Max_Subscript, DIMENSION=value, MIN=variable,$
;                   /NAN] )
;
; INPUTS:
;    Array = any dimensions
;
; OPTIONAL INPUT PARAMETERS:
;	None
;
; KEYWORD PARAMETERS:
;	None
;
; OUTPUTS:
;    Result = 
;
; OPTIONAL OUTPUT PARAMETERS:
;	None
;
; PROCEDURE:
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
mx=max(array,loc,MIN=minv,NAN=nan)

if (n_elements(dim) eq 1) then begin
  if (dim eq 0) then return,mn
  sz = size(array)
  if (dim gt sz(0)) then $
    message,'DIMENSION parameter bigger than array dimension!'

  case sz(0) of
      1: return,mx
      2: begin
          if (dim eq 1) then return,reform(array((loc mod sz(1)),*))
          return,reform(array(*,loc/sz(1)))
      end
      3: begin
          r = loc mod (sz(1)*sz(2))
          if (dim eq 1) then return,reform(array((r mod sz(1)),*,*))
          if (dim eq 2) then return,reform(array(*,r/sz(1),*))
          return,reform(array(*,*, (loc / (sz(1)*sz(2))) ))
      end
      else : message,'array size illegal or not supported!'
  endcase

endif $
else return,mx
end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: max_d.pro,v $
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.2  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
; Written by Pietro N. Bernasconi JHU/APL:
