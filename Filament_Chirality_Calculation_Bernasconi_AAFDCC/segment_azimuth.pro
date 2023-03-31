;******************************************************************************
;***                                                                        ***
     function segment_azimuth, x1, y1, x2, y2
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    SEGMENT_AZIMUTH
;
; PURPOSE:
;    calculates the AZIMUTH angle in a 2D space of a segment with
;    respect to the horizontal right direction, normalized at +/- 180 degrees.
;    So pointing left is 0 deg, pointing up is +90 deg, pointing right is +(-)
;    180 deg, and pointign down is -90 deg.
;
; CALLING SEQUENCE:
;    azimuth = segment_azimuth(x1, y1, x2, y2)
;
; INPUTS:
;    x1,y1 = float or int: coordinates of starting point
;    x2,y2 = float or int: coordinates of second point
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    None
;
; OUTPUTS:
;    aziumth = double: the azimuth angle of the segment that starts ad (x1,y1)
;                      and ends at (x2,y2)
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES USED:
;    None
;
; COMMON BLOCKS:
; PROCEDURE:
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 4) then $
  message,'Syntax: azimuth = segment_azimuth(x1,y1,x2,y2)'

numer = double(y2-y1)
denom = double(x2-x1)

if (denom eq 0) then begin
    az = !pi/2. * sign_pb(numer)
endif else if (numer eq 0) then begin
    az = 0
    if (x2 lt x1) then az = !pi
endif else begin
    az = atan(numer/denom)
    ; put it in the correct quadrant
    if (denom lt 0) then az = az + sign_pb(numer)*!PI
endelse

return,az

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: segment_azimuth.pro,v $
; Revision 3.1  2010/10/27 19:40:56  bernapn1
; New
;
;
; Written by Pietro N. Bernasconi JHU/APL
