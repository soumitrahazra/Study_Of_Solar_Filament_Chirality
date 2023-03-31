;******************************************************************************
;***                                                                        ***
      function spine_center, spine, LENGTH = length
;***                                                                        ***
;******************************************************************************
;+
; PROJECT: Automated Solar Filament Detection and Characterization
;
; NAME:
;    SPINE_CENTER
;
; PURPOSE:
;    Finds the (x,y) coordinates of the center point of a filament spine
;
; CALLING SEQUENCE:
;    center_coords = spine_center(spine [, LENGTH = length])
;
; INPUTS:
;    spine = array(2,N_spine_pnts): array with the coordinates of the spine
;                  points. The first dimention has two elements. The 0th
;                  element is the X cooridinate and the 1st element is the Y
;                  coordinate of the spine point. The second dimention
;                  identifies which point on the spine it is.
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    LENGTH = float: total lenght of the spine in pixels
;
; OUTPUTS:
;    center_coords = intarr(2): array with the X (0th element) and Y (1st
;                  element) coordinates (in pixels) of the location of the
;                  center point along the spine
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES USED:
;    distance
;
; PROCEDURE:
;    Finds the position along the spine that has the same lenght on both sides.
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
sz = size(spine,/dim)

spine_ln = fltarr(sz(1))

for i=1,sz(1)-1 do spine_ln(i) = spine_ln(i-1) + $
  distance(spine(0,i-1),spine(1,i-1),spine(0,i),spine(1,i))

length  = spine_ln(sz(1)-1)
midlen  = length/2.

w=where(spine_ln-midlen gt 0)

ratio = (midlen - spine_ln(w(0)-1)) / (spine_ln(w(0))-spine_ln(w(0)-1))

out = round(spine(*,w(0)-1) + (spine(*,w(0))-spine(*,w(0)-1)) * ratio)

return,out

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: spine_center.pro,v $
; Revision 3.1  2010/03/10 19:20:05  bernapn1
; Added to version control
;
;
; Written by Pietro N. Bernasconi JHU/APL
