;******************************************************************************
;***                                                                        ***
      function barb_direction, barb, spine, REQ_SLOPE=req_slope
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    BARB_CHIRALITY
;
; PURPOSE:
;    Function to calculate the direction of a barb: that means wheter the
;    barb is right bearing or left bearing. We have to calculate
;    the angle between barb and the spine.  We do this by translating the
;    barb and its closest spine segment to the origin, then determining
;    how much we need to rotate the barb to match the spine.
;
; CALLING SEQUENCE:
;    direction = barb_direction(barb, spine [, REQ_SLOPE=reqslope])
;
; INPUTS:
;    barb = intarr(2,2): vertices of the barb. first  dim = x,y coords
;                                              second dim = vertices
;    spine = fltarr(2, nVerts): coordinates of the spine vertices
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    REQ_SLOPE = float: minimum slope in degrees of barb spine for deciding
;                       the left/right. Default = 0.6 deg
;
; OUTPUTS:
;    direction = int: -1 = Right-bearing barb, 1 = Left-bearing barb,
;                      0 = unclassyfyable
;
; OPTIONAL OUTPUT PARAMETERS:
;	None
;
; PROCEDURES USED:
;    segment_parms,  dist_to_line, diatance
;
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 2) then message,'Input parameters missing!'
x=0 & y=1

if (n_elements(req_slope) le 0) then req_slope = 0.6
reqslope = req_slope*!pi/180.

;--- Computes the spine segment parameters if not provided or
;--- provided incorrectly:
dims=size(spine,/dim)
nvert = dims[1]

;--- calculates the closest vertices:
dv = fltarr(nvert)
for i=0,nvert-1 do begin
    dv[i] = distance(barb[x,1],barb[y,1],spine[x,i],spine[y,i])
    ;print,i,dv(i)
endfor

;--- determines the closest spine segment:
tmp = min(dv, wdv)
if (wdv eq nvert-1) then $
  cv = spine[*,wdv-1:wdv] $
else if (wdv eq 0) then $
  cv = spine[*,wdv:wdv+1] $
else begin
    if ( dv(wdv-1) lt dv(wdv+1) ) then $
      cv = spine[*,wdv-1:wdv] $
    else $
      cv = spine[*,wdv:wdv+1]
endelse

;-----------------
;--- angle of spine segment with closest vertices:
numer = double(cv[y,1]-cv[y,0])
denom = double(cv[x,1]-cv[x,0])
if (denom eq 0) then begin
    alpha_sp = !pi/2.
endif else begin
    alpha_sp = atan(numer/denom)
endelse

;--- angle of barb:
numer = double(barb[y,0]-barb[y,1])
denom = double(barb[x,0]-barb[x,1])
if (denom eq 0) then begin
    alpha_br = !pi/2.*sign_pb(numer)
endif else begin
    alpha_br = atan(numer/denom)
    if (denom lt 0) then alpha_br = sign_pb(numer)*!pi + alpha_br
endelse

;--- rotate spine (and barb) so that now barb is horizontal.
;--- ! alpha_br will become 0 !!
alpha_sp = (alpha_sp - alpha_br) mod (!pi*2) ;<-- mod to keep angle in range!
;--- move angle into the "right" two quadrants:
if ( abs(alpha_sp) gt !pi/2.) then alpha_sp = alpha_sp - !pi*sign_pb(alpha_sp)

;--- look for direction: ---
;-- right:
if (alpha_sp ge reqSlope) and (alpha_sp le !pi/2.-reqSlope) then return,-1
;-- left:
if (alpha_sp le -reqSlope) and (alpha_sp ge reqSlope-!pi/2.) then return,1
;-- non definable:
return,0

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: barb_direction.pro,v $
; Revision 3.2  2010/10/27 19:31:52  bernapn1
; Improved the detection of which spine segment the barb is closest to.
;
; Revision 3.1  2009/12/18 21:52:24  bernapn1
; Improved fainter filaments detection
; Improved connection of nearby filaments
; Added MAX_DIST paramenter in input file (adds more flexibility)
;
; Revision 3.0  2004/10/27 14:38:17  pietro
; Given revision 3.0
;
; Revision 2.1  2004/06/29 19:54:38  pietro
; Fixed a bug that would change the req_slope of the caller routine
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.3  2004/06/11 20:35:20  pietro
; Added comment in header
;
; Revision 1.2  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
;    Written by Pietro N. Bernasconi JHU/APL:
;    03/10/04 by PNB: Improved the logic for finding the spine segment
;        closest to the barb.
