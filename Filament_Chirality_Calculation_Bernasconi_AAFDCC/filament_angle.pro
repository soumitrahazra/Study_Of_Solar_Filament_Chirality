;******************************************************************************
;***                                                                        ***
      function filament_angle, spine, DEGREES=degs
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    FILAMENT_ANGLE
;
; PURPOSE:
;    This function calculates the orientation of a given filament by
;    determining the average angle of the spine segments.
;
; CALLING SEQUENCE:
;    angle = filament_angle(spine, /DEGREES)
;
; INPUTS:
;    spine = intarr(2,n_segments): array containing the spine vertices
;
; OPTIONAL INPUT PARAMETERS:
;	None
;
; KEYWORD PARAMETERS:
;    DEGREES = flag: If set then the angle returned is given in
;                        degrees. Otherwise is in radians
;
; OUTPUTS:
;    angle = filament orientation in radians (in degrees if requested)
;
; OPTIONAL OUTPUT PARAMETERS:
;	None
;
; PROCEDURE:
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() le 0) then message,'spine input parameter missing!'

sz=size(spine)
if ((sz(0) ne 2) or (sz(1) ne 2)) then $
  message,'incorrect dimention of input parameter: arr(2,n_seg)'

x=0  &  y=1

;-- Finding the true number of points in the spine array:
tmp = spine(0,*)+spine(1,*)
w=where(tmp eq 0., nn)
if (nn gt 0) then $
  npts = w(0) $
else $
  npts = sz(2)-1
;print,'npts =',npts

;-- Getting the angle from start to end of filament:
tmp = segment_parms(spine(x,0), spine(y,0), spine(x,npts-1), spine(y,npts-1))
bound_ang = atan(tmp(0))
;print,'bound_ang =',bound_ang*180./!pi
if (bound_ang lt -!pi/2.) then bound_ang = bound_ang + !pi
if (bound_ang gt  !pi/2.) then bound_ang = bound_ang - !pi

;-- This determines whether the filament has a hign inclination or not
if (abs(bound_ang) gt !pi/4) then zero2pi = 1 else zero2pi = 0

avang=0
for i=0, npts-2 do begin
    ;- the following is for finding when the spine data ends
    ;if (abs(spine(x,i+1))+abs(spine(y,i+1)) eq 0.) then  goto,at_end_loop
    tmp = segment_parms(spine(x,i), spine(y,i), spine(x,i+1), spine(y,i+1))
    tmp = atan(tmp(0))

    if (zero2pi) then begin
        ;-- High inclination treatment of angles
        if (tmp lt 0) then tmp = tmp + !pi        
    endif else begin
        ;-- Low inclilantion treatment of angles
        if (tmp lt -!pi/2.) then tmp = tmp + !pi
        if (tmp gt  !pi/2.) then tmp = tmp - !pi
    endelse
    ;print,i,tmp*180/!pi

    avang = avang + tmp
endfor

avang=avang/float(i)

if (avang lt -!pi/2.) then avang = avang + !pi
if (avang gt  !pi/2.) then avang = avang - !pi

if keyword_set(degs) then return,avang*180./!PI

return, avang

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: filament_angle.pro,v $
; Revision 3.2  2009/12/15 22:03:05  bernapn1
; Improved logic to determine the angle by adding a prejudgement of whether
; the filament is more horizontal or more vertical. Also fixed a bug that would
; give a different angle whether the filament nubering goes from left to right
; or from right to left.
; Now the calcualted angles make much more sense than before.
;
; Revision 3.1  2004/11/19 15:33:48  pietro
; Added a patch that allows the correct calculatioin of the average angle
; even if the spine array is larger than the actual number of vertices
; contained in the spine. This patch was necessary in order to make the code
; calculate the angle properly even when called by the merge_filams.
;
; Revision 3.0  2004/10/27 14:38:17  pietro
; Given revision 3.0
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.2  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
; Written by Pietro N. Bernasconi JHU/APL: 04/13/04
