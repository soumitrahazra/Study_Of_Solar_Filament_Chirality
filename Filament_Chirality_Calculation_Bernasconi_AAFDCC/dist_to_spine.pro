;******************************************************************************
;***                                                                        ***
      function dist_to_spine, pnts, spine, spineLine, STOPIT=stopit, $
                              PERP_INTERCEPT=perp_intrc
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    DIST_TO_SPINE
;
; PURPOSE:
;    Calculates the distance of an array of  points from a spine (a line 
;    of points).
;
; CALLING SEQUENCE:
;    dist = dist_to_spine(points,spine [,spineLine,PERP_INTERCEPT=prp_intrc])
;
; INPUTS:
;    points = fltarr(2,npoints): X and Y coordinates of the array of points
;    spine = fltarr(2,  npts): array with coordinates of the spine points
;
; OPTIONAL INPUT PARAMETERS:
;    spineline = fltarr(2, npts-1): array with the slope and y-intercept
;                for each segment in the spine.
;
; KEYWORD PARAMETERS:
;    PERP_INTERCEPT = fltarr(2) : [X,Y] coordinates of the intercept
;                 between the spine and the perpendicular passing through
;                 the point. This is useful only if only one point is given
;                 in the variable "points". Otherwise it will give the the
;                 coords for the last point in the array
;
; OUTPUTS:
;    dist = fltarr(npts): Array with the distances.
;
; OPTIONAL OUTPUT PARAMETERS:
;	None
;
; PROCEDURES USED:
;    segment_parms,  dist_to_line
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 2) then $
  message,'syntax: dist_to_spine(pntarr, spine [, spineLine])'

x = 0 & y = 1

if (n_elements(stopit) le 0) then stopit=0

dimp = size(pnts)
if (dimp(0) eq 0) then message,'Incorrect size of first input parameter'
if (dimp(1) ne 2) then message,'Incorrect size of first input parameter'
if (dimp(0) eq 1) then npts = 1 else npts = dimp(2)
out = fltarr(npts)

dims  = size(spine)
if (dims(0) ne 2) then message,'spine (2nd parm) must have at least 2 points!'
if (dims(1) ne 2) then message,'Incorrect size of second input parameter'
nvert = dims(2)

;--- Computes the spine segment parameters if not provided or
;--- provided incorrectly:
if (n_elements(spineLine) lt ((nvert-1)*2)) then begin
    ;print,'dist_to_spine: Calculating the spine parameters ...'
    spineLine = dblarr(2, nvert-1)
    for i=0, nvert-2 do begin  &$
        spineLine(*,i) = segment_parms(spine(x,i), spine(y,i),$
                                       spine(x,i+1), spine(y,i+1) )  &$
    endfor  &$
endif else spineLine=double(spineLine)

;--- Calculate the distance for each point: -----------------------------------
d = dblarr(2)
prp_int_tmp = fltarr(2,2)
for p=0,npts-1 do begin
    px = pnts(x,p)
    py = pnts(y,p)

    ;--- Determine the closest vertex to the point
    for i=0,nvert-1 do begin  &$
        ddd = double(spine(x,i)-px)^2 + double(spine(y,i)-py)^2  &$
        if (i eq 0) then begin  &$
            close_v = ddd  &$
            close_ind = 0  &$
        endif else if (ddd lt close_v) then begin  &$
            close_v = ddd  &$
            close_ind = i  &$
        endif  &$
    endfor

    ;--- determine the indices of the two segmens of spine closest to point
    loop_str  = close_ind-1
    loop_stop = close_ind
    if (close_ind eq 0) then begin
        loop_str = 0
    endif else if (close_ind ge nvert-1) then begin
        loop_str = nvert-2
        loop_stop = nvert-2
    endif

    ;--- determine the distance between closest spine segments and point
    d_ind = 0
    for i= loop_str, loop_stop do begin
        if (i eq 0) then endseg = -1 $
        else if (i eq nvert-2) then endseg = 1 $
        else endseg = 0

        slope = spineLine(0,i)
        intercept = spineLine(1,i)
        d[d_ind] = dist_to_line(px, py, spine[x,i], spine[y,i], spine[x,i+1], $
                                spine[y,i+1], slope, intercept, /m1_out,$
                                endseg=endseg, PERP_INTERCEPT=tmp)
        prp_int_tmp[*,d_ind] = tmp
        if (d_ind eq 0) then begin
            d[d_ind+1] = d[d_ind]
            prp_int_tmp[*,d_ind+1] = prp_int_tmp[*,d_ind]
        endif
        if (stopit eq 1) then stop
        d_ind = d_ind +1
    endfor

    good = where (d ge 0)
    if (good(0) eq -1) then begin
        ;-- when vertex is closest
        out(p) = sqrt(close_v)
        perp_intrc = spine(*,close_ind)
    endif else begin
        ;-- when segment is closest
        out(p) = min(d(good), wm)
        perp_intrc = prp_int_tmp(*,wm)
    endelse
endfor

return, reform(out)

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: dist_to_spine.pro,v $
; Revision 3.1  2004/12/23 18:43:21  pietro
; Fixed a bug that happens when only one point is given and the pert_intercept
; is required. Tested and now it works good.
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
; March 5, 2004 by PNB: now it is possible to give an array of points
; Written by Pietro N. Bernasconi JHU/APL:
