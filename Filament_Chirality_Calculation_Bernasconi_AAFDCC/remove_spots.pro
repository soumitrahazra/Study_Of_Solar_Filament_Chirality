;******************************************************************************
;***                                                                        ***
      function remove_spots, data, sun_p, SPOT_THRESHOLD=sp_thr, $
                             FILAMENT_THRESHOLD = fil_thr, FILL_VALUE=fill_v,$
                             MAX_SPOT_AREA = mx_spot_area
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    REMOVE_SPOTS
;
; PURPOSE:
;    This routine removes all the spots with threshold below a given value
;
; CALLING SEQUENCE:
;    out = remove_spots(data, sun_p [, SPOT_THRESHOLD = sp_thr,$
;                       FILAMENT_THRESHOLD = fil_thr, FILL_VALUE=fill_v])
;
; INPUTS:
;    data  = intarr(X,Y): array with data
;    sun_p = intarr(3)  : array with Sun radius, and X & Y coords of Sun center
;
; OPTIONAL INPUT PARAMETERS:
;	None
;
; KEYWORD PARAMETERS:
;    SPOT_THRESHOLD     = Value below which spots are identifyed. Def =-3000
;    MAX_SPOT_AREA      = Maximum area allowed for a spot. If above then
;                               it is NOT considered a spot. Def = 1500
;    FILAMENT_THRESHOLD = Value below which filams are indentifyed. Def = -600
;    FILL_VALUE         = Value used to fill up the spot.
;                                     Def = FILAMENT_THRESHOLD + 10
;
; OUTPUTS:
;    out = intarr(X,Y): Input image but with the sunspots removed.
;
; OPTIONAL OUTPUT PARAMETERS:
;	None
;
; CALLED ROUTINES:
;    dist_circle, mask_grow
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 2) then mesage,'Required input parameter missing!'

sz=size(data)
if (sz(0) ne 2) then message,'1st input parameter must be a 2D array!'
szp=size(sun_p)
if ((szp(0) ne 1) and (szp(1) lt 3)) then $
  message,'2nd input parameter must be a 3 elements array!'

if (n_elements(sp_thr) le 0) then sp_thr = -3000
if (n_elements(mx_spot_area) le 0) then mx_spot_area = 1500
if (n_elements(fil_thr) le 0) then fil_thr = -600
if (n_elements(fill_v) le 0) then fill_v = fil_thr+10

dist_circle, rad_d, sz(1), sun_p(1), sun_p(2)

w=where( (data lt sp_thr) and (rad_d lt sun_p(0)) , nw)
if (nw eq 0) then return,data

mask=intarr(sz(1),sz(2))
mask(w) = 1

;--- Uses mask seeds to grow the mask abowe the filament threshold
mask = mask_grow(mask, data, fill_v)

;--- Making sure that only spots are catched:
clusters = label_region(mask)
h = histogram(clusters)
for i=1,n_elements(h)-1 do begin  &$
    if (h(i) gt mx_spot_area) then begin  &$
        w = where(clusters eq i)  &$
        mask(w) = 0  &$
    endif  &$
endfor

out = data
w=where(mask eq 1, nw)
if (nw gt 0) then out(w) = fill_v

return,out

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: remove_spots.pro,v $
; Revision 3.2  2005/01/05 14:58:42  pietro
; Fixed a bug: when no spots are found at the end of code there was
; previously no check for that and there was a crash at the
; assignment "out(w)=..."
; Now this will not happen any more.
;
; Revision 3.1  2004/12/30 21:48:38  pietro
; Added keyword MAX_SPOT_AREA in an attempt fix the problem that some
; filaments are so dark that they too are removed.
;
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.1  2004/07/01 19:25:38  pietro
; passe parameter fill_v to  mask_grow
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.2  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
; 04/07/04 PNB: Now uses routine mask_grow to enlarge the mask.
; Written by Pietro N. Bernasconi JHU/APL: 03/12/04
