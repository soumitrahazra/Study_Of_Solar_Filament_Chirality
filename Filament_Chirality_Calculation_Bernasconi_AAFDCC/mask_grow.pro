;******************************************************************************
;***                                                                        ***
      function mask_grow, mask, img, thr
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    MASK_GROW
;
; PURPOSE:
;    This function grows a mask (the seed of growth) to match a certain
;    threshold in a given image.
;
; CALLING SEQUENCE:
;    new_mask = mask_grow(mask, image, threshold)
;
; INPUTS:
;    mask  = array(X,Y): array with seeds for growth. 0 for empty and 1 for
;               seeds.
;    image = array(X,Y): image used to grow the seeds from
;    threshold = int or float: upper threshold limit below which the growth
;               is performed
;
; OPTIONAL INPUT PARAMETERS:
;	None
;
; KEYWORD PARAMETERS:
;	None
;
; OUTPUTS:
;	new_mask = array(X,Y): mask grown from the seeds to reach the
;                    threshold. 0 empty, 1 mask
;
; OPTIONAL OUTPUT PARAMETERS:
;	None
;
; PROCEDURE:
;       The seed mask is taken as a starting point. then a 1 pixel contour
;       is added around each seed. The new pixels are compared against the
;       image. If for the new pixels the value in the image is BELOW the
;       threshold then the new pixel is marked as 1 otherwise is left 0 in
;       the new mask. A new growth is the recursively performed until no
;       new pixels are added to the new mask. At this point the routine
;       returns the new mask to the caller.
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 3) then mesage,'Required input parameter missing!'

sz=size(mask)
if (sz(0) ne 2) then message,'1st input parameter must be a 2D array!'
szi=size(img)
if (szi(0) ne 2) then message,'2nd input parameter must be a 2D array!'
if (sz(1) ne szi(1)) then $
    message,'1st dimension of 1st and 2nd input parameters must be the same!'
if (sz(2) ne szi(2)) then $
    message,'2nd dimension of 1st and 2nd input parameters must be the same!'

out = mask

s8=replicate(1,3,3)  ;<-- 8-connectivity box

repeat begin
    adds = 0
    tmpm=dilate(out,s8)-out
    w=where(tmpm eq 1)
    ww=where(img(w) lt thr, nw)
    if (nw gt 0) then begin
        adds=1
        out(w(ww))=1
    endif
endrep until (adds eq 0)

return,out

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: mask_grow.pro,v $
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.3  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
; Written by Pietro N. Bernasconi JHU/APL: April 7, 2004
