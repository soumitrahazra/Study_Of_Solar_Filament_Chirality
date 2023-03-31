;******************************************************************************
;***                                                                        ***
      function get_filaments, data, i_thresh, U_THRESHOLD = u_thresh, $
                              MORPH_FILTER = morph_f, BOX_SIZE=boxs, $
                              VERBOSE = verbose
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    GET_FILAMENTS
;
; PURPOSE:
;    This function extracts the filaments from and image if requested it
;    applies some morphological filters to the input image to better
;    isolate the filaments
;
; CALLING SEQUENCE:
;    filament_mask = get_filaments(data, i_threshold [, U_THRESHOLD=u_thresh,$
;                          MORPH_FILTER = morph_f, BOX_SIZE=boxs, $
;                          /VERBOSE] )
;
; INPUTS:
;    data = intarr(XS,YS): Input data that is probed to look for filaments.
;    i_threshold = int: Image intensity threshold below which a filaments
;                          is identifyed
;
; OPTIONAL INPUT PARAMETERS:
;	None
;
; KEYWORD PARAMETERS:
;    U_THRESHOLD = int: Upper threshold to which a filament can grow after
;                          being identifyed. This must be > i_threshold.
;                          Default = i_threshold
;    MORPH_FILTER= flag: If set then morphological filters are applyed to
;                          the mask to remove noise and fearures that are
;                          not filaments.
;    BOX_SIZE    = int: size of the structural element box used in the
;                          morphological filtering. Max available = 15.
;                          Default = 11.
;    VERBOSE     = flag: if set then print some infos in the sdout
;
; OUTPUTS:
;    filament_mask = intarr(XS,YS): Mask that identyfies filaments as 1,
;            and 0 as background.
;
; OPTIONAL OUTPUT PARAMETERS:
;	None
;
; PROCEDURE:
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 2) then mesage,'Required input parameter missing!'

sz=size(data)
if (sz(0) ne 2) then message,'1st input parameter must be a 2D array!'

if keyword_set(verbose) then print,'Identifying filaments on solar disk ...'

if (n_elements(u_thresh) le 0) then u_thresh = i_thresh

;--- Generates initial masks
mask=intarr(sz(1),sz(2))
core0_mask = mask
core1_mask = mask
core2_mask = mask
w = where(data lt i_thresh, nw)
if (nw gt 0) then begin
    mask(w) = 1
    ww0 = where(data lt -2300, nww0)
    ww1 = where(data lt -2000, nww1)
    ww2 = where(data lt -1500, nww2)
endif

;--- If requested applies morphological filters to the mask: ---
if ( keyword_set(morph_f) and (nw gt 0) )then begin
    if keyword_set(verbose) then $
      print,format='("    Morphological filtering ... ",$)'

    ;--- Create the 8 structuring elements
    ;--- (see Shih & Kovalsky Sol.Phys. 218, 99)
    s=intarr(15,15,8)
    s(*,7,0)=1
    s(7,*,1)=1
    for i=0,14 do s(i,i,2)=1
    s(*,*,3)=reverse(s(*,*,2),2)
    s(0:1,4,4)=1
    s(2:3,5,4)=1
    s(4:5,6,4)=1
    s(6:8,7,4)=1
    s(9:10,8,4)=1
    s(11:12,9,4)=1
    s(13:14,10,4)=1

    s(*,*,5)=reverse(s(*,*,4),2)
    s(*,*,6)=rotate(s(*,*,4),4)
    s(*,*,7)=rotate(s(*,*,4),1)

    if (n_elements(boxs) le 0) then boxs=5 else begin
        boxs=boxs/2
        if (boxs gt 7) then boxs = 7
        if (boxs lt 1) then boxs = 1
    endelse
    if (boxs ne 7) then begin
        s=s(7-boxs:7+boxs, 7-boxs:7+boxs,*)
    endif

    new_mask = intarr(sz(1),sz(2))
    for i=0,7 do new_mask = new_mask + morph_open(mask,s(*,*,i))

    mask = intarr(sz(1),sz(2))
    w=where(new_mask ge 2, nw)
    if (nw gt 0) then mask(w) = 1
    if keyword_set(verbose) then print,'DONE!'

    ww0 = where((data lt -2300) and (new_mask ge 2), nww0)
    ww1 = where((data lt -2000) and (new_mask ge 2), nww1)
    ww2 = where((data lt -1500) and (new_mask ge 2), nww2)
endif
if (total(mask) eq 0) then return,mask

;--- Generates the core masks
if (nww0 gt 0) then core0_mask(ww0) = 8
if (nww1 gt 0) then core1_mask(ww1) = 4
if (nww2 gt 0) then core2_mask(ww2) = 2
core3_mask = mask

;--- Grows the filament to reach the upper threshold: --- 
if keyword_set(verbose) then print,format='("    Grow filaments ... ",$)'
mask = mask_grow(mask, data, u_thresh)
if keyword_set(verbose) then begin
    print,'DONE!'
    print
endif

;--- Morphological close to close some small gaps in the filament
;s8=replicate(1,3,3)  ;<-- 8-connectivity box
;mask = morph_close(mask,s8)
ss = [[0,1,1,1,0],$
      [1,1,1,1,1],$
      [1,1,1,1,1],$
      [1,1,1,1,1],$
      [0,1,1,1,0]]
mask = morph_close(mask,ss)

;--- Add the other cores to create a more structured mask useful for weighting
mask = mask  + core0_mask + core1_mask + core2_mask + core3_mask

return,mask

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: get_filaments.pro,v $
; Revision 3.4  2010/04/08 14:32:53  bernapn1
; Added a number of thresholds lower than the i_threshold to create a "weighted"
; mask where higner values in the mask are given to places in the filaments that
; are darker than others.
;
; Revision 3.3  2009/12/18 21:52:24  bernapn1
; Improved fainter filaments detection
; Improved connection of nearby filaments
; Added MAX_DIST paramenter in input file (adds more flexibility)
;
; Revision 3.2  2008/11/18 21:45:03  bernapn1
; Added logic that handles no pixels in the mask
;
; Revision 3.1  2004/11/23 21:02:59  pietro
; Now after the morfological filtering the supremum is 2 (instead of 3)
;
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
; Written by Pietro N. Bernasconi JHU/APL: April, 8, 2004
