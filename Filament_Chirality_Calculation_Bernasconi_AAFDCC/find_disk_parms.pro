;******************************************************************************
;***                                                                        ***
        function find_disk_parms, data, DISK_THRESHOLD = d_threshold, $
                                  FORCE_SIZE=force, OPTIMIZE=optim, $
                                  VERBOSE=verb
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    FIND_DISK_PARMS
;
; PURPOSE:
;    Finds the solar disk size (radius) and its center coordinates
;
; CALLING SEQUENCE:
;    disk_parms = find_disk_parms( data [, DISK_THRESHOLD = d_threshold, $
;                                  /FORCE_SIZE, /OPTIMIZE, /VERBOSE=verb] )
;
; INPUTS:
;    data = intarr(X,Y)
;
; OPTIONAL INPUT PARAMETERS:
;	None
;
; KEYWORD PARAMETERS:
;    DISK_THRESHOLD = int: value above which the solar disk is identifyed.
;                          Default = -1500
;    FORCE_SIZE = flag: if set then if size of image is > 1024 it is then
;                       forced to 1024 for this procedure.
;    OPTIMIZE = flag: if set then a full disk optimization is performed (slow)
;    VERBOSE = flag: if set then some printouts are displayed
;
; OUTPUTS:
;    disk_parms = intarr(3): 0) = solar radius, 1) = X coords of center
;                                               2) = Y coords of center
;
; OPTIONAL OUTPUT PARAMETERS:
;	None
;
; CALLED ROUTINES:
;    fit_disk
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 1) then message,'missing input parameter! (a 2D array)'

sz=size(data)
if (sz(0) ne 2) then message,'input parameter MUST be a 2D array!'

if (n_elements(d_threshold) le 0) then d_threshold = -1500
if (n_elements(verb) le 0) then verb = 0 else if verb then $
  print,form='("    Finding solar disk parameters  ... ",$)'

if keyword_set(optim) then $
  common disk_fit, xquads, yquads, avgrad, xc, yc, xwd, ywd

;--- Resizing the data if requested and data is too big:
changesz = 0
xy_ratio = replicate(1.,2)
if keyword_set(force) then begin
    oldsz=sz
    if sz(1) gt 1024 then begin
        changesz = 1
        xy_ratio = sz(1) / 1024.
        for i=1,2 do sz(i) = 1024
    endif
endif
if changesz then begin
    if verb then print,form='("Resize image ... ",$)'
    im = congrid(data, sz(1), sz(2))
endif else begin
    im = data
    xy_ratio = 1
endelse

;--- Creating disk mask:
w = where(im gt d_threshold)
disk = intarr(sz(1),sz(2))
disk(w) = 1
disk = median(disk, 10)   ;<-- this to remove some unwanted details

;--- finds first guesses:
;- left-right:
l = disk(*,sz(2)/2.)
w = where(l gt 0, nw)
xc = (w(0) + w(nw-1)) / 2.

;- up-down:
l = disk(xc,*)
w = where(l gt 0, nw)
yc = fix( (w(0) + w(nw-1)) / 2. + 0.5 )
yrad = (w(nw-1) - w(0)) / 2.

;- redo left-right:
l = disk(*,yc)
w = where(l gt 0, nw)
xc = fix( (w(0) + w(nw-1)) / 2. + 0.5 )
xrad = (w(nw-1) - w(0)) / 2.
avgrad = (xrad+yrad)/2

out = intarr(3)
if keyword_set(optim) then begin
    if verb then print,form='("Optimize ... ",$)'
    xwd = 30
    ywd = 30

    while ( (xc-avgrad-xwd lt 0) or (xc+avgrad+xwd gt 1023) ) do xwd=xwd-1
    while ( (yc-avgrad-ywd lt 0) or (yc+avgrad+ywd gt 1023) ) do ywd=ywd-1

    xquads = intarr(xwd*2+1,201,2)
    xquads(*,*,0)=disk(xc-avgrad-xwd:xc-avgrad+xwd, yc-100:yc+100)
    xquads(*,*,1)=disk(xc+avgrad-xwd:xc+avgrad+xwd, yc-100:yc+100)

    yquads = intarr(201,ywd*2+1,2)
    yquads(*,*,0)=disk(xc-100:xc+100, yc-avgrad-ywd:yc-avgrad+ywd)
    yquads(*,*,1)=disk(xc-100:xc+100, yc+avgrad-ywd:yc+avgrad+ywd)

    d_parms =amoeba(0.1, scale=[2,2,2], p0=[avgrad,xc,yc], $
                    function_name='fit_disk')

    out = fix(d_parms*xy_ratio+0.5)
endif else begin
    out(0) = fix(avgrad*xy_ratio+0.5)
    out(1) = fix(xc*xy_ratio+0.5)
    out(2) = fix(yc*xy_ratio+0.5)
endelse

if verb then begin
    print,'DONE!'
    print,format='(6X,"Sun radius =",I5,4X,"Coord center = (",I5,",",I5,")")',$
      out
endif

return, out

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: find_disk_parms.pro,v $
; Revision 3.2  2008/11/19 17:19:17  bernapn1
; More robust: now the small windows at the four corners are determined
; dynamically.
;
; Revision 3.1  2008/11/18 18:57:55  bernapn1
; Updated
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
; Written by Pietro N. Bernasconi JHU/APL: 03/11/04
