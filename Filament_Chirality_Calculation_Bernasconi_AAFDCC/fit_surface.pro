;******************************************************************************
;***                                                                        ***
      function fit_surface, imsi, MIN=mn, MAX=mx, SMOOTH=smth, $
                            FORCE_SIZE=force, ADJ_OFFSET=adj_offs, $
                            SUN_PARAMS=sun_pi, VERBOSE=verb
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    FIT_SURFACE
;
; PURPOSE:
;    This routine performs a polynomial fit of the solar surface. 
;
; CALLING SEQUENCE:
;    fit = fit_surface(image [, MIN=mn, MAX=mx, SMOOTH=smth, /FORCE_SIZE,$
;                              /ADJ_OFFSET, SUN_PARAMS=sun_p, /VERBOSE] )
;
; INPUTS:
;    image = array(X,Y): array with te solar image to be fitted
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    MIN = int: minimum cutoff value (default -800)
;    MAX = int: maximum cutoff value (default +800)
;    SMOOTH = int: How much smoothing to apply to the interpolated image
;                  (Default = 30)
;    FORCE_SIZE = flag: if set then if image size is bigger than 1024 in
;                       either direction that direction is forced to
;                       be 1024 and re-expanded at the end.
;    ADJ_OFFSET = flag: if set then the image average offset is adjusted
;    SUN_PARMS = fltarr(7): Solar disk parameters: 0) Sun radius [pix]
;                                                  1) X coord of center [pix]
;                                                  2) Y coord of center [pix]
;                                                  3) Julian Date
;                                                  4) Sun radius [arcsec]
;                                                  5) Solar P  angle [deg]
;                                                  6) Solar B0 angle [deg]
;    VERBOSE = flag: if set then some information messages are plotted
;
; OUTPUTS:
;    fit = fltarr(X,Y) 
;
; OPTIONAL OUTPUT PARAMETERS:
;	None
;
; ROUTINES USED:
;    find_disk_parms
;
; PROCEDURE:
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_elements(imsi) le 0) then message,'input image missing!'

ims = float(reform(imsi))
sz=size(ims,/dim)
if (n_elements(sz) ne 2) then message,'input must be a 2D image'

if keyword_set(verb) then $
  print,form='("    Fitting a surface to the image ... ",$)' $
else verb = 0

if (n_elements(smth) le 0) then smth = 30

if (n_elements(mn) le 0) then mn = -800
if (n_elements(mx) le 0) then mx = +800
if (mn gt mx) then begin
    tmp = mn
    mn  = mx
    mx = tmp
endif

;--- Getting the solar radius and center location if not given:
if (n_elements(sun_pi) lt 3) then begin
    if verb then $
      print,'-- FIT_SURFACE: Sun disk parameters not given => finds them ...'
    sun_pi = find_disk_parms(ims, disk_thr = -1000, verbose=verb, $
                            /force, /optim)
endif
sun_p = sun_pi

changesz = 0
if keyword_set(force) then begin
    oldsz=sz
    for i=0,1 do if (sz(i) gt 1024) then begin
        changesz = 1
        sz(i) = 1024
    endif
    if changesz then begin
        if verb then print,form='("Resize image ... ",$)'
        ims = congrid(ims,sz(0),sz(1))

        ;- Resizing the solar disk parameters:
        frac = sz(0:1)/float(oldsz(0:1))
        sun_p(1:2) = fix(sun_p(1:2) * frac + 0.5)
        sun_p(0) = fix(sun_p(0) * min(frac))
    endif
endif


;--- remove the average offset if requested:
if keyword_set(adj_offs) then begin
    ;offs=avg(ims(sz(0)/4:sz(0)-sz(0)/4,sz(1)/4:sz(1)-sz(1)/4))
    offs = median(ims(sun_p(1)-sun_p(0)/2: sun_p(1)+sun_p(0)/2,$
                   sun_p(2)-sun_p(0)/2: sun_p(2)+sun_p(0)/2))
    mn=mn+offs
    mx=mx+offs
endif

mask=fltarr(sz(0),sz(1))
w=where((ims gt mn) and (ims lt mx), cnt)
if (cnt gt 0) then mask(w)=1

cf=replicate(1.,sz(0),sz(1),2)

for dir = 0, 1 do begin
    x=findgen(sz(dir))

    for i=0,sz(dir)-1 do begin
        ;if i/10 eq i/10. then print,i
        if (dir eq 0) then w=where(mask(*,i) eq 1, nel) $
        else w=where(mask(i,*) eq 1, nel)

        if (nel gt 10) then begin
            if (nel gt 200) then degr = 4 $
            else if (nel gt 130) then degr = 3 $
            else degr = 2

            if (dir eq 0) then tmpy=ims(*,i) else tmpy=ims(i,*)
            tmpy=tmpy(w)

            res1=poly_fit(float(w),tmpy,degr,yf,yb,std1)
            res2=poly_fit(float(w),tmpy,degr-1,yf,yb,std2)
            if (std1 lt std2) then begin
                res = res1
            endif else begin
                res = res2
                degr = degr - 1
            endelse

            if (dir eq 0) then begin
                for j=w(0), w(nel-1) do begin
                    tmp=res(0)
                    for dd=1,degr do tmp = tmp + res(dd)*x(j)^dd
                    cf(j,i,0)=tmp
                endfor
            endif else begin
                for j=w(0), w(nel-1) do begin
                    tmp=res(0)
                    for dd=1,degr do tmp = tmp + res(dd)*x(j)^dd 
                    cf(i,j,1)=tmp
                endfor
            endelse

        endif
    endfor
endfor

w=where(cf lt mn, cnt)
if (cnt gt 0) then cf(w) = mn
w=where(cf gt mx, cnt)
if (cnt gt 0) then cf(w) = mx

out=smooth(avg(cf,2),smth,/ed)
if changesz then out=congrid(out,oldsz(0),oldsz(1),/interp)

if verb then print,'DONE!'
return,out

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: fit_surface.pro,v $
; Revision 3.2  2005/01/25 21:29:52  pietro
; Sixed a bug with the verbose keyword
;
; Revision 3.1  2004/12/21 16:33:50  pietro
; Changed max & min thresholds to +-800 (from +-1000), changed smooth parameter
; smth to 30 (from 40). Added keyword SUN_PARMS -> now when the offset is
; determined it looks for a box centered at precisely sun center ad of 1 solar
; radius in size.
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
; Written by Pietro N. Bernasconi JHU/APL: March 3, 2004
