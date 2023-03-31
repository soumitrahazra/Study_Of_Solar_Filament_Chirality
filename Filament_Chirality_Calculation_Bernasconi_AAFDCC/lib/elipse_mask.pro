;******************************************************************************
;***                                                                        ***
	function elipse_mask,nx,ny,xradi,yradi,XCEN=xceni,YCEN=yceni,$
                             INVERT=invert,ANGLE=angle,UNSCRAMBLE=uns
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;	ELIPSE_MASK
;
; PURPOSE:
;	Make an eliptical mask with 1 inside the elipse and 0 outside.
;
; CALLING SEQUENCE:
;	mask=elipse_mask(nx,ny,xrad,yrad [,XCEN=xcen,YCEN=ycen,ANGLE=angle,$
;                        /INVERT,/UNSCRAMBLE])
;
; INPUTS:
;	nx = integer : size in x direction of output array
;	ny = integer : size in y direction of output array
;	xrad = integer : radius's length in x direction
;	yrad = integer : radius's length in y direction
;
; OPTIONAL INPUT PARAMETERS:
;	None
;
; KEYWORD PARAMETERS:
;       XCEN = integer : x coordiante of ellipse center (Default = nx/2-1)
;       YCEN = integer : y coordiante of ellipse center (Default = ny/2-1)
;       ANGLE  = float: Orientation angle in degres of elipse. >0 ==>
;                       rotation in clockvise direction
;       INVERT = flag: If set then the values 1 and 0 are exchanged
;       UNSCRAMBLE = flag: If set then the quadrants are permuted along the
;                       diagonal. This is useful when dealing with fourier
;                       trasform filtering.
;
; OUTPUTS:
;	mask = bytarr(nx,ny) : array with mask
;
; OPTIONAL OUTPUT PARAMETERS:
;	None
;
; PROCEDURE:
;	Formula used: [(X-xc)/xr]^2 + [(Y-yc)/yr]^2 = 1 . If left side <= 1
;	than the pixel value is set to 1, and 0 when left side > 1. With the
;	/INVERT keyword everything is reversed.
;	
; MODIFICATION HISTORY:
;	Pietro N. Bernasconi, 22 Aug 1995. -> From an original program of
;		S. Keil (NOAO/Sunspot)
;       PNB, 2003: Added keywords INVERT and ANGLE.
;       PNB, 20 Sept 2004: Added the keyword UNSCRAMBLE.
;       PNB, 03 Mar  2005: Implemented a new, faster method to compute the mask
;       PNB, 16 Mar  2005: changed impus xrad, yrad, xcen, ycen   to
;               xradi, yradi, xceni, yceni
;-
;******************************************************************************

if (n_params() ne 4) then message,'Wrong number of input parameters'
if (n_elements(angle) le 0) then angle = 0
if (n_elements(uns) le 0) then uns = 0

msk=bytarr(nx,ny)
if keyword_set(invert) then begin
  msk(*,*)=1
  value=0
endif else value=1

if not keyword_set(xceni) then xceni=nx/2-1
if not keyword_set(yceni) then yceni=ny/2-1

xrad=float(xradi)
yrad=float(yradi)
xcen=float(xceni)
ycen=float(yceni)

if 0 then begin
    ;-- obsolete method to compute the mask:
    for i=0,ny-1 do begin 
        y=(float(i)-ycen)/yrad
        ry=y*y
        for j=0,nx-1 do begin 
            x=(float(j)-xcen)/xrad
            r = x*x + ry
            if(r le 1.) then msk(j,i)=value
        endfor
    endfor
endif else begin
    ;-- new method to compute the mask:
    tmp=((indgen(nx)-xcen)/float(xrad))^2
    dx=fltarr(nx,ny)
    for i=0,ny-1 do dx(*,i)=tmp

    tmp=((indgen(ny)-ycen)/float(yrad))^2
    dy=fltarr(nx,ny)
    for i=0,nx-1 do dy(i,*)=tmp
    tmp=dx+dy
    w=where(tmp lt 1, count)
    msk(w)=value
endelse

if (angle gt 0) then msk = rot(msk,angle,1,xcen,ycen,/int,/pivot)

if (uns) then msk = unscramble(msk)

return,msk

end
;******************************************************************************
