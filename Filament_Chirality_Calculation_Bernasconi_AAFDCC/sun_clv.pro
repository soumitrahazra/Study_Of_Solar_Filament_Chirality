;******************************************************************************
;***                                                                        ***
      function sun_clv,xc=xc, yc=yc, XR=xr, YR=yr, RAD=rad, $
                       xsize=xs, ysize=ys, $
                       background=backgr, $
                       offset = offset, parms = parms
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    SUN_CLV
;
; PURPOSE:
;    Generates a center-to-limb curve used for KANZELHOEHE data to revert the
;    observatory builtin contrast ehnancement they do towards the limb.
;
; CALLING SEQUENCE:
;    mask = sun_clv(xc=xc, yc=yc, XR=xr, YR=yr, RAD=rad, $
;                       xsize=xs, ysize=ys, background=backgr, $
;                       offset = offset, parms = parms
;
; INPUTS:
;    There are no required inputs
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    XSIZE = int: X-axis Size of the output 2D array. Default = 512
;    YSIZE = int: Y-axis Size of the output 2D array. Default = 512
;    XC    = int: Center of the Sun on the X-axis. Default = XS/2
;    YC    = int: Center of the Sun on the Y-axis. DEfault = YS/2
;    RAD   = int: Radius ot the Sun. Default = 501
;    OFFSET = flag: If set then it adds a 0.5 (1/2 pixel) to the sun radius
;    XR    = int: Radius of the Sun along X-axis. Default = RAD
;    YR    = int: Radius of the Sun along Y-axis. Default = RAD
;    BACKGROUND = float: Value of the area outside Sun disk. Default = 1.5
;    PARMS = fltarr(2): 2 elements vector containting the input parameters
;               [A, B] to generate the center-to-limb function:
;               F(x) = 1 - (alog(1.000001 - x*A)^2) * B,
;               where x is the radial distance from Sun center (0 to 1)
;
; OUTPUTS:
;    mask = fltarr(XS,YS): 2d image with the center-to-limb variation
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES USED:
;    None
;
; PROCEDURE:
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_elements(xs) le 0) then xs = 512
if (n_elements(ys) le 0) then ys = 512
if (n_elements(xc) le 0) then xc = xs/2
if (n_elements(yc) le 0) then yc = ys/2
if (n_elements(rad) le 0) then rad = 501
if (n_elements(xr) le 0) then xr = rad
if (n_elements(yr) le 0) then yr = rad
if (n_elements(parms le 1)) then parms = [0.84, 0.098]

dr = 0.d
if (keyword_set(offset)) then dr = 0.5d

if (n_elements(backgr) le 0) then backgr = 1

;--- Generates a Sun disk image with values inside the disk reflecting
;    the distance of the pixel from Sun center in units from 0 to 1 (Sun limb)
tmp=((indgen(xs)-xc)/float(xr+dr))^2
dx=fltarr(xs,ys)
for i=0,ys-1 do dx(*,i)=tmp
tmp=((indgen(ys)-yc)/float(yr+dr))^2
dy=fltarr(xs,ys)
for i=0,xs-1 do dy(i,*)=tmp
xx=sqrt(dx+dy)

ww  = where(xx le 1, nw)
ww1 = where(xx gt 1, nw1)
if (nw1 gt 0) then xx(ww1) = 1

;--- Generates the output image with the center-to-limb variation
mu = dblarr(xs, ys)
if (nw gt 0) then begin
    mult = parms(0)
    Amp = parms(1)
    mu(ww) = 1 - (alog(1.000001-xx(ww)*mult)^2)*Amp
endif

if (nw1 gt 0) then mu(ww1) = float(backgr)

return,mu

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: sun_clv.pro,v $
; Revision 3.1  2012/04/26 18:24:16  bernapn1
; Created
;
;
; Written by Pietro N. Bernasconi JHU/APL
