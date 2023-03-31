;******************************************************************************
;***                                                                        ***
     function hel2cart, lati, lngi, visible, B0_ANGLE = b0i, P_ANGLE = pangi,$
                        SUN_RADIUS=sunr
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    HEL2CART
;
; PURPOSE:
;    This function converts heliographic coordinates into cartesian (X,Y).
;    coordinates
;
; CALLING SEQUENCE:
;    XY_coord = hel2cart( lat, long, [ is_visible, B0_ANGLE = b0, $
;                         SUN_RADIUS = sun_rad])
;
; INPUTS:
;    lat  = float or fltarr(npts): array or single value for the latitude.
;    long = float or fltarr(npts): array or single value for the longitude.
;
; OPTIONAL INPUT PARAMETERS:
;    none
;
; KEYWORD PARAMETERS:
;    B0_ANGLE   = float: heliographic latitude of the Sun center. Def = 0.
;    SUN_RADIUS = float: radius of the sun for the cartesian reference
;                     system. I could be arcsec, pixels ... Default = 1.
;
; OUTPUTS:
;    XY_coord = fltarr(2) of fltarr(2, npts): X & Y cartesian coordinates
;
; OPTIONAL OUTPUT PARAMETERS:
;    is_visible = int or intarr(npts): 1 if the position is on the visible
;                     side of the solar disk, 0 if NOT.
;
; PROCEDURE:
; Standard spherical transformations are used. The cartesian XYZ reference
; system is defined as follows:
;    X = Horizontal direction, parallel to Sun equator. > 0 is Sun West
;    Y = Vertical direction, parallel to Sun axis of rotation. > 0 is Sun North
;    Z = "Depth" direction along the observer LOS. > 0 it towards the observer
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
;--- Handles the input:
if (n_params() lt 2) then message,'missing one required input parameter!'
if (n_elements(b0i) le 0) then b0i=0.
if (n_elements(pangi) le 0) then pangi=0.
if (n_elements(sunr) le 0) then sunr = 1.

npts = max([n_elements(lati), n_elements(lngi)])
if (n_elements(lati) ne n_elements(lngi)) then begin
   print,'% HEL2ARCSEC: lat and long do not have same # of elements!'
   print,'              reset to smallest =',npts
   lati = lati(0:npts-1)
   lngi = lngi(0:npts-1)
endif
xy = fltarr(2, npts)

;--- Convert input angles in radians:
b0  = b0i / !radeg
pang= pangi / !radeg
lat = lati / !radeg
lng = lngi / !radeg

;--- Computes the angles sin and cos:
sb0  = sin(b0)
cb0  = cos(b0)
slat = sin(lat)
clat = cos(lat)
slng = sin(lng)
clng = cos(lng)

;--- Computes the z conponent that will discriminate wheter the position
;    is visible by the observer or not. > 0 ==> Visible!
z = sb0 * slat + cb0 * clat * clng
visible = (z gt 0)

;--- Computes the X & Y coordinates:
xy(0,*) = sunr * clat * slng
xy(1,*) = sunr * ( cb0 * slat - sb0 * clat * clng)

;--- Apply rotation if P angle is set:
if (pang ne 0) then begin
    cp = cos(-pang)
    sp = sin(-pang)
    tmp = xy
    xy(0,*) = cp*tmp(0,*) - sp*tmp(1,*)
    xy(1,*) = sp*tmp(0,*) + cp*tmp(1,*)
endif

return, xy

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: hel2cart.pro,v $
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
;    Written by Pietro N. Bernasconi JHU/APL: 06/09/04 Inspired by the
;        Solar Soft routine hel2arcmin, but algorygthm a litle different.
