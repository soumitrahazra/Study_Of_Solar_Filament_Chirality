;******************************************************************************
;***                                                                        ***
      function diff_rotate, difftime, lat, lng, OLD=old, ALLEN=allen, HHF=hhf,$
                            AVERAGE=do_average
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    DIFF_ROTATE
;
; PURPOSE:
;    This function calculates the new longitude a feature on the sun will
;    have when a given time lapse is give. The computation takes into
;    account the differential rotation of the Sun for specific latitudes.
;
; CALLING SEQUENCE:
;    new_long = diff_rotate( difftime, latitude, longitude)
;
; INPUTS:
;    difftime  = float: Time difference (in days) from the current time and
;                  the time for which the coordinates of the region are given.
;    latitude  = float: Heliographic latitude of input location (degrees)
;    longitude = float: Heliographic longitude of input location (degrees)
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    None
;
; OUTPUTS:
;    new_long = float: New heliographic longitude (degrees)
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES USED:
;    None
;
; PROCEDURE:
;    new_long = long + (14.48 - 2.16 * sin^2(lat)) * difftime
;    Reference: Allen: 2000, Allen's Astrophysical Quantities, Springer, p.362
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 3) then message,'3 inputs required! difftime, lat, long'

if (keyword_set(old)) then old = 1 else old = 0
if (keyword_set(allen)) then allen = 1 else allen = 0
if (keyword_set(hhf)) then hhf = 1 else hhf = 0

slat = sin(lat/!radeg)

div = 1.
if (keyword_set(do_average)) then begin
    old = 1
    allen = 0
    hhf = 1
    div = 2.
endif

new_lng = 0.

;-- Old value:
if (old eq 1) then $
    new_lng = new_lng + lng + (13.33 - 2.7 * slat*slat) * difftime

;-- Allen:
if (allen eq 1) then $
    new_lng = new_lng + lng + (14.48 - 2.16 * slat*slat) * difftime

;-- hhf (Howard, Harvey, and Forgach
if (hhf eq 1) then $
    new_lng = new_lng + lng + 4.95035*(2.894-0.428*slat^2-0.37*slat^4)*difftime

new_lng = new_lng / div

new_lng = new_lng mod 360

if (new_lng gt 180) then new_lng=new_lng-360
if (new_lng lt -180) then new_lng=new_lng+360

return,new_lng

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: diff_rotate.pro,v $
; Revision 3.2  2010/04/28 18:48:25  bernapn1
; Added a 3rd method of calculating the differential rotation. Given the option
; to choose one of the three or the average between two of them
;
; Revision 3.1  2005/01/21 15:28:11  pietro
; Changed the parametes for calculating the differential rotation.
; The new paramenters match what given in the 2000 edition of
; Allen's Astropyisical Quantities.
;
; Revision 3.0  2004/10/27 14:38:17  pietro
; Given revision 3.0
;
; Revision 2.1  2004/07/15 19:55:57  pietro
; Fixed bug in differential rotation formula
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.2  2004/06/16 20:33:09  pietro
; don't know
;
; Revision 1.1  2004/06/15 19:01:27  pietro
; Finished first version
;
;
; Written by Pietro N. Bernasconi JHU/APL
