;******************************************************************************
;***                                                                        ***
      function get_jdate, str_date, DATE_TIME= dt, VERBOSE=verb
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    GET_JDATE
;
; PURPOSE:
;    This routines extract the date informations from a provided string and
;    returns the corresponding julian date
;
; CALLING SEQUENCE:
;    jd = get_jdate(date [, DATE_TIME=tade_time, /VERBOSE])
;
; INPUTS:
;    date = string: preferabely of format "2002-05-18T15:38:27.000Z" if not
;                   it tries different formats but don't count on it ...
;
; OPTIONAL INPUT PARAMETERS:
;	None
;
; KEYWORD PARAMETERS:
;    DATE_TIME = intarr(6): array with the extracted, year, month, day,
;                           hour, minutes, seconds
;
; OUTPUTS:
;    jd = double: julian date of the corresponding date. Returns -1 if it
;                 can not figure out what date format it is
;
; OPTIONAL OUTPUT PARAMETERS:
;	None
;
; CALLED ROUTINES:
;    js2jd, ymds2js, hms2sec
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
sz=size(str_date)
if (sz(0) ne 0) or (sz(1) ne 7) then message,'Input parameter must be a string'
if (strlen(str_date) lt 20) then return,-1d0  ;<-- Format unknown

if (n_elements(verb) le 0) then verb = 0

y = fix(strmid(str_date,0,4))
m = fix(strmid(str_date,5,2))
d = fix(strmid(str_date,8,2))
h = fix(strmid(str_date,11,2))
min = fix(strmid(str_date,14,2))
s = fix(strmid(str_date,17,2))

dt = [y, m, d, h, min, s]

if verb then $
  print,form='("Date = ",I4,"-",I2,"-",I2,4X,I2,":",I2,":",I2," UT")', dt

return,js2jd( ymds2js(y, m, d, hms2sec(h, min, s)) )

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: get_jdate.pro,v $
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.1  2004/08/24 17:52:55  pietro
; Fixed a very small syntax mistake that did not affect the functionality
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.2  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
; Written by Pietro N. Bernasconi JHU/APL: 03/12/04
