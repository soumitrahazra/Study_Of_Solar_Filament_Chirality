;******************************************************************************
;***                                                                        ***
    function helio2pix, date, JULIAN_DATE = juldat, DATE_TIME = dt, $
                        sun_r, lat_long, ARCSECS=arcsec, XY_CENTER = xycent, $
                        SUN_RADIUS = semidiam, C_PATH=c_path
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    HELIO2PIX
;
; PURPOSE:
;    This function converts heliographic (lat,long) coordinates into cartesian
;    (x,y) coordinates either in pixels or in arcseconds
;
; CALLING SEQUENCE:
;    cartesian = helio2pix, date, sun_r, lat_long, /ARCSECS, $
;                          [/JULIAN_DATE, /DATE_TIME, XY_CENTER=xycent, $
;                           C_PATH=c_path]
;
; INPUTS:
;    date = string,double,intarr(6): date for which do the conversion. The
;                 date can be expressed in either string format (default) as
;                 follows: "2002-05-18T15:38:27.000Z", or in julian date, or
;                 in a 6 element array as follows: [yyyy,MM,dd,hh,mm,ss]. To
;                 tell the routine if you are using julian date use the
;                 keyword /JULIAN_DATE. To tell it it is in the array format
;                 select the keyword /DATE_TIME.
;    sun_r = int or float: radius of Sun in pixels.
;    lat_long = fltarr[2,n_points]: array with the coordinates of the points
;                 on the solar surface given in lat, long in degrees
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    ARCSEC = flag: if set then output coordinates are in arcsecs
;    JULIAN_DATE = flag: if set then it tells the routine that the date
;                 parameter was given in julian date
;    DATE_TIME = flag: if set the it tells the routine that the date parameter
;                 is given as a 6 elements array [yyyy,MM,dd,hh,mm,ss].
;    XY_CENTER = fltarr(2): use this to enter the coordinates of the center
;                 if the cartesian coordiante system IN PIXELS (if it is
;                 different from (0,0) ).  This parameter is be used only if
;                 the keyword ARCSEC is NOT set.
;    SUN_RADIUS= float: optional output of the Sun radius in arcsecs
;    C_PATH = string: Directory path where the c executables are located.
;                 Default is given by the call to the routine:
;                 filam_get_paths(/bin)
;
; OUTPUTS:
;    cartesian = fltarr(2, n_points): array with the coordinates converted in
;                 both pixels and arcseconds as follows (for each point):
;                 Default = [X_coord_pix, Y_coord_pix]
;                 Alternatively = [X_coord_arcsec, Y_coord_arcsec]
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES USED:
;    get_jdate, + the external C routine helio2pix needs to be in the c_path
;
; PROCEDURE:
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************

if not(keyword_set(juldat)) then begin
    if (keyword_set(dt)) then begin
        szd=size(date,/dim)
        if (szd(0) ne 6) then $
          message,'Incorrect input date. Must be a 6-elem array'
        date = make_datestring(date(0:2),sep='-')+'T'+$
          make_datestring(date(3:*),sep=':')+'.000Z'
    endif
    szd = size(date)
    if (szd(1) ne 7) then $
      message,'Incorrect input date. Must be in string format'
    juldat = get_jdate(date)
endif else begin
    szd = size(date)
    if ((szd(1) lt 2) and (szd(1) gt 5)) then $
      message,'Incorrect input date. Must be a int, float, or double'
    juldat = date
endelse

if (n_elements(xycent) le 0) then xycent = [0.,0.]

if (n_elements(c_path) le 0) then c_path = filam_get_paths(/bin)

tmpinf  = 'tmp_in.dat'
tmpoutf = 'tmp_out.dat'

;--- Writes a temporary file
openw, unit, tmpinf, /get_lun
printf, unit, form='(d15.6)', juldat
printf, unit, sun_r
printf, unit

lat_long = float(lat_long)
sz=size(lat_long)
if (sz(0) eq 1) then begin
    coords = fltarr(4,1)
    printf, unit, form='(F,F)',lat_long
endif else if (sz(0) eq 2) then begin
    coords = fltarr(4, sz(2))
    for i=0,sz(2)-1 do begin
        printf, unit, form='(F,F)',lat_long(*,i)
    endfor
endif else begin
    coords = fltarr(4, sz(2)*sz(3))
    for i=0,sz(2)-1 do begin
        for j=0,sz(3)-1 do begin
            printf, unit, form='(F,F)',lat_long(*,i,j)
        endfor
    endfor
endelse
close, unit

;--- Calls c program to calculate cartesian coords:
spawn,c_path+'helio2pix '+tmpinf+' '+tmpoutf+' -p'

;--- Reads the temporary outfile:
openr, unit, tmpoutf
readf, unit, semidiam
readf, unit, coords
close, unit
free_lun,unit

file_delete,[tmpinf,tmpoutf]

if (keyword_set(arcsec)) then $
    coords = reform(coords(2:3,*)) $
else begin
    szc = size(coords)
    if (szc(0) eq 1) then $
      coords(0:1) = coords(0:1)+xycent $
    else $
      for i=0,szc(2)-1 do coords(0:1,i) = coords(0:1,i)+xycent
    coords = reform(coords(0:1,*))
endelse

if (sz(0) le 2) then $
    out = coords $
else begin
    out = fltarr(2,sz(2),sz(3))
    count = 0
    for i=0,sz(2)-1 do begin
        for j=0,sz(3) -1 do begin
            out(*,i,j) = coords(*,count)
            count = count + 1
        endfor
    endfor
endelse

return,out

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: helio2pix.pro,v $
; Revision 3.3  2010/04/29 20:56:19  bernapn1
; Added output keyword SUN_RADIUS
;
; Revision 3.2  2010/04/28 18:47:02  bernapn1
; Improved handling of multiple entries
;
; Revision 3.1  2010/04/15 20:13:09  bernapn1
; Added to version control
;
; Written by Pietro N. Bernasconi JHU/APL
