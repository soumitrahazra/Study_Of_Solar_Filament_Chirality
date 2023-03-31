;******************************************************************************
;***                                                                        ***
	function addframe,image,frmszi,AVERAGE=aver,BACKGROUND=bkgr,$
			SMOOTH=smth
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;	ADDFRAME
;
; PURPOSE:
;	This function adds a frame around the input image. By default the
;	frame has value zero, but optionally it can be set as the average of
;	the image. A smoothing outside the im age edges can also be applied.
;
; CALLING SEQUENCE:
;	result = addframe(image, [framesize, /AVERAGE, BACKGOUND=background,$
;			SMOOTH=smooth])
;
; INPUTS:
;	image = fltarr(X,Y): image to be added a frame
;
; OPTIONAL INPUT PARAMETERS:
;	framesize = integer: size in pixels of the frame to be added around
;			     the image. Default = 20 pixels
;
; KEYWORD PARAMETERS:
;	AVERAGE = if set then the average of the image is computed and this
;		  value is set for the frame area.
;	BACKGROUND = float : value to be set for the frame area (AVERAGE has
;			     the priority).
;	SMOOTH  = integer: creates a smooth area around the imagte of smooth
;			   size. if just set (value =1) then the smooth area
;			   extends 10 pixels.
;
; OUTPUTS:
;	result = fltarr(X+2*frmsz,Y+2*frmsz): image with added frame
;
; OPTIONAL OUTPUT PARAMETERS:
;	None
;
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written by Pietro N. Bernasconi: Apr 21/24, 2000
;-
;******************************************************************************
frmsz=frmszi
if (n_elements(frmsz) le 0) then frmsz=[20,20]
sz=size(frmsz)
if (sz(0) eq 0) then frmsz=[frmsz,frmsz]

sz=size(image)
szo=[sz(1)+2*frmsz(0),sz(2)+2*frmsz(1)]

if keyword_set(aver) then out=replicate(mean(image),szo(0),szo(1)) $
else $
  if (n_elements(bkgr) gt 0) then out=replicate(float(bkgr),szo(0),szo(1)) $
else out=fltarr(szo(0),szo(1))
out(frmsz(0):frmsz(0)+sz(1)-1,frmsz(1):frmsz(1)+sz(2)-1)=image

if (n_elements(smth) ne 0) then begin
  if (smth eq 1) then smth=10
  mnf=out(0,0)

  ;---- Vertical smoothing
  hanx=rebin(hanning(smth*2),smth*2,sz(2))
  for i=0,sz(2)-1 do begin
    hanx(0:smth-1,i)=hanx(0:smth-1,i)*(image(0,i)-mnf)+mnf
    hanx(smth:*,i)=hanx(smth:*,i)*(image(sz(1)-1,i)-mnf)+mnf
  endfor
  out(frmsz(0)-smth:frmsz(0)-1,frmsz(1):frmsz(1)+sz(2)-1)=hanx(0:smth-1,*)
  out(frmsz(0)+sz(1):frmsz(0)+sz(1)+smth-1,frmsz(1):frmsz(1)+sz(2)-1)=hanx(smth:*,*)

  ;---- Horizontal smoothing
  hany=rotate(rebin(hanning(smth*2),smth*2,sz(1)),1)
  for i=0,sz(1)-1 do begin
    hany(i,0:smth-1)=hany(i,0:smth-1)*(image(i,0)-mnf)+mnf
    hany(i,smth:*)=hany(i,smth:*)*(image(i,sz(2)-1)-mnf)+mnf
  endfor
  out(frmsz(0):frmsz(0)+sz(1)-1,frmsz(1)-smth:frmsz(1)-1)=hany(*,0:smth-1)
  out(frmsz(0):frmsz(0)+sz(1)-1,frmsz(1)+sz(2):frmsz(1)+sz(2)+smth-1)=hany(*,smth:*)

  ;---- Corner snoothing
  hanx=rebin(hanning(smth*2),smth*2,smth*2)
  for i=0,smth*2-1 do begin
    hanx(0:smth-1,i)=hanx(0:smth-1,i)*(hany(0,i)-mnf)+mnf
    hanx(smth:*,i)=hanx(smth:*,i)*(hany(sz(1)-1,i)-mnf)+mnf
  endfor
  out(frmsz(0)-smth:frmsz(0)-1,frmsz(1)-smth:frmsz(1)-1)=hanx(0:smth-1,0:smth-1)
  out(frmsz(0)-smth:frmsz(0)-1,frmsz(1)+sz(2):frmsz(1)+sz(2)+smth-1)=hanx(0:smth-1,smth:*)
  out(frmsz(0)+sz(1):frmsz(0)+sz(1)+smth-1,frmsz(1)-smth:frmsz(1)-1)=hanx(smth:*,0:smth-1)
  out(frmsz(0)+sz(1):frmsz(0)+sz(1)+smth-1,frmsz(1)+sz(2):frmsz(1)+sz(2)+smth-1)=hanx(smth:*,smth:*)

endif

return,out

end
;******************************************************************************
