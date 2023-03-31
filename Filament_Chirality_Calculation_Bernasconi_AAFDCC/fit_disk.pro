;******************************************************************************
;***                                                                        ***
      function fit_disk,P
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    FIT_DISK
;
; PURPOSE:
;    This function is used in the amoeba optimization called by
;    find_disk_parms for the optimization of Sun radius and position of center.
;
; CALLING SEQUENCE:
;    residual = fit_disk(P)
;
; INPUTS:
;    P = fltarr(3): 0) Sun radius; 1) X center position; 2) Y center position
;
; OPTIONAL INPUT PARAMETERS:
;	None
;
; KEYWORD PARAMETERS:
;	None
;
; OUTPUTS:
;    residual = float: residual of fit
;
; ROUTINES CALLED:
;    elipse_mask
;
; COMMON BLOCKS:
;    disk_fit: xquads, yquads, avgrad, xc, yc
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
common disk_fit, xquads, yquads, avgrad, xc, yc, xwd, ywd

mask=elipse_mask(xwd*2+1, 201, p(0), p(0), $
                 xc=xwd+avgrad+p(1)-xc, yc=100+p(2)-yc)
t = total(abs(xquads(*,*,0)-mask))
;tvscl,xquads(*,*,0)-mask

mask=elipse_mask(xwd*2+1, 201, p(0), p(0), $
                 xc=xwd-avgrad+p(1)-xc, yc=100+p(2)-yc)
t = t + total(abs(xquads(*,*,1)-mask))
;tvscl,xquads(*,*,1)-mask,280,0

mask=elipse_mask(201, ywd*2+1, p(0), p(0), $
                 xc=100+p(1)-xc, yc=ywd+avgrad+p(2)-yc)
t = t + total(abs(yquads(*,*,0)-mask))
;tvscl,yquads(*,*,0)-mask,70,0

mask=elipse_mask(201, ywd*2+1, p(0), p(0), $
                 xc=100+p(1)-xc, yc=ywd-avgrad+p(2)-yc)
t = t + total(abs(yquads(*,*,1)-mask))
;tvscl,yquads(*,*,1)-mask,70,70

;print,p, t
return, t

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: fit_disk.pro,v $
; Revision 3.1  2008/11/19 17:20:17  bernapn1
; Added common variables xwd and ywd, to accomodate changes in calling
; routine find_disk_parms
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
