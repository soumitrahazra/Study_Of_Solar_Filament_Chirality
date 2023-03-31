;******************************************************************************
;***                                                                        ***
	function mfilter,imagei,width,limit,SILENT=sil,EDGES=edges
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;	MFILTER
;
; PURPOSE:
;	Apply median filter to an image with window size "width*width". Pixels
;	are only replaced when the median deviates by more then limit.
;
; CALLING SEQUENCE:
;	result=mfilter(image,width,limit,/SILENT,/EDGES)
;
; INPUTS:
;	image = array(x,y) : image to be filtered
;	width  = size of median neighborhood
;	limit = limit above which the value is replaced by the median of
;	        its neighborohood
;
; KEYWORD PARAMETERS:
;	SILENT = If set then the message "NO SUBSITUTIONS" is not printed.
;       EDGES  = If set then it also handels the edges, by adding a frame
;           around the image.
;
; OUTPUTS:
;	result = array(x,y) : filtered image
;
; MODIFICATION HISTORY:
;	Written by Pietro N. Bernasconi, 17 Aug. 1994
;	->  6 Dec. 94, P.N. Bernasconi: check if there are substitutions or not
;       ->  1 Nov. 95, P.N. Bernasconi: Check if the input image is 2-dim
;	->  1 Mar. 96, P.N. Bernasconi: Added Keyword SILENT
;       -> 11 Nov. 03, P.N. Bernasconi: Added Keyword EDGE
;-
;******************************************************************************

;--- Trap tests ---------------------------------------------------------------
if (n_params() ne 3) then message,'3 input parameters are needed'
szi=size(imagei)
if (szi(0) gt 2) then message,'Input image must be 2-dim'

if (keyword_set(edges)) then begin
  cornice = width/2+1
  image = addframe(imagei, cornice, smooth= cornice, /average)
endif else $
  image=imagei
szi=size(image)

out=image
work=median(image,width)

subs=where(abs(image-work) ge limit)
sz=size(subs)

if (sz(0) ne 0) then out(subs)=work(subs) else $
	if not keyword_set(sil) then print,'mfilter: NO SUBSTITUTIONS'

if (keyword_set(edges)) then $
  return,out(cornice:szi(1)-cornice-1,cornice:szi(2)-cornice-1) $
else $
  return,out

end
;******************************************************************************


