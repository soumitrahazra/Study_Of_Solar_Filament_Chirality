function OS_FAMILY, LOWER=LOWER
;+
; NAME:
;	OS_FAMILY
; PURPOSE:
;	Return the current operating system as in !VERSION.OS_FAMILY 
;
; CALLING SEQUENCE
;	result = OS_FAMILY( [ /LOWER] )
; INPUTS: 
;	None
; OUTPUTS:
;	result - scalar string containing one of the four values
;		'Windows','MacOS','vms' or 'unix'
; OPTIONAL INPUT KEYWORD:
;       /LOWER - If set then returns lower-case strings
; NOTES:
;	OS_FAMILY is assumed to be 'unix' if !VERSION.OS is not 'windows',
;		'MacOS' or 'vms'
;
;	To make procedures from IDL V4.0 and later compatibile with earlier
;	versions of IDL, replace calls to !VERSION.OS_FAMILY with OS_FAMILY().	
;
; PROCEDURES CALLED
;	function TAG_EXISTS()
; REVISION HISTORY:
;	Written,  W. Landsman     
;       Version 2, 15 May 2000, Zarro (SM&A/GSFC) - added /LOWER
;-

 if tag_exist(!VERSION, 'OS_FAMILY') then begin
  os=!VERSION.OS_FAMILY
  if keyword_set(lower) then os=strlowcase(os)
  return,os
 endif

 case !VERSION.OS of


'windows': os= 'Windows'
  'MacOS': os= 'MacOS'
    'vms': os= 'vms'
     else: os= 'unix'
 endcase

 if keyword_set(lower) then os=strlowcase(os)
 return,os

 end
