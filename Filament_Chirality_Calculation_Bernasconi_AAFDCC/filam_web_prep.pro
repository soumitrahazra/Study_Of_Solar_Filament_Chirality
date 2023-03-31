;******************************************************************************
;***                                                                        ***
      pro filam_web_prep, date, web_location, RESULTS_PATH = resdir, $
                          verbose=verbose
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    FILAM_WEB_PREP
;
; PURPOSE:
;    This routine is used to create a web page with the latest H-alpha
;    image analyzed and its results.
;
; CALLING SEQUENCE:
;    filam_web_prep, date [, web_location, /verbose]
;
; INPUTS:
;    date = [yyyy,mm,dd] : Date for which the web page is desired to be made
;
; OPTIONAL INPUT PARAMETERS:
;    web_location = string: path where the web page is located. Default ='/.'
;
; KEYWORD PARAMETERS:
;    VERBOSE = flag: if set then prints some debugging information
;
; OUTPUTS:
;    None
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES USED:
;    make_datestring
;
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_elements(resdir) eq 0) then resdir = filam_get_paths(/results)
if (n_elements(web_location) le 0) then web_location='./'

strdate = make_datestring(date,sep='/')

;---- Getting the summary table from the results:
year_month = strcompress(string(date(0)),/rem) + '/'
if (date(1) lt 10) then year_month = year_month + '0'
year_month = year_month + strcompress(string(date(1)),/rem) + '/'
resdir =  resdir + year_month

tmpstrdate = make_datestring(date)
table = file_search(resdir+'*'+tmpstrdate+'*Ha.tab', count = n_tables)

if (n_tables eq 0) then begin
    if (keyword_set(verbose)) then $
      print,'%FILAM_WEB_PREP:No images found for date '+strdate
    return
endif
tab_nam=table(n_tables-1)

;---- Getting the image and making a smaller one:
img_nam = strmid(tab_nam,0,strlen(tab_nam)-4)+'.png'
file_copy, img_nam, web_location+'big_ha.png', /overwrite
spawn,'/usr/bin/convert '+web_location+'big_ha.png -resize 50% '+$
  web_location+'small_ha.png'

;---- Create the html file with the summary: ----------------------------------
;-- Extract the time of the image:
refp=strpos(tab_nam,'_Ha')-6
hour=fix(strmid(tab_nam,refp,2))
mins=fix(strmid(tab_nam,refp+2,2))
sec=fix(strmid(tab_nam,refp+4,2))
;print,hour,mins,sec
strdate = strdate+' &nbsp;&nbsp; ' + $
  make_datestring(hour,mins,sec,sep=':')+' UT'

;-- Opens the files:
openr,templ_unit,web_location+'index_template.txt',/get_lun
openw,index_unit,web_location+'index.html',/get_lun

;---- defines some variables:
s=''
num=0
area=0
a_rat=0.
latit=0.
longit=0.
angle=0.
length=0
n_brbs=0
n_right=0
n_left=0
chir=0

;-- Writes the table caption in the appropriate place:
repeat begin
    readf,templ_unit,s
    printf,index_unit,s
endrep until (strpos(s,'<!-- Here comes the caption: -->') ne -1)
printf,index_unit,'Filaments for &nbsp;&nbsp; '+strdate

;-- Fills in until it comes to the table:
repeat begin
    readf,templ_unit,s
    printf,index_unit,s
endrep until (strpos(s,'<!-- Here comes the list summary: -->') ne -1)

;------------------------
;--- preparing the table: ---
openr,tab_unit,tab_nam,/get_lun

;- skip some lines
repeat readf,tab_unit,s until (strpos(s,'-------------') ne -1)

formextr='(I3,I6,F6.2,14X,2(F7.2),F7.1,I5,4(I4))'
while (EOF(tab_unit) le 0) do begin
    readf,tab_unit,s
    ;- Exits if no filaments are listed
    if (s eq '00') then break

    reads,s,form=formextr,num,area,a_rat,latit,longit,angle,length,n_brbs,$
      n_right,n_left,chir

    ;print,num,area,a_rat,latit,longit,angle,length,n_brbs,n_right,n_left,chir
 
    printf,index_unit,form='(3X,"<tr align=right>")'
    printf,index_unit,form='(5X,"<td class=num>",I6,"</td>")',num
    printf,index_unit,form='(5X,"<td class=lat>",f6.2,"</td>")',latit
    printf,index_unit,form='(5X,"<td class=lng>",f6.2,"</td>")',longit
    printf,index_unit,form='(5X,"<td class=area>",I6,"</td>")',area
    printf,index_unit,form='(5X,"<td class=leng>",I6,"</td>")',length
    printf,index_unit,form='(5X,"<td class=ang>",f6.1,"</td>")',angle
    printf,index_unit,form='(5X,"<td class=n_b>",I6,"</td>")',n_brbs
    printf,index_unit,form='(5X,"<td class=n_r>",I6,"</td>")',n_right
    printf,index_unit,form='(5X,"<td class=n_l>",I6,"</td>")',n_left
    printf,index_unit,form='(5X,"<td class=chir>",I6,"</td>")',chir
    printf,index_unit,form='(3X,"</tr>")'
endwhile
;------------------------


;---- Wrapping up
while (EOF(templ_unit) le 0) do begin
    readf,templ_unit,s
    printf,index_unit,s
endwhile

close,/all
free_lun,templ_unit,index_unit,tab_unit

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: filam_web_prep.pro,v $
; Revision 3.17  2010/05/07 18:41:43  bernapn1
; Removed the call to get_table
;
; Revision 3.16  2010/04/30 19:12:42  bernapn1
; Removed a print left over from debugging
;
; Revision 3.15  2010/04/30 19:01:59  bernapn1
; Now handles the multiple sub-directories "results_path/YYYY/MM/"
; and the slightly different table output
;
; Revision 3.14  2010/04/19 19:30:13  bernapn1
; Now using file_copy instead of spawn and 'rm' to remove a file from file system
;
; Revision 3.13  2008/11/18 18:57:55  bernapn1
; Updated
;
; Revision 3.12  2008/11/07 14:42:29  bernapn1
; Updated
;
; Revision 3.8  2005/04/05 21:55:34  pietro
; Modified the TD tags
;
; Revision 3.7  2005/02/18 22:01:25  pietro
; At end added "temporary" commands to copy files created in the
; /var/ftp/pub/filam directory
;
; Revision 3.6  2005/02/16 22:48:35  pietro
; Fixed a bug in the call to "convert"
;
; Revision 3.5  2005/02/14 21:25:33  pietro
; Added the path for the shell function "convert"
;
; Revision 3.4  2005/02/14 16:16:48  pietro
; Last try to print a log at the end
;
;
; Written by Pietro N. Bernasconi JHU/APL
