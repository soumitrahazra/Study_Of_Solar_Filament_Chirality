;******************************************************************************
;***                                                                        ***
      pro solar_filaments, infile, data, FILAM_DATA=fil_pars, $
                           OUTPUT_TABLE = out_tab, $
                           DISP_DATA=data_nmsk, INTERACTIVE=interactive,$
                           VERBOSE=verbose, SUN_PARMS = sun_p, $
                           STATUS = status, C_PATH=c_path, $
                           SHOW_ALL_BRBS=show_all_brbs
;***                                                                        ***
;******************************************************************************
;+
; PROJECT: Automated Solar Filament Detection and Characterization
;
; NAME:
;    SOLAR_FILAMENTS
;
; PURPOSE:
;    Identify all filaments in the given file, based on thresholds,
;    display them in a window, and output them to a catalog.
;
; CALLING SEQUENCE:
;    solar_filaments, 'infile.in' [,data, FILAM_DATA=fil_pars, $
;                      OUTPUT_TABLE = output_table, $
;                      DISP_DATA=disp_data, SUN_PARMS=sun_p, /VERBOSE, $
;                      /INTERACTIVE, STATUS = status, C_PATH = c_path]
;
; INPUTS:
;    infile.in = ASCII file with all the input values required to run the
;                solar_filaments program. See the template file template.in
;                for a list and information about the different inputs.
;
; OPTIONAL INPUT PARAMETERS:
;	None
;
; KEYWORD PARAMETERS:
;       FILAM_DATA  = structure_array(nFilaments): structure containing all
;                         the filaments data. See study_filaments for a
;                         detailed description of the structure content.
;       OUTPUT_TABLE = string: name of the oputput table with the results.
;       DISP_DATA   = fltarr(X,Y): Image used for the display. That means,
;                         without any filtering or smoothing.
;       SUN_PARMS   = intarr(7): Solar disk parameters:
;                            0) Sun radius [pix]
;                            1) X coord of center [pix]
;                            2) Y coord of center [pix]
;                            3) Julian Date
;                            4) Sun radius [arcsec]
;                            5) Solar P  angle [deg]
;                            6) Solar B0 angle [deg]
;	VERBOSE     = flag: if set then do some printouts
;       INTERACTIVE = flag: if set then after calculating all the filaments
;                     the code enters an interactive mode that allows to
;                     view different areas with more detail.
;       STATUS      = int: 0 if everything went O.K. 1 otherwise
;       C_PATH = string: Directory path where the c executables are located.
;                    Default is given by the call to the routine:
;                    filam_get_paths(/bin)
;       SHOW_ALL_BRBS = flag: if set then plots all the candidate barbs. For
;                     testing purposes.
;
; OUTPUTS:
;	None
;
; OPTIONAL OUTPUT PARAMETERS:
;	data = fltarr(X,Y): image processed (filtering and smoothing) used
;	       for the actual filament analysis.
;
; IDL PROCEDURES USED:
;    read_input, preprocess, get_jdate, plot_helio_grid, study_filaments,
;    interactive_disp
;
; C PROCEDURES & SHELL SCRIPTS USED:
;    pix2helio, gzip
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
status = 1           ;<-- Initialize status as error
z_buf_flag = 131072  ;<-- bit value for the z-buffer flag

use_z_buf = 0
if ((!d.flags and z_buf_flag) gt 0) then use_z_buf = 1

;------------------------------------------------------------------------------
;--- Print the help page: -----------------------------------------------------
if (n_params() eq 0) then begin
  print,'PURPOSE:'
  print,'  Identify all filaments in the given file, based on thresholds,'
  print,'  display them in a window, and output them to a catalog.'
  print
  print,'CALLING SEQUENCE:'
  print,'  solar_filaments, "infile.in" [,data, FILAM_DATA=fil_pars, $'
  print,'                    DISP_DATA=disp_data, SUN_PARMS=sun_p, /VERBOSE, $'
  print,'                    /INTERACTIVE]'
  print
  print,'INPUTS:'
  print,'  infile.in = ASCII file with all the input values required to run'
  print,'                 the solar_filaments program. See the template file'
  print,'                 template.in for a list and information about the'
  print,'                 different inputs.'
  print
  print,'KEYWORD PARAMETERS:'
  print,'  FILAM_DATA  = structure_array(nFilaments): structure containing all'
  print,'                 the filaments data. See study_filaments for a'
  print,'                 detailed description of the structure content.'
  print,'  DISP_DATA   = fltarr(X,Y): Image used for the display. That means,'
  print,'                  without any filtering or smoothing.'
  print,'  SUN_PARMS   = intarr(3): Solar disk parameters:'
  print,'                  0) Sun radius; 1) X coord of center;'
  print,'                  2) Y coord of center'
  print,'  VERBOSE     = flag: if set then do some printouts'
  print,'  INTERACTIVE = flag: if set then after calculating all the filaments'
  print,'                  the code enters an interactive mode that allows to'
  print,'                  view different areas with more detail.'
  print
  print,'OPTIONAL OUTPUT PARAMETERS:'
  print,'  data = fltarr(X,Y): image processed (siltering and smoothing) used'
  print,'	    for the actual filament analysis.'
  return    
endif
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

if (n_elements(verbose) le 0) then verbose = 0

if (n_elements(c_path) le 0) then c_path=filam_get_paths(/bin) $
else if (size(c_path, /type) ne 7) then begin
    print,'% SOLAR_FILAMENTS: C_PATH is not a string!'
    c_path=filam_get_paths(/bin)
    print,'                reset to directory '+c_path
endif


;------------------------------------------------------------------------------
;--- Reads the input from the input file and handles it: ----------------------

;--- Reads the input file and puts in the structure 'pars':
pars = read_input(infile, verbose=verbose)
szp = size(pars,/str)
if (szp.type_name ne 'STRUCT') then begin
    print,'% SOLAR FILAMENTS: Premature exit'
    return
endif

;--- Do a test of the input file:
pp=strpos(pars.image,'.')
if (pp lt 0) then begin
  print,'WARNING input file may not be in fits format! Proceeding anyway ...'
  pp = strlen(pars.image)
endif else begin
  fltyp = strlowcase(strmid(pars.image,pp+1))
  if ( (strpos(fltyp, 'fits') eq -1) and (strpos(fltyp, 'fts') eq -1) ) then $
    print,'WARNING input file may not be in fits format! Proceeding anyway ...'
endelse

;--- Builds the input file name with full path:
fits_img = pars.in_path+pars.image

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
;--- Data preprocessing: ------------------------------------------------------
data = preprocess(fits_img, pars.i_thrsh, pars.startx, pars.starty, $
                  pars.width, pars.height, sun_parms=sun_p, DATE_STR=date, $
                  fits_h=header, background=pars.u_thrsh+10, $
                  MASK_ABOVE=pars.mask_deg, $
                  smooth=pars.smth_img, MEDIAN_W=3, MEDIAN_L=80, $
                  data_unmasked=data_nmsk, fit_surf=pars.fix_bgr, $
                  sun_radius=970, out_size=2048, $
                  /equalize, verbose=verbose, C_PATH=c_path)

;--- Exit if no data returned
sz=size(data, /dim)
if (sz(0) eq 0) then begin
    print,'% SOLAR FILAMENTS: Premature exit'
    return
endif

;--- Gets the OBSERVATORY information:
origin = strcompress(sxpar(header, 'ORIGIN'),/rem)
if (strpos(origin,'KANZ') ne -1) then origin ='KANZELHOEHE OBSERVATORY' 

;--- Gets the INSTRUMENT information:
instrument = strcompress(sxpar(header, 'INSTRUME', count=count),/rem)
if (count lt 1) then $
  instrument = strcompress(sxpar(header, 'TELESCOP', count=count),/rem)

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
;--- Display the selected data: -----------------------------------------------
disp_width  = fix(pars.disp_dim*sz(0))
disp_height = fix(pars.disp_dim*sz(1))

;- Creates the window only if it does not exist or its size is not correct:
if use_z_buf then begin
    ;-- set the device resolution if it is a z-buffer
    device,set_resolution=[disp_width,disp_height], $
           set_character_size=[6,10]
    erase
endif else $
if ( (!d.window eq -1) or $
     (!d.x_size ne disp_width) or $
     (!d.y_size ne disp_height)) then $
  window, title='Solar Filaments analysis', xs=disp_width, ys=disp_height

factor = sz(0)/float(disp_width)
disp_img = congrid(data_nmsk, disp_width, disp_height, /int)
;- Handle the color problem:
if (!d.n_colors gt 256) then begin
    true_color = 1
    green_c = fsc_color('green')
    cyan_c = fsc_color('cyan')
endif else begin
    true_color = 0
    common colortab, c_tab, grey_lim, c_red, c_green, c_blue
    c_tab = make_colortab(grey_limit=grey_lim, red=c_red, green=c_green, $
                          blue=c_blue)
    green_c = ctb_color('green',c_tab)
    cyan_c = ctb_color('cyan',c_tab)
endelse
tv,bytscl(disp_img, min=-1500, max=1500, top = grey_lim)

;--- Plot the Solar grid:
disk_center = intarr(2)
disk_center(0) = round(sun_p(1)) - pars.startx
disk_center(1) = round(sun_p(2)) - pars.starty
plot_helio_grid, sun_p(0)/factor, sun_c=disk_center/factor, B0=sun_p(6), $
  /noer, color=green_c, line=2
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
;--- Analyzes the data to find suitable filaments and does some display: ------
fil_pars = study_filaments(data, pars.i_thrsh, pars.min_area, $
                             factor, CLUSTERS = clusters, $
                             U_THRESHOLD = pars.u_thrsh, $
                             DISK_CENTER = disk_center, $
                             SHOW_PIX=pars.show_pix, $
                             SHOW_BOUND=pars.show_bnd, $
                             REQ_SLOPE = pars.barb_ang, $
                             MAX_DIST = pars.max_dist, $
                             SHOW_ALL_BRBS=show_all_brbs, $
                             SHOW_BBOX=pars.show_bbx,$
                             SHOW_SPINE=pars.show_spn, VERBOSE=verbose)

;--- Gets the number of filaments identified:
szf_d=n_elements(fil_pars)

;--- Redraws all the filaments: ---
if 1 then begin
    draw_image, fil_pars, data_nmsk, sun_p, 0, pars.disp_dim, /as_is,$
      XOFF=pars.startx, YOFF=pars.starty, XSZ=pars.width, YSZ=pars.height,$
      SHOW_BOUND=pars.show_bnd, SHOW_SPINE=pars.show_spn, $
      SHOW_ALL_BRBS=show_all_brbs, SHOW_BBOX=pars.show_bbx
endif
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
;--- Performs the filament tracking in the past: ------------------------------
have_track = 0
if ((pars.do_track) and (szf_d gt 0)) then begin
    if verbose then begin
        print
        print,'Tracking filaments in past images ...'
    endif

    track_pars = track_filaments(fil_pars, sun_p, VERBOSE=verbose)

    sz_tr = size(track_pars,/str)
    if (sz_tr.type_name eq 'STRUCT') then have_track = 1
endif
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;---- Generating the outputs:
;----------------------------
print
print,'OUTPUTS:'
print,'--------'

;------------------------------------------------------------------------------
;--- Generates some useful parameters: ----------------------------------------

;--- Conversion factor arcseconds per pixel. Is derived from the fits header
;    information about how many pixels is the solar radius in the image and
;    how many arcsecs is the Sun radius that day:
acsxpix = sun_p(4)/sun_p(0)           ;<-- sun_rad_arcsec/sun_rad_pix
pixunit_str = 'milliRs'
pixunit  = 1000./sun_p(0)             ;<-- pixel unit in thousands of Rs
areunit_str = 'millihemisphere'
areaunit = 1000./sun_p(0)/sun_p(0)/!PI ;<-- area unit in milli hemisphere

;--- Calulate the Julian Date. Actual purpose is for extracting the date_elem:
jul_dat = get_jdate(date, date_time = date_elem)

;--- Generates some name prefixes:
;- Date prefix:
date_prefix = make_datestring(date_elem(0:2))+'_'+$
  make_datestring(date_elem(3:5))

;- Prefix for the output files:
if (jul_dat gt 0) then $
  prefix = date_prefix + '_Ha' $
else $
  prefix = strmid(pars.image,0,pp)
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
;--- Creates the results output directory & Checks that it exist: -------------
year = strcompress(string(date_elem(0)),/rem) + '/'

year_month = year
if (date_elem(1) lt 10) then year_month = year_month +'0'
year_month = year_month+strcompress(string(date_elem(1)),/rem) + '/'

res_path = pars.out_path + year_month
if not(file_test(res_path,/directory)) then file_mkdir,res_path
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
;--- Generates the output in an ASCII file: -----------------------------------
if pars.save_tab then begin
    out_tab = res_path + prefix + '.tab'
    if verbose then print,'tab file   = "',out_tab,'"'

    openw, outf, out_tab, /get_lun
    printf, outf, 'Ha Filament statistics for:'
    printf, outf, '               Date : ', date_prefix
    if (jul_dat ne -1) then $
      printf, outf, form='("        Julian Date :", d13.4)',jul_dat $
    else $
      printf, outf

    printf, outf, '        Observatory : ', origin

    printf, outf,     form='("Sun radius [pixels] :", f8.2)',sun_p(0)

    if (szf_d ne 0) then begin
        coords = pix2helio(jul_dat, /julian, sun_p(0), fil_pars.coords,$
                           SUN_R = semidiam)

        printf, outf, format='("Sun radius [arcsec] :", d8.2)', semidiam
        printf, outf, '         Area Units : ' + areunit_str
        printf, outf, '       Length Units : ' + pixunit_str
        printf, outf
        printf, outf, format='("  #   Area   A_rat   Xp   Yp    Lat   Long",$)'
        printf, outf, format='("     Ang  Len #Brb #R  #L Chir",$)'
        if (pars.do_track) then $
          printf, outf, '   Track: jday   YYYYMMDD_hhmmss  FW/MR/SP' $
        else printf, outf

        printf,outf, format='("-------------------------------------------",$)'
        printf,outf, format='("-----------------------------",$)'
        if (pars.do_track) then $
          printf,outf,'--------------------------------------------' $
        else printf,outf

        for i=0,szf_d-1 do begin
            fp = fil_pars(i)
            printf, outf, $
              format = '(I3,F8.4,F6.2,X,2(I5),X,2(F7.2), F7.1, $)',$
              i+1, fp.area*areaunit, fp.area_rat, fp.coords, coords(*, i), $
              fp.angle
            printf, outf, format = '(I5, I4, I4, I4, I4,$)',$
              fp.length * pixunit, fp.nbarbs, fp.nRight, fp.nLeft, fp.chir

            if (have_track) then begin
                tr = track_pars(i)
                if (tr.found gt 0) then begin
                    strdate = make_datestring(tr.date(0:2))+'_'+$
                      make_datestring(tr.date(3:5))
                    printf, outf, format='(d16.4,2X,A,$)',$
                      tr.jdate, strdate

                    if (tr.n_follow gt 0) then begin
                        printf, outf,format='("  FW",I3)',tr.follow.num
                        continue
                    endif

                    if (tr.n_merge gt 0) then begin
                        printf, outf, format='("  MR",$)'
                        for j=0,tr.n_merge-1 do $
                          printf, outf, format='(I3,$)',tr.merge(j).num
                    endif

                    if (tr.n_split gt 0) then begin
                        if (tr.n_merge gt 0) then printf,outf,format='("  ",$)'
                        printf, outf,format='("  SP",$)'
                        for j=0,tr.n_split-1 do $
                          printf, outf, format='(I3,$)',tr.split(j).num
                    endif

                    printf, outf

                endif else printf, outf, '      NEW'
            endif else begin
                if (pars.do_track) then $
                  printf, outf, '      NO_DATA' $
                else $
                  printf,outf
            endelse
        endfor


    endif else begin
        printf, outf
        printf, outf
        printf, outf, 'No filaments detected!'
        printf, outf, '----------------------'
        printf, outf, '00'
    endelse
    close, outf
    free_lun, outf
endif
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
;--- Saves the clusters mask and the sun_p in an IDL save file ----------------
if ( (pars.save_msk) and (szf_d gt 0) ) then begin
    if verbose then print,'mask file  = "',res_path+prefix+'_msk.idl','"'

    clusters1 = clusters
    sun_p1 = sun_p
    save, file = res_path+prefix+'_msk.idl', clusters1, sun_p1, /compress
endif
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
;--- Saves image in a png file: -----------------------------------------------
if pars.save_png then begin
    png_file = res_path + prefix +'.png'
    if verbose then print,'png file   = "',png_file,'"'

    date_string=make_datestring(date_elem(0:2),separ='/')+'   UT '
    for i=3,5 do begin &$
        if (date_elem(i) lt 10) then date_string=date_string+'0' &$
        date_string=date_string+strcompress(string(date_elem(i)),/rem) &$
        if (i ne 5) then date_string=date_string+':' &$
    endfor
    xyouts,10,27,origin,/dev,color=cyan_c,size=1.5
    xyouts,10,10,date_string,/dev,color=cyan_c,size=1.5

    if true_color then $
      write_png,png_file,tvrd(true=1) $
    else $
      write_png,png_file,tvrd(),c_red,c_green,c_blue
endif
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;--- Saves a png file for each individual filament: ---------------------------
if ((pars.save_fil) and (szf_d gt 0)) then begin
    if verbose then print,'fils png in: "',res_path + prefix,'_XX.png"'

    magnif = 2
    for i=0,szf_d-1 do begin
        if (i+1 lt 10) then filID='0' else filID=''
        filID=filID+strcompress(string(i+1),/rem)
        fil_png = res_path + prefix+'_'+filID+'.png'

        ll=fil_pars(i).bbox(*,0)
        ur=fil_pars(i).bbox(*,1)
        xsz = (ur(0)-ll(0))+1
        ysz = (ur(1)-ll(1))+1

        if (xsz lt 99) then begin
            dlt = (100-xsz)/2
            ll(0) = ll(0)-dlt
            ur(0) = ur(0)+dlt
            xsz = (ur(0)-ll(0))+1
        endif
        if (ysz lt 99) then begin
            dlt = (100-ysz)/2
            ll(1) = ll(1)-dlt
            ur(1) = ur(1)+dlt
            ysz = (ur(1)-ll(1))+1
        endif

        if use_z_buf then begin
            ;-- set the device resolution if it is a z-buffer
            device,set_resolution=[xsz*magnif,ysz*magnif], $
              set_character_size=[6,10]
            erase
        endif else $
          window, 1, xs=xsz*magnif, ys=ysz*magnif

        draw_image, fil_pars(i), data_nmsk, sun_p, 1, magnif, $
          XOFF = ll(0), YOFF = ll(1), XSZ = xsz, YSZ = ysz, $
          SHOW_BOUND=pars.show_bnd, SHOW_SPINE=pars.show_spn, $
          SHOW_ALL_BRBS=show_all_brbs, SHOW_BBOX=pars.show_bbx

        if true_color then $
          write_png,fil_png,tvrd(true=1) $
        else $
          write_png,fil_png,tvrd(),c_red,c_green,c_blue

    endfor
    if not(use_z_buf) then wdelete,1
endif
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
;--- Generates the VOEvent XML output files -----------------------------------
if ( (pars.do_voe) and (szf_d gt 0) ) then begin

    ;--- Checks if SSW environment exist. VOEvents can not be generated
    ;    without SSW:
    defsysv, '!SSW', exist=is_there
    if not(is_there) then begin
        print,'    SSW environment missing! ==> can not generate VOEvents!'
        goto,skip_voevent
    endif

    ;--- Creates the final VOE path & Checks that the directory exist:
    voe_path = pars.voe_path + year
    if not(file_test(voe_path,/directory)) then file_mkdir,voe_path
    voe_path = pars.voe_path + year_month
    if not(file_test(voe_path,/directory)) then file_mkdir,voe_path
    
    ;--- Checks if VOEvents for the same image have already been created:
    voe_list = file_search(voe_path+'*'+date_prefix+'*', count = n_voe_files)
    if (n_voe_files gt 0) then begin
        print,'File system already has VOEvents files for this image'

        if not(pars.voe_update) then begin
            print,'    ==> will not generate new ones!'
            goto, skip_voevent
        endif

        print,'    ==> will use same names but replace XML content!'
    endif

    if verbose then print,'VOEvents in: "',voe_path,'"'

    ;--- Computes current date/time for DateRun:
    caldat,systime(/UTC,/julian),mm,dd,yy,hh,minut,secs
    tmp = strcompress(string(mm),/rem)
    if (mm lt 10) then mm = '0'+ tmp else mm = tmp
    tmp = strcompress(string(dd),/rem)
    if (dd lt 10) then dd = '0'+ tmp else dd = tmp
    tmp = strcompress(string(hh),/rem)
    if (hh lt 10) then hh = '0'+ tmp else hh = tmp
    tmp = strcompress(string(minut),/rem)
    if (minut lt 10) then minut = '0'+ tmp else minut = tmp
    tmp = strcompress(string(round(secs)),/rem)
    if (secs lt 10) then secs = '0'+ tmp else secs = tmp  
    date_run_string = strcompress(string(yy),/rem) + '-' + $
      mm + '-' + dd  + 'T' + hh + ':' + minut + ':' + secs

    ;--- Creates the VOEvent structure:
    fil_voe_str = struct4event('FI')

    ;--- Patch to make sure each new single filament VOEVENT is unique
    tmp_id = fil_voe_str.required.KB_archivID

    ;--- Fill in parameters common to all filaments: --------------------------
    fil_voe_str.required.event_coordUnit = 'arcsec'
    fil_voe_str.required.event_endTime   = date ;<-- when Ha was taken
    ;fil_voe_str.optional.event_mapURL    = TBD
    fil_voe_str.required.event_c1error   = 10.0 ;<-- Rough estimation
    fil_voe_str.required.event_c2error   = 10.0 ;<-- Rough estimation
    ;fil_voe_str.optional.event_clippedSpatial  = 'T'
    ;fil_voe_str.optional.event_clippedTemporal = 'T'
    fil_voe_str.optional.event_testFlag  = 'F'
    fil_voe_str.required.frm_contact     = 'Pietro N. Bernasconi - '+$
                                           'pietro.bernasconi@jhuapl.edu'
    fil_voe_str.required.frm_dateRun     = date_run_string
    fil_voe_str.required.frm_humanFlag   = 'F'
    fil_voe_str.required.frm_identifier  = 'Feature Finding Team, '+$
      'Pietro Bernasconi'
    ;fil_voe_str.required.frm_identifier  = 'Pietro Bernasconi'
    fil_voe_str.required.frm_institute   = 'JHU/APL'
    fil_voe_str.required.frm_name        = 'AAFDCC'

    if (pars.smth_img eq 0) then $
      smth_img = 'NO' $
    else $
      smth_img = strcompress(string(pars.smth_img),/rem)+'pix_box'
    if (pars.fix_bgr eq 0) then fixbgr = 'NO' else fixbgr = 'YES'
    fil_voe_str.required.frm_paramSet   = $
      'Smooth_image='+smth_img+';  Fix_image_background='+fixbgr+$
      ';  Mask_out_beyond='+strcompress(string(pars.mask_deg,form='(F6.1)'),$
                                        /rem)+'deg'+$
      ';  Identification_threshold='+strcompress(string(pars.i_thrsh),/rem)+$
      ';  Fil_boundary_threshold='+strcompress(string(pars.u_thrsh),/rem)+$
      ';  Fil_min_area='+strcompress(string(pars.min_area),/rem)+'pix'+$
      ';  Barb_min_angle='+strcompress(string(pars.barb_ang,format='(F6.3)'),$
                                       /rem)+'deg'+$
      ';  Fil_dist_merge_thresh='+strcompress(string(pars.max_dist),/rem)+'pix'

    ;********************************************
    fil_voe_str.optional.frm_VersionNumber = 6.3 ;<-- UPDATE IT WHEN NEEDED!
    ;********************************************
    fil_voe_str.required.frm_URL = 'http://helio.cfa.harvard.edu/sdosc/' ;<-- temporary

    fil_voe_str.required.OBS_observatory = origin
    fil_voe_str.required.OBS_channelID   = 'H-alpha'
    fil_voe_str.required.OBS_instrument  = instrument
    fil_voe_str.required.OBS_meanWavel   = 6563
    fil_voe_str.required.OBS_wavelUnit   = 'Angstrom'

    fil_voe_str.optional.chainCodeType   = 'ordered list'
    fil_voe_str.optional.area_unit       = areunit_str
    ;fil_voe_str.optional.dataPrepURL    = 'TBD'
    fil_voe_str.optional.event_pixelUnit = $
                          strcompress(string(acsxpix),/rem)+' arcsec/pix'
    fil_voe_str.optional.FI_lengthUnit   = pixunit_str
    ;--------------------------------------------------------------------------

    ;--- Loops through all filaments to fill in specific parameters: ----------
    ;--- First opens two ASCII files where to dump the XML file names:
    openw, unit, pars.voe_path+'VOE_list.txt',/get_lun,/append
    openw, unit1, voe_path+date_prefix+'_VOE_list.txt',/get_lun

    for i=0,szf_d-1 do begin
        fil_voe_str.required.event_startTime = date

        chirstr = 'ambiguous/uncertain'
        if (fil_pars(i).chir eq 1) then $
          chirstr = 'right handed' $
        else if (fil_pars(i).chir eq -1) then $
          chirstr = 'left handed'
        fil_voe_str.description = 'On disk H-alpha filament of '+chirstr+$
          ' chirality.'

        ;--- Clears the reference arrays:
        fil_voe_str.reference_names = strarr(20)
        fil_voe_str.reference_links = strarr(20)
        fil_voe_str.reference_types = strarr(20)
        ref_ind = 0

        ;fil_voe_str.required.event_probability = TBD
        fil_voe_str.required.KB_ArchivID = tmp_id

        ;--- Fill the fil coordinates into the VOEvent structure:
        fil_voe_str.required.event_coord1 = fil_pars(i).coords(0)*acsxpix
        fil_voe_str.required.event_coord2 = fil_pars(i).coords(1)*acsxpix

        ;--- Fill the bounding box data into the VOEvent structure:
        fil_voe_str.required.boundBox_C1LL = $
          (fil_pars(i).bbox(0,0)-sun_p(1))*acsxpix
        fil_voe_str.required.boundBox_C2LL = $
          (fil_pars(i).bbox(1,0)-sun_p(2))*acsxpix
        fil_voe_str.required.boundBox_C1UR = $
          (fil_pars(i).bbox(0,1)-sun_p(1))*acsxpix
        fil_voe_str.required.boundBox_C2UR = $
          (fil_pars(i).bbox(1,1)-sun_p(2))*acsxpix

        ;--- Fill the boundary chain code into the VOEvent structure:
        ;- Choosing only 1 out of 3 points in the boundary
        bnd = fil_pars(i).bnd(*,0:fil_pars(i).nbnd_p-1:3)
        sz_bnd = size(bnd,/dim)
        nbnd_p = sz_bnd(1)
        fil_voe_str.optional.bound_CCNsteps = nbnd_p
        fil_voe_str.optional.bound_CCStartC1 = (bnd(0,0)-sun_p(1))*acsxpix
        fil_voe_str.optional.bound_CCStartC2 = (bnd(1,0)-sun_p(2))*acsxpix
        ccs = ''
        for j = 0, nbnd_p-1 do begin
            xx = (bnd(0,j) - sun_p(1)) * acsxpix
            yy = (bnd(1,j) - sun_p(2)) * acsxpix
           ccs = ccs + strcompress(string(xx, form = '(f9.2)'),/rem) + ',' + $
              strcompress(string(yy, form = '(f9.2)'),/rem)
            if (j ne nbnd_p-1) then ccs = ccs + ','
        endfor
        fil_voe_str.optional.bound_ChainCode = ccs

        ;--- Fill the spine skeleton chain code into the VOEvent structure:
if 0 then begin  ;<-- for now the skeleton export is bypassed
        nskel_s = fil_pars(i).nverts  ;<-- number of segments in skeleton
        fil_voe_str.optional.skel_Nsteps = nskel_s
        fil_voe_str.optional.skel_StartC1 = $
          (fil_pars(i).spine(0,0)-sun_p(1))*acsxpix
        fil_voe_str.optional.skel_StartC2 = $
          (fil_pars(i).spine(1,0)-sun_p(2))*acsxpix
        ccs = ''
        for j = 0, nskel_s-1 do begin  &$
            xx = (fil_pars(i).spine(0,j) - sun_p(1)) * acsxpix  &$
            yy = (fil_pars(i).spine(1,j) - sun_p(2)) * acsxpix  &$
            ccs = ccs + strcompress(string(xx, form = '(f9.2)'),/rem) + ',' +$
              strcompress(string(yy, form = '(f9.2)'),/rem)  &$
            if (j ne nskel_s-1) then ccs = ccs + ','  &$
        endfor

        fil_voe_str.optional.skel_ChainCode = ccs
        ;fil_voe_str.optional.skel_Curvature = TBD
      endif

        ;--- Fill the area parameters into the VOEvent structure:
        ;fil_voe_str.optional.area_atDiskCenter = TBD
        ;fil_voe_str.optional.area_atDiskCenterUncert = TBD
        fil_voe_str.optional.area_raw = fil_pars(i).area * areaunit
        ;fil_voe_str.optional.area_uncert = TBD
        fil_voe_str.optional.event_nPixels = fil_pars(i).area

        ;--- Fill in filament specific VOEvent entry:
        fil_voe_str.optional.FI_length    = fil_pars(i).length * pixunit
        fil_voe_str.optional.FI_tilt      = fil_pars(i).angle
        fil_voe_str.optional.FI_barbsTot  = fil_pars(i).nbarbs
        fil_voe_str.optional.FI_barbsR    = fil_pars(i).nright
        fil_voe_str.optional.FI_barbsL    = fil_pars(i).nleft
        fil_voe_str.optional.FI_chirality = fil_pars(i).chir

        ;--- Fill in coodinates of start and end points of barbs
        sc1 = ''  &  sc2 = ''
        ec1 = ''  &  ec2 = ''
        for j = 0, fil_pars(i).nbarbs - 1 do begin
           sc1 = sc1 + $
             strcompress(string(round((fil_pars(i).brb_coord(0,1,j)-sun_p(1))*$
                                      acsxpix*100)/100.,form='(f10.2)'),/rem)
           sc2 = sc2 + $
             strcompress(string(round((fil_pars(i).brb_coord(1,1,j)-sun_p(2))*$
                                      acsxpix*100)/100.,form='(f10.2)'),/rem)
           ec1 = ec1 + $
             strcompress(string(round((fil_pars(i).brb_coord(0,0,j)-sun_p(1))*$
                                      acsxpix*100)/100.,form='(f10.2)'),/rem)
           ec2 = ec2 + $
             strcompress(string(round((fil_pars(i).brb_coord(1,0,j)-sun_p(2))*$
                                      acsxpix*100)/100.,form='(f10.2)'),/rem)
            if (j ne (fil_pars(i).nbarbs - 1) ) then begin
                sc1 = sc1 + ','  &  sc2 = sc2 + ','
                ec1 = ec1 + ','  &  ec2 = ec2 + ','
            endif
        endfor
        if (strlen(sc1) eq 0) then sc1 = ' ' 
        if (strlen(sc2) eq 0) then sc2 = ' ' 
        if (strlen(ec1) eq 0) then ec1 = ' ' 
        if (strlen(ec2) eq 0) then sc2 = ' ' 
        fil_voe_str.optional.FI_barbsStartC1 = sc1
        fil_voe_str.optional.FI_barbsStartC2 = sc2
        fil_voe_str.optional.FI_barbsEndC1   = ec1
        fil_voe_str.optional.FI_barbsEndC2   = ec2

        ;--- Fills the tracking information (if exists)
        if (have_track) then begin
            tr = track_pars(i)
            if (tr.found) then begin
                ;fil_voe_str.required.event_startTime = tr.datestr

                if ( (tr.n_follow gt 0) and $
                     (strlen(tr.follow.ivo) gt 0) ) then begin
                    fil_voe_str.reference_names(ref_ind) = 'Edge'
                    fil_voe_str.reference_links(ref_ind) = tr.follow.ivo
                    fil_voe_str.reference_types(ref_ind) = 'follows'
                    ref_ind = ref_ind + 1
                endif

                if (tr.n_merge gt 0) then for jj=0,tr.n_merge -1 do begin
                    if (strlen(tr.merge(jj).ivo) gt 0) then begin
                        fil_voe_str.reference_names(ref_ind) = 'Edge'
                        fil_voe_str.reference_links(ref_ind) = tr.merge(jj).ivo
                        fil_voe_str.reference_types(ref_ind) = 'merges_from'
                        ref_ind = ref_ind + 1
                    endif
                endfor

                if (tr.n_split gt 0) then for jj=0,tr.n_split -1 do begin
                    if (strlen(tr.split(jj).ivo) gt 0) then begin
                        fil_voe_str.reference_names(ref_ind) = 'Edge'
                        fil_voe_str.reference_links(ref_ind) = tr.split(jj).ivo
                        fil_voe_str.reference_types(ref_ind) = 'splits_from'
                        ref_ind = ref_ind + 1
                    endif
                endfor

            endif
        endif

        ;**** Generating the VOEvent XML file *********************************
        ;- Unique identifier for this filament:
        if (i+1 lt 10) then filID='0' else filID=''
        filID=filID+strcompress(string(i+1),/rem)

        ;- Exports the event: --------------------------------
        is_update = ''
        if ( (n_voe_files gt 0) and (i+1 le n_voe_files-1))then begin
            ;- Do this if a previous VOEvent was generated
            outfil_voe = voe_list(i+1)
            fil_voe_str.required.kb_archivid = get_voe_ivorn(outfil_voe)
            file_delete,outfil_voe  ;<-- to prevent a warning from export_event
            export_event,fil_voe_str, /write, outdir='',$
              outfil_voevent=outfil_voe, filenameout = voe_nam
            is_update = 'U_'
        endif else begin
            ;- Do this if no previous VOEvents were generated
            export_event,fil_voe_str,/write, outdir=voe_path,$
              suff=date_prefix+'_'+filID, filenameout = voe_nam
        endelse
        ;------------------------------------------------------

        ;- The printings here below remove the paths:
        printf,unit,is_update+$
          strmid(voe_nam,strpos(voe_nam,year_month,/reverse_se))
        printf,unit1,strmid(voe_nam,strpos(voe_nam,'/',/reverse_se)+1)
        ;**********************************************************************

      endfor
      close,unit,unit1
      free_lun, unit,unit1
    ;--------------------------------------------------------------------------
    endif
skip_voevent:
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
;--- Writes gathered data in a .dat output file: ------------------------------
if ( (pars.save_dat) and (szf_d gt 0) ) then begin
  out_dat  = res_path + prefix +'.dat'
  if verbose then print,'dat file   = "',out_dat,'"'

  openw, outf, out_dat, /get_lun
    
  printf, outf, fits_img    ;<-- Name of the input fits file
  printf, outf, sun_p       ;<-- solar parameters
  
  mxnbnd   = max(fil_pars.nbnd_p)  ;<-- max # of boundary points
  mxnverts = max(fil_pars.nverts)  ;<-- max # of number of vertices
  mxnbarbs = max(fil_pars.nbarbs)  ;<-- max # of number of barbs
  printf, outf, szf_d, mxnbnd, mxnverts, mxnbarbs
  
  ;--- print the data filament by filament:
  for i=0,szf_d-1 do begin
    printf, outf
    printf, outf, fil_pars(i).area
    printf, outf, fil_pars(i).coords
    printf, outf, fil_pars(i).bnd(*,0:mxnbnd-1)
    printf, outf, fil_pars(i).nbnd_p
    printf, outf, fil_pars(i).spine(*,0:mxnverts-1)
    printf, outf, fil_pars(i).angle
    printf, outf, fil_pars(i).length
    printf, outf, fil_pars(i).nverts
    printf, outf, fil_pars(i).brb_coord(*,*,0:mxnbarbs-1)
    printf, outf, fil_pars(i).barbdir(0:mxnbarbs-1)
    printf, outf, fil_pars(i).nbarbs
    printf, outf, fil_pars(i).nright
    printf, outf, fil_pars(i).nleft
    printf, outf, fil_pars(i).chir
  endfor
  
  close, outf
  free_lun, outf
  
  ;--- compress the data since it is in ASCII and highly compressable:
  spawn, '/bin/gzip -f '+out_dat
endif
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;--- Starts interactive display: ----------------------------------------------
if keyword_set(interactive) then w_interactive_disp, fil_pars, data_nmsk,$
    SUN_PARAMS=sun_p, MAGN=pars.disp_dim, SHOW_ALL_BRBS=show_all_brbs

status = 0    ;<-- OK esit status, everything went fine.
print, 'End solar_filaments'
end

;******************************************************************************

;  MODIFICATION HISTORY:
; $Log: solar_filaments.pro,v $
; Revision 3.26  2010/10/27 20:56:06  bernapn1
; New version number in VOEvents to 6.3
;
; Revision 3.25  2010/10/27 19:50:20  bernapn1
; Added call to as_is keyword to call of subroutine draw_image
;
; Revision 3.24  2010/05/10 17:23:42  bernapn1
; Mofified output for both table and VOEvents: Now the area is given in
; millihemisphere, and the length is given in milliRs
;
; Revision 3.23  2010/05/06 17:56:23  bernapn1
; Updated version number to 6.0
;
; Revision 3.22  2010/05/06 14:33:50  bernapn1
; Several changes and improvements.  Now using a new technique to track
; filaments
;
; Revision 3.21  2010/04/15 13:46:05  bernapn1
; Changed version #
;
; Revision 3.20  2010/04/13 21:03:30  bernapn1
; Improved merging of filaments (written new routine).
; Improved way to remove small detected filaments.  Now before removing one it
; checks its area ratio. If it is a skinny one then it keeps it.
;
; Revision 3.19  2010/04/02 19:11:06  bernapn1
; Removed absolute path in call to gzip
;
; Revision 3.18  2010/03/30 14:13:56  bernapn1
; *** empty log message ***
;
; Revision 3.17  2010/03/29 21:11:19  bernapn1
; Added improvements:
;   1) Now the output file names consistently have YYYYMMDD instead of the
;      old YYYY_MM_DD
;   2) Now there is the possiblity to save in addition to the full disk image
;      also images of the individual filaments at 2x magnification
;
; Plus some bug fixes
;
; Revision 3.16  2010/03/08 21:21:55  bernapn1
; Improved
;
; Revision 3.15  2010/02/04 19:55:27  bernapn1
; Updated the screen output messages
;
; Revision 3.14  2010/02/03 20:11:18  bernapn1
; Updated ORIGIN output in voevent
;
; Revision 3.13  2010/01/25 17:21:20  bernapn1
; Updated the revision number from 5.2 to 5.3
;
; Revision 3.12  2010/01/25 16:52:32  bernapn1
; run_analyze: Included keep_fits flag
; preprocess:  Now if beyond Sun is NOT dark makes it dark (mostly for KANZ)
; solar_filaments: Changes slightly the size of fonts in z-buffer
; analyze_image: updated
;
; Revision 3.11  2010/01/21 22:04:28  bernapn1
; Small irrelevant change
;
; Revision 3.10  2010/01/21 21:56:14  bernapn1
; Adapted code to SDO FFT requirements. Added capability to generate VOEvent
; metadata in XML format for each filament identified
;
; Revision 3.9  2009/12/18 21:52:24  bernapn1
; Improved fainter filaments detection
; Improved connection of nearby filaments
; Added MAX_DIST paramenter in input file (adds more flexibility)
;
; Revision 3.8  2008/11/07 14:42:29  bernapn1
; Updated
;
; Revision 3.6  2005/01/06 22:37:22  pietro
; Now capable of handeling also the z-buffer device
;
; Revision 3.5  2004/12/30 19:24:53  pietro
; Now getting the default paths from filam_get_paths.
;
; Revision 3.4  2004/12/23 16:31:11  pietro
; Changed the call for preprocess.pro to be in sinc with the new calling
; protocoll for preprocess.
;
; Revision 3.3  2004/11/23 21:45:38  pietro
; Improved labeling of png image
;
; Revision 3.2  2004/11/23 15:46:44  pietro
; Now before creating the dispay window it checks if one already exist and
; if it has the correct dimensions. If not then creates it.
;
; Revision 3.1  2004/11/19 22:35:37  pietro
; Improved the data output
;
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.10  2004/10/26 14:15:29  pietro
; Added output keyword OUTPUT_TABLE
;
; Revision 2.9  2004/10/25 18:51:58  pietro
; Added some documentation
;
; Revision 2.8  2004/07/23 13:43:51  hakimd
; Added a -f to make gzip force overwriting the data file instead
; of asking for confirmation
;
; Revision 2.7  2004/07/15 19:59:54  pietro
; - Corrected check of exit status from preprocess
; - Added spine length in filam_data structure
;
; Revision 2.6  2004/06/29 19:59:24  pietro
; Now it passes REQ_SLOPE to study_filaments
;
; Revision 2.5  2004/06/25 17:38:41  pietro
; Now calls w_interactive_disp instead of the simpler interactive_disp
;
; Revision 2.4  2004/06/22 14:04:16  pietro
; fixed bug with true color display
;
; Revision 2.3  2004/06/21 20:05:24  pietro
; Fixed bug with the color identification now tested and works
;
; Revision 2.2  2004/06/21 18:04:00  pietro
; Adde color code cyan_c
;
; Revision 2.1  2004/06/21 17:42:30  pietro
; Now it can handle colors also for not True Color devices. It uses a
; 	common block named "colortab". If the display is not a True Color
; 	display it calls the routine make_colortab to download the appropriate
; 	color table with all the colors needed.
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.6  2004/06/16 20:33:09  pietro
; don't know
;
; Revision 1.5  2004/06/15 21:13:53  pietro
; Fixed an output text misalignment
;
; Revision 1.4  2004/06/15 17:58:24  pietro
; Removed the printing of an extra empty line at the end of the *.tab file
;
; Revision 1.3  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;    06/10/04: Status variable initialized at beginnig. Added some comments.
;    05/10/04: Added STATUS keyword
;    04/05/04: Added option to save current image in a png file
;    Written by Pietro N. Bernasconi JHU/APL: February 9, 2004. derived
;	form a routine originally written by Kiri Wagstaff, JHU/APL.
;
