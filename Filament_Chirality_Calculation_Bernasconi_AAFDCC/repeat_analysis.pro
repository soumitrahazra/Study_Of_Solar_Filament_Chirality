;******************************************************************************
;***                                                                        ***
    function repeat_analysis, startdate, enddate, FILAM_DATA=fil_pars, $
                           IMAGES_PATH  = img_path, $
                           RESULTS_PATH = res_path, $
                           DO_VOEVENT = do_voevent, $
                           VOEVENT_PATH = voevent_path, $
                           ALLOW_VOE_UPDATE = allow_voe_update, $
                           DOWNLOAD = download, $
                           CHECKDISK = checkdisk, $
                           BBSO_ONLY = bbso_only, $
                           KANZ_ONLY = kanz_only, $
                           ANALYZE  = analyze, $
                           TRACK    = do_track, $
                           MERGE    = do_merge, $
                           NOAPPEND = no_append, $
                           SAVE_TABLE = save_table, $
                           SAVE_MASK  = save_mask, $
                           SAVE_PNG   = save_png, $
                           SAVE_FILAM_PNG = save_filam_png, $
                           SAVE_DAT   = save_dat, $
                           KEEP_FITS  = keep_fits, $
                           VERBOSE = verbose, BELL = bell, $
                           INTERACTIVE = do_interactive, $
                           C_PATH = c_path
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    REPEAT_ANALYSIS
;
; PURPOSE:
;    Run all the different analysis steps, downloading, analyzing
;    individual images and tracking, all at the same time over a series
;    of days 
;
; CALLING SEQUENCE:
;                      (startDate)  (endDate)
;     repeat_analysis, [yyyy,mm,dd],[yyyy,mm,dd], images_path=imagespath, $
;                      res_path=res_path, /download, /checkdisk, /analyze, $
;                      /track, /merge, /noappend, /verbose, /bell
;
; INPUTS:
;    startdate = intarr(3): array with the start date of the form [yyyy,mm,dd]
;      enddate = intarr(3): array with the end of the form [yyyy,mm,dd]. If
;                   this is omitted then the start date is also used as
;                   end date and only that day is processed.
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    IMAGES_PATH = string: path in file system where the images are (will
;        be) stored. Default is given by the call to the routine:
;        filam_get_paths(/images)
;    RES_PATH    = string: path in file system where results of analysis and
;        tracking are stored. Default is given by the call to the routine:
;        filam_get_paths(/results)
;    DOWNLOAD   = Flag: set to 1 to tell analyze_web_image to download
;        images it does not have
;    CHECKDISK = Flag: if set then before do the download it checks if the
;        image is already stored in the file system
;    ANALYZE    = Flag: set to 1 to tell analyze_web_image to call solar
;        filaments to analyze images
;    TRACK      = Flag: set to 1 to call repeat_track for the same start
;        date and end date
;    MERGE      = Flag: set if you want to create the big merged table
;    NOAPPEND   = Flag: used only if MERGE is set. If set then a new
;        file named "TrackdFilaments" is created instead to append the new
;        merged data to the old file.
;    ANGLE_TOLERANCE = float: helio graphic angle (in degrees) of the
;         acceptable distance from predicted filament position to track
;         a filament. Default = 5 degrees
;    VERBOSE    = Flag: If set then print some infos.
;    BELL = Flag: If set then at end of the execution rings 3 times the
;        terminal bell. Useful to warn that the process is finished.
; 
; OUTPUTS:
;    None
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES USED:
;    download_image, analyze_image, make_datestring, track_filaments,
;    merge_track
;
; PROCEDURE:
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************

;------------------------------------------------------------------------------
;---- This section handles the input parameters and KEYWORDS: -----------------
if (n_params() lt 1) then message,'At least one input parameter (startDate) '+$
  'is required!'
if (n_elements(startdate) lt 3) then message,'startDate (1st param) must be'+$
  ' a 3 elements array.'
if (n_params() eq 1) then enddate = startdate
if (n_elements(enddate) lt 3) then message,'endDate (2nd param) must be a 3'+$
  ' elements array.'

if (n_elements(img_path) eq 0) then img_path = filam_get_paths(/images)
if (n_elements(res_path) eq 0) then res_path = filam_get_paths(/results)
if (n_elements(voevent_path) eq 0) then voevent_path = filam_get_paths(/voe)
if (n_elements(download) eq 0) then download = 0
if (n_elements(checkdisk) eq 0) then checkdisk = 0
if (n_elements(bbso_only) eq 0) then bbso_only = 0
if (n_elements(kanz_only) eq 0) then kanz_only = 0
if (n_elements(analyze)  eq 0) then analyze  = 0
if (n_elements(do_track) eq 0) then do_track = 0
if (n_elements(do_merge) eq 0) then do_merge = 0
if (n_elements(no_append) eq 0) then no_append = 0
if (n_elements(keep_fits) eq 0) then keep_fits = 0
if (n_elements(verbose)  eq 0) then verbose  = 0
;------------------------------------------------------------------------------

status = 0

;------------------------------------------------------------------------------
;---- Sets the start and end dates of the repeated analysis: ------------------
;--- Computes the julian day for the given dates:
start_jday = julday(startdate(1),startdate(2),startdate(0))
end_jday = julday(enddate(1),enddate(2),enddate(0))

;--- Swaps start and end date if in wrong order:
if (start_jday gt end_jday) then begin
    print, 'The enddate is before the startdate, --reversing them'

    tmp = startdate
    startdate = enddate
    enddate = tmp

    tmp = start_jday
    start_jday = end_jday
    end_jday = tmp
endif
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
;---- Download (if needed ) and do analysis of multiple days in sequence: -----
if ( (analyze) or (download) ) then begin
    from_all = 1
    frm_obs = ''
    obs_wldk = ''
    if (bbso_only) then begin
        frm_obs  = ' from Big Bear Solar Observatory'
        obs_wldk = 'bbso'
        from_all = 0
    endif
    if (kanz_only) then begin
        frm_obs  = ' fom KANZELHOEHE'
        obs_wldk = 'kanz'
        from_all = 0
    endif

    print
    if (analyze and download) then print,'  DOWNLOAD and ANALYZE Ha images' $
    else if analyze then print,'  ANALYZE Ha images' $
    else print,'  DOWNLOAD Ha images'+frm_obs
    print,'===================================================='
    print,'===================================================='
    print

    now_jday = start_jday

    ;--- Cycles  all the selected days and upon request download & analyzes
    while (now_jday le end_jday) do begin
        caldat, now_jday, month, day, year

        print
        print,'  Working on date: ', $
          make_datestring([year,month,day],sep='/')
        print,'****************************************************'

        cnt = 0
        status = 0
        image_name = ''

        ;--- Do the download: -------------------------------------------------
        if (download) then begin
            if (checkdisk) then begin
                ;-- FIrst checks if the image is already on disk:
                tmpnam = make_datestring(year,month,day)
                image_name=file_search(img_path+obs_wldk+'*'+tmpnam+'*.fts*',$
                                       count=cnt)
            endif

            ;-- If not then tries to download it from the Ha network:
            if (cnt le 0) then begin
                ;- Checks the BBSO ftp site:
                if (from_all) or (bbso_only) then begin
                    status = download_image([year,month,day], /BBSO, $
                                            out_image = image_name, $
                                            img_path = img_path, $
                                            verbose=verbose)
                endif else status = 1

                ;- If nothing found then checks the KANZ ftp site:
                if ((status eq 1) and (from_all or kanz_only)) then begin
                    print
                    status = download_image([year,month,day], /KANZ, $
                                            out_image=image_name, $
                                            img_path=img_path, verbose=verbose)
                endif

                if (status eq 0) then cnt = 1
            endif else begin
                if not(analyze) then begin
                    if (cnt eq 1) then $
                      print,' 1  image already on file for this date' $
                    else $
                      print,format='(I3,"  images already on file'+$
                            ' for this date.")', cnt
                endif
            endelse
            print
        endif
        ;----------------------------------------------------------------------

        ;--- do the analysis: -------------------------------------------------
        if (analyze) then begin

            if not(download) then begin
                tmpnam = make_datestring(year,month,day)
                image_name = findfile(img_path+'*'+tmpnam+'*.fts*',count=cnt)
            endif

            case cnt of
                0 : print,' No images for this date!'
                1 : print,' 1  image for this date. Analyzing it'
                else: print,format='(I3,"  images for this date.'+$
                            ' Analyzing all of them")', cnt
            endcase
            print

            ;-- Orders the images correctly in time:
            if (cnt gt 1) then begin
                repeat begin
                    swapped = 0
                    for i=1,cnt-1 do begin
                        pp = strpos(image_name(i-1),'.')
                        time0 = long(strmid(image_name(i-1),pp-6,6))
                        pp = strpos(image_name(i),'.')
                        time1 = long(strmid(image_name(i),pp-6,6))
                        if (time0 gt time1) then begin
                            image_name(i-1:i) = shift(image_name(i-1:i),1)
                            swapped = 1
                        endif
                    endfor
                endrep until(swapped eq 0)
                
            endif

            ;-- Cycles through all the images and do the analysis:
            for i=0,cnt-1 do begin
                status = analyze_image(image_name(i), filam_data = fil_pars, $
                                       res = res_path, $
                                       do_voevent = do_voevent, $
                                       voevent_path = voevent_path, $
                                       allow_voe_update = allow_voe_update, $
                                       save_table = save_table, $
                                       save_mask = save_mask, $
                                       do_track = do_track, $
                                       save_png = save_png, $
                                       save_filam_png = save_filam_png, $
                                       SAVE_DAT = save_dat, $
                                       interactive = do_interactive, $
                                       c_path=c_path, /verbose)
                print,'*****************************************************'+$
                  '**************************'

                if not(keep_fits) then begin
                    print,'!WARNING! the fits file:'
                    print,'   ',image_name
                    print,'WILL BE REMOVED from local file system!'
                    file_delete,image_name
                endif

                print
            endfor
        endif
        ;----------------------------------------------------------------------

        ;-- Increase the day by one to go to the next day:
        now_jday = now_jday+1
    endwhile
endif
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


if (do_merge) then begin
    ;--- Merging all the tracking tables into one big table:
    ;merge_track, startdate, enddate, trackLoc=res_path, NOAPPEND=no_append, $
    ;  verbose=verbose
    print,'!! MERGE is disabled for now.'
endif


;---- Rings a bell when is done
if (keyword_set(bell)) then begin
    for i=1,3 do begin
        wait,0.1
        print,string(7b)
    endfor
endif

return, status
end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: repeat_analysis.pro,v $
; Revision 3.14  2010/10/27 20:54:08  bernapn1
; Added keyword checklist and its functionality
;
; Revision 3.13  2010/05/06 14:33:50  bernapn1
; Several changes and improvements.  Now using a new technique to track
; filaments
;
; Revision 3.12  2010/03/29 21:11:19  bernapn1
; Added improvements:
;   1) Now the output file names consistently have YYYYMMDD instead of the
;      old YYYY_MM_DD
;   2) Now there is the possiblity to save in addition to the full disk image
;      also images of the individual filaments at 2x magnification
;
; Plus some bug fixes
;
; Revision 3.11  2010/02/04 19:55:27  bernapn1
; Updated the screen output messages
;
; Revision 3.10  2010/01/21 22:00:52  bernapn1
; Not sure what was done but small changes to improve performance
;
; Revision 3.9  2009/12/18 21:52:24  bernapn1
; Improved fainter filaments detection
; Improved connection of nearby filaments
; Added MAX_DIST paramenter in input file (adds more flexibility)
;
; Revision 3.8  2005/01/05 15:44:15  pietro
; Added keyword NOAPPEND that is passed to merge_track.
;
; Revision 3.7  2005/01/03 14:20:44  pietro
; Added the option to print a bell when process is done.
;
; Revision 3.6  2005/01/03 14:10:27  pietro
; Fixed a bug in the logic to determine wheter to perform the analysis or
; not.
;
; Revision 3.5  2004/12/30 19:24:53  pietro
; Now getting the default paths from filam_get_paths.
;
; Revision 3.4  2004/12/23 16:29:54  pietro
; Now if only the start date is given it puts also the end date as the same
; causing the code to process only the given date in start date.
; Improved the imput error detection.
;
; Revision 3.3  2004/12/02 21:28:54  pietro
; Fixed an output problem ad added verbose features
;
; Revision 3.2  2004/12/01 15:56:08  pietro
; *** empty log message ***
;
; Revision 3.1  2004/11/19 22:36:25  pietro
; Improved the functionality and readibility of the routine, AND improved the
; output
;
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.10  2004/10/26 22:20:52  pietro
; Improved and tested
;
; Revision 2.9  2004/10/26 21:44:09  pietro
; Improved and tested
;
; Revision 2.8  2004/10/26 16:09:21  pietro
; Minor modifications
;
; Revision 2.7  2004/10/26 14:33:22  pietro
; Removed some unnecessary subroutines written by Daniel and improved
; readability of code
;
; Revision 2.6  2004/08/24 20:19:55  pietro
; Made code more readable
;
; Revision 2.5  2004/07/23 16:10:45  hakimd
; Added modification log and comments.
;  Also told track_day to default dss to 20 before passing to track
;
;Written by Daniel Hakim JHU/APL
