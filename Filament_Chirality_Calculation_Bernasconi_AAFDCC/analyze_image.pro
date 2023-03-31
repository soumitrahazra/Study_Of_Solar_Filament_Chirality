;******************************************************************************
;***                                                                        ***
    function analyze_image, ha, FILAM_DATA=fil_pars, $
                    IN_PATH = in_path, $
                    RESULTS_PATH = res_path, $
                    DO_VOEVENT = do_voevent, $
                    VOEVENT_PATH = voevent_path, $
                    ALLOW_VOE_UPDATE = allow_voe_update, $
                    OUTPUT_TABLE = out_tab, $
                    SAVE_TABLE = save_table, $
                    SAVE_MASK  = save_mask, $
                    DO_TRACK   = do_track, $
                    SAVE_PNG = save_png, $
                    SAVE_FILAM_PNG = save_fil, $
                    SAVE_DAT = save_dat, $
                    VERBOSE = verbose, $
                    INTERACTIVE = interactive, $
                    C_PATH = c_path
;***                                                                        ***
;******************************************************************************
;+
; PROJECT: Automated Solar Filament Detection and Characterization
;
; NAME:
;    ANALIZE_IMAGE
;
; PURPOSE:
;    This function analyzes an H-alpha image. It first creates an input
;    file with all the start parameters to be fed to the routine
;    solar_filaments.
;
; CALLING SEQUENCE:
;    status = analyze_image(ha, [FILAM_DATA=fil_pars, $
;                           IN_PATH=img_path, RESULTS_PATH =res_path,$
;                           DO_VOEVENT=do_voevent, VOEVENT_PATH=voevent_path, $
;                           ALLOW_VOE_UPDATE = allow_voe_update, $
;                           OUTPUT_TABLE = output_table, $
;                           /SAVE_TABLE, /SAVE_MASK, /DO_TRACK, /SAVE_PNG, $
;                           /SAVE_FILAM_PNG, /SAVE_DAT,
;                           /VERBOSE, /INTERACTIVE, C_PATH=c_path])
;
; INPUTS:
;    ha = string: Name of the input fits file with the Ha image to be
;                 analyzed
;
; OPTIONAL INPUT PARAMETERS:
;    NONE
;
; KEYWORD PARAMETERS:
;    FILAM_DATA  = structure_array(nFilaments): structure containing all
;                         the filaments data. See study_filaments for a
;                         detailed description of the structure content.
;    IN_PATH = string: Path in the local file system where the
;                image is stored. Default is given by the call to the
;                routine: filam_get_paths(/images)
;    RESULTS_PATH = string: Path in the local file system where the results
;                of the analysis will be saved. Default is given by the
;                call to the routine: filam_get_paths(/results)
;    DO_VOEVENT = flag: if set then for each filam saves a VOEvent XML
;    VOEVENT_PATH = string: Path in the local file system where the VOEvent
;                metadata XML files are stored.
;    ALLOW_VOE_UPDATE = flag: if set then allows to update a VOEvent already
;                generated.
;    OUTPUT_TABLE = string: name (with full path) of the table where the
;                results of the filament are printed.
;    SAVE_TABLE = flag: if set then the an ASCII table is saved
;    SAVE_MASK = flag: if set then saves the clusters mask and sun_p
;    DO_TRACK = flag: if set then it performs filament tracking in the past
;    SAVE_PNG = flag: if set then the the result image is output in png format
;    SAVE_DAT = flag: if set then result table is stored in an idl save file
;    VERBOSE = flag: If set then some additional information is dispalyed
;                on the screen
;    INTERACTIVE = flag: if set then after calculating all the filaments
;                     the code enters an interactive mode that allows to
;                     view different areas with more detail.
;    C_PATH = string: Directory in the local file system where the shell
;                scripts and c-code executable necessary for this
;                application are stored. Default is given by the
;                call to the routine: filam_get_paths(/bin)
;
; OUTPUTS:
;    status = int: 0 if no errors were encountered
;                  1 if some type of error occurred
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES CALLED:
;    create_input_file, solar_filaments
;
;-
; MODIFICATION HISTORY: See end of file
;******************************************************************************

;---- Creates the 'temporary' input file with well tested default parameters:
create_input_file, 'tmp.in', ha, $
  IN_PATH  = in_path, $
  OUT_PATH = res_path, $
  VOE_PATH = voevent_path, $
  MASK_DEG = 75.0, $
  I_THRSH  =-750, $
  U_THRSH  =-500, $
  MIN_AREA = 250, $
  SMTH_IMG = 0, $
  BARB_ANG = 5.0, $
  MAX_DIST = 35, $
  FIX_BGR  = 1, $
  SHOW_PIX = 0, $
  SHOW_BBX = 0, $
  SHOW_BND = 1, $
  SHOW_SPN = 1, $
  DO_VOE   = do_voevent, $
  VOE_UPDT = allow_voe_update, $
  DO_TRACK = do_track, $
  DISP_DIM = 0.49, $
  SAVE_TAB = save_table, $
  SAVE_MSK = save_mask, $
  SAVE_PNG = save_png, $
  SAVE_FIL = save_fil, $
  SAVE_DAT = save_dat

;---- Do the analysis of the image:
solar_filaments, 'tmp.in', FILAM_DATA=fil_pars, VERBOSE=verbose, $
  STATUS=status, OUTPUT_TAB = out_tab, C_PATH=c_path, INTERACTIVE=interactive

spawn,'/bin/rm tmp.in'

return, status

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: analyze_image.pro,v $
; Revision 3.15  2010/10/27 20:59:31  bernapn1
; Now barb angle is set to 5.0 deg
;
; Revision 3.14  2010/05/06 14:33:49  bernapn1
; Several changes and improvements.  Now using a new technique to track
; filaments
;
; Revision 3.13  2010/04/29 20:52:14  bernapn1
; Added input keywords DO_TRACK and SAVE_MASK
;
; Revision 3.12  2010/04/13 21:03:30  bernapn1
; Improved merging of filaments (written new routine).
; Improved way to remove small detected filaments.  Now before removing one it
; checks its area ratio. If it is a skinny one then it keeps it.
;
; Revision 3.11  2010/03/29 21:11:18  bernapn1
; Added improvements:
;   1) Now the output file names consistently have YYYYMMDD instead of the
;      old YYYY_MM_DD
;   2) Now there is the possiblity to save in addition to the full disk image
;      also images of the individual filaments at 2x magnification
;
; Plus some bug fixes
;
; Revision 3.10  2010/03/26 19:06:54  bernapn1
; Addeed keyword SAVE_FILAM_PNG
;
; Revision 3.9  2010/03/10 20:03:39  bernapn1
; Added keyword FILAM_DATA to allow to retrieve the strucure that
; solar_filaments generates with all the filaments parameters
;
; Revision 3.8  2010/01/25 16:52:32  bernapn1
; run_analyze: Included keep_fits flag
; preprocess:  Now if beyond Sun is NOT dark makes it dark (mostly for KANZ)
; solar_filaments: Changes slightly the size of fonts in z-buffer
; analyze_image: updated
;
; Revision 3.7  2010/01/21 21:44:48  bernapn1
; Added calls for MASK_DEG, DO_VOE, and VOE_PATH to input file
;
; Revision 3.6  2009/12/18 21:52:24  bernapn1
; Improved fainter filaments detection
; Improved connection of nearby filaments
; Added MAX_DIST paramenter in input file (adds more flexibility)
;
; Revision 3.5  2004/12/30 19:24:53  pietro
; Now getting the default paths from filam_get_paths.
;
; Revision 3.4  2004/12/23 19:28:16  pietro
; Changet the min area from 250 to 300
;
; Revision 3.3  2004/12/01 15:56:08  pietro
; *** empty log message ***
;
; Revision 3.2  2004/11/23 15:47:49  pietro
; Now DISP_DIM is set to 0.49 (instead of 0.5) to make window fit in screen.
;
; Revision 3.1  2004/11/22 21:44:30  pietro
; Tested
;
; Revision 3.0  2004/10/27 14:38:17  pietro
; Given revision 3.0
;
; Revision 2.9  2004/10/26 14:19:14  pietro
; Added keyword OUTPUT_TABLE
;
; Revision 2.8  2004/08/24 17:32:10  pietro
; Improved the logic in checking if the Ha image is legitimate
;
; Revision 2.7  2004/07/22 15:35:08  hakimd
; Added a check to see that Ha exists before seeing if it contains strings
; -Daniel Hakim
;
; Revision 2.6  2004/07/22 12:36:43  pietro
; Fixed bug when ha is not defined but a strpos(ha, 'bbso') is called
;
; Revision 2.5  2004/07/21 12:39:55  pietro
; Now checks that the image originates only from BBSO or KANZELHOEHE
;
; Revision 2.4  2004/07/16 21:20:02  pietro
; Defined download and analyze flags at beginnig
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.3  2004/06/11 15:17:45  pietro
; Added log tag at end
;
; Written by Pietro N. Bernasconi JHU/APL: 05/10-13/04
