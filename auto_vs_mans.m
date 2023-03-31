% Script for comparing automatic and manual results of filaments from
% 2000-2009:

clear all;

afid=fopen('automatic_filenames.txt');
mfid=fopen('manual_filenames.txt');

start_yr=2000;
end_yr=2016;
month=08;

start_jd=juliandate(datetime([start_yr,01,01,00,00,00 ]));
jds=linspace(start_jd, start_jd+365.24*(end_yr-start_yr+1),end_yr-start_yr+2);

for fileno=1:215
    
    % Get the names of the files:
    autofile=fgets(afid);
    manfile=fgets(mfid);
    
    if(strcmp(autofile,manfile(5:end))~=1)
        disp(strcmp(autofile,manfile(5:end)))
        error('The automatic and manual filenames are different!')
    end
    
    yy=str2num(autofile(1:4));
    mm=str2num(autofile(5:6));
    dd=str2num(autofile(7:8));

    hh=str2num(autofile(10:11));
    mn=str2num(autofile(12:13));
    ss=str2num(autofile(14:15));
    
    Date=[yy,mm,dd,hh,mn,ss];
    jd(fileno)=juliandate(datetime(Date));
    
    % Open the current files:
    fid1=fopen(sprintf('%s',autofile(1:end-1)));
    fid2=fopen(sprintf('%s',manfile(1:end-1)));
    
    for i=0:12
        line1=fgets(fid1);
        line2=fgets(fid2);
    end
    
    while(line1~=-1)
        no=str2num(line1(1:4));
        area(fileno,no)=str2num(line1(6:12));
        arat(fileno,no)=str2num(line1(14:18));
        xp(fileno,no)=str2num(line1(19:23));
        yp(fileno,no)=str2num(line1(24:28));
        lat(fileno,no)=str2num(line1(31:36));
        lon(fileno,no)=str2num(line1(37:43));
        ang(fileno,no)=str2num(line1(45:50));
        len(fileno,no)=str2num(line1(52:55));
        nbarbs1(fileno,no)=str2num(line1(57:60));
        nrite1(fileno,no)=str2num(line1(61:64));
        nleft1(fileno,no)=str2num(line1(65:68));
        chir1(fileno,no)=str2num(line1(69:72));
        nbarbs2(fileno,no)=str2num(line2(57:60));
        nrite2(fileno,no)=str2num(line2(61:64));
        nleft2(fileno,no)=str2num(line2(65:68));
        chir2(fileno,no)=str2num(line2(69:72));
        
        % Get the next lines from the files:
        line1=fgets(fid1);
        line2=fgets(fid2);
    end
    
    nlen(fileno)=no;
    fclose(fid1);
    fclose(fid2);
    
end

fclose(afid);
fclose(mfid);

% All data loaded, now binning it yearly:

for i = 1:length(jds)-1
   [tmp,filenos]=select1d(jd,jds(i),jds(i+1));
   if(filenos~=0)
      no=0;
      for j=1:length(filenos)
       area_yr(i,no+1:no+nlen(filenos(j)))=area(filenos(j),1:nlen(filenos(j))); 
       arat_yr(i,no+1:no+nlen(filenos(j)))=arat(filenos(j),1:nlen(filenos(j))); 
         xp_yr(i,no+1:no+nlen(filenos(j)))=xp(filenos(j),1:nlen(filenos(j))); 
         yp_yr(i,no+1:no+nlen(filenos(j)))=yp(filenos(j),1:nlen(filenos(j))); 
         lat_yr(i,no+1:no+nlen(filenos(j)))=lat(filenos(j),1:nlen(filenos(j))); 
         lon_yr(i,no+1:no+nlen(filenos(j)))=lon(filenos(j),1:nlen(filenos(j))); 
         ang_yr(i,no+1:no+nlen(filenos(j)))=ang(filenos(j),1:nlen(filenos(j))); 
         len_yr(i,no+1:no+nlen(filenos(j)))=len(filenos(j),1:nlen(filenos(j))); 
         nbarbs1_yr(i,no+1:no+nlen(filenos(j)))=nbarbs1(filenos(j),1:nlen(filenos(j))); 
         nrite1_yr(i,no+1:no+nlen(filenos(j)))=nrite1(filenos(j),1:nlen(filenos(j))); 
         nleft1_yr(i,no+1:no+nlen(filenos(j)))=nleft1(filenos(j),1:nlen(filenos(j))); 
         chir1_yr(i,no+1:no+nlen(filenos(j)))=chir1(filenos(j),1:nlen(filenos(j))); 
         nbarbs2_yr(i,no+1:no+nlen(filenos(j)))=nbarbs2(filenos(j),1:nlen(filenos(j))); 
         nrite2_yr(i,no+1:no+nlen(filenos(j)))=nrite2(filenos(j),1:nlen(filenos(j))); 
         nleft2_yr(i,no+1:no+nlen(filenos(j)))=nleft2(filenos(j),1:nlen(filenos(j))); 
         chir2_yr(i,no+1:no+nlen(filenos(j)))=-chir2(filenos(j),1:nlen(filenos(j))); 
         no=no+nlen(filenos(j));
      end
      ndata_yr(i)=no;
   end
end

% Calculating Hemispheric Chirality
for i=1:length(jds)-1
    pos1=find(yp_yr(i,:)>0);
    pos2=find(yp_yr(i,:)<0);
    
    chir1_hem1(i)=mean(chir1_yr(i,pos1));
    chir1_hem2(i)=mean(chir1_yr(i,pos2));
    chir2_hem1(i)=mean(chir2_yr(i,pos1));
    chir2_hem2(i)=mean(chir2_yr(i,pos2));
end

% Calculating Matches/mismatches excluding undetermined:

chirm1=chir1_yr(find(chir1_yr~=0 | chir2_yr~=0));
chirm2=chir2_yr(find(chir1_yr~=0 | chir2_yr~=0));
chirm3=chir1_yr(find(chir1_yr==1 & chir2_yr==0));
chirm4=chir1_yr(find(chir1_yr==0 & chir2_yr==1));
chirm5=chir1_yr(find(chir1_yr==-1 & chir2_yr==0));
chirm6=chir1_yr(find(chir1_yr== 0 & chir2_yr==-1));
chirm7=chir1_yr(find(chir1_yr==-1 & chir2_yr==-1));
chirm8=chir1_yr(find(chir1_yr== 1 & chir2_yr== 1));
chirm9=chir1_yr(find(chir1_yr== 1 & chir2_yr==-1));
chirm10=chir1_yr(find(chir1_yr== -1 & chir2_yr== 1));
disp(sprintf('Filament changes fron 1 to 0=%f percent', 100*length(chirm3)/length(chirm1)))
disp(length(chirm3))
disp(length(chirm4))
disp(length(chirm5))
disp(length(chirm6))
disp(length(chirm7))
disp(length(chirm8))
disp(length(chirm9))
disp(length(chirm10))
disp(length(chirm1))
disp(sprintf('Matching results= %f percent',100*length(find(chirm1==chirm2))/length(chirm1)))
disp(sprintf('Filaments which are opposite= %f percent',100*length(find(chir1_yr.*chir2_yr<0))/length(chirm1)))
disp(sprintf('Filaments which changes 0 to 1= %f percent',100*length(find(chirm1.*chirm2==0))/length(chirm1)))
% Calculating how many filaments match including undetermined:
matches=0;
for i=1:length(jds)-2
    matches=matches+length(find(chir1_yr(i,1:ndata_yr(i))==chir2_yr(i,1:ndata_yr(i))));
end
disp(sprintf('Complete Matches(including 0s)= %f percent',100*matches/sum(ndata_yr)))
