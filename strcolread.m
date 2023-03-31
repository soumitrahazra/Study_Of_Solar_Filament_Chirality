function [ array ] = strcolread( filename,x,y )
% READ a file as characters x rows and y columns
% Beware of the last character on each line (Return)

names=fileread(filename);

n=1;
for i=1:x
    for j=1:y
        array(i,j)=names(n);
        n=n+1;
    end
end


end

