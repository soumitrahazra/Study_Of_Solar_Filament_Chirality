function [ data2,pos ] = select1d( data,min,max )
% [data,pos]=select1d(data,min,max)
%Returns elements of an array in the same order but selected to lie
% between the min and max values specified
%  

data2=0;
pos=0;

j=1;
for i=1:length(data)
    if(data(i)<=max)
        if(data(i)>=min)
            data2(j)=data(i);
            pos(j)=i;
            j=j+1;
        end
    end
end

% if(isempty(data2)==1)
%     data2=0;
%     pos=0;
% end

end